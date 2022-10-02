# This R scripts helps us generate data("d_500", package = "scBubbletree").
# These steps are already explained in Seurat's tutorial at:
# https://satijalab.org/seurat/articles/multimodal_reference_mapping.html

# necessary R-packages
install.packages("Seurat")
remotes::install_github("mojaveazure/seurat-disk")
library(Seurat)
library(SeuratDisk)
library(ggplot2)
library(patchwork)
library(SeuratData)


# download and then load the reference dataset (acessed 30. Sep. 2022):
# https://atlas.fredhutch.org/data/nygc/multimodal/pbmc_multimodal.h5seurat
reference <- LoadH5Seurat("../data/pbmc_multimodal.h5seurat")


# download 2,700 PBMCs -> query data
InstallData('pbmc3k')


# data normalization with SCTransform
pbmc3k <- SCTransform(pbmc3k, verbose = FALSE)


# find anchors between reference and query
anchors <- FindTransferAnchors(
  reference = reference,
  query = pbmc3k,
  normalization.method = "SCT",
  reference.reduction = "spca",
  dims = 1:50
)

# transfer cell type labels and protein data from reference to query
pbmc3k <- MapQuery(
  anchorset = anchors,
  query = pbmc3k,
  reference = reference,
  refdata = list(
    celltype.l1 = "celltype.l1",
    celltype.l2 = "celltype.l2",
    predicted_ADT = "ADT"
  ),
  reference.reduction = "spca",
  reduction.model = "wnn.umap"
)

# compute PCA
pbmc3k <- RunPCA(object = pbmc3k,
                 npcs = 20,
                 features = VariableFeatures(object = pbmc3k))


# draw 500 random cells from pbmc3k without replacement
set.seed(seed = 1234)
is <- sample(x = 1:nrow(pbmc3k@meta.data), size = 500, replace = F)


# f: get cell type predictions of annotation set l1 for 500 cells
f <- pbmc3k@meta.data$predicted.celltype.l1[is]


# fs: get normalized gene expressions for a number of marker genes of 500 cells
fs <- t(as.matrix(pbmc3k@assays$SCT@data[
  rownames(pbmc3k@assays$SCT@data) %in%
    c("IL7R",
      "CD14", "LYZ",
      "MS4A1",
      "CD8A",
      "GNLY", "NKG7",
      "FCGR3A", "MS4A7",
      "FCER1A", "CST3",
      "PPBP"), ]))
fs <- fs[is, ]


# A: get first 15 principal components of PCA matrix 
A <- pbmc3k@reductions$pca@cell.embeddings[is, 1:15]


# merged data in a list and save
d_500 <- list(f = f, fs = fs, A = A)
save(d_500, file = "data/d_500.RData")

