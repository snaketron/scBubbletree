# This R scripts helps us generate data("d_500", package = "scBubbletree").
# These steps are already explained in Seurat's tutorial at:
# https://satijalab.org/seurat/articles/multimodal_reference_mapping.html

# necessary R-packages
# install.packages("Seurat")
# remotes::install_github("mojaveazure/seurat-disk")
# devtools::install_github('satijalab/seurat-data')
library(Seurat)
library(SeuratDisk)
library(SeuratData)


# create temporary directory and download the reference dataset 
# (acessed on 30 Sep. 2022) from:
if(dir.exists("temp_folder")==FALSE) {
  dir.create(path = "temp_folder")
}
options(timeout = 6000)
utils::download.file(
  url = "https://atlas.fredhutch.org/data/nygc/multimodal/pbmc_multimodal.h5seurat",
  destfile = "temp_folder/pbmc_multimodal.h5seurat")


# load the reference dataset
reference <- SeuratDisk::LoadH5Seurat(
  file = "temp_folder/pbmc_multimodal.h5seurat")


# download 2,700 PBMCs -> query data
SeuratData::InstallData('pbmc3k')


# data normalization with SCTransform
pbmc3k <- Seurat::SCTransform(pbmc3k, verbose = FALSE)


# find anchors between reference and query
anchors <- Seurat::FindTransferAnchors(
  reference = reference,
  query = pbmc3k,
  normalization.method = "SCT",
  reference.reduction = "spca",
  dims = 1:50
)

# transfer cell type labels and protein data from reference to query
pbmc3k <- Seurat::MapQuery(
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
pbmc3k <- Seurat::RunPCA(object = pbmc3k,
                         npcs = 20,
                         features = Seurat::VariableFeatures(object = pbmc3k))


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
save(d_500, file = "temp_folder/d_500.RData")

