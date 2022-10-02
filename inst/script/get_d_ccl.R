# This scripts can be used to generate data("d_ccl", package = "scBubbletree")

# create directory
dir.create(path = "case_study/")

# download the data from:
https://github.com/LuyiTian/sc_mixology/raw/master/data/
  sincell_with_class_5cl.RData

# load the data
load(file = "case_study/sincell_with_class_5cl.RData")

# we are only interested in the 10x data object 'sce_sc_10x_5cl_qc'
d <- sce_sc_10x_5cl_qc

# remove the remaining objects (cleanup)
rm(sc_Celseq2_5cl_p1, sc_Celseq2_5cl_p2, sc_Celseq2_5cl_p3, sce_sc_10x_5cl_qc)

# get the meta data for each cell
meta <- colData(d)[,c("cell_line_demuxlet","non_mt_percent","total_features")]

# create Seurat object from the raw counts and append the meta data to it
d <- Seurat::CreateSeuratObject(counts = d@assays$data$counts,
                                project = '')

# check if all cells are matched between d and meta
# table(rownames(d@meta.data) == meta@rownames)
d@meta.data <- cbind(d@meta.data, meta@listData)

# cell type predictions are provided as part of the meta data
table(d@meta.data$cell_line)

# select 5,000 most variable genes
d <- Seurat::FindVariableFeatures(object = d,
                                  selection.method = "vst",
                                  nfeatures = 5000)

# Preprocessing with Seurat: SCT transformation + PCA
d <- SCTransform(object = d,
                 variable.features.n = 5000)
d <- RunPCA(object = d,
            npcs = 50,
            features = VariableFeatures(object = d))

# perform UMAP + t-SNE
d <- RunUMAP(d, dims = 1:15)
d <- RunTSNE(d, dims = 1:15)

# save the preprocessed data
save(d, file = "case_study/d.RData")

# save the PCA matrix 'A', meta data 'm' and
# marker genes matrix 'e'
d <- get(load(file ="case_study/d.RData"))
A <- d@reductions$pca@cell.embeddings[, 1:15]
m <- d@meta.data
e <- t(as.matrix(d@assays$SCT@data[
  rownames(d@assays$SCT@data) %in%
    c("ALDH1A1",
      "PIP4K2C",
      "SLPI",
      "CT45A2",
      "CD74"), ]))

d_ccl <- list(A = A, m = m, e = e)
save(d_ccl, file = "data/d_ccl.RData")