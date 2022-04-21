## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = F,
                      comment = F, 
                      warning = F, 
                      message = F)


## ---- echo = T, results='hide'------------------------------------------------
source(file = "~/Rutil/Init_Rpack.R")
# source(file = "~/Rutil/Graphics.R")
# source(file = "~/Rutil/Stats.R")

# source(file = "src/ClusteringUtil.R")
source(file = "../scBubbletree/R/util.R")
source(file = "../scBubbletree/R/main.R")
source(file = "../scBubbletree/R/annotation.R")

# library(farver, lib.loc = "/usr/local/lib/R/site-library")

#library(scBubbletree, lib.loc = lib.loc)
library(Seurat, lib.loc = lib.loc)
library(ggplot2, lib.loc = lib.loc)
library(reshape2, lib.loc = lib.loc)
library(parallel, lib.loc = lib.loc)
library(ape, lib.loc = lib.loc)
library(ggtree, lib.loc = lib.loc)
library(cluster, lib.loc = lib.loc)
library(org.Hs.eg.db, lib.loc = lib.loc)
library(bluster, lib.loc = lib.loc)
library(SummarizedExperiment, lib.loc = lib.loc)



## ---- echo=T, results=F-------------------------------------------------------
# Lets load the benchmark data
load(file = "raw_data/Tian_2019/sc_mixology-master/data/sincell_with_class_5cl.RData")


# We are only interested in the 10x data object 'sce_sc_10x_5cl_qc'
d <- sce_sc_10x_5cl_qc


# Remove the remaining objects
rm(sc_Celseq2_5cl_p1, sc_Celseq2_5cl_p2, sc_Celseq2_5cl_p3, sce_sc_10x_5cl_qc)


# Get the meta data for each cell
meta <- colData(d)[, c("cell_line", "cell_line_demuxlet", "demuxlet_cls")]


# Create Seurat object from the raw counts and append the meta data to it
d <- Seurat::CreateSeuratObject(counts = d@assays$data$counts,
                                project = '')

# check if all cells are matched between d and meta
# table(rownames(d@meta.data) == meta@rownames) 
d@meta.data <- cbind(d@meta.data, meta@listData)


# cell type predictions are provided as part of the meta data
table(d@meta.data$cell_line_demuxlet)

# Preprocessing with Seurat: SCT transformation + PCA + UMAP 
d <- SCTransform(object = d)
d <- RunPCA(object = d, npcs = 20, features = VariableFeatures(object = d))
d <- RunUMAP(d, dims = 1:20)


## ---- fig.width=8, fig.height=3.25, fig.align='center'------------------------
# Lets show the generated 2D UMAP
u <- UMAPPlot(object = d, 
              reduction = "umap",
              cols = 'black',
              pt.size = 0.25)|
  UMAPPlot(object = d, 
           reduction = "umap",
           group.by = 'cell_line_demuxlet',
           pt.size = 0.25)

u


## ---- echo = T----------------------------------------------------------------
# This is the main input of scBubbletree -> matrix A
A <- d@reductions$pca@cell.embeddings

# A has n=cells as rows, f=features as columns (e.g. from PCA)
dim(A)


## ---- fig.width=4, fig.height=3, echo=T, results=T----------------------------
b <- get_k(B = 20, 
           cv_clust_p = 1, 
           cv_gap_p = 1, 
           ks = 2:15,
           x = A,
           n_start = 10, 
           iter_max = 50, 
           cores = 20)


## ---- fig.width=9, fig.height=3, fig.align='center'---------------------------
ggplot(data = b$sil_stats_summary)+
  geom_point(aes(x = k, y = sil_median))+
  geom_errorbar(aes(x = k, y = sil_median, ymin = L95, ymax = H95), width = 0.1)+
  ggtitle(label = "Silhouette")|
ggplot(data = b$gap_stats_summary)+
  geom_point(aes(x = k, y = gap_median))+
  geom_errorbar(aes(x = k, y = gap_median, ymin = L95, ymax = H95), width = 0.1)+
  ggtitle(label = "Gap")|
ggplot(data = b$wss_stats_summary)+
  geom_point(aes(x = k, y = wss_median))+
  geom_errorbar(aes(x = k, y = wss_median, ymin = L95, ymax = H95), width = 0.1)+
  ggtitle(label = "WSS")


## ---- echo=T------------------------------------------------------------------
# Perform clustering to get data for scBubbletree
btd_k5 <- get_bubbletree_data(x = A,
                              k = 5,
                              seed = 1234,
                              cores = 1,
                              B = 50,
                              N_eff = 100,
                              verbose = F,
                              n_start = 100,
                              iter_max = 100,
                              round_digits = 1,
                              show_branch_support = T)


## ---- fig.width=4, fig.height=4, fig.align='center', echo=T-------------------
btd_k5$tree


## ---- echo=T, results=T-------------------------------------------------------
knitr::kable(btd_k5$tree_meta, digits = 1)


## ---- echo=T, results=T-------------------------------------------------------
# c_i, c_j = pair of clusters/bubbles
# M = mean inter-cluster dissimilarity
# L95/H95 = lower/upper bounds 95% confidence 
# interval of the mean dissimilarity
knitr::kable(btd_k5$pair_dist$hc_pair_dist, digits = 1)


## ---- fig.width=5, fig.height=4, fig.align='center', echo=T-------------------
ggtree::ggdensitree(btd_k5$ph$boot_ph, alpha=0.1, colour='steelblue')+
  geom_tiplab(size = 7)+
  hexpand(ratio = 0.5)


## ---- echo=T------------------------------------------------------------------
w1 <- get_cat_feature_tiles(k = btd_k5$cluster,
                            a = d@meta.data$cell_line_demuxlet,
                            tree_meta = btd_k5$tree_meta,
                            feature_composition = T,
                            round_digits = 1)


## ---- fig.width=7, fig.height=4, fig.align='center', echo=T-------------------
(btd_k5$tree|w1$w)+patchwork::plot_layout(ncol = 2, widths = c(1, 1))


## ---- fig.width=7, fig.height=4, fig.align='center', echo=T-------------------
w2 <- get_cat_feature_tiles(k = btd_k5$cluster,
                                a = d@meta.data$cell_line_demuxlet,
                                tree_meta = btd_k5$tree_meta,
                                feature_composition = TRUE) # <- changed

(btd_k5$tree|w2$w)+patchwork::plot_layout(ncol = 2, widths = c(1, 1))


## -----------------------------------------------------------------------------
# gini
get_gini(labels = d@meta.data$cell_line_demuxlet, 
         clusters = btd_k5$cluster)


## ---- echo = T----------------------------------------------------------------
gini_boot <- get_gini_boot(labels = d@meta.data$cell_line_demuxlet,
                           kmeans_boot_obj = b)


## ---- fig.width=4, fig.height=3, fig.align='center', echo=T-------------------
ggplot(data = gini_boot$total_gini_summary)+
  geom_point(aes(x = k, y = total_gini_median))+
  geom_errorbar(aes(x = k, y = total_gini_median, ymin = L95, 
                    ymax = H95), width = 0.1)+
  ggtitle(label = "Total Gini")


## ---- echo=T------------------------------------------------------------------
# First we need to select gene expressions for each cell and 
# also for five marker genes
as <- as.matrix(t(d@assays$SCT@data[
  rownames(d@assays$SCT@data) %in% 
    c("ALDH1A1", "OS9", "PEG10", "S100A9", "SLPI"), ]))

# 'as' is a matrix with n=rows for cells and a=columns for 
# annotations (genes). The column names will be shown in
# the plot.

# We will order the columns in 'as' in the same way we want
# them to be plotted. These genes are markers for: A549, 
# HCC827, H1975, H2228 and H838
as <- as[, c("ALDH1A1", "OS9", "PEG10", "S100A9", "SLPI")]



## ---- fig.width=7, fig.height=5, fig.align='center', echo=T-------------------
# This function build the nummeric annotations plot
w3 <- get_num_feature_tiles(k = btd_k5$cluster,
                               as = as,
                               tree_meta = btd_k5$tree_meta,
                               plot_title = "",
                               round_digits = 1)

# Plot
(btd_k5$tree|w3$w)+patchwork::plot_layout(widths = c(1, 1))


## ---- fig.width=9, fig.height=5, fig.align='center', echo=T-------------------
w4 <- get_num_feature_violins(k = btd_k5$cluster,
                             as = as,
                             tree_meta = btd_k5$tree_meta,
                             plot_title = "",
                             scales = 'free_x')

(btd_k5$tree|w3$w|w4$w)+patchwork::plot_layout(widths = c(1.5, 1, 2.5))


## ---- fig.width=10, fig.height=9, fig.align='center', echo=T------------------
(((btd_k5$tree|w3$w|w4$w)+patchwork::plot_layout(widths = c(1, 1, 2)))/
  ((w1$w|u)+patchwork::plot_layout(widths = c(1, 3))))


## ---- fig.width=4.5, fig.height=4---------------------------------------------
# w1 <- get_cat_feature_tiles(k = btd_k5$cluster,
#                                 a = d@meta.data$cell_line_demuxlet,
#                                 tree_meta = btd_k5$tree_meta,
#                                 feature_composition = F,
#                                 round_digits = 1,
#                                 rotate_x_axis = F)
# w1$w
# g <- (btd_k5$tree|w1$w)+patchwork::plot_layout(widths = c(1, 4))
# g
# ggsave(plot = g,
#        filename = "fig.pdf",
#        device = "pdf",
#        width = 4.75,
#        height = 4)

# btd_k5$pair_dist$



## -----------------------------------------------------------------------------
# save for rainy days
# case_study_A <- list(b = b, btd_k5 = btd_k5, d = d, meta = meta, A = A) 
# save(case_study_A, file = "case_study_A.RData")
# case_stdy_A <- get(load(file = "case_study_A.RData"))

# variable cleanup
rm(b, btd_k5, d, meta, w1, w2, w3, w4, A, as, u, gini_boot)


## ---- echo=T------------------------------------------------------------------
# To get the data used in this case study do the following steps:
# 1. download reference data from vignette:
# https://satijalab.org/seurat/articles/multimodal_reference_mapping.html
# 2. load SeuratDistk
library(SeuratDisk, lib.loc = lib.loc)
# 3. subset data at time=0
d <- LoadH5Seurat("~/BubbleMap/raw_data/Hao_2021/pbmc_multimodal.h5seurat")
save(d_complete, file = "Hao_2021_complete.RData")
# d_t0 <- subset(x = d, subset = time == 0)


## ---- echo=T------------------------------------------------------------------
d_t0 <- get(load(file = "raw_data/Hao_2021_t0.RData"))


## ---- echo=T------------------------------------------------------------------
table(d_t0@meta.data$celltype.l1,
      d_t0@meta.data$donor)


## ---- fig.width=12, fig.height=4, fig.align='center', echo=T------------------
# Lets show the generated 2D UMAP
UMAPPlot(object = d_t0, 
         reduction = "umap", 
         group.by = 'celltype.l1', 
         pt.size = 0.25)|
  UMAPPlot(object = d_t0, 
         reduction = "umap", 
         group.by = 'celltype.l2', 
         pt.size = 0.25)


## ---- fig.width=3.5, fig.height=3, fig.align='center', echo = T---------------
ElbowPlot(object = d_t0, ndims = 50, reduction = "pca")

# This is the main input of scBubbletree 
# -> matrix A with n=cells, f=features (from PCA)
A <- d_t0@reductions$pca@cell.embeddings[, 1:20]



## ---- fig.width=4, fig.height=3, echo = T-------------------------------------
# Determine appropriate number of clusters (k)
b <- get_k(B = 30,
           cv_clust_p = 1, # use 100% for clustering
           cv_gap_p = 0.1, # use 10% for gap stat.
           ks = 2:40,
           x = A,
           n_start = 10,
           iter_max = 50,
           cores = 20)


## ---- fig.width=10, fig.height=3, fig.align='center', echo=T------------------
ggplot(data = b$sil_stats_summary)+
  geom_point(aes(x = k, y = sil_median))+
  geom_errorbar(aes(x = k, y = sil_median, ymin = L95, ymax = H95), width = 0.1)+
  ggtitle(label = "Silhouette")|
ggplot(data = b$gap_stats_summary)+
  geom_point(aes(x = k, y = gap_median))+
  geom_errorbar(aes(x = k, y = gap_median, ymin = L95, ymax = H95), width = 0.1)+
  ggtitle(label = "Gap")|
ggplot(data = b$wss_stats_summary)+
  geom_point(aes(x = k, y = wss_median))+
  geom_errorbar(aes(x = k, y = wss_median, ymin = L95, ymax = H95), width = 0.1)+
  ggtitle(label = "WSS")


## ---- echo=T------------------------------------------------------------------
btd_k7 <- get_bubbletree_data(x = A,
                              k = 7,
                              n_start = 100,
                              iter_max = 100,
                              seed = 4321,
                              cores = 1,
                              B = 50,
                              N_eff = 100,
                              verbose = FALSE)


## ---- fig.width=6, fig.height=4, fig.align='center', echo=T-------------------
btd_k7$tree


## ---- fig.width=8, fig.height=4.5, fig.align='center', echo=T-----------------
# cell type annotations (level 1)
a <- d_t0@meta.data$celltype.l1 

# create tile plot
w <- get_cat_feature_tiles(k = btd_k7$cluster,
                               a = a,
                               tree_meta = btd_k7$tree_meta,
                               feature_composition = F,
                               round_digits = 1)

(btd_k7$tree|w$w)+patchwork::plot_layout(ncol = 2, widths = c(2, 1))


## ---- fig.width=8, fig.height=4.5, fig.align='center', echo=T-----------------
# cell type annotations (level 1)
a <- d_t0@meta.data$donor 

# create tile plot
w <- get_cat_feature_tiles(k = btd_k7$cluster,
                               a = a,
                               tree_meta = btd_k7$tree_meta,
                               feature_composition = T,
                               round_digits = 1)

(btd_k7$tree|w$w)+patchwork::plot_layout(ncol = 2, widths = c(2, 2))


## ---- fig.width=8, fig.height=4.5, fig.align='center', echo=T-----------------
# cell type annotations (level 1)
a <- d_t0@meta.data$Phase 

# create tile plot
w <- get_cat_feature_tiles(k = btd_k7$cluster,
                               a = a,
                               tree_meta = btd_k7$tree_meta,
                               feature_composition = F,
                               round_digits = 1)

(btd_k7$tree|w$w)+patchwork::plot_layout(ncol = 2, widths = c(2, 1))


## ---- echo=T------------------------------------------------------------------
# do clustering with k=12
btd_k12 <- get_bubbletree_data(x = A,
                              k = 12,
                              n_start = 100,
                              iter_max = 100,
                              seed = 4123,
                              cores = 1,
                              B = 50,
                              N_eff = 100,
                              verbose = FALSE)


## ---- fig.width=8, fig.height=6, fig.align='center', echo=T-------------------
# cell type annotations (level 1)
a <- d_t0@meta.data$celltype.l1 

# create tile plot
w <- get_cat_feature_tiles(k = btd_k12$cluster,
                               a = a,
                               tree_meta = btd_k12$tree_meta,
                               feature_composition = F,
                               round_digits = 1)

(btd_k12$tree|w$w)+patchwork::plot_layout(ncol = 2, widths = c(1, 1))


## ---- fig.width=9, fig.height=7, fig.align='center', echo=T-------------------
# cell type annotations (level 2)
a <- d_t0@meta.data$celltype.l2

# create tile plot
w <- get_cat_feature_tiles(k = btd_k12$cluster,
                               a = a,
                               tree_meta = btd_k12$tree_meta,
                               feature_composition = F,
                               round_digits = 0)

(btd_k12$tree|w$w)+patchwork::plot_layout(ncol = 2, widths = c(3, 9))


## ---- echo=T------------------------------------------------------------------
u_btd_k12 <- update_bubbletree_data(btd = btd_k12,
                                    updated_bubbles = c("5", "1", "2"),
                                    ks = c(5, 5, 5),
                                    cores = 20)


## ---- fig.width=12, fig.height=12, fig.align='center', echo=T-----------------
# create tile plot
w <- get_cat_feature_tiles(k = u_btd_k12$btd$cluster,
                               a = a,
                               tree_meta = u_btd_k12$tree_meta,
                               feature_composition = F,
                               round_digits = 0)

(u_btd_k12$tree|w$w)+patchwork::plot_layout(ncol = 2, widths = c(4, 9))


## ---- echo = T----------------------------------------------------------------
gini_l1 <- get_gini_boot(labels = d_t0@meta.data$celltype.l1,
                         kmeans_boot_obj = b)
gini_l2 <- get_gini_boot(labels = d_t0@meta.data$celltype.l2,
                         kmeans_boot_obj = b)
gini_l3 <- get_gini_boot(labels = d_t0@meta.data$celltype.l3,
                         kmeans_boot_obj = b)


## ---- fig.width=5, fig.height=3, fig.align='center'---------------------------
l1 <- gini_l1$total_gini_summary
l1$level <- "l1"

l2 <- gini_l2$total_gini_summary
l2$level <- "l2"

l3 <- gini_l3$total_gini_summary
l3$level <- "l3"

l_summary <- rbind(l1, l2, l3)

ggplot(data = l_summary)+
  geom_point(aes(x = k, y = total_gini_median, col = level))+
  geom_errorbar(aes(x = k, y = total_gini_median, ymin = L95, 
                    ymax = H95, col = level), width = 0.1)



## ---- fig.width=7, fig.height=6, echo=T---------------------------------------
# First we need to select gene expressions for each cell and 
# also for five marker genes
as <- as.matrix(t(d_t0@assays$SCT@data[
  rownames(d_t0@assays$SCT@data) %in% 
    c("IL7R", 
      "CD14", "LYZ", 
      "MS4A1", 
      "CD8A", 
      "GNLY", "NKG7",
      "FCGR3A", "MS4A7",
      "FCER1A", "CST3",
      "PPBP"), ]))

# 'as' is a matrix with n=rows for cells and a=columns for 
# annotations (genes). The column names will be shown in
# the plot.



## ---- fig.width=8, fig.height=8, fig.align='center', echo=T-------------------
# This function build the nummeric annotations plot
w1 <- get_num_feature_tiles(k = u_btd_k12$btd$cluster,
                               as = as,
                               tree_meta = u_btd_k12$tree_meta,
                               plot_title = "",
                               round_digits = 1)

# Plot
(u_btd_k12$tree|w1$w)+patchwork::plot_layout(widths = c(1, 1))


## ---- fig.width=12, fig.height=14, fig.align='center', echo=T-----------------
w2 <- get_num_feature_violins(k = u_btd_k12$btd$cluster,
                             as = as,
                             tree_meta = u_btd_k12$tree_meta,
                             plot_title = "",
                             scales = 'free_x')

((u_btd_k12$tree|w1$w)+patchwork::plot_layout(widths = c(1.5, 3)))/w2$w

