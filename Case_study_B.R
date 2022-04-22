source(file = "~/Rutil/Init_Rpack.R")
source(file = "../Rutil/Graphics.R")
source(file = "../scBubbletree/R/util.R")
source(file = "../scBubbletree/R/main.R")
source(file = "../scBubbletree/R/annotation.R")

library(cluster, lib.loc = lib.loc)
library(Seurat, lib.loc = lib.loc)
library(ggplot2, lib.loc = lib.loc)
library(reshape2, lib.loc = lib.loc)
library(parallel, lib.loc = lib.loc)
library(ape, lib.loc = lib.loc)
library(ggtree, lib.loc = lib.loc)
library(org.Hs.eg.db, lib.loc = lib.loc)
library(bluster, lib.loc = lib.loc)
library(SummarizedExperiment, lib.loc = lib.loc)
library(ggrepel, lib.loc = lib.loc)

redo_case_a <- F
redo_case_b <- F

d <- get(load(file = "raw_data/Hao_2021/Hao_2021.RData"))



A <- d@reductions$pca@cell.embeddings[, 1:15]
meta <- d@meta.data
as <- as.matrix(t(d@assays$SCT@data[
  rownames(d@assays$SCT@data) %in%
    c("IL7R",
      "CD14", "LYZ",
      "MS4A1",
      "CD8A",
      "GNLY", "NKG7",
      "FCGR3A", "MS4A7",
      "FCER1A", "CST3",
      "PPBP"), ]))


##### UMAP figures #####

## L1
umap_data <- cbind(d@meta.data, d@reductions$umap@cell.embeddings)
umap_data$closest_cluster <- NA

umap_centers <- merge(x = aggregate(UMAP_1~celltype.l1, data = umap_data, FUN = median),
                      y = aggregate(UMAP_2~celltype.l1, data = umap_data, FUN = median),
                      by = "celltype.l1")

g_umap <- ggplot()+
  geom_point(data = umap_data,
             aes(x = UMAP_1, y = UMAP_2, col = celltype.l1), size = 0.25)+
  geom_text_repel(data = umap_centers,
            aes(x = UMAP_1, y = UMAP_2, label = celltype.l1),
            min.segment.length = 0, size = 3)+
  theme(legend.position = "none")+
  guides(colour = guide_legend(nrow = 4,
                               override.aes = list(size=2)))

g_umap
ggsave(plot = g_umap,
       filename = "manuscript_data/UMAP_B_lv1.pdf",
       device = "pdf",
       width = 6,
       height = 6)
ggsave(plot = g_umap,
       filename = "manuscript_data/UMAP_B_lv1.png",
       device = "png",
       dpi = 600,
       width = 6,
       height = 6)



## L2
umap_data <- cbind(d@meta.data, d@reductions$umap@cell.embeddings)
umap_data$closest_cluster <- NA

umap_centers <- merge(x = aggregate(UMAP_1~celltype.l2, data = umap_data, FUN = median),
                      y = aggregate(UMAP_2~celltype.l2, data = umap_data, FUN = median),
                      by = "celltype.l2")

g_umap <- ggplot()+
  geom_point(data = umap_data,
             aes(x = UMAP_1, y = UMAP_2, col = celltype.l2), size = 0.25)+
  geom_text_repel(data = umap_centers,
            aes(x = UMAP_1, y = UMAP_2, label = celltype.l2),
            min.segment.length = 0, size = 3)+
  theme(legend.position = "none")+
  guides(colour = guide_legend(nrow = 4,
                               override.aes = list(size=2)))

g_umap
ggsave(plot = g_umap,
       filename = "manuscript_data/UMAP_B_lv2.pdf",
       device = "pdf",
       width = 6,
       height = 6)
ggsave(plot = g_umap,
       filename = "manuscript_data/UMAP_B_lv2.png",
       device = "png",
       dpi = 600,
       width = 6,
       height = 6)




##### Var explained figure #####
var_explained <- ((d[["pca"]]@stdev)^2)/d[["pca"]]@misc$total.variance

g_var_explained <- ggplot(data = data.frame(var_explained = var_explained*100,
                                            PC = 1:length(var_explained)))+
  geom_point(aes(y = var_explained, x = PC), size = 1)+
  ylab(label = "Variance explained [%]")

g_var_explained

ggsave(plot = g_var_explained,
       filename = "manuscript_data/Var_explained_PCA_B.pdf",
       device = "pdf",
       width = 4,
       height = 3)




##### get_k figure #####
# b <- get_k(B = 30,
#            cv_clust_p = 1, # use 100% for clustering
#            cv_index_p = 0.2, # use 20% for gap stat.
#            ks = 1:40,
#            x = A,
#            n_start = 10,
#            iter_max = 20,
#            cores = 20)
# save(b, file = "case_study_B/b.RData")
b <- get(load("~/scBubbletree/case_study_B/b.RData"))

# gini
gini_l1 <- get_gini_k(labels = meta$celltype.l1, kmeans_boot_obj = b)
gini_l2 <- get_gini_k(labels = meta$celltype.l2, kmeans_boot_obj = b)
gini_l3 <- get_gini_k(labels = meta$celltype.l3, kmeans_boot_obj = b)

l1 <- gini_l1$total_gini_summary
l1$level <- "l1"
l2 <- gini_l2$total_gini_summary
l2$level <- "l2"
l3 <- gini_l3$total_gini_summary
l3$level <- "l3"

l_summary <- rbind(l1, l2, l3)
rm(l1, l2, l3, gini_l1, gini_l2, gini_l3)

g0 <- ggplot(data = b$sil_stats_summary)+
  geom_point(aes(x = k, y = sil_mean), size = 1)+
  geom_errorbar(aes(x = k, y = sil_mean, ymin = L95, ymax = H95), width = 0.1)+
  ggtitle(label = '', subtitle = "Silhouette")+
  ylab(label = "Mean Silhouette")|
  ggplot(data = b$gap_stats_summary)+
  geom_point(aes(x = k, y = gap_mean), size = 1)+
  geom_errorbar(aes(x = k, y = gap_mean, ymin = L95, ymax = H95), width = 0.1)+
  ggtitle(label = '', subtitle = "Gap")+
  ylab(label = "Mean Gap")|
  ggplot(data = b$wss_stats_summary)+
  geom_point(aes(x = k, y = wss_mean), size = 1)+
  geom_errorbar(aes(x = k, y = wss_mean, ymin = L95, ymax = H95), width = 0.1)+
  ggtitle(label = '', subtitle = "WSS")+
  ylab(label = "Mean WSS")|
  ggplot(data = l_summary)+
  geom_point(aes(x = k, y = total_gini_mean, col = level), size = 1)+
  geom_errorbar(aes(x = k, y = total_gini_mean, ymin = L95,
                    ymax = H95, col = level), width = 0.1)+
  scale_color_manual(name = "R",
                     values = c("black", "#787777", "gray"))+
  theme(legend.position = c(0.8, 0.7))+
  ylab(label = "Mean impurity")+
  ggtitle(label = '', subtitle = "Gini impurity")

















##### get_bubbletree with k=15 figure #####
k15 <- get_bubbletree(x = A,
                      k = 15,
                      n_start = 200,
                      iter_max = 1000,
                      seed = 4321,
                      cores = 5,
                      B = 100,
                      N_eff = 100,
                      verbose = FALSE,
                      round_digits = 1)
save(k15, file = "case_study_B/k15.RData")
k15 <- get(load("~/scBubbletree/case_study_B/k15.RData"))



# create tile plot
w1 <- get_cat_feature_tiles(d = k15,
                            a = meta$celltype.l2,
                            feature_composition = T,
                            round_digits = 0,
                            show_hclust = F,
                            tile_text_size = 2)
#
# if(redo_case_b) {
#   # do clustering with k=15
#   u_k15 <- update_bubbletree(btd = k15,
#                              updated_bubbles = c("2", "1"),
#                              ks = c(2, 2),
#                              cores = 20)
#   save(u_k15, file = "case_study_B/u_k15.RData")
# } else {
#   u_k15 <- get(load("~/scBubbletree/case_study_B/u_k15.RData"))
# }
# w2 <- get_cat_feature_tiles(d = u_k15,
#                             a = a,
#                             feature_composition = T,
#                             round_digits = 0,
#                             show_hclust = F)
#
# (u_k15$tree|w2$w)+patchwork::plot_layout(ncol = 2, widths = c(4, 9))




###### Main Figure 2 ##########
g <- (g0|g_gini)/((k15$tree|w1$w)+patchwork::plot_layout(widths = c(1.5,6)))+
  patchwork::plot_layout(heights = c(1, 4.5))+
  patchwork::plot_annotation(tag_levels = 'A')

ggsave(plot = g,
       filename = "manuscript_data/Fig_2.svg",
       device = "svg",
       width = 8,
       height = 8)






###### Supplementary Figure I #######
w3 <- get_num_feature_tiles(d = k15,
                            as = as,
                            plot_title = "",
                            round_digits = 1,
                            tile_text_size = 2.5)

w5 <- get_cat_feature_tiles(d = k15,
                            a = apaste0(meta$donor, '_', meta$time),
                            feature_composition = T,
                            round_digits = 0,
                            show_hclust = T,
                            tile_text_size = 2.5)


g_sup <- (((k15$tree|w3$w)+patchwork::plot_layout(widths = c(1, 1.3)))/w5$w)+
  patchwork::plot_layout(widths = c(1.5, 1))+
  patchwork::plot_annotation(tag_levels = 'A')

ggsave(plot = g_sup,
       filename = "manuscript_data/Fig_S2.svg",
       device = "svg",
       width = 7,
       height = 9)





###### Supplementary Figure II #######

w4 <- get_num_feature_violins(d = k15,
                              as = as,
                              plot_title = "",
                              scales = 'free_x',
                              show_cells = F)


g_sup <- (((k15$tree|w4$w)+patchwork::plot_layout(widths = c(1, 1.3))))+
  patchwork::plot_annotation(tag_levels = 'A')

