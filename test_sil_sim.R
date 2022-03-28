source(file = "~/Rutil/Init_Rpack.R")
source(file = "~/Rutil/Graphics.R")
source(file = "~/Rutil/Stats.R")

# source(file = "src/ClusteringUtil.R")
source(file = "../bubbletree/R/util.R")
library(farver, lib.loc = "/usr/local/lib/R/site-library")

library(Seurat, lib.loc = lib.loc)
library(ggplot2, lib.loc = lib.loc)
library(reshape2, lib.loc = lib.loc)
library(parallel, lib.loc = lib.loc)
library(ggtree, lib.loc = lib.loc)
library(cluster, lib.loc = lib.loc)
library(ape, lib.loc = lib.loc)
library(org.Hs.eg.db, lib.loc = lib.loc)
library(bluster, lib.loc = lib.loc)
library(SummarizedExperiment, lib.loc = lib.loc)

d_t0 <- get(load(file = "raw_data/Hao_2021_t0.RData"))
A <- d_t0@reductions$pca@cell.embeddings

# K-means clustering
# +++++++++++++++++++++
x <- A[sample(x = 1:nrow(A), size = 10000, replace = F), 1:10]
silh_out <- c()
for(k in 2:12) {
  cat(k, "\n")
  km <- kmeans(x = x, centers = k, nstart = 2)
  sil <- silhouette(km$cluster, dist(x, method = "euclidean"))
  hdi <- getHdi(vec = w$data$sil_width, hdi.level = 0.95)
  w <- fviz_silhouette(sil)
  p_neg <- sum(w$data$sil_width<0)/length(w$data$sil_width)*100
  silh_out <- rbind(silh_out, data.frame(k = k,
                                         mean = mean(w$data$sil_width),
                                         median = median(w$data$sil_width),
                                         L = hdi[1],
                                         H = hdi[2],
                                         p_neg = p_neg))
}
plot(x=silh_out$k, y=silh_out$median)
# Visualize kmeans clustering
require(factoextra)
fviz_cluster(km.res, A, ellipse.type = "norm")+
  theme_minimal()

# Visualize silhouhette information
require("cluster")


hist(w$data$sil_width)




# Identify observation with negative silhouette
neg_sil_index <- which(sil[, "sil_width"] < 0)
sil[neg_sil_index, , drop = FALSE]
## Not run:
# PAM clustering
# ++++++++++++++++++++
require(cluster)
pam.res <- pam(iris.scaled, 3)
# Visualize pam clustering
fviz_cluster(pam.res, ellipse.type = "norm")+
  theme_minimal()
# Visualize silhouhette information
fviz_silhouette(pam.res)

# Hierarchical clustering
# ++++++++++++++++++++++++
# Use hcut() which compute hclust and cut the tree
hc.cut <- hcut(iris.scaled, k = 3, hc_method = "complete")
# Visualize dendrogram
fviz_dend(hc.cut, show_labels = FALSE, rect = TRUE)
# Visualize silhouhette information
fviz_silhouette(hc.cut)

## End(Not run)
