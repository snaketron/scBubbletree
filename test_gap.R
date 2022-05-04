source(file = "~/Rutil/Init_Rpack.R")
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



a <- matrix(data = rnorm(n = 500*8, mean = 0, sd = 1), ncol = 8)
b <- rbind(matrix(data = rnorm(n = 250*1, mean = 0, sd = 1), ncol = 1),
           matrix(data = rnorm(n = 250*1, mean = 4, sd = 1), ncol = 1))
c <- rbind(matrix(data = rnorm(n = 250*1, mean = 6, sd = 1), ncol = 1),
           matrix(data = rnorm(n = 250*1, mean = 10, sd = 1), ncol = 1))
a <- cbind(b, c, a)
rm(b,c)


b_test_0 <- get_k(B = 20,
                  cv_prop = 1,
                  ks = 1:20,
                  x = a,
                  n_start = 10,
                  iter_max = 50,
                  cores = 5,
                  mini_output = T,
                  approx_silhouette = F)

ggplot(data = b_test_0$sil_stats_summary)+
  geom_point(aes(x = k, y = sil_mean))+
  geom_errorbar(aes(x = k, y = sil_mean, ymin = L95, ymax = H95), width = 0.1)+
  ggtitle(label = "Silhouette")+
  theme_bw()+
  ylab(label = "Average Silhouette")|
  ggplot(data = b_test_0$gap_stats_summary)+
  geom_point(aes(x = k, y = gap_mean))+
  geom_errorbar(aes(x = k, y = gap_mean, ymin = L95, ymax = H95), width = 0.1)+
  ggtitle(label = "Gap")+
  theme_bw()+
  ylab(label = "Average Gap statistic")|
  ggplot(data = b_test_0$wss_stats_summary)+
  geom_point(aes(x = k, y = wss_mean))+
  geom_errorbar(aes(x = k, y = wss_mean, ymin = L95, ymax = H95), width = 0.1)+
  ggtitle(label = "WSS")+
  theme_bw()+
  ylab(label = "Average WSS")




require(cluster)
gaps <- cluster::clusGap(x = a, FUNcluster = kmeans, K.max = 20,
                         B = 100, d.power = 1,
                         spaceH0 = "original")
plot(gaps)



sils <- c()
for(k in 2:20) {
  sils <- c(sils, mean(cluster::silhouette(x = kmeans(x = a, centers = k)$cluster,
                           dist = dist(a, method = "euclidean"))[, 3]))
}
plot(sils)

