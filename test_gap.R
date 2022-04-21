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



a <- matrix(data = rnorm(n = 200*8, mean = 0, sd = 1), ncol = 8)
b <- rbind(matrix(data = rnorm(n = 100*1, mean = 0, sd = 1), ncol = 1),
           matrix(data = rnorm(n = 100*1, mean = 4, sd = 1), ncol = 1))
c <- rbind(matrix(data = rnorm(n = 100*1, mean = 6, sd = 1), ncol = 1),
           matrix(data = rnorm(n = 100*1, mean = 10, sd = 1), ncol = 1))
a <- cbind(b, c, a)
rm(b,c)


b_test_0 <- get_k(B = 50,
                  cv_clust_p = 1,
                  cv_index_p = 0.5,
                  ks = 1:20,
                  x = a,
                  n_start = 10,
                  iter_max = 50,
                  cores = 1)

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



k2 <- kmeans(x = a, centers = 2)
sil2 <- silhouette(x = k2$cluster, dist = dist(x = a, method = "euclidean"))
mean(sil2[, 3])


require(cluster)
gaps <- cluster::clusGap(x = a, FUNcluster = kmeans, K.max = 20, B = 100, d.power = 1,
                         spaceH0 = "original")

plot(gaps)



sils <- c()
for(k in 2:20) {
  sils <- c(sils, mean(cluster::silhouette(x = kmeans(x = a, centers = k)$cluster,
                           dist = dist(a, method = "euclidean"))[, 3]))
}
plot(sils)

k20 <-
sil20 <- silhouette(x = k20$cluster, dist = dist(x = a, method = "euclidean"))
mean(sil20[, 3])
z <- bluster::approxSilhouette(x = a, clusters = k20$cluster)
mean(z$width)


get_gap(km = kmeans(x = a, centers = 19),
        a,
        B = 100,
        d.power = 1,
        cv_index_p = 1,
        spaceH0 = "original")$gap






get_gap <- function (x, km, d.power = 1, B = 100, cv_index_p = 1, spaceH0 = "original") {
  if(cv_index_p < 0 | cv_index_p > 1) {
    stop("cv_index_p is a number between 0 (excluded) and 1.")
  }
  if(cv_index_p < 1) {
    cs <- km$cluster
    js <- base::sample(x = 1:nrow(x),
                       size = ceiling(nrow(x)*cv_index_p),
                       replace = F)
    x <- x[js, ]
    cs <- cs[js]
  } else {
    cs <- km$cluster
  }

  n <- nrow(x)
  ii <- seq_len(n)
  kk <- length(unique(cs))

  W.k <- function(X, kk) {
    clus <- kmeans(x = X, centers = kk)$cluster
    0.5 * sum(vapply(split(ii, clus), function(I) {
      xs <- X[I, , drop = FALSE]
      sum(dist(xs)^d.power/nrow(xs))
    }, 0))
  }

  get_logW <- function(X, cs) {
    0.5 * sum(vapply(split(ii, cs), function(I) {
      xs <- X[I, , drop = FALSE]
      sum(dist(xs)^d.power/nrow(xs))
    }, 0))
  }

  logW <- E.logW <- SE.sim <- numeric(1)

  logW <- log(get_logW(x, cs=cs))
  spaceH0 <- match.arg(spaceH0)
  xs <- scale(x, center = TRUE, scale = FALSE)
  m.x <- rep(attr(xs, "scaled:center"), each = n)
  switch(spaceH0, scaledPCA = {
    V.sx <- svd(xs, nu = 0)$v
    xs <- xs %*% V.sx
  }, original = {
  }, stop("invalid 'spaceH0':", spaceH0))
  rng.x1 <- apply(xs, 2L, range)
  logWks <- matrix(0, B, 1)
  for (b in 1:B) {
    z1 <- apply(rng.x1, 2, function(M, nn) runif(nn, min = M[1],
                                                 max = M[2]), nn = n)
    z <- switch(spaceH0, scaledPCA = tcrossprod(z1, V.sx),
                original = z1) + m.x
    logWks[b, 1] <- log(W.k(z, kk = kk))
  }
  E.logW <- colMeans(logWks)
  SE.sim <- sqrt((1 + 1/B) * apply(logWks, 2, var))
  return(list(gap = E.logW - logW,
              SE.sim = SE.sim,
              logW = logW,
              logWks = logWks,
              E.logW = E.logW))
}
