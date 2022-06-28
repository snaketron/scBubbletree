require(scBubbletree)


a <- matrix(data = rnorm(n = 500*9, mean = 0, sd = 1), ncol = 9)
b <- rbind(matrix(data = rnorm(n = 250*1, mean = 0, sd = 1), ncol = 1),
           matrix(data = rnorm(n = 250*1, mean = 4, sd = 1), ncol = 1))
a <- cbind(b, a)
rm(b,c)


a <- matrix(data = rnorm(n = 500*5, mean = 0, sd = 1), ncol = 5)
a <- matrix(data = runif(n = 500*5, min = 0, max = 1), ncol = 5)



# require(cluster)
# gaps <- cluster::clusGap(x = a,
#                          FUNcluster = kmeans,
#                          K.max = 400,
#                          iter.max = 100,
#                          B = 50,
#                          d.power = 1,
#                          spaceH0 = "original")
# plot(gaps)
# gap_cluster <- data.frame(gaps$Tab)
# gap_cluster$k <- as.numeric(rownames(gap_cluster))
#
#
# ggplot()+
#   geom_point(data = gap_cluster,
#              aes(x = k, y = gap))+
#   geom_errorbar(data = gap_cluster,
#                 aes(x = k, y = gap, ymin = gap-SE.sim,
#                     ymax = gap+SE.sim), width = 0.3)
#
#
# b_test_0 <- get_k(B = 5,
#                   cv_prop = 1,
#                   ks = 1:10,
#                   x = a,
#                   cores = 4,
#                   mini_output = F,
#                   B_gap = 100)
#
#
#
# ggplot()+
#   geom_point(data = b_test_0$gap_stats_summary,
#              aes(x = k, y = gap_mean))+
#   geom_errorbar(data = b_test_0$gap_stats_summary,
#                 aes(x = k, y = gap_mean, ymin = L95, ymax = H95), width = 0.3)+
#   geom_point(data = b_test_0$gap_stats,
#              aes(x = k, y = gap), size = 0.25, col = "red",
#              position = position_jitter(width = 0.25, height = 0))+
#   theme_bw()+
#   ylab(label = "Average Gap")|
# ggplot(data = b_test_0$wcss_stats_summary)+
#   geom_point(aes(x = k, y = wcss_mean))+
#   geom_errorbar(aes(x = k, y = wcss_mean, ymin = L95, ymax = H95), width = 0.1)+
#   theme_bw()+
#   scale_y_log10()+
#   ylab(label = "Average WCSS")
#
#
#
#
# plot(b_test_0$gap_stats_summary$gap_mean,
#      gap_cluster$gap);abline(0,1)






# compute gap statistics
get_gap <- function (km,
                     x,
                     d.power = 1,
                     B_gap,
                     n_start,
                     iter_max,
                     spaceH0 = "original",
                     kmeans_algorithm) {

  cs <- km$cluster
  n <- nrow(x)
  ii <- seq_len(n)
  kk <- length(unique(cs))

  W.k <- function(X, kk) {
    clus <- stats::kmeans(x = X,
                          centers = kk,
                          algorithm = kmeans_algorithm,
                          iter.max = iter_max,
                          nstart = n_start)$cluster
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

  logW <- numeric(1)
  E.logW <- numeric(1)
  SE.sim <- numeric(1)

  logW <- log(get_logW(x, cs=cs))
  xs <- scale(x, center = TRUE, scale = FALSE)
  m.x <- rep(attr(xs, "scaled:center"), each = n)
  rng.x1 <- apply(xs, 2L, base::range)
  logWks <- matrix(0, B_gap, 1)
  for (b in 1:B_gap) {
    z1 <- apply(rng.x1, 2, function(M, nn) runif(nn, min = M[1],
                                                 max = M[2]), nn = n)
    z <- z1 + m.x
    logWks[b, 1] <- log(W.k(z, kk = kk))
  }
  E.logW <- colMeans(logWks)
  SE.sim <- sqrt((1 + 1/B_gap) * apply(logWks, 2, stats::var))
  return(list(gap = E.logW - logW,
              SE.sim = SE.sim,
              logW = logW,
              logWks = logWks,
              E.logW = E.logW))
}


get_clusgap <- function(x,
                        ks,
                        B = 100,
                        d.power = 1,
                        spaceH0 = c("scaledPCA",
                                    "original"),
                        verbose = F)  {

  n <- nrow(x)
  if (B != (B. <- as.integer(B)) || (B <- B.) <= 0)
    stop("'B' has to be a positive integer")
  cl. <- match.call()
  if (is.data.frame(x))
    x <- as.matrix(x)
  ii <- seq_len(n)
  W.k <- function(X, kk) {
    clus <- if (kk > 1)
      kmeans(X, kk, algorithm = "MacQueen", iter.max = 100, nstart = 100)$cluster
    else rep.int(1L, nrow(X))
    0.5 * sum(vapply(split(ii, clus), function(I) {
      xs <- X[I, , drop = FALSE]
      sum(dist(xs)^d.power/nrow(xs))
    }, 0))
  }
  logW <- E.logW <- SE.sim <- numeric(length(ks))
  if (verbose)
    cat("Clustering k = 1,2,..., K.max (= ", length(ks), "): .. ",
        sep = "")
  for (k in 1:length(ks)) logW[k] <- log(W.k(x, ks[k]))
  if (verbose)
    cat("done\n")
  spaceH0 <- match.arg(spaceH0)
  xs <- scale(x, center = TRUE, scale = FALSE)
  m.x <- rep(attr(xs, "scaled:center"), each = n)
  switch(spaceH0, scaledPCA = {
    V.sx <- svd(xs, nu = 0)$v
    xs <- xs %*% V.sx
  }, original = {
  }, stop("invalid 'spaceH0':", spaceH0))
  rng.x1 <- apply(xs, 2L, range)
  logWks <- matrix(0, B, length(ks))
  if (verbose)
    cat("Bootstrapping, b = 1,2,..., B (= ", B, ")  [one \".\" per sample]:\n",
        sep = "")
  for (b in 1:B) {
    z1 <- apply(rng.x1, 2, function(M, nn) runif(nn, min = M[1],
                                                 max = M[2]), nn = n)
    z <- switch(spaceH0, scaledPCA = tcrossprod(z1, V.sx),
                original = z1) + m.x
    for (k in 1:length(ks)) {
      logWks[b, k] <- log(W.k(z, ks[k]))
    }
    if (verbose)
      cat(".", if (b%%50 == 0)
        paste(b, "\n"))
  }
  if (verbose && (B%%50 != 0))
    cat("", B, "\n")
  E.logW <- colMeans(logWks)
  SE.sim <- sqrt((1 + 1/B) * apply(logWks, 2, var))
  structure(class = "clusGap", list(Tab = cbind(logW, E.logW,
                                                gap = E.logW - logW, SE.sim),
                                    call = cl., spaceH0 = spaceH0,
                                    n = n, B = B, logWks = logWks))
}



ks <- seq(from = 1, to = 200, by = 10)

clusgap <- get_clusgap(x = a,
            ks = ks,
            B = 20,
            d.power = 1,
            spaceH0 = "original")
clusgap_summary <- data.frame(clusgap$Tab)
clusgap_summary$ks <- ks

log_ewk <- c()
log_wk <- c()
for(k in ks) {
  cat(k, "\n")
  km <- kmeans(x = a, centers = k, iter.max = 100, nstart = 100, algorithm = "MacQueen")
  gap <- get_gap(km,
                 x = a,
                 d.power = 1,
                 B_gap = 100,
                 n_start = 100,
                 iter_max = 100,
                 spaceH0 = "original",
                 kmeans_algorithm = "MacQueen")

  log_ewk <- c(log_ewk, gap$E.logW)
  log_wk <- c(log_wk, gap$logW)
}

gap_summary <- data.frame(log_ewk = log_ewk,
                          log_wk = log_wk,
                          gap = log_ewk-log_wk,
                          ks = ks)

ggplot()+
  geom_point(data = gap_summary, aes(x = ks, y = log_ewk))+
  geom_point(data = gap_summary, aes(x = ks, y = log_wk), col = "red")+
  geom_point(data = clusgap_summary, aes(x = ks, y = logW), col = "blue")+
  geom_point(data = clusgap_summary, aes(x = ks, y = E.logW), col = "darkgray")


ggplot()+
  geom_point(data = gap_summary, aes(x = ks, y = log_ewk-log_wk))+
  geom_line(data = gap_summary, aes(x = ks, y = log_ewk-log_wk))+
  geom_point(data = clusgap_summary, aes(x = ks, y = E.logW-logW), col = "blue")+
  geom_line(data = clusgap_summary, aes(x = ks, y = E.logW-logW), col = "blue")

