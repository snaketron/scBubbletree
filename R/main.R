#' Wrapper for k-means. Input: features matrix \code{x} with cells
#' as rows and features (such as PC embeddings) as columns; vector \code{ks}
#' for the number of k-means to try; B=number of bootstrapping iterations; the
#' remaining parameters are described in the function \code{kmeans} or are used
#' for multicore execution.
#'
#' @param x matrix
#' @param ks vector
#' @param B number (default = 100)
#' @param n_start (default = 100)
#' @param iter_max (default = 50)
#' @param cores
#' @return list of B kmeans outputs
#' @exportMethod
#'
get_k <- function(B = 20,
                  cv_clust_p = 1,
                  cv_index_p = 0.5 ,
                  ks,
                  x,
                  n_start = 100,
                  iter_max = 50,
                  cores = 1,
                  low_size_output = F,
                  approx_silhouette = T) {


  # compute approximate average silhouette
  get_avg_sil <- function(km, df, cv_index_p, approx_silhouette) {
    js <- base::sample(x = 1:nrow(df),
                       size = ceiling(nrow(df)*cv_index_p),
                       replace = F)
    cs <- km$cluster[js]
    if(length(unique(cs))==1) {
      return(NA)
    }

    if(approx_silhouette == F) {
      ss <- cluster::silhouette(x = cs,
                                stats::dist(x = df[js,],
                                            method = "euclidean"))
      if(length(ss)==1 || is.na(ss)) {
        return(NA)
      }
      return(mean(ss[, 3]))
    } else {
      ss <- bluster::approxSilhouette(x = df, clusters = km$cluster)
      return(mean(ss$width))
    }
  }


  # compute gap statistics
  get_gap <- function (km,
                       x,
                       d.power = 1,
                       B = 100,
                       cv_index_p = 1,
                       spaceH0 = "original") {
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

    logW <- numeric(1)
    E.logW <- numeric(1)
    SE.sim <- numeric(1)

    logW <- log(get_logW(x, cs=cs))
    xs <- scale(x, center = TRUE, scale = FALSE)
    m.x <- rep(attr(xs, "scaled:center"), each = n)
    rng.x1 <- apply(xs, 2L, base::range)
    logWks <- matrix(0, B, 1)
    for (b in 1:B) {
      z1 <- apply(rng.x1, 2, function(M, nn) runif(nn, min = M[1],
                                                   max = M[2]), nn = n)
      z <- z1 + m.x
      logWks[b, 1] <- log(W.k(z, kk = kk))
    }
    E.logW <- colMeans(logWks)
    SE.sim <- sqrt((1 + 1/B) * apply(logWks, 2, stats::var))
    return(list(gap = E.logW - logW,
                SE.sim = SE.sim,
                logW = logW,
                logWks = logWks,
                E.logW = E.logW))
  }


  if(is.numeric(cv_clust_p) == F) {
    stop("cv_clust_p is a number between 0 (excluding) and 1")
  }
  if(length(cv_clust_p) != 1) {
    stop("cv_clust_p is a number between 0 (excluding) and 1")
  }
  if(cv_clust_p<0|cv_clust_p>1) {
    stop("cv_clust_p is a number between 0 (excluding) and 1")
  }


  if(is.numeric(B) == F) {
    stop("cv_clust_p is a number >0")
  }
  if(length(B) != 1) {
    stop("cv_clust_p is a number >0")
  }
  if(B<0) {
    stop("cv_clust_p is a number >0")
  }

  boot_obj <- vector(mode = "list", length = B)
  for(b in 1:B) {
    cat("boot:", b, " : ")

    if(cv_clust_p<1) {
      j <- base::sample(x = 1:nrow(x),
                        size = ceiling(nrow(x)*cv_clust_p),
                        replace = T)
    } else {
      j <- 1:nrow(x)
    }

    # clustering
    cat("1) clustering, ")
    kmeans_obj <- parallel::mclapply(X = ks,
                                     FUN = stats::kmeans,
                                     x = x[j,],
                                     nstart = n_start,
                                     iter.max = iter_max,
                                     mc.cores = cores)
    base::names(kmeans_obj) <- ks
    boot_obj[[b]] <- kmeans_obj

    # extract WSS
    wss_data <- lapply(X = kmeans_obj, FUN =  function(x) {
      return(x$tot.withinss)
    })

    # # between_SS / total_SS
    # ratio_betweenss_totalss <- lapply(X = kmeans_obj, FUN =  function(x) {
    #   return(x$betweenss/x$totss)
    # })

    # compute silhouette
    cat("2) silhouette, ")
    sil_kmeans <- parallel::mclapply(X = kmeans_obj,
                                     FUN = get_avg_sil,
                                     df = x[j,],
                                     mc.cores = cores,
                                     mc.cleanup = T,
                                     cv_index_p = cv_index_p,
                                     approx_silhouette = approx_silhouette)


    cat("3) gap-stat, ")
    gap_stats <- parallel::mclapply(X = kmeans_obj,
                                    FUN = get_gap,
                                    x = x[j,],
                                    B = 5,
                                    d.power = 1,
                                    cv_index_p = cv_index_p,
                                    mc.cores = cores,
                                    mc.cleanup = T)
    # within sum of squares
    cat("4) WSS. \n")
    boot_obj[[b]] <- list(obj = kmeans_obj,
                          wss = wss_data,
                          sil = sil_kmeans,
                          gap = gap_stats,
                          cell_i = j)

  }
  base::names(boot_obj) <- 1:B



  # collect clustering info data
  sil_stats <- vector(mode = "list", length = length(boot_obj))
  gap_stats <- vector(mode = "list", length = length(boot_obj))
  wss_stats <- vector(mode = "list", length = length(boot_obj))

  for(i in 1:length(boot_obj)) {
    sil_vec <- numeric(length = length(ks))
    gap_vec <- numeric(length = length(ks))
    wss_vec <- numeric(length = length(ks))

    for(j in 1:length(ks)) {
      # browser()
      sil_vec[j] <- boot_obj[[i]]$sil[[j]]
      gap_vec[j] <- boot_obj[[i]]$gap[[j]]$gap
      wss_vec[j] <- boot_obj[[i]]$wss[[j]]
    }

    sil_stats[[i]] <- data.frame(boot = i, sil = sil_vec, k = ks)
    gap_stats[[i]] <- data.frame(boot = i, gap = gap_vec, k = ks)
    wss_stats[[i]] <- data.frame(boot = i, wss = wss_vec, k = ks)

  }

  # collect DFs
  sil_stats <- do.call(rbind, sil_stats)
  gap_stats <- do.call(rbind, gap_stats)
  wss_stats <- do.call(rbind, wss_stats)


  # compute gap-stat summary from B values for each matrix
  gap_stats_summary <- base::merge(
    x = stats::aggregate(gap~k, data = gap_stats, FUN = base::mean),
    y = stats::aggregate(gap~k, data = gap_stats, FUN = get_se),
    by = "k")
  colnames(gap_stats_summary) <- c("k", "gap_mean", "gap_SE")
  gap_stats_summary$L95 <- gap_stats_summary$gap_mean-gap_stats_summary$gap_SE*1.96
  gap_stats_summary$H95 <- gap_stats_summary$gap_mean+gap_stats_summary$gap_SE*1.96


  # compute silhouette summary from B values for each matrix
  sil_stats_summary <- base::merge(
    x = stats::aggregate(sil~k, data = sil_stats, FUN = base::mean),
    y = stats::aggregate(sil~k, data = sil_stats, FUN = get_se),
    by = "k")
  colnames(sil_stats_summary) <- c("k", "sil_mean", "sil_SE")
  sil_stats_summary$L95 <- sil_stats_summary$sil_mean-sil_stats_summary$sil_SE*1.96
  sil_stats_summary$H95 <- sil_stats_summary$sil_mean+sil_stats_summary$sil_SE*1.96

  # compute wss summary from B values for each matrix
  wss_stats_summary <- base::merge(
    x = stats::aggregate(wss~k, data = wss_stats, FUN = base::mean),
    y = stats::aggregate(wss~k, data = wss_stats, FUN = get_se),
    by = "k")
  colnames(wss_stats_summary) <- c("k", "wss_mean", "wss_SE")
  wss_stats_summary$L95 <- wss_stats_summary$wss_mean-wss_stats_summary$wss_SE*1.96
  wss_stats_summary$H95 <- wss_stats_summary$wss_mean+wss_stats_summary$wss_SE*1.96

  if(low_size_output) {
    return(list(boot_obj = NA,
                wss_stats_summary = wss_stats_summary,
                sil_stats_summary = sil_stats_summary,
                gap_stats_summary = gap_stats_summary,
                wss_stats = wss_stats,
                sil_stats = sil_stats,
                gap_stats = gap_stats))

  }

  return(list(boot_obj = boot_obj,
              wss_stats_summary = wss_stats_summary,
              sil_stats_summary = sil_stats_summary,
              gap_stats_summary = gap_stats_summary,
              wss_stats = wss_stats,
              sil_stats = sil_stats,
              gap_stats = gap_stats))
}




#'
#' @exportMethod
#'
get_bubbletree <- function(x,
                           k,
                           n_start = 10,
                           iter_max = 50,
                           B = 1,
                           N_eff = 200,
                           cores,
                           seed = NA,
                           verbose = F,
                           round_digits = 2,
                           show_branch_support = T) {

  # check input
  if(is.na(x) || is.null(x) || is.matrix(x)==F) {
    stop("x should be a numeric matrix")
  }

  if(is.na(k) || is.null(k) || is.numeric(k)==F || k<=0 || k>=nrow(x)) {
    stop("k should be a number between 1 and nrow(x)")
  }

  # set seed for reproducibility
  if(is.na(seed) == F) {
    set.seed(seed = seed)
  }
  else {
    seed <- base::sample(x = 1:10^6, size = 1)
    set.seed(seed = seed)
  }

  # perform k-means clustering
  cat("Clustering ... \n")
  km <- stats::kmeans(x = x,
                      centers = k,
                      nstart = n_start,
                      iter.max = iter_max)

  # pairwise distances
  cat("Bubbletree construction ... \n")
  pair_dist <- get_dist(B = B,
                        m = x,
                        c = km$cluster,
                        N_eff = N_eff,
                        cores = cores,
                        verbose = verbose)



  # compute hierarchical clustering dendrogram
  d <- reshape2::acast(data = pair_dist$pca_pair_dist,
                       formula = c_i~c_j, value.var = "M")
  d <- stats::as.dist(d)
  hc <- stats::hclust(d, method = "average")
  ph <- ape::as.phylo(x = hc)
  ph <- ape::unroot(phy = ph)

  # get branch support
  ph <- get_ph_support(main_ph = ph,
                       x = pair_dist$raw_pair_dist)


  # build treetree
  t <- get_dendrogram(ph = ph$main_ph,
                      cluster = km$cluster,
                      round_digits = round_digits,
                      show_branch_support = show_branch_support)

  # collect input parameters: can be used for automated update
  input_par <- list(n_start = n_start,
                    iter_max = iter_max,
                    N_eff = N_eff,
                    B = B,
                    seed = seed,
                    round_digits = round_digits,
                    show_branch_support = show_branch_support)

  return(base::structure(class = "bubbletree",
                         list(A = x,
                              km = km,
                              ph = ph,
                              hc = hc,
                              pair_dist = pair_dist,
                              k = k,
                              cluster = km$cluster,
                              input_par = input_par,
                              tree = t$tree,
                              tree_meta = t$tree_meta)))

}

#'
#' @exportMethod
#'
update_bubbletree <- function(btd,
                              updated_bubbles,
                              ks,
                              cores,
                              verbose = F) {

  update_bubble <- function(A,
                            bubble,
                            k,
                            n_start,
                            iter_max) {

    # perform k-means clustering
    km <- stats::kmeans(x = A,
                        centers = k,
                        nstart = n_start,
                        iter.max = iter_max)

    return(km)

  }


  # get input from previous bubbletree data
  A <- btd$A
  n_start <- btd$input_par$n_start
  iter_max <- btd$input_par$iter_max
  N_eff <- btd$input_par$N_eff
  B <- btd$input_par$B
  round_digits <- btd$input_par$round_digits
  show_branch_support <- btd$input_par$show_branch_support
  seed <- btd$input_par$seed


  # set seed for reproducibility
  if(is.na(seed) == F) {
    set.seed(seed = seed)
  }


  # loop around updated_bubbles
  u_kms <- vector(mode = "list", length = length(updated_bubbles))
  for(i in 1:length(updated_bubbles)) {
    cat("Updating bubble:", updated_bubbles[i], "\n")
    j <- which(btd$cluster == updated_bubbles[i])

    # run update
    u_kms[[i]] <- update_bubble(A = A[j,],
                                bubble = updated_bubbles[i],
                                k = ks[i],
                                n_start = n_start,
                                iter_max = iter_max)

    # update cluster naming
    btd$cluster[j] <- paste0(updated_bubbles[i], '_',
                             u_kms[[i]]$cluster)
  }

  cat("Updating dendrogram \n")
  pair_dist <- get_dist(B = B,
                        m = A,
                        c = btd$cluster,
                        N_eff = N_eff,
                        cores = cores,
                        verbose = verbose)

  # compute hierarchical clustering dendrogram
  d <- reshape2::acast(data = pair_dist$pca_pair_dist,
                       formula = c_i~c_j,
                       value.var = "M")

  d <- stats::as.dist(d)
  hc <- stats::hclust(d, method = "average")
  ph <- ape::as.phylo(x = hc)
  ph <- ape::unroot(phy = ph)

  # get branch support
  ph <- get_ph_support(main_ph = ph,
                       x = pair_dist$raw_pair_dist)

  # tree
  t <- get_dendrogram(ph = ph$main_ph,
                      cluster = btd$cluster,
                      round_digits = round_digits,
                      show_branch_support = show_branch_support)



  return(base::structure(class = "bubbletree",
                         list(A = A,
                              km = u_kms,
                              ph = ph,
                              hc = hc,
                              pair_dist = pair_dist,
                              k = length(unique(btd$cluster)),
                              cluster = btd$cluster,
                              input_par = btd$input_par,
                              tree = t$tree,
                              tree_meta = t$tree_meta)))
}



#'
#' @exportMethod
#'
dummy_bubbletree <- function(x,
                             cs,
                             B = 1,
                             N_eff = 200,
                             cores,
                             seed = NA,
                             verbose = F,
                             round_digits = 2,
                             show_branch_support = T) {

  # check input
  if(is.na(x) || is.null(x) || is.matrix(x)==F) {
    stop("x should be a numeric matrix")
  }

  # length cs => nrow(x)

  # set seed for reproducibility
  if(is.na(seed) == F) {
    set.seed(seed = seed)
  }
  else {
    seed <- base::sample(x = 1:10^6, size = 1)
    set.seed(seed = seed)
  }

  # pairwise distances
  cat("Bubbletree construction ... \n")
  pair_dist <- get_dist(B = B,
                        m = x,
                        c = cs,
                        N_eff = N_eff,
                        cores = cores,
                        verbose = verbose)

  # compute hierarchical clustering dendrogram
  d <- reshape2::acast(data = pair_dist$pca_pair_dist,
                       formula = c_i~c_j, value.var = "M")
  d <- stats::as.dist(d)
  hc <- stats::hclust(d, method = "average")
  ph <- ape::as.phylo(x = hc)
  ph <- ape::unroot(phy = ph)

  # get branch support
  ph <- get_ph_support(main_ph = ph,
                       x = pair_dist$raw_pair_dist)

  # build treetree
  t <- get_dendrogram(ph = ph$main_ph,
                      cluster = cs,
                      round_digits = round_digits,
                      show_branch_support = show_branch_support)

  # collect input parameters: can be used for automated update
  input_par <- list(n_start = NA,
                    iter_max = NA,
                    N_eff = N_eff,
                    B = B,
                    seed = seed,
                    round_digits = round_digits,
                    show_branch_support = show_branch_support)

  return(base::structure(class = "bubbletree",
                         list(A = x,
                              km = NA,
                              ph = ph,
                              hc = hc,
                              pair_dist = pair_dist,
                              k = length(unique(cs)),
                              cluster = cs,
                              input_par = input_par,
                              tree = t$tree,
                              tree_meta = t$tree_meta)))

}




#'
#' @exportMethod
#'
get_gini <- function(labels, clusters) {

  get_gini_cluster <- function(c, l) {
    ls <- unique(l)
    l_len <- length(l)

    s <- 0
    for(i in 1:length(ls)) {
      s <- s + (sum(l == ls[i])/l_len)^2
    }

    return(s)

  }

  cs <- unique(clusters)

  # for each cluster we get gini-index
  cluster_gini <- numeric(length = length(cs))
  base::names(cluster_gini) <- cs

  # cluster weights used to compute total gini
  cluster_weight <- numeric(length = length(cs))
  base::names(cluster_weight) <- cs

  for(i in 1:length(cs)) {
    j <- which(clusters == cs[i])

    cluster_weight[i] <- length(j)/length(clusters)

    cluster_gini[i] <- 1-get_gini_cluster(c = clusters[j],
                                          l = labels[j])

  }

  total_gini = sum(cluster_gini*cluster_weight)

  return(list(cluster_gini = cluster_gini,
              total_gini = total_gini))
}


#'
#' @exportMethod
#'
get_gini_k <- function(labels, kmeans_boot_obj) {

  if(length(kmeans_boot_obj$boot_obj)==1&&
     is.na(kmeans_boot_obj$boot_obj)) {
    stop("You have to run 'get_k' with low_size_output=FALSE. \n")
  }

  B <- length(kmeans_boot_obj$boot_obj)

  total_o <- c()
  cluster_o <- c()
  for(i in 1:B) {
    ks <- base::names(kmeans_boot_obj$boot_obj[[i]]$obj)

    for(j in 1:length(ks)) {
      cell_id <- kmeans_boot_obj$boot_obj[[i]]$cell_i
      gini <- get_gini(clusters = kmeans_boot_obj$boot_obj[[i]]$obj[[ks[j]]]$cluster,
                       labels = labels[cell_id])

      # collect total gini and cluster specific gini scores
      # total
      total_o <- rbind(total_o,
                       data.frame(B = i,
                                  k = as.numeric(ks[j]),
                                  total_gini = gini$total_gini))
      # cluster
      cluster_o <- rbind(cluster_o,
                         data.frame(B = i,
                                    k = as.numeric(ks[j]),
                                    cluster = base::names(gini$cluster_gini),
                                    gini = gini$cluster_gini))
    }
  }

  # next: compute summary from B values for each metric
  # total
  total_gini_summary <- base::merge(
    x = stats::aggregate(total_gini~k, data = total_o, FUN = base::mean),
    y = stats::aggregate(total_gini~k, data = total_o, FUN = get_se),
    by = "k")
  colnames(total_gini_summary) <- c("k", "total_gini_mean", "total_gini_SE")
  total_gini_summary$L95 <- total_gini_summary$total_gini_mean-
    total_gini_summary$total_gini_SE*1.96
  total_gini_summary$H95 <- total_gini_summary$total_gini_mean+
    total_gini_summary$total_gini_SE*1.96

  # cluster
  cluster_gini_summary <- base::merge(
    x = stats::aggregate(gini~k+cluster, data = cluster_o, FUN = base::mean),
    y = stats::aggregate(gini~k+cluster, data = cluster_o, FUN = get_se),
    by = c("k", "cluster"))
  colnames(cluster_gini_summary) <- c("k", "clusters", "gini", "gini_SE")
  cluster_gini_summary$L95 <- cluster_gini_summary$gini-
    cluster_gini_summary$gini_SE*1.96
  cluster_gini_summary$H95 <- cluster_gini_summary$gini+
    cluster_gini_summary$gini_SE*1.96


  return(list(total_gini_summary = total_gini_summary,
              cluster_gini_summary = cluster_gini_summary,
              total_gini = total_o,
              cluster_gini = cluster_o))
}


