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
                  cv_gap_p = 0.5 ,
                  ks,
                  x,
                  n_start = 100,
                  iter_max = 50,
                  cores =1,
                  approx_silhouette = T) {


  # compute average silhouette
  get_avg_sil <- function(km, df, approx) {
    if(approx == F) {
      ss <- cluster::silhouette(km$cluster, stats::dist(
        x = df, method = "euclidean"))
      if(is.na(ss)) {
        return(NA)
      }
      return(mean(ss[, 3]))
    } else {
      ss <- bluster::approxSilhouette(x = df, clusters = km$cluster)
      return(mean(ss$width))
    }
  }


  # compute gap statistics
  get_gap <- function(x, km, B = 100, d.power = 1, cv_gap_p = 1) {
    if(cv_gap_p < 0 | cv_gap_p > 1) {
      stop("cv_gap_p is a number between 0 (excluded) and 1.")
    }
    if(cv_gap_p < 1) {
      cs <- km$cluster
      js <- base::sample(x = 1:nrow(x),
                         size = ceiling(nrow(x)*cv_gap_p),
                         replace = F)
      x <- x[js, ]
      cs <- cs[js]
    } else {
      cs <- km$cluster
    }
    n <- nrow(x)
    ii <- seq_len(n)


    Wk <- function(X, cs) {
      0.5 * sum(vapply(split(ii, cs), function(I) {
        xs <- X[I, , drop = FALSE]
        sum(dist(xs, method = "euclidean")^d.power/nrow(xs))
      }, 0))
    }

    logW <- E.logW <- SE.sim <- numeric(1)
    logW <- log(Wk(x, cs=cs))

    xs <- scale(x, center = TRUE, scale = FALSE)
    m.x <- rep(attr(xs, "scaled:center"), each = n)
    rng.x1 <- apply(xs, 2L, range)
    logWks <- numeric(length = B)
    for (b in 1:B) {
      z1 <- apply(X = rng.x1,
                  MARGIN = 2,
                  FUN = function(M, nn) {
                    runif(nn, min = M[1], max = M[2])
                  },
                  nn = n)
      z <- z1 + m.x
      logWks[b] <- log(Wk(z, cs=cs))
    }
    E.logW <- mean(logWks)
    SE.sim <- sqrt((1 + 1/B) * var(logWks))
    return(list(gap = E.logW - logW,
                SE.sim = SE.sim,
                logW = logW,
                logWks = logWks,
                E.logW = E.logW))
  }

  # get standard error
  get_se <- function(x) {
    if(length(x) == 1) {
      se <- NA
    }
    else {
      se <- stats::sd(x)/base::sqrt(base::length(x))
    }
    return(se)
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

    j <- base::sample(x = 1:nrow(x),
                      size = ceiling(nrow(x)*cv_clust_p),
                      replace = T)

    # clustering
    cat("1) clustering, ")
    kmeans_obj <- parallel::mclapply(X = ks,
                                     FUN = kmeans,
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

    # compute silhouette
    cat("2) silhouette, ")
    sil_kmeans <- parallel::mclapply(X = kmeans_obj,
                                     FUN = get_avg_sil,
                                     df = x[j,],
                                     mc.cores = cores,
                                     mc.cleanup = T,
                                     approx = approx_silhouette)


    # get_gapstat <- function (x, km, k, B = 100, d.power = 1)
    cat("3) gap-stat, ")
    gap_stats <- parallel::mclapply(X = kmeans_obj,
                                    FUN = get_gap,
                                    x = x[j,],
                                    B = 1,
                                    d.power = 1,
                                    cv_gap_p = cv_gap_p,
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
  sil_stats <- c()
  gap_stats <- c()
  wss_stats <- c()

  for(i in 1:length(boot_obj)) {

    sil_vec <- numeric(length = length(ks))
    gap_vec <- numeric(length = length(ks))
    wss_vec <- numeric(length = length(ks))

    for(j in 1:length(ks)) {
      sil_vec[j] <- boot_obj[[i]]$sil[[j]]
      gap_vec[j] <- boot_obj[[i]]$gap[[j]]$gap
      wss_vec[j] <- boot_obj[[i]]$wss[[j]]
    }

    sil_stats <- rbind(sil_stats, data.frame(boot = i, sil = sil_vec, k = ks))
    gap_stats <- rbind(gap_stats, data.frame(boot = i, gap = gap_vec, k = ks))
    wss_stats <- rbind(wss_stats, data.frame(boot = i, wss = wss_vec, k = ks))

  }

  # next: compute summary from B values for each metrix
  gap_stats_summary <- base::merge(
    x = stats::aggregate(gap~k, data = gap_stats, FUN = stats::median),
    y = stats::aggregate(gap~k, data = gap_stats, FUN = get_se),
    by = "k")
  colnames(gap_stats_summary) <- c("k", "gap_median", "gap_SE")
  gap_stats_summary$L95 <- gap_stats_summary$gap_median-gap_stats_summary$gap_SE*1.96
  gap_stats_summary$H95 <- gap_stats_summary$gap_median+gap_stats_summary$gap_SE*1.96


  sil_stats_summary <- base::merge(
    x = stats::aggregate(sil~k, data = sil_stats, FUN = stats::median),
    y = stats::aggregate(sil~k, data = sil_stats, FUN = get_se),
    by = "k")
  colnames(sil_stats_summary) <- c("k", "sil_median", "sil_SE")
  sil_stats_summary$L95 <- sil_stats_summary$sil_median-sil_stats_summary$sil_SE*1.96
  sil_stats_summary$H95 <- sil_stats_summary$sil_median+sil_stats_summary$sil_SE*1.96

  wss_stats_summary <- base::merge(
    x = stats::aggregate(wss~k, data = wss_stats, FUN = stats::median),
    y = stats::aggregate(wss~k, data = wss_stats, FUN = get_se),
    by = "k")
  colnames(wss_stats_summary) <- c("k", "wss_median", "wss_SE")
  wss_stats_summary$L95 <- wss_stats_summary$wss_median-wss_stats_summary$wss_SE*1.96
  wss_stats_summary$H95 <- wss_stats_summary$wss_median+wss_stats_summary$wss_SE*1.96

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
get_bubbletree_data <- function(x,
                                k,
                                n_start = 10,
                                iter_max = 50,
                                B = 1,
                                N_eff = 500,
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
    seed <- base::sample(x = 1:10^6,
                         size = 1)
    set.seed(seed = seed)
  }

  # perform k-means clustering
  km <- stats::kmeans(x = x,
                      centers = k,
                      nstart = n_start,
                      iter.max = iter_max)


  # pairwise distances
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
  ph <- treeio::as.phylo(x = hc)
  ph <- ape::unroot(phy = ph)

  # get branch support
  ph <- get_ph_support(main_ph = ph,
                       x = pair_dist$raw_pair_dist)


  # build treetree
  t <- get_bubbletree(ph = ph$main_ph,
                      cluster = km$cluster,
                      round_digits = round_digits,
                      show_branch_support = show_branch_support)

  return(list(A = x,
              km = km,
              ph = ph,
              hc = hc,
              pair_dist = pair_dist,
              k = k,
              cluster = km$cluster,
              input_par = list(n_start = n_start,
                               iter_max = iter_max,
                               N_eff = N_eff,
                               B = B,
                               seed = seed,
                               round_digits = round_digits,
                               show_branch_support = show_branch_support),
              tree = t$tree,
              tree_meta = t$tree_meta))
}

#'
#' @exportMethod
#'
update_bubbletree_data <- function(btd,
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

    # add comments for debugging
    u_kms[[i]]$comment <- list(note = "update",
                               bubble = updated_bubbles[i],
                               k = ks[i])

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
  ph <- treeio::as.phylo(x = hc)
  ph <- ape::unroot(phy = ph)

  # get branch support
  ph <- get_ph_support(main_ph = ph,
                       x = pair_dist$raw_pair_dist)

  btd$ph <- ph
  btd$hc <- hc
  btd$pair_dist <- pair_dist
  btd$km <- "updated"
  btd$k <- "updated"

  # tree
  t <- get_bubbletree(ph = ph$main_ph,
                      cluster = btd$cluster,
                      round_digits = round_digits,
                      show_branch_support = show_branch_support)

  return(list(btd = btd,
              u_kms = u_kms,
              tree = t$tree,
              tree_meta = t$tree_meta))
}



#'
#' @exportMethod
#'
get_bubbletree_data_from_clustering <- function(x,
                                                c,
                                                B = 1,
                                                N_eff = 500,
                                                cores,
                                                seed = NA,
                                                verbose = F) {

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
get_gini_boot <- function(labels, kmeans_boot_obj) {
  B <- length(kmeans_boot_obj$boot_obj)

  # get standard error
  get_se <- function(x) {
    if(length(x) == 1) {
      se <- NA
    }
    else {
      se <- stats::sd(x)/base::sqrt(base::length(x))
    }
    return(se)
  }

  total_o <- c()
  cluster_o <- c()
  for(i in 1:B) {
    # b$boot_obj[[b]]$obj$`2`$
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
    x = stats::aggregate(total_gini~k, data = total_o, FUN = stats::median),
    y = stats::aggregate(total_gini~k, data = total_o, FUN = get_se),
    by = "k")
  colnames(total_gini_summary) <- c("k", "total_gini_median", "total_gini_SE")
  total_gini_summary$L95 <- total_gini_summary$total_gini_median-
    total_gini_summary$total_gini_SE*1.96
  total_gini_summary$H95 <- total_gini_summary$total_gini_median+
    total_gini_summary$total_gini_SE*1.96

  # cluster
  cluster_gini_summary <- base::merge(
    x = stats::aggregate(gini~k+cluster, data = cluster_o, FUN = stats::median),
    y = stats::aggregate(gini~k+cluster, data = cluster_o, FUN = get_se),
    by = c("k", "cluster"))
  colnames(cluster_gini_summary) <- c("k", "clusters", "gini_median", "gini_SE")
  cluster_gini_summary$L95 <- cluster_gini_summary$gini_median-
    cluster_gini_summary$gini_SE*1.96
  cluster_gini_summary$H95 <- cluster_gini_summary$gini_median+
    cluster_gini_summary$gini_SE*1.96


  return(list(total_gini_summary = total_gini_summary,
              cluster_gini_summary = cluster_gini_summary,
              total_gini = total_o,
              cluster_gini = cluster_o))
}


