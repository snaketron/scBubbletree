
get_k <- function(
    x,
    ks,
    B_gap = 20,
    n_start = 1000,
    iter_max = 300,
    kmeans_algorithm = "MacQueen",
    cores = 1,
    verbose = TRUE) {

  # check input parameters
  check_input_get_k(
    B_gap = B_gap,
    ks = ks,
    x = x,
    n_start = n_start,
    iter_max = iter_max,
    cores = cores,
    kmeans_algorithm = kmeans_algorithm,
    verbose = verbose)

  # sort ks, smallest k first, largest last
  ks <- base::sort(ks, decreasing = FALSE)

  # clustering
  if(verbose) {
    base::message("1) clustering")
  }
  
  future::plan(future::cluster, workers = cores)
  kmeans_obj <- future.apply::future_lapply(
    X = ks,
    FUN = stats::kmeans,
    x = x,
    nstart = n_start,
    iter.max = iter_max,
    algorithm = kmeans_algorithm,
    future.seed = TRUE)
  base::names(kmeans_obj) <- ks

  # gap statistics
  if(verbose) {
    base::message("2) gap-stat")
  }
  gap_stats <- future.apply::future_lapply(
    X = kmeans_obj,
    FUN = get_gap_k,
    x = x,
    B_gap = B_gap,
    n_start = n_start,
    iter_max = iter_max,
    kmeans_algorithm = kmeans_algorithm,
    future.seed = TRUE)

  # within cluster sum of squares
  if(verbose) {
    base::message("3) WCSS")
  }
  wcss_data <- lapply(
    X = kmeans_obj, FUN =  function(x) {
      return(x$tot.withinss)
    })

  # convert result to vectors as opposed to kmeans objects
  kmeans_obj <- lapply(
    X = kmeans_obj, FUN = km2vec <- function(x) {
      c <- as.character(x$cluster)
      return(c)
    })

  boot_obj <- list(
    obj = kmeans_obj,
    wcss = wcss_data,
    gap = gap_stats)

  # raw gap stats
  gap_matrix <- base::matrix(
    data = 0, nrow = B_gap, ncol = base::length(ks))
  wcss_matrix <- base::matrix(
    data = 0, nrow = 1, ncol = base::length(ks))

  # loop over top-bootstrap iterations B
  gap_vec <- base::numeric(length = base::length(ks))
  wcss_vec <- base::numeric(length = base::length(ks))

  # loop over ks
  for(j in base::seq_len(length.out = base::length(ks))) {
    gap_vec[j] <- boot_obj$gap[[j]]$gap
    wcss_vec[j] <- boot_obj$wcss[[j]]
    gap_matrix[,j] <- boot_obj$gap[[j]]$logWks-boot_obj$gap[[j]]$logW
    wcss_matrix[,j] <- boot_obj$wcss[[j]]
  }
  gap_stats <- base::data.frame(gap = gap_vec, k = ks)
  wcss_stats <- base::data.frame(wcss = wcss_vec, k = ks)

  # collect DFs
  gap_stats <- base::do.call(base::rbind, gap_stats)
  wcss_stats <- base::do.call(base::rbind, wcss_stats)

  # compute gap summary
  gap_mean <- base::apply(
    X = gap_matrix,
    MARGIN = 2,
    FUN = base::mean)
  gap_se <- base::sqrt((1/(B_gap) + 1)*base::apply(
    X = gap_matrix, MARGIN = 2, FUN = stats::var))
  gap_stats_summary <- base::data.frame(
    gap_mean = gap_mean,
    k = ks,
    gap_SE = gap_se,
    L95 = gap_mean-gap_se*1.96,
    H95 = gap_mean+gap_se*1.96)

  # compute wcss summary
  wcss_mean <- base::apply(
    X = wcss_matrix,
    MARGIN = 2,
    FUN = base::mean)
  wcss_stats_summary <- base::data.frame(
    wcss_mean = wcss_mean,
    k = ks)

  # remove unused connections
  future::plan(future::sequential())
  
  return(base::structure(
    class = "boot_k",
    base::list(boot_obj = boot_obj,
               wcss_stats_summary = wcss_stats_summary,
               gap_stats_summary = gap_stats_summary,
               wcss_stats = wcss_stats,
               gap_stats = gap_stats)))
}


# check input param
check_input_get_k <- function(
    B_gap,
    ks,
    x,
    n_start,
    iter_max,
    cores,
    kmeans_algorithm,
    verbose) {

  check_x(x = x)
  check_B_gap(B_gap = B_gap)
  check_ks(ks = ks, x = x)
  check_cores(cores = cores)
  check_n_start(n_start = n_start)
  check_iter_max(iter_max = iter_max)
  check_kmeans_algorithm(kmeans_algorithm = kmeans_algorithm)
  check_verbose(verbose = verbose)
}


# compute gap statistics
get_gap_k <- function(
    km,
    x,
    B_gap,
    n_start,
    iter_max,
    kmeans_algorithm) {

  spaceH0 <- "original"
  cs <- km$cluster
  n <- base::nrow(x)
  ii <- base::seq_len(length.out = n)
  kk <- base::length(base::unique(cs))

  get_Wk <- function(X, kk) {
    clus <- stats::kmeans(x = X,
                          centers = kk,
                          algorithm = kmeans_algorithm,
                          iter.max = iter_max,
                          nstart = n_start)

    log_Wk <- base::log(base::sum(clus$withinss)*0.5)
    return(log_Wk)
  }

  logW <- base::numeric(1)
  E.logW <- base::numeric(1)
  SE.sim <- base::numeric(1)


  logW <- base::log(base::sum(km$withinss)*0.5)
  xs <- base::scale(x, center = TRUE, scale = FALSE)
  m.x <- base::rep(base::attr(xs, "scaled:center"), each = n)
  rng.x1 <- base::apply(xs, 2L, base::range)
  logWks <- base::matrix(0, B_gap, 1)
  for (b in base::seq_len(length.out = B_gap)) {
    z1 <- base::apply(rng.x1, 2, function(M, nn) stats::runif(
      nn, min = M[1], max = M[2]), nn = n)
    z <- z1 + m.x

    # simpler
    logWks[b, 1] <- get_Wk(X = z, kk = kk)
  }
  E.logW <- base::colMeans(logWks)
  SE.sim <- base::sqrt((1 + 1/B_gap) * base::apply(logWks, 2, stats::var))
  return(list(gap = E.logW - logW,
              SE.sim = SE.sim,
              logW = logW,
              logWks = logWks,
              E.logW = E.logW))
}

