
get_r <- function(
    x,
    rs,
    B_gap = 20,
    n_start = 20,
    iter_max = 100,
    algorithm = "original",
    knn_k = 50,
    cores = 1,
    verbose = TRUE) {

  get_ks <- function(l) {
    return(base::length(base::unique(l)))
  }

  # check input
  check_input_get_r(
    B_gap = B_gap,
    rs = rs,
    x = x,
    n_start = n_start,
    iter_max = iter_max,
    cores = cores,
    algorithm = algorithm,
    knn_k = knn_k,
    verbose = verbose)



  # sort rs, smallest r first, largest r last
  rs <- base::sort(rs, decreasing = FALSE)


  # add cell ids if needed
  if(is.null(rownames(x))) {
    rownames(x) <- base::seq_len(length.out = nrow(x))
  }
  # create Knn graph
  knn <- Seurat::FindNeighbors(
    object = x,
    k.param = knn_k)


  # clustering
  if(verbose) {
    base::message("1) clustering")
  }
  future::plan(future::multisession, workers = cores)
  louvain_obj <- future.apply::future_lapply(
    X = rs,
    FUN = Seurat::FindClusters,
    object = knn$snn,
    n.start = n_start,
    n.iter = iter_max,
    algorithm = map_louvain_algname(algorithm),
    modularity.fxn = 1,
    initial.membership = NULL,
    node.sizes = NULL,
    verbose = FALSE,
    future.seed = TRUE)
  base::names(louvain_obj) <- rs


  # convert result to vectors as opposed to data.frames
  louvain_obj <- lapply(
    X = louvain_obj,
    FUN = df2vec <- function(x) {
      res <- names(x)
      res <- gsub(pattern = "res\\.", replacement = '', x = res)
      c <- as.character(x[, 1])
      return(c)
    })


  # Gap stats
  if(verbose) {
    base::message("2) gap statistic")
  }
  q <- future.apply::future_lapply(
    X = base::seq_len(length.out = length(louvain_obj)),
    FUN = get_gap_r,
    l = louvain_obj,
    x = x,
    B_gap = B_gap,
    n_start = n_start,
    iter_max = iter_max,
    algorithm = algorithm,
    knn_k = knn_k,
    future.seed = TRUE)

  # if k = 1 not present do
  q0 <- vector(mode = "list", length = 1)
  q0[[1]] <- get_gap_r_k1(x = x, B_gap = B_gap)
  gap_stats <- append(q, q0)
  rm(q, q0)

  # get k for each louvain obj
  ks <- base::unlist(base::lapply(
    X = louvain_obj,
    FUN = get_ks))

  # within cluster sum of squares
  if(verbose) {
    base::message("3) WCSS")
  }
  wcss_data <- lapply(
    X = louvain_obj,
    FUN = get_wcss_get_r,
    x = x)

  boot_obj <- list(
    obj = louvain_obj,
    wcss = wcss_data,
    gap = gap_stats)

  # raw gap stats
  gap_matrix <- base::matrix(
    data = 0,
    nrow = B_gap,
    ncol = base::length(rs))
  wcss_matrix <- base::matrix(
    data = 0,
    nrow = 1,
    ncol = base::length(rs))

  # loop over top-bootstrap iterations B
  gap_vec <- base::numeric(length = base::length(rs))
  wcss_vec <- base::numeric(length = base::length(rs))

  # loop over rs
  for(j in base::seq_len(length.out = base::length(rs))) {
    gap_vec[j] <- boot_obj$gap[[j]]$gap
    wcss_vec[j] <- boot_obj$wcss[[j]]
    gap_matrix[,j] <- boot_obj$gap[[j]]$logWks-boot_obj$gap[[j]]$logW
    wcss_matrix[,j] <- boot_obj$wcss[[j]]
  }
  gap_stats <- base::data.frame(gap = gap_vec, r = rs)
  wcss_stats <- base::data.frame(wcss = wcss_vec, r = rs)

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
    r = rs,
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
    r = rs,
    k = ks)

  # remove unused connections
  future::plan(future::sequential())

  return(base::structure(
    class = "boot_r",
    base::list(boot_obj = boot_obj,
               wcss_stats_summary = wcss_stats_summary,
               gap_stats_summary = gap_stats_summary,
               wcss_stats = wcss_stats,
               gap_stats = gap_stats)))
}


# Checks inputs of get_r
check_input_get_r <- function(
    B_gap,
    rs,
    x,
    n_start,
    iter_max,
    cores,
    algorithm,
    knn_k,
    verbose) {

  check_x(x)
  check_B_gap(B_gap = B_gap)
  check_rs(rs = rs)
  check_cores(cores = cores)
  check_n_start(n_start = n_start)
  check_iter_max(iter_max = iter_max)
  check_louvain_algorithm(algorithm = algorithm)
  check_knn_k(knn_k)
  check_verbose(verbose = verbose)
}



# Computes the gap statistic
get_gap_r <- function(
    i,
    l,
    x,
    B_gap,
    n_start,
    iter_max,
    algorithm,
    knn_k) {

  spaceH0 <- "original"
  n <- base::nrow(x)
  ii <- base::seq_len(n)


  get_Wk <- function(X, r, knn_k) {

    # create Knn graph
    knn <- Seurat::FindNeighbors(
      object = X,
      k.param = knn_k,
      verbose = FALSE)


    lc <- Seurat::FindClusters(
      object = knn$snn,
      resolution = r,
      n.start = n_start,
      n.iter = iter_max,
      algorithm = map_louvain_algname(algorithm),
      verbose = FALSE)

    wcss <- get_wcss_get_r(x = X, l = lc[,1])
    return(base::log(wcss))
  }


  r <- base::as.numeric(base::names(l)[i])
  l <- l[[i]]
  logW <- base::log(get_wcss_get_r(l = l, x = x))
  k <- base::length(base::unique(l))

  E.logW <- base::numeric(1)
  SE.sim <- base::numeric(1)


  xs <- base::scale(x, center = TRUE, scale = FALSE)
  m.x <- base::rep(base::attr(xs, "scaled:center"), each = n)
  rng.x1 <- base::apply(xs, 2L, base::range)
  logWks <- base::matrix(0, B_gap, 1)
  for (b in base::seq_len(length.out = B_gap)) {
    z1 <- base::apply(rng.x1, 2, function(M, nn) stats::runif(
      nn, min = M[1], max = M[2]), nn = n)
    z <- z1 + m.x
    base::rownames(z) <- base::paste0("z_", base::seq_len(
      length.out = nrow(z)))

    # simpler
    logWks[b, 1] <- get_Wk(X = z, r = r, knn_k = knn_k)
  }
  E.logW <- base::colMeans(logWks)
  SE.sim <- base::sqrt((1 + 1/B_gap) * base::apply(logWks, 2, stats::var))
  return(list(gap = E.logW - logW,
              SE.sim = SE.sim,
              logW = logW,
              logWks = logWks,
              E.logW = E.logW,
              k = k))
}


# Computes the gap statistic for special case k=1
get_gap_r_k1 <- function(
    x,
    B_gap) {

  spaceH0 <- "original"
  n <- base::nrow(x)
  ii <- base::seq_len(length.out = n)


  get_Wk <- function(X) {
    return(base::log(get_wcss_get_r(x = X, l = base::rep(
      1, times = base::nrow(X)))))
  }


  logW <- base::log(get_wcss_get_r(
    l = base::rep(1, times = base::nrow(x)), x=x))
  k <- 1

  E.logW <- base::numeric(1)
  SE.sim <- base::numeric(1)


  xs <- base::scale(x, center = TRUE, scale = FALSE)
  m.x <- base::rep(base::attr(xs, "scaled:center"), each = n)
  rng.x1 <- base::apply(xs, 2L, base::range)
  logWks <- base::matrix(0, B_gap, 1)
  for (b in base::seq_len(length.out = B_gap)) {
    z1 <- base::apply(rng.x1, 2, function(M, nn) stats::runif(
      nn, min = M[1], max = M[2]), nn = n)
    z <- z1 + m.x
    base::rownames(z) <- base::paste0("z_", base::seq_len(
      length.out = base::nrow(z)))

    # simpler
    logWks[b, 1] <- get_Wk(X = z)
  }
  E.logW <- base::colMeans(logWks)
  SE.sim <- base::sqrt((1 + 1/B_gap)*base::apply(logWks, 2, stats::var))
  return(list(gap = E.logW - logW,
              SE.sim = SE.sim,
              logW = logW,
              logWks = logWks,
              E.logW = E.logW,
              k = k))
}


get_wcss_get_r <- function(
    x,
    l) {
  c <- l

  get_sum_squares <- function(x,y) {
    return(base::outer(base::rowSums(x^2),
                       base::rowSums(y^2), '+') -
             base::tcrossprod(x, 2 * y))
  }

  wcss <- 0
  cs <- base::unique(c)
  for(i in base::seq_len(length.out = base::length(cs))) {
    j <- base::which(c == cs[i])

    mu <- base::apply(X = x[j,], MARGIN = 2, FUN = base::mean)
    mu <- base::matrix(data = mu, nrow = 1)
    wcss <- wcss+base::sum(get_sum_squares(x = x[j,], y = mu))
  }
  return(wcss)
}
