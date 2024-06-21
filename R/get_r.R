
get_r <- function(x,
                  rs,
                  B_gap = 20,
                  n_start = 20,
                  iter_max = 100,
                  algorithm = "original",
                  knn_k = 20,
                  cores = 1,
                  verbose = TRUE) {
  
  get_ks <- function(l) {
    return(length(unique(l)))
  }
  
  # check input
  check_input_get_r(B_gap = B_gap,
                    rs = rs,
                    x = x,
                    n_start = n_start,
                    iter_max = iter_max,
                    cores = cores,
                    algorithm = algorithm,
                    knn_k = knn_k,
                    verbose = verbose)
  
  # sort rs, smallest r first, largest r last
  rs <- sort(rs, decreasing = FALSE)
  
  # add cell ids if needed
  if(is.null(rownames(x))) {
    rownames(x) <- seq_len(length.out = nrow(x))
  }
  # create Knn graph
  knn <- FindNeighbors(object = x, k.param = knn_k)
  
  # clustering
  if(verbose) {
    message("1) clustering")
  }
  
  gcd_obj <- bplapply(X = rs,
                      FUN = FindClusters,
                      object = knn$snn,
                      n.start = n_start,
                      n.iter = iter_max,
                      algorithm = map_louvain_algname(algorithm),
                      modularity.fxn = 1,
                      initial.membership = NULL,
                      node.sizes = NULL,
                      verbose = FALSE,
                      BPPARAM = SnowParam(workers = cores, type = "SOCK"))
  names(gcd_obj) <- rs
  
  # convert result to vectors as opposed to data.frames
  gcd_obj <- lapply(X = gcd_obj, FUN = df2vec <- function(x) {
      res <- names(x)
      res <- gsub(pattern = "res\\.", replacement = '', x = res)
      c <- as.character(x[, 1])
      return(c)
    })
  
  # Gap stats
  if(verbose) {
    message("2) gap statistic")
  }
  q <- bplapply(X = seq_len(length.out=length(gcd_obj)),
                FUN = get_gap_r,
                l = gcd_obj,
                x = x,
                B_gap = B_gap,
                n_start = n_start,
                iter_max = iter_max,
                algorithm = algorithm,
                knn_k = knn_k,
                BPPARAM = SnowParam(workers = cores, type = "SOCK"))
  
  # if k = 1 not present do
  q0 <- vector(mode = "list", length = 1)
  q0[[1]] <- get_gap_r_k1(x = x, B_gap = B_gap)
  gap_stats <- append(q, q0)
  rm(q, q0)
  
  # get k for each louvain obj
  ks <- unlist(lapply(X = gcd_obj, FUN = get_ks))
  
  # within cluster sum of squares
  if(verbose) {
    message("3) WCSS")
  }
  wcss_data <- lapply(X = gcd_obj, FUN = get_wcss_get_r, x = x)
  
  boot_obj <- list(obj = gcd_obj, wcss = wcss_data, gap = gap_stats)
  
  # raw gap stats
  gap_matrix <- matrix(data = 0, nrow = B_gap, ncol = length(rs))
  wcss_matrix <- matrix(data = 0, nrow = 1, ncol = length(rs))
  
  # loop over top-bootstrap iterations B
  gap_vec <- numeric(length = length(rs))
  wcss_vec <- numeric(length = length(rs))
  
  # loop over rs
  for(j in seq_len(length.out = length(rs))) {
    gap_vec[j] <- boot_obj$gap[[j]]$gap
    wcss_vec[j] <- boot_obj$wcss[[j]]
    gap_matrix[,j] <- boot_obj$gap[[j]]$logWks-boot_obj$gap[[j]]$logW
    wcss_matrix[,j] <- boot_obj$wcss[[j]]
  }
  gap_stats <- data.frame(gap = gap_vec, r = rs)
  wcss_stats <- data.frame(wcss = wcss_vec, r = rs)
  
  # collect DFs
  gap_stats <- do.call(rbind, gap_stats)
  wcss_stats <- do.call(rbind, wcss_stats)
  
  # compute gap summary
  gap_mean <- apply(X = gap_matrix, MARGIN = 2, FUN = mean)
  gap_se <- sqrt((1/(B_gap) + 1)*apply(X = gap_matrix, MARGIN = 2, FUN = var))
  gap_stats_summary <- data.frame(gap_mean = gap_mean,
                                  r = rs,
                                  k = ks,
                                  gap_SE = gap_se,
                                  L95 = gap_mean-gap_se*1.96,
                                  H95 = gap_mean+gap_se*1.96)
  
  # compute wcss summary
  wcss_mean <- apply(X = wcss_matrix, MARGIN = 2, FUN = mean)
  wcss_stats_summary <- data.frame(wcss_mean = wcss_mean, r = rs, k = ks)
 
  return(structure(class = "boot_r",
                   list(boot_obj = boot_obj,
                        wcss_stats_summary = wcss_stats_summary,
                        gap_stats_summary = gap_stats_summary,
                        wcss_stats = wcss_stats,
                        gap_stats = gap_stats)))
}


# Checks inputs of get_r
check_input_get_r <- function(B_gap,
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
get_gap_r <- function(i,
                      l,
                      x,
                      B_gap,
                      n_start,
                      iter_max,
                      algorithm,
                      knn_k) {
  
  spaceH0 <- "original"
  n <- nrow(x)
  ii <- seq_len(n)
  
  
  get_Wk <- function(X, r, knn_k) {
    
    # create Knn graph
    knn <- FindNeighbors(object = X, k.param = knn_k, verbose = FALSE)
    
    lc <- FindClusters(object = knn$snn,
                       resolution = r,
                       n.start = n_start,
                       n.iter = iter_max,
                       algorithm = map_louvain_algname(algorithm),
                       verbose = FALSE)
    
    wcss <- get_wcss_get_r(x = X, l = lc[,1])
    return(log(wcss))
  }
  
  
  r <- as.numeric(names(l)[i])
  l <- l[[i]]
  logW <- log(get_wcss_get_r(l = l, x = x))
  k <- length(unique(l))
  
  E.logW <- numeric(1)
  SE.sim <- numeric(1)
  
  
  xs <- scale(x, center = TRUE, scale = FALSE)
  m.x <- rep(attr(xs, "scaled:center"), each = n)
  rng.x1 <- apply(xs, 2L, range)
  logWks <- matrix(0, B_gap, 1)
  for (b in seq_len(length.out = B_gap)) {
    z1 <- apply(rng.x1, 2, function(M, nn) runif(nn,min=M[1],max=M[2]),nn=n)
    z <- z1 + m.x
    
    rownames(z) <- paste0("z_", seq_len(
      length.out = nrow(z)))
    
    # simpler
    logWks[b, 1] <- get_Wk(X = z, r = r, knn_k = knn_k)
  }
  E.logW <- colMeans(logWks)
  SE.sim <- sqrt((1 + 1/B_gap) * apply(logWks, 2, var))
  return(list(gap = E.logW - logW,
              SE.sim = SE.sim,
              logW = logW,
              logWks = logWks,
              E.logW = E.logW,
              k = k))
}


# Computes the gap statistic for special case k=1
get_gap_r_k1 <- function(x,
                         B_gap) {
  
  spaceH0 <- "original"
  n <- nrow(x)
  ii <- seq_len(length.out = n)
  
  
  get_Wk <- function(X) {
    return(log(get_wcss_get_r(x = X, l = rep(
      1, times = nrow(X)))))
  }
  
  
  logW <- log(get_wcss_get_r(
    l = rep(1, times = nrow(x)), x=x))
  k <- 1
  
  E.logW <- numeric(1)
  SE.sim <- numeric(1)
  
  
  xs <- scale(x, center = TRUE, scale = FALSE)
  m.x <- rep(attr(xs, "scaled:center"), each = n)
  rng.x1 <- apply(xs, 2L, range)
  logWks <- matrix(0, B_gap, 1)
  for (b in seq_len(length.out = B_gap)) {
    z1 <- apply(rng.x1, 2, function(M, nn) runif(nn,min=M[1],max=M[2]),nn=n)
    z <- z1 + m.x
    rownames(z) <- paste0("z_", seq_len(length.out = nrow(z)))
    
    # simpler
    logWks[b, 1] <- get_Wk(X = z)
  }
  E.logW <- colMeans(logWks)
  SE.sim <- sqrt((1 + 1/B_gap)*apply(logWks, 2, var))
  return(list(gap = E.logW - logW,
              SE.sim = SE.sim,
              logW = logW,
              logWks = logWks,
              E.logW = E.logW,
              k = k))
}


get_wcss_get_r <- function(x,
                           l) {
  c <- l
  
  get_sum_squares <- function(x,y) {
    return(outer(rowSums(x^2),
                 rowSums(y^2), '+') -
             tcrossprod(x, 2 * y))
  }
  
  wcss <- 0
  cs <- unique(c)
  for(i in seq_len(length.out = length(cs))) {
    j <- which(c == cs[i])
    
    mu <- apply(X = x[j,], MARGIN = 2, FUN = mean)
    # row vector of centroids
    mu <- matrix(data = mu, nrow = 1)
    
    # within-cluster sum of squares
    wcss <- wcss+sum(get_sum_squares(x = x[j,], y = mu))
  }
  return(wcss)
}
