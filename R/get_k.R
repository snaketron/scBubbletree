
get_k <- function(x,
                  ks,
                  B_gap = 20,
                  n_start = 1000,
                  iter_max = 300,
                  kmeans_algorithm = "MacQueen",
                  cores = 1,
                  verbose = TRUE) {
  
  # check input parameters
  check_input_get_k(B_gap = B_gap,
                    ks = ks,
                    x = x,
                    n_start = n_start,
                    iter_max = iter_max,
                    cores = cores,
                    kmeans_algorithm = kmeans_algorithm,
                    verbose = verbose)
  
  # sort ks, smallest k first, largest last
  ks <- sort(ks, decreasing = FALSE)
  
  # clustering
  if(verbose) {
    message("1) clustering")
  }
  kmeans_obj <- bplapply(X = ks,
                         FUN = kmeans,
                         x = x,
                         nstart = n_start,
                         iter.max = iter_max,
                         algorithm = kmeans_algorithm,
                         BPPARAM = MulticoreParam(workers = cores))
  names(kmeans_obj) <- ks
  
  # gap statistics
  if(verbose) {
    message("2) gap-stat")
  }
  gap_stats <- bplapply(X = kmeans_obj,
                        FUN = get_gap_k,
                        x = x,
                        B_gap = B_gap,
                        n_start = n_start,
                        iter_max = iter_max,
                        kmeans_algorithm = kmeans_algorithm,
                        BPPARAM = MulticoreParam(workers = cores))
  
  # within cluster sum of squares
  if(verbose) {
    message("3) WCSS")
  }
  wcss_data <- lapply(X = kmeans_obj, FUN =  function(x) {
    return(x$tot.withinss)
  })
  
  # convert result to vectors as opposed to kmeans objects
  kmeans_obj <- lapply(X = kmeans_obj, FUN = km2vec <- function(x) {
    c <- as.character(x$cluster)
    return(c)
  })
  
  boot_obj <- list(obj = kmeans_obj, wcss = wcss_data, gap = gap_stats)
  
  # raw gap stats
  gap_matrix <- matrix(data = 0, nrow = B_gap, ncol = length(ks))
  wcss_matrix <- matrix(data = 0, nrow = 1, ncol = length(ks))
  
  # loop over top-bootstrap iterations B
  gap_vec <- numeric(length = length(ks))
  wcss_vec <- numeric(length = length(ks))
  
  # loop over ks
  for(j in seq_len(length.out = length(ks))) {
    gap_vec[j] <- boot_obj$gap[[j]]$gap
    wcss_vec[j] <- boot_obj$wcss[[j]]
    gap_matrix[,j] <- boot_obj$gap[[j]]$logWks-boot_obj$gap[[j]]$logW
    wcss_matrix[,j] <- boot_obj$wcss[[j]]
  }
  gap_stats <- data.frame(gap = gap_vec, k = ks)
  wcss_stats <- data.frame(wcss = wcss_vec, k = ks)
  
  # collect DFs
  gap_stats <- do.call(rbind, gap_stats)
  wcss_stats <- do.call(rbind, wcss_stats)
  
  # compute gap summary
  gap_mean <- apply(X = gap_matrix, MARGIN = 2, FUN = mean)
  gap_se <- sqrt((1/(B_gap) + 1)*apply(X = gap_matrix, MARGIN = 2, FUN = var))
  gap_stats_summary <- data.frame(gap_mean = gap_mean,
                                  k = ks,
                                  gap_SE = gap_se,
                                  L95 = gap_mean-gap_se*1.96,
                                  H95 = gap_mean+gap_se*1.96)
  
  # compute wcss summary
  wcss_mean <- apply(X = wcss_matrix, MARGIN = 2, FUN = mean)
  wcss_stats_summary <- data.frame(wcss_mean = wcss_mean, k = ks)
 
  return(structure(class = "boot_k",
                   list(boot_obj = boot_obj,
                        wcss_stats_summary = wcss_stats_summary,
                        gap_stats_summary = gap_stats_summary,
                        wcss_stats = wcss_stats,
                        gap_stats = gap_stats)))
}


# check input param
check_input_get_k <- function(B_gap,
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
get_gap_k <- function(km,
                      x,
                      B_gap,
                      n_start,
                      iter_max,
                      kmeans_algorithm) {
  
  spaceH0 <- "original"
  cs <- km$cluster
  n <- nrow(x)
  ii <- seq_len(length.out = n)
  kk <- length(unique(cs))
  
  get_Wk <- function(X, kk) {
    clus <- kmeans(x = X,
                   centers = kk,
                   algorithm = kmeans_algorithm,
                   iter.max = iter_max,
                   nstart = n_start)
    
    # log_Wk <- log(sum(clus$withinss)*0.5)
    log_Wk <- log(sum(clus$withinss))
    return(log_Wk)
  }
  
  logW <- numeric(1)
  E.logW <- numeric(1)
  SE.sim <- numeric(1)
  
  # logW <- log(sum(km$withinss)*0.5)
  logW <- log(sum(km$withinss))
  xs <- scale(x, center = TRUE, scale = FALSE)
  m.x <- rep(attr(xs, "scaled:center"), each = n)
  rng.x1 <- apply(xs, 2L, range)
  logWks <- matrix(0, B_gap, 1)
  for (b in seq_len(length.out = B_gap)) {
    z1 <- apply(rng.x1, 2, function(M, nn) runif(
      nn, min = M[1], max = M[2]), nn = n)
    z <- z1 + m.x
    
    # simpler
    logWks[b, 1] <- get_Wk(X = z, kk = kk)
  }
  E.logW <- colMeans(logWks)
  SE.sim <- sqrt((1 + 1/B_gap) * apply(logWks, 2, var))
  return(list(gap = E.logW - logW,
              SE.sim = SE.sim,
              logW = logW,
              logWks = logWks,
              E.logW = E.logW))
}

