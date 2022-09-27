
get_k <- function(x,
                  ks,
                  B_gap = 20,
                  n_start = 100,
                  iter_max = 200,
                  kmeans_algorithm = "MacQueen",
                  cores = 1) {
  
  
  # check input param
  check_input <- function(B_gap,
                          ks,
                          x,
                          n_start,
                          iter_max,
                          cores,
                          kmeans_algorithm) {
    
    # check x
    if(base::missing(x)) {
      stop("x input not found")
    }
    if(methods::is(x, "SummarizedExperiment")) {
      x <- x@assays@data@listData
    }
    if(base::is.numeric(x)==FALSE) {
      stop("x must be numeric matrix")
    }
    if(base::is.matrix(x)==FALSE) {
      stop("x must be numeric matrix")
    }
    if(base::any(base::is.infinite(x))==TRUE) {
      stop("x must be numeric matrix, infinite values not allowed")
    }
    if(base::any(base::is.na(x))==TRUE) {
      stop("x must be numeric matrix, NAs not allowed")
    }
    if(base::any(base::is.null(x))==TRUE) {
      stop("x must be numeric matrix, NULLs not allowed")
    }
    if(base::all(x == x[1,1])==TRUE) {
      stop("all elements in x are identical")
    }
    if(base::ncol(x)>base::nrow(x)) {
      warning("more columns (features) than rows (cells) in x")
    }
    
    
    
    
    # check B_gap
    if(base::missing(B_gap)==TRUE) {
      stop("B_gap input not found")
    }
    if(base::is.numeric(B_gap)==FALSE) {
      stop("B_gap must be a positive integer > 0")
    }
    if(base::length(B_gap)!=1) {
      stop("B_gap must be a positive integer > 0")
    }
    if(B_gap<1) {
      stop("B_gap must be a positive integer > 0")
    }
    if(base::is.infinite(B_gap)==TRUE) {
      stop("B_gap must be a positive integer > 0")
    }
    if(base::is.na(B_gap)==TRUE) {
      stop("B_gap must be a positive integer > 0")
    }
    if(B_gap%%1!=0) {
      stop("B_gap must be a positive integer > 0")
    }
    
    
    
    # check ks
    if(base::missing(ks)==TRUE) {
      stop("ks input not found")
    }
    if(base::is.numeric(ks)==FALSE) {
      stop("ks must be a positive integer or vector of positive integers")
    }
    if(base::is.vector(x = ks)==FALSE) {
      stop("ks must be a positive integer or vector of positive integers")
    }
    if(base::length(ks)<=0) {
      stop("ks must be a positive integer or vector of positive integers")
    }
    if(base::any(base::is.infinite(ks))==TRUE) {
      stop("ks must be a positive integer or vector of positive integers,
           no infinite values are allowed")
    }
    if(base::any(base::is.na(ks))==TRUE) {
      stop("ks must be a positive integer or vector of positive integers,
           no NAs are allowed")
    }
    if(base::any(base::is.null(ks))==TRUE) {
      stop("ks must be a positive integer or vector of positive integers,
           no NULLs are allowed")
    }
    if(base::any(ks<0)==TRUE) {
      stop("ks must be a positive integer or vector of positive integers")
    }
    if(base::any(base::duplicated(ks))==TRUE) {
      stop("ks must be a positive integer or vector of positive integers,
           duplicate k values are not allowed")
    }
    if(base::length(ks)==1) {
      if(base::all(ks<1)) {
        stop("ks must be a positive integer or vector of positive integers")
      }
    }
    if(base::any(ks>=base::nrow(x))==TRUE) {
      stop("max(ks) should be smaller than nrow(x)")
    }
    
    
    
    # check cores
    if(base::missing(cores)==TRUE) {
      stop("cores input not found")
    }
    if(base::is.numeric(cores)==FALSE) {
      stop("cores must be a positive integer")
    }
    if(base::length(cores)!=1) {
      stop("cores must be a positive integer")
    }
    if(base::is.infinite(cores)==TRUE) {
      stop("cores must be a positive integer")
    }
    if(base::is.na(cores)==TRUE) {
      stop("cores must be a positive integer")
    }
    if(cores<1) {
      stop("cores must be a positive integer")
    }
    if(cores%%1!=0) {
      stop("cores must be a positive integer")
    }
    
    
    
    # n_start
    if(base::missing(n_start)==TRUE) {
      stop("n_start input not found")
    }
    if(base::is.numeric(n_start)==FALSE) {
      stop("n_start must be a positive integer")
    }
    if(base::length(n_start) != 1) {
      stop("n_start must be a positive integer")
    }
    if(n_start < 1) {
      stop("n_start must be a positive integer")
    }
    if(base::is.infinite(n_start)==TRUE) {
      stop("n_start must be a positive integer")
    }
    if(base::is.na(n_start)==TRUE) {
      stop("n_start must be a positive integer")
    }
    if(base::is.null(n_start)==TRUE) {
      stop("n_start must be a positive integer")
    }
    if(n_start%%1!=0) {
      stop("n_start must be a positive integer")
    }
    
    
    
    # iter_max
    if(base::missing(iter_max)==TRUE) {
      stop("iter_max input not found")
    }
    if(base::is.numeric(iter_max)==FALSE) {
      stop("iter_max must be a positive integer")
    }
    if(base::length(iter_max)!=1) {
      stop("iter_max must be a positive integer")
    }
    if(iter_max<1) {
      stop("iter_max must be a positive integer")
    }
    if(base::is.infinite(iter_max)==TRUE) {
      stop("iter_max must be a positive integer")
    }
    if(base::is.na(iter_max)==TRUE) {
      stop("iter_max must be a positive integer")
    }
    if(base::is.null(iter_max)==TRUE) {
      stop("iter_max must be a positive integer")
    }
    if(iter_max%%1!=0) {
      stop("iter_max must be a positive integer")
    }
    
    
    # kmeans_algorithm
    if(base::missing(kmeans_algorithm)) {
      stop("kmeans_algorithm input not found")
    }
    if(base::length(kmeans_algorithm)!=1) {
      stop("see ?kmeans: kmeans_algorithm must be one of: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")
    }
    if(base::is.character(kmeans_algorithm)==FALSE) {
      stop("see ?kmeans: kmeans_algorithm must be one of: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")
    }
    if(kmeans_algorithm %in% c("Hartigan-Wong", "Lloyd", "Forgy",
                               "MacQueen")==FALSE) {
      stop("see ?kmeans: kmeans_algorithm must be one of: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")
    }
  }
  
  
  
  # compute gap statistics
  get_gap_k <- function (km,
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
  
  
  
  # check input parameters
  check_input(B_gap = B_gap,
              ks = ks,
              x = x,
              n_start = n_start,
              iter_max = iter_max,
              cores = cores,
              kmeans_algorithm = kmeans_algorithm)
  
  
  
  # sort ks, smallest k first, largest last
  ks <- base::sort(ks, decreasing = FALSE)
  
  
  
  # clustering
  base::message("1) clustering \n")
  kmeans_obj <- parallel::mclapply(X = ks,
                                   FUN = stats::kmeans,
                                   x = x,
                                   nstart = n_start,
                                   iter.max = iter_max,
                                   mc.cores = cores,
                                   algorithm = kmeans_algorithm)
  base::names(kmeans_obj) <- ks
  
  
  
  base::message("2) gap-stat \n")
  gap_stats <- parallel::mclapply(X = kmeans_obj,
                                  FUN = get_gap_k,
                                  x = x,
                                  B_gap = B_gap,
                                  mc.cores = cores,
                                  mc.cleanup = TRUE,
                                  n_start = n_start,
                                  iter_max = iter_max,
                                  kmeans_algorithm = kmeans_algorithm)
  
  
  # within cluster sum of squares
  base::message("3) WCSS \n")
  wcss_data <- lapply(X = kmeans_obj, FUN =  function(x) {
    return(x$tot.withinss)
  })
  
  
  
  # convert result to vectors as opposed to kmeans objects
  kmeans_obj <- lapply(X = kmeans_obj, FUN = km2vec <- function(x) {
    c <- as.character(x$cluster)
    return(c)
  })
  
  
  boot_obj <- list(obj = kmeans_obj,
                   wcss = wcss_data,
                   gap = gap_stats)
  
  
  
  # raw gap stats
  gap_matrix <- base::matrix(data = 0, nrow = B_gap, ncol = base::length(ks))
  wcss_matrix <- base::matrix(data = 0, nrow = 1, ncol = base::length(ks))
  
  
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
  gap_mean <- base::apply(X = gap_matrix,
                          MARGIN = 2,
                          FUN = base::mean)
  gap_se <- base::sqrt((1/(B_gap) + 1)*base::apply(
    X = gap_matrix, MARGIN = 2, FUN = stats::var))
  gap_stats_summary <- base::data.frame(gap_mean = gap_mean,
                                        k = ks,
                                        gap_SE = gap_se,
                                        L95 = gap_mean-gap_se*1.96,
                                        H95 = gap_mean+gap_se*1.96)
  
  
  # compute wcss summary
  wcss_mean <- base::apply(X = wcss_matrix,
                           MARGIN = 2,
                           FUN = base::mean)
  wcss_stats_summary <- base::data.frame(wcss_mean = wcss_mean,
                                         k = ks)
  
  
  return(base::structure(class = "boot_k",
                         base::list(boot_obj = boot_obj,
                                    wcss_stats_summary = wcss_stats_summary,
                                    gap_stats_summary = gap_stats_summary,
                                    wcss_stats = wcss_stats,
                                    gap_stats = gap_stats)))
}
