
get_r <- function(x,
                  rs,
                  B_gap = 20,
                  n_start = 20,
                  iter_max = 100,
                  louvain_algorithm = "original",
                  knn_k = 50,
                  cores = 1) {


  # check input param
  check_input <- function(B_gap,
                          rs,
                          x,
                          n_start,
                          iter_max,
                          cores,
                          louvain_algorithm,
                          knn_k) {

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
    if(base::missing(B_gap)) {
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



    # check rs
    if(base::missing(rs)) {
      stop("rs input not found")
    }
    if(base::is.numeric(rs)==FALSE) {
      stop("rs must be a positive number or vector of positive numbers")
    }
    if(base::is.vector(x = rs)==FALSE) {
      stop("rs must be a positive number or vector of positive numbers")
    }
    if(base::length(rs)<=0) {
      stop("rs must be a positive number or vector of positive numbers")
    }
    if(base::any(base::is.infinite(rs))==TRUE) {
      stop("rs must be a positive number or vector of positive numbers,
           no infinite values are allowed")
    }
    if(base::any(base::is.na(rs))==TRUE) {
      stop("rs must be a positive number or vector of positive numbers,
           no NAs are allowed")
    }
    if(base::any(base::is.null(rs))==TRUE) {
      stop("rs must be a positive number or vector of positive numbers,
           no NULLs are allowed")
    }
    if(base::any(rs<0)==TRUE) {
      stop("rs must be a positive number or vector of positive numbers")
    }
    if(base::any(base::duplicated(rs))==TRUE) {
      stop("rs must be a positive number or vector of positive numbers,
           duplicate r values are not allowed")
    }
    if(base::length(rs)==1) {
      stop("rs must be a positive number or vector of positive numbers")
    }



    # check cores
    if(base::missing(cores)) {
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
    if(base::missing(x = n_start)) {
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
    if(base::missing(iter_max)) {
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


    # louvain_algorithm
    if(base::missing(louvain_algorithm)) {
      stop("louvain_algorithm input not found")
    }
    if(base::length(louvain_algorithm)!=1) {
      stop("see ?FindClusters from R-package Seurat: louvain_algorithm must be
      one of: original, LMR, SLM or Leiden")
    }
    if(base::is.character(louvain_algorithm)==FALSE) {
      stop("see ?FindClusters from R-package Seurat: louvain_algorithm must be
      one of: original, LMR, SLM or Leiden")
    }
    if(louvain_algorithm %in% c("original", "LMR", "SLM", "Leiden")==FALSE) {
      stop("see ?FindClusters from R-package Seurat: louvain_algorithm must be
      one of: original, LMR, SLM or Leiden")
    }


    # check knn_k
    if(base::missing(knn_k)) {
      stop("knn_k input not found")
    }
    if(base::is.numeric(knn_k)==FALSE) {
      stop("knn_k must be a positive integer")
    }
    if(base::length(knn_k)!=1) {
      stop("knn_k must be a positive integer")
    }
    if(base::is.infinite(knn_k)==TRUE) {
      stop("knn_k must be a positive integer")
    }
    if(base::is.na(knn_k)==TRUE) {
      stop("knn_k must be a positive integer")
    }
    if(knn_k<1) {
      stop("knn_k must be a positive integer")
    }
    if(knn_k%%1!=0) {
      stop("knn_k must be a positive integer")
    }
  }


  get_wcss <- function(x, l) {
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


  get_ks <- function(l) {
    return(base::length(base::unique(l)))
  }


  get_gap_r_k1 <- function(x, B_gap) {

    spaceH0 <- "original"
    n <- base::nrow(x)
    ii <- base::seq_len(length.out = n)


    get_Wk <- function(X) {
      return(base::log(get_wcss(x = X, l = base::rep(
        1, times = base::nrow(X)))))
    }


    logW <- base::log(get_wcss(l = base::rep(1, times = base::nrow(x)), x=x))
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



  # compute gap statistics
  get_gap_r <- function(i,
                        l,
                        x,
                        B_gap,
                        n_start,
                        iter_max,
                        louvain_algorithm,
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
        algorithm = map_louvain_algname(louvain_algorithm),
        verbose = FALSE)

      wcss <- get_wcss(x = X, l = lc[,1])
      return(base::log(wcss))
    }


    r <- base::as.numeric(base::names(l)[i])
    l <- l[[i]]
    logW <- base::log(get_wcss(l = l, x = x))
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




  # check input
  check_input(B_gap = B_gap,
              rs = rs,
              x = x,
              n_start = n_start,
              iter_max = iter_max,
              cores = cores,
              louvain_algorithm = louvain_algorithm,
              knn_k = knn_k)



  # sort rs, smallest r first, largest r last
  rs <- base::sort(rs, decreasing = FALSE)


  # add cell ids if needed
  if(is.null(rownames(x))) {
    rownames(x) <- base::seq_len(length.out = nrow(x))
  }
  # create Knn graph
  knn <- Seurat::FindNeighbors(object = x,
                               k.param = knn_k)


  # clustering
  base::message("1) clustering")
  louvain_obj <- parallel::mclapply(
    X = rs,
    FUN = Seurat::FindClusters,
    object = knn$snn,
    n.start = n_start,
    n.iter = iter_max,
    mc.cores = cores,
    algorithm = map_louvain_algname(louvain_algorithm),
    modularity.fxn = 1,
    initial.membership = NULL,
    node.sizes = NULL,
    verbose = FALSE)
  base::names(louvain_obj) <- rs


  # convert result to vectors as opposed to data.frames
  louvain_obj <- lapply(X = louvain_obj, FUN = df2vec <- function(x) {
    res <- names(x)
    res <- gsub(pattern = "res\\.", replacement = '', x = res)
    c <- as.character(x[, 1])
    return(c)
  })



  # Gap stats
  base::message("2) gap statistic")
  q <- parallel::mclapply(X = base::seq_len(length.out = length(louvain_obj)),
                          FUN = get_gap_r,
                          l = louvain_obj,
                          x = x,
                          B_gap = B_gap,
                          n_start = n_start,
                          iter_max = iter_max,
                          louvain_algorithm = louvain_algorithm,
                          mc.cores = cores,
                          knn_k = knn_k)


  # if k = 1 not present do
  q0 <- vector(mode = "list", length = 1)
  q0[[1]] <- get_gap_r_k1(x = x, B_gap = B_gap)
  gap_stats <- append(q, q0)
  rm(q, q0)


  # get k for each louvain obj
  ks <- base::unlist(base::lapply(X = louvain_obj,
                                  FUN = get_ks))


  # within cluster sum of squares
  base::message("3) WCSS")
  wcss_data <- lapply(X = louvain_obj,
                      FUN = get_wcss,
                      x = x)


  boot_obj <- list(obj = louvain_obj,
                   wcss = wcss_data,
                   gap = gap_stats)


  # raw gap stats
  gap_matrix <- base::matrix(data = 0, nrow = B_gap, ncol = base::length(rs))
  wcss_matrix <- base::matrix(data = 0, nrow = 1, ncol = base::length(rs))


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
  gap_mean <- base::apply(X = gap_matrix,
                          MARGIN = 2,
                          FUN = base::mean)
  gap_se <- base::sqrt((1/(B_gap) + 1)*base::apply(
    X = gap_matrix, MARGIN = 2, FUN = stats::var))
  gap_stats_summary <- base::data.frame(gap_mean = gap_mean,
                                        r = rs,
                                        k = ks,
                                        gap_SE = gap_se,
                                        L95 = gap_mean-gap_se*1.96,
                                        H95 = gap_mean+gap_se*1.96)


  # compute wcss summary
  wcss_mean <- base::apply(X = wcss_matrix,
                           MARGIN = 2,
                           FUN = base::mean)
  wcss_stats_summary <- base::data.frame(wcss_mean = wcss_mean,
                                         r = rs,
                                         k = ks)


  return(base::structure(class = "boot_r",
                         base::list(boot_obj = boot_obj,
                                    wcss_stats_summary = wcss_stats_summary,
                                    gap_stats_summary = gap_stats_summary,
                                    wcss_stats = wcss_stats,
                                    gap_stats = gap_stats)))
}

