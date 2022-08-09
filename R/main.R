

get_k <- function(x,
                  ks,
                  B = 10,
                  B_gap = 5,
                  cv_prop = 1,
                  n_start = 100,
                  iter_max = 200,
                  kmeans_algorithm = "MacQueen",
                  mini_output = F,
                  cores = 1) {


  # check input param
  check_input <- function(B,
                          B_gap,
                          cv_prop,
                          ks,
                          x,
                          n_start,
                          iter_max,
                          cores,
                          mini_output,
                          kmeans_algorithm) {

    # check x
    if(base::missing(x)==TRUE) {
      stop("x input not found")
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



    # check B
    if(base::missing(B)==TRUE) {
      stop("B input not found")
    }
    if(base::is.numeric(B)==FALSE) {
      stop("B must be a positive integer > 0")
    }
    if(base::length(B)!=1) {
      stop("B must be a positive integer > 0")
    }
    if(B<1) {
      stop("B must be a positive integer > 0")
    }
    if(base::is.infinite(B)==TRUE) {
      stop("B must be a positive integer > 0")
    }
    if(base::is.na(B)==TRUE) {
      stop("B must be a positive integer > 0")
    }
    if(B%%1!=0) {
      stop("B must be a positive integer > 0")
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


    # check cv_prop
    if(base::missing(cv_prop)==TRUE) {
      stop("cv_prop input not found")
    }
    if(base::is.numeric(cv_prop) == FALSE) {
      stop("cv_prop is a number between 0 (excluding) and 1")
    }
    if(base::length(cv_prop)!=1) {
      stop("cv_prop is a number between 0 (excluding) and 1")
    }
    if(base::is.infinite(cv_prop)==TRUE) {
      stop("cv_prop is a number between 0 (excluding) and 1")
    }
    if(base::is.na(cv_prop)==TRUE) {
      stop("cv_prop is a number between 0 (excluding) and 1")
    }
    if(cv_prop<=0|cv_prop>1) {
      stop("cv_prop is a number between 0 (excluding) and 1")
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


    # mini_output
    if(base::missing(mini_output)==TRUE) {
      stop("mini_output input not found")
    }
    if(base::is.logical(mini_output)==FALSE) {
      stop("mini_output is a logical parameter (TRUE or FALSE)")
    }
    if(base::length(mini_output)!=1) {
      stop("mini_output is a logical parameter (TRUE or FALSE)")
    }
    if(base::is.na(mini_output)==TRUE) {
      stop("mini_output is a logical parameter (TRUE or FALSE)")
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
    if(base::missing(kmeans_algorithm)==TRUE) {
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
  get_gap <- function (km,
                       x,
                       d.power = 1,
                       B_gap,
                       n_start,
                       iter_max,
                       spaceH0 = "original",
                       kmeans_algorithm) {


    cs <- km$cluster
    n <- base::nrow(x)
    ii <- base::seq_len(n)
    kk <- base::length(base::unique(cs))

    W.k <- function(X, kk) {
      clus <- stats::kmeans(x = X,
                            centers = kk,
                            algorithm = kmeans_algorithm,
                            iter.max = iter_max,
                            nstart = n_start)$cluster

      0.5 * base::sum(base::vapply(base::split(ii, clus), function(I) {
        xs <- X[I, , drop = FALSE]
        base::sum(stats::dist(xs)^d.power/base::nrow(xs))
      }, 0))
    }

    get_Wk <- function(X, kk) {
      clus <- stats::kmeans(x = X,
                            centers = kk,
                            algorithm = kmeans_algorithm,
                            iter.max = iter_max,
                            nstart = n_start)

      log_Wk <- base::log(base::sum(clus$withinss)*0.5)
      return(log_Wk)
    }

    get_logW <- function(X, cs) {
      0.5 * base::sum(base::vapply(base::split(ii, cs), function(I) {
        xs <- X[I, , drop = FALSE]
        base::sum(stats::dist(xs)^d.power/base::nrow(xs))
      }, 0))
    }

    logW <- base::numeric(1)
    E.logW <- base::numeric(1)
    SE.sim <- base::numeric(1)

    # original
    # logW <- base::log(get_logW(x, cs=cs))
    # simpler
    logW <- base::log(base::sum(km$withinss)*0.5)

    xs <- base::scale(x, center = TRUE, scale = FALSE)
    m.x <- base::rep(base::attr(xs, "scaled:center"), each = n)
    rng.x1 <- base::apply(xs, 2L, base::range)
    logWks <- base::matrix(0, B_gap, 1)
    for (b in 1:B_gap) {
      z1 <- base::apply(rng.x1, 2, function(M, nn) stats::runif(
        nn, min = M[1], max = M[2]), nn = n)
      z <- z1 + m.x

      # original
      # logWks[b, 1] <- base::log(W.k(z, kk = kk))

      # simpler
      logWks[b, 1] <- get_Wk(X = z,
                             kk = kk)
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
  check_input(B = B,
              B_gap = B_gap,
              cv_prop = cv_prop,
              ks = ks,
              x = x,
              n_start = n_start,
              iter_max = iter_max,
              cores = cores,
              mini_output = mini_output,
              kmeans_algorithm = kmeans_algorithm)



  # sort ks, smallest k first, largest last
  ks <- base::sort(ks, decreasing = F)



  boot_obj <- base::vector(mode = "list", length = B)
  for(b in 1:B) {
    base::cat("boot:", b, " : ")


    # draw sample of cells
    if(cv_prop==1) {
      js <- 1:base::nrow(x)
    } else {
      js <- base::sample(x = 1:nrow(x),
                         size = base::ceiling(nrow(x)*cv_prop),
                         replace = F)
    }

    # clustering
    base::cat("1) clustering, ")
    kmeans_obj <- parallel::mclapply(X = ks,
                                     FUN = stats::kmeans,
                                     x = x[js, ],
                                     nstart = n_start,
                                     iter.max = iter_max,
                                     mc.cores = cores,
                                     algorithm = kmeans_algorithm)
    base::names(kmeans_obj) <- ks
    boot_obj[[b]] <- kmeans_obj






    cat("2) gap-stat, ")
    gap_stats <- parallel::mclapply(X = kmeans_obj,
                                    FUN = get_gap,
                                    x = x[js, ],
                                    B_gap = B_gap,
                                    d.power = 1,
                                    mc.cores = cores,
                                    mc.cleanup = T,
                                    n_start = n_start,
                                    iter_max = iter_max,
                                    kmeans_algorithm = kmeans_algorithm)

    # within cluster sum of squares
    cat("3) WCSS. \n")
    # extract WCSS
    wcss_data <- lapply(X = kmeans_obj, FUN =  function(x) {
      return(x$tot.withinss)
    })


    # extract CH: Calinski-Harabasz Index
    ch_data <- lapply(X = kmeans_obj, FUN = function(x) {
      k <- base::nrow(x$centers)
      return((x$betweenss/x$tot.withinss)*
               ((base::sum(x$size)-k)/
                  (k-1)))
    })


    boot_obj[[b]] <- list(obj = kmeans_obj,
                          wcss = wcss_data,
                          ch = ch_data,
                          gap = gap_stats,
                          cell_i = js)

  }
  base::names(boot_obj) <- 1:B



  # collect clustering data
  gap_stats <- base::vector(mode = "list", length = base::length(boot_obj))
  wcss_stats <- base::vector(mode = "list", length = base::length(boot_obj))
  ch_stats <- base::vector(mode = "list", length = base::length(boot_obj))

  # raw gap stats
  gap_matrix <- base::matrix(data = 0, nrow = B*B_gap, ncol = base::length(ks))
  wcss_matrix <- base::matrix(data = 0, nrow = B, ncol = base::length(ks))
  ch_matrix <- base::matrix(data = 0, nrow = B, ncol = base::length(ks))


  # loop over top-bootstrap iterations B
  for(i in 1:base::length(boot_obj)) {

    gap_vec <- base::numeric(length = base::length(ks))
    wcss_vec <- base::numeric(length = base::length(ks))
    ch_vec <- base::numeric(length = base::length(ks))

    # loop over ks
    for(j in 1:base::length(ks)) {
      gap_vec[j] <- boot_obj[[i]]$gap[[j]]$gap
      wcss_vec[j] <- boot_obj[[i]]$wcss[[j]]
      ch_vec[j] <- boot_obj[[i]]$ch[[j]]

      gap_matrix[((i-1)*B_gap+1):((i-1)*B_gap+B_gap), j] <-
        boot_obj[[i]]$gap[[j]]$logWks-boot_obj[[i]]$gap[[j]]$logW
      wcss_matrix[,j] <- boot_obj[[i]]$wcss[[j]]
      ch_matrix[,j] <- boot_obj[[i]]$ch[[j]]

    }

    gap_stats[[i]] <- base::data.frame(boot = i, gap = gap_vec, k = ks)
    wcss_stats[[i]] <- base::data.frame(boot = i, wcss = wcss_vec, k = ks)
    ch_stats[[i]] <- base::data.frame(boot = i, wcss = ch_vec, k = ks)

  }

  # collect DFs
  gap_stats <- base::do.call(base::rbind, gap_stats)
  wcss_stats <- base::do.call(base::rbind, wcss_stats)
  ch_stats <- base::do.call(base::rbind, ch_stats)

  # compute gap summary
  gap_mean <- base::apply(X = gap_matrix,
                          MARGIN = 2,
                          FUN = base::mean)
  gap_se <- base::sqrt((1/(B*B_gap) + 1)*base::apply(X = gap_matrix,
                                                     MARGIN = 2,
                                                     FUN = stats::var))
  gap_stats_summary <- base::data.frame(gap_mean = gap_mean,
                                        k = ks,
                                        gap_SE = gap_se,
                                        L95 = gap_mean-gap_se*1.96,
                                        H95 = gap_mean+gap_se*1.96)


  # compute wcss summary
  wcss_mean <- base::apply(X = wcss_matrix,
                           MARGIN = 2,
                           FUN = base::mean)
  wcss_se <- base::sqrt((1/B + 1)*base::apply(X = wcss_matrix,
                                              MARGIN = 2,
                                              FUN = stats::var))
  wcss_stats_summary <- base::data.frame(wcss_mean = wcss_mean,
                                         k = ks,
                                         wcss_SE = wcss_se,
                                         L95 = wcss_mean-wcss_se*1.96,
                                         H95 = wcss_mean+wcss_se*1.96)


  # compute ch summary
  ch_mean <- base::apply(X = ch_matrix,
                         MARGIN = 2,
                         FUN = base::mean)
  ch_se <- base::sqrt((1/B + 1)*base::apply(X = ch_matrix,
                                            MARGIN = 2,
                                            FUN = stats::var))
  ch_stats_summary <- base::data.frame(ch_mean = ch_mean,
                                       k = ks,
                                       ch_SE = ch_se,
                                       L95 = ch_mean-ch_se*1.96,
                                       H95 = ch_mean+ch_se*1.96)


  if(mini_output) {
    return(base::structure(class = "boot_k",
                           base::list(boot_obj = NA,
                                      wcss_stats_summary = wcss_stats_summary,
                                      gap_stats_summary = gap_stats_summary,
                                      ch_stats_summary = ch_stats_summary,
                                      wcss_stats = wcss_stats,
                                      gap_stats = gap_stats,
                                      ch_stats = ch_stats)))

  }

  return(base::structure(class = "boot_k",
                         base::list(boot_obj = boot_obj,
                                    wcss_stats_summary = wcss_stats_summary,
                                    gap_stats_summary = gap_stats_summary,
                                    ch_stats_summary = ch_stats_summary,
                                    wcss_stats = wcss_stats,
                                    gap_stats = gap_stats,
                                    ch_stats = ch_stats)))
}




get_bubbletree <- function(x,
                           k,
                           B = 100,
                           N_eff = 100,
                           n_start = 1000,
                           iter_max = 300,
                           kmeans_algorithm = "MacQueen",
                           cores = 1,
                           seed = NULL,
                           round_digits = 2,
                           show_simple_count = F) {

  # check input param
  check_input <- function(x,
                          k,
                          n_start,
                          iter_max,
                          B,
                          N_eff,
                          cores,
                          seed,
                          round_digits,
                          show_simple_count,
                          kmeans_algorithm) {


    # check x
    if(base::missing(x)==TRUE) {
      stop("x input not found")
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



    # check B
    if(base::missing(B)==TRUE) {
      stop("B input not found")
    }
    if(base::is.numeric(B)==FALSE) {
      stop("B must be a positive integer > 0")
    }
    if(base::length(B)!=1) {
      stop("B must be a positive integer > 0")
    }
    if(B<1) {
      stop("B must be a positive integer > 0")
    }
    if(base::is.infinite(B)==TRUE) {
      stop("B must be a positive integer > 0")
    }
    if(base::is.na(B)==TRUE) {
      stop("B must be a positive integer > 0")
    }
    if(B%%1!=0) {
      stop("B must be a positive integer > 0")
    }



    # check k
    if(base::missing(k)==TRUE) {
      stop("k input not found")
    }
    if(is.numeric(k)==F) {
      stop("k must be a positive integer (k>=2) to build a bubbletree")
    }
    if(length(k)!=1) {
      stop("k must be a positive integer (k>=2) to build a bubbletree")
    }
    if(k<=1) {
      stop("k must be a positive integer (k>=2) to build a bubbletree")
    }
    if(is.infinite(k)==TRUE) {
      stop("k must be a positive integer (k>=2) to build a bubbletree")
    }
    if(k%%1!=0) {
      stop("k must be a positive integer (k>=2) to build a bubbletree")
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
    if(base::missing(kmeans_algorithm)==TRUE) {
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





    # check N_eff
    if(base::missing(N_eff)==TRUE) {
      stop("N_eff input not found")
    }
    if(base::is.numeric(N_eff)==FALSE) {
      stop("N_eff must be a positive integer")
    }
    if(base::length(N_eff)!=1) {
      stop("N_eff must be a positive integer")
    }
    if(N_eff<1) {
      stop("N_eff must be a positive integer")
    }
    if(base::is.infinite(N_eff)==TRUE) {
      stop("N_eff must be a positive integer")
    }
    if(base::is.na(N_eff)==TRUE) {
      stop("N_eff must be a positive integer")
    }
    if(base::is.null(N_eff)==TRUE) {
      stop("N_eff must be a positive integer")
    }
    if(N_eff%%1!=0) {
      stop("N_eff must be a positive integer")
    }



    # check seed
    if(base::is.null(seed)==FALSE) {
      if(base::is.numeric(seed)==FALSE) {
        stop("seed must be a positive integer")
      }
      if(base::length(seed)!=1) {
        stop("seed must be a positive integer")
      }
      if(seed<=0) {
        stop("seed must be a positive integer")
      }
      if(base::is.finite(seed)==FALSE) {
        stop("seed must be a positive integer")
      }
      if(seed%%1!=0) {
        stop("seed must be a positive integer")
      }
    }



    # check round_digits
    if(base::missing(round_digits)==TRUE) {
      stop("round_digits input not found")
    }
    if(base::is.numeric(round_digits)==F) {
      stop("round_digits must be a positive integer")
    }
    if(base::length(round_digits)!=1) {
      stop("round_digits must be a positive integer")
    }
    if(round_digits<0) {
      stop("round_digits must be a positive integer")
    }
    if(base::is.finite(round_digits)==FALSE) {
      stop("round_digits must be a positive integer")
    }
    if(round_digits%%1!=0) {
      stop("round_digits must be a positive integer")
    }



    # show_simple_count
    if(base::missing(show_simple_count)==TRUE) {
      stop("show_simple_count input not found")
    }
    if(length(show_simple_count)!=1) {
      stop("show_simple_count is a logical parameter (TRUE or FALSE)")
    }
    if(is.logical(show_simple_count)==F) {
      stop("show_simple_count is a logical parameter (TRUE or FALSE)")
    }
    if(base::is.na(show_simple_count)==TRUE) {
      stop("show_simple_count is a logical parameter (TRUE or FALSE)")
    }


  }


  # check inputs
  check_input(x = x,
              k = k,
              n_start = n_start,
              iter_max = iter_max,
              B = B,
              N_eff = N_eff,
              cores = cores,
              seed = seed,
              round_digits = round_digits,
              show_simple_count = show_simple_count,
              kmeans_algorithm = kmeans_algorithm)


  # set seed for reproducibility
  if(base::is.null(seed)==FALSE) {
    base::set.seed(seed = seed)
  }
  else {
    seed <- base::sample(x = 1:10^6, size = 1)
    base::set.seed(seed = seed)
  }



  # perform k-means clustering
  cat("Clustering ... \n")
  km <- stats::kmeans(x = x,
                      centers = k,
                      nstart = n_start,
                      iter.max = iter_max,
                      algorithm = kmeans_algorithm)


  # pairwise distances
  cat("Bubbletree construction ... \n")
  pair_dist <- get_dist(B = B,
                        m = x,
                        c = km$cluster,
                        N_eff = N_eff,
                        cores = cores)



  # compute hierarchical clustering dendrogram
  d <- reshape2::acast(data = pair_dist$pca_pair_dist,
                       formula = c_i~c_j,
                       value.var = "M")
  d <- stats::as.dist(d)
  hc <- stats::hclust(d, method = "average")
  ph <- ape::as.phylo(x = hc)

  if(k<=2) {

    # browser()
    # build treetree
    t <- get_dendrogram(ph = ph,
                        cluster = km$cluster,
                        round_digits = round_digits,
                        show_simple_count = show_simple_count)

  }
  else {

    ph <- ape::unroot(phy = ph)

    # get branch support
    ph <- get_ph_support(main_ph = ph,
                         x = pair_dist$raw_pair_dist)

    # build bubbletree
    t <- get_dendrogram(ph = ph$main_ph,
                        cluster = km$cluster,
                        round_digits = round_digits,
                        show_simple_count = show_simple_count)

  }




  # collect input parameters: can be used for automated update
  input_par <- list(n_start = n_start,
                    iter_max = iter_max,
                    N_eff = N_eff,
                    B = B,
                    seed = seed,
                    round_digits = round_digits,
                    show_simple_count = show_simple_count,
                    kmeans_algorithm = kmeans_algorithm,
                    update_iteration = 0)


  return(base::structure(class = "bubbletree",
                         list(A = x,
                              k = k,
                              km = km,
                              ph = ph,
                              pair_dist = pair_dist,
                              cluster = km$cluster,
                              input_par = input_par,
                              tree = t$tree,
                              tree_meta = t$tree_meta)))

}



# Not exported at the moment.
update_bubbletree <- function(btd,
                              updated_bubbles,
                              k,
                              cores = 1) {

  # check input param
  check_input <- function(btd,
                          updated_bubbles,
                          k,
                          cores = 1) {

    # check btd
    if(is.na(btd)||is.null(btd)||is.na(class(btd))||
       is.null(class(btd))||class(btd)!="bubbletree") {
      stop("problem with the input bubbletree")
    }

    if(is.vector(btd$cluster)==F||
       is.na(is.vector(btd$cluster))||
       is.null(is.vector(btd$cluster))) {
      stop("no clustering results in bubbletree")
    }

    # check k
    if(is.numeric(k)==F) {
      stop("k must be a positive integer")
    }
    if(length(k)!=1) {
      stop("k must be a positive integer")
    }
    if(k<=0) {
      stop("k must be a positive integer")
    }


    if(is.vector(updated_bubbles)==F & length(updated_bubbles)==0) {
      stop("updated_bubbles should be a vector of positive integers")
    }
    if(length(updated_bubbles) >= k) {
      stop("k must be larger than the number of updated_bubbles")
    }


    if(all(updated_bubbles %in% unique(btd$cluster))==F) {
      stop(paste0("not all bubbles to be updated are found in bubbletree\n",
                  "bubbles to be updated:", paste0(updated_bubbles, collapse = ','), "\n",
                  "bubbles present:", paste0(unique(btd$cluster), collapse = ','), "\n"))
    }


    # check cores
    if(is.numeric(cores)==F) {
      stop("cores must be a positive integer")
    }
    if(length(cores)!=1) {
      stop("cores must be a positive integer")
    }
    if(cores<=0) {
      stop("cores must be a positive integer")
    }


    # kmeans_algorithm
    if(length(kmeans_algorithm) != 1) {
      stop("see ?kmeans: kmeans_algorithm must be one of: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")
    }
    if(kmeans_algorithm %in% c("Hartigan-Wong", "Lloyd", "Forgy",
                               "MacQueen") == F) {
      stop("see ?kmeans: kmeans_algorithm must be one of: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")
    }
  }

  update_bubbles <- function(A,
                             k,
                             n_start,
                             iter_max,
                             kmeans_algorithm) {

    # perform k-means clustering
    km <- stats::kmeans(x = A,
                        centers = k,
                        nstart = n_start,
                        iter.max = iter_max,
                        algorithm = kmeans_algorithm)

    return(km)

  }


  # get input from previous bubbletree data
  A <- btd$A
  n_start <- btd$input_par$n_start
  iter_max <- btd$input_par$iter_max
  N_eff <- btd$input_par$N_eff
  B <- btd$input_par$B
  round_digits <- btd$input_par$round_digits
  show_simple_count <- btd$input_par$show_simple_count
  seed <- btd$input_par$seed
  kmeans_algorithm <- btd$input_par$kmeans_algorithm
  update_iteration <- btd$input_par$update_iteration


  # set seed for reproducibility
  if(is.na(seed) == F) {
    set.seed(seed = seed)
  }

  # check input
  check_input(btd = btd,
              updated_bubbles = updated_bubbles,
              k = k,
              cores = cores)


  # loop around updated_bubbles
  cat("Updating bubble ... \n")
  j <- which(btd$cluster %in% updated_bubbles)

  # run update
  u_kms <- update_bubbles(A = A[j,],
                          k = k,
                          n_start = n_start,
                          iter_max = iter_max,
                          kmeans_algorithm = kmeans_algorithm)


  # update -> increase iteration
  update_iteration <- update_iteration + 1

  # update cluster naming
  btd$cluster[j] <- paste0(update_iteration, '_',
                           c(base::LETTERS,
                             base::letters)[u_kms$cluster])

  cat("Updating dendrogram ... \n")
  pair_dist <- get_dist(B = B,
                        m = A,
                        c = btd$cluster,
                        N_eff = N_eff,
                        cores = cores)

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
                      show_simple_count = show_simple_count)

  # update iteration
  btd$input_par$update_iteration <- update_iteration


  return(base::structure(class = "bubbletree",
                         list(A = A,
                              k = length(unique(btd$cluster)),
                              km = u_kms,
                              ph = ph,
                              pair_dist = pair_dist,
                              cluster = btd$cluster,
                              input_par = btd$input_par,
                              tree = t$tree,
                              tree_meta = t$tree_meta)))
}



get_dummy_bubbletree <- function(x,
                                 cs,
                                 B = 100,
                                 N_eff = 100,
                                 cores = 1,
                                 seed = NA,
                                 round_digits = 2,
                                 show_simple_count = F) {


  # check input param
  check_input <- function(x,
                          cs,
                          B,
                          N_eff,
                          cores,
                          seed,
                          round_digits,
                          show_simple_count) {

    # check x
    if(is.numeric(x)==F) {
      stop("x must be numeric matrix")
    }
    if(is.matrix(x)==F) {
      stop("x must be numeric matrix")
    }


    # check cs
    if(is.vector(cs)==F) {
      stop("cs must be a vector")
    }
    if(length(cs)!=nrow(x)) {
      stop("Error: length(cs) != nrow(x)")
    }
    if(length(unique(cs))<=1) {
      stop("1 or 0 clusters found in vector cs")
    }
    if(any(is.na(cs)|is.null(cs))==T) {
      stop("NA or NULL elements are found in cs")
    }
    if(any(is.na(cs)|is.null(cs))==T) {
      stop("NA or NULL elements are found in cs")
    }


    # check B
    if(is.numeric(B)==F) {
      stop("B must be a positive integer")
    }
    if(length(B)!=1) {
      stop("B must be a positive integer")
    }
    if(B<=0) {
      stop("B must be a positive integer")
    }


    # check N_eff
    if(is.numeric(N_eff)==F) {
      stop("N_eff must be a positive integer")
    }
    if(length(N_eff) != 1) {
      stop("N_eff must be a positive integer")
    }
    if(N_eff<=0) {
      stop("N_eff must be a positive integer")
    }


    # check cores
    if(is.numeric(cores)==F) {
      stop("cores must be a positive integer")
    }
    if(length(cores)!=1) {
      stop("cores must be a positive integer")
    }
    if(cores<=0) {
      stop("cores must be a positive integer")
    }


    # check seed
    if(is.numeric(seed)==F) {
      stop("seed must be a positive integer")
    }
    if(length(seed)!=1) {
      stop("seed must be a positive integer")
    }
    if(seed<=0) {
      stop("seed must be a positive integer")
    }



    # check round_digits
    if(is.numeric(round_digits)==F) {
      stop("round_digits must be a positive integer")
    }
    if(length(round_digits)!=1) {
      stop("round_digits must be a positive integer")
    }
    if(round_digits<0) {
      stop("round_digits must be a positive integer")
    }


    # show_simple_count
    if(length(show_simple_count)!=1) {
      stop("show_simple_count is a logical parameter (TRUE or FALSE)")
    }
    if(is.logical(show_simple_count)==F) {
      stop("show_simple_count is a logical parameter (TRUE or FALSE)")
    }
  }


  # set seed for reproducibility
  if(is.na(seed) == F) {
    set.seed(seed = seed)
  }
  else {
    seed <- base::sample(x = 1:10^6, size = 1)
    set.seed(seed = seed)
  }


  # check inputs
  check_input(x = x,
              cs = cs,
              B = B,
              N_eff = N_eff,
              cores = cores,
              seed = seed,
              round_digits = round_digits,
              show_simple_count = show_simple_count)


  # pairwise distances
  cat("Generating bubbletree ... \n")
  pair_dist <- get_dist(B = B,
                        m = x,
                        c = cs,
                        N_eff = N_eff,
                        cores = cores)

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

  # build treetree
  t <- get_dendrogram(ph = ph$main_ph,
                      cluster = cs,
                      round_digits = round_digits,
                      show_simple_count = show_simple_count)

  # collect input parameters: can be used for automated update
  input_par <- list(n_start = NA,
                    iter_max = NA,
                    N_eff = N_eff,
                    B = B,
                    seed = seed,
                    round_digits = round_digits,
                    show_simple_count = show_simple_count,
                    kmeans_algorithm = NA)


  return(base::structure(class = "dummy_bubbletree",
                         list(A = x,
                              k = length(unique(cs)),
                              km = NULL,
                              ph = ph,
                              pair_dist = pair_dist,
                              cluster = cs,
                              input_par = input_par,
                              tree = t$tree,
                              tree_meta = t$tree_meta)))
}




get_gini <- function(labels, clusters) {


  # check input param
  check_input <- function(labels, clusters) {

    # check labels
    if(base::missing(labels)==TRUE) {
      stop("labels is missing")
    }
    if(base::length(labels)<=1) {
      stop("labels must be a vector with more than one element")
    }
    if(base::is.character(labels)==FALSE&
       base::is.numeric(labels)==FALSE) {
      stop("labels can only contain characters or numbers")
    }
    if(base::is.vector(labels)==F) {
      stop("labels must be a vector")
    }
    if(base::any(base::is.infinite(labels))==TRUE) {
      stop("labels cannot have INF/NA/NULL values")
    }
    if(base::any(base::is.na(labels))==TRUE) {
      stop("labels cannot have INF/NA/NULL values")
    }
    if(base::any(base::is.null(labels))==TRUE) {
      stop("labels cannot have INF/NA/NULL values")
    }


    # check clusters
    if(base::missing(clusters)==TRUE) {
      stop("clusters is missing")
    }
    if(base::length(clusters)<=1) {
      stop("clusters must be a vector with more than one element")
    }
    if(base::is.character(clusters)==FALSE&
       base::is.numeric(clusters)==FALSE) {
      stop("clusters can only contain characters or numbers")
    }
    if(base::is.vector(clusters)==F) {
      stop("clusters must be a vector")
    }
    if(base::any(base::is.infinite(clusters))==TRUE) {
      stop("clusters cannot have INF/NA/NULL values")
    }
    if(base::any(base::is.na(clusters))==TRUE) {
      stop("clusters cannot have INF/NA/NULL values")
    }
    if(base::any(base::is.null(clusters))==TRUE) {
      stop("clusters cannot have INF/NA/NULL values")
    }

    if(base::length(labels)!=base::length(clusters)) {
      stop("labels and clusters must be equal-length vectors")
    }
  }


  get_gi <- function(c, l) {
    ls <- base::unique(l)
    l_len <- base::length(l)
    s <- 0
    for(i in 1:base::length(ls)) {
      s <- s + (base::sum(l == ls[i])/l_len)^2
    }
    return(s)
  }


  # check inputs
  check_input(labels = labels,
              clusters = clusters)


  cs <- base::unique(clusters)

  # for each cluster we get gini-index
  gi <- base::numeric(length = base::length(cs))
  base::names(gi) <- cs

  # cluster weights used to compute total gini
  wgi <- base::numeric(length = base::length(cs))
  base::names(wgi) <- cs

  for(i in 1:base::length(cs)) {
    j <- base::which(clusters == cs[i])
    wgi[i] <- base::length(j)/base::length(clusters)
    gi[i] <- 1-get_gi(c = clusters[j], l = labels[j])
  }

  # compute WGI
  wgi = base::sum(gi*wgi)

  # convert to data.frame for better plotting
  gi <- base::data.frame(cluster = names(gi),
                         GI = as.numeric(gi))

  return(base::list(gi = gi, wgi = wgi))
}




get_gini_k <- function(labels, k_obj) {


  # check input param
  check_input <- function(labels, k_obj) {

    # check labels
    if(base::missing(labels)==TRUE) {
      stop("labels is missing")
    }
    if(base::length(labels)<=1) {
      stop("labels must be a vector with more than one element")
    }
    if(base::is.vector(labels)==F) {
      stop("labels must be a vector")
    }
    if(base::any(base::is.infinite(labels))==TRUE) {
      stop("labels must be a vector")
    }
    if(base::any(base::is.na(labels))==TRUE) {
      stop("labels must be a vector")
    }
    if(base::any(base::is.null(labels))==TRUE) {
      stop("labels must be a vector")
    }


    # check k_obj
    if(base::missing(k_obj)==TRUE) {
      stop("k_obj is missing")
    }
    if(base::is.na(k_obj)||
       base::is.null(k_obj)||
       base::is.na(class(k_obj))||
       base::is.null(class(k_obj))||
       base::class(k_obj)!="boot_k") {
      stop("problem with k_obj")
    }

    if(base::is.list(k_obj$boot_obj)==F||
       base::is.na(k_obj$boot_obj)||
       base::is.null(k_obj$boot_obj)||
       base::length(k_obj)<=1) {
      stop("no boot_obj found in k_obj")
    }
  }


  # check inputs
  check_input(labels = labels,
              k_obj = k_obj)


  if(base::length(k_obj$boot_obj)==1&&
     base::is.na(k_obj$boot_obj)) {
    stop("You have to run 'get_k' with mini_output=FALSE. \n")
  }

  B <- base::length(k_obj$boot_obj)
  ks <- base::names(k_obj$boot_obj[[1]]$obj)

  total_o <- base::vector(mode = "list", length = B*base::length(ks))
  cluster_o <- base::vector(mode = "list", length = B*base::length(ks))
  counter <- 1
  for(i in 1:B) {
    for(j in 1:length(ks)) {
      cell_id <- k_obj$boot_obj[[i]]$cell_i
      gini <- get_gini(clusters = k_obj$boot_obj[[i]]$obj[[ks[j]]]$cluster,
                       labels = labels[cell_id])

      # collect total gini and cluster specific gini scores
      # total
      total_o[[counter]] <- base::data.frame(B = i,
                                             k = as.numeric(ks[j]),
                                             total_gini = gini$wgi)
      # cluster
      cluster_o[[counter]] <- base::data.frame(B = i,
                                               k = as.numeric(ks[j]),
                                               cluster = base::names(gini$gi),
                                               gini = gini$gi)
      counter <- counter + 1
    }
  }
  cluster_o <- base::do.call(rbind, cluster_o)
  total_o <- base::do.call(rbind, total_o)


  # next: compute summary from B values for each metric
  # total
  wgi_summary <- base::merge(
    x = stats::aggregate(wgi~k, data = total_o, FUN = base::mean),
    y = stats::aggregate(wgi~k, data = total_o, FUN = get_se),
    by = "k")
  base::colnames(wgi_summary) <- c("k", "wgi_mean", "wgi_SE")
  wgi_summary$L95 <- wgi_summary$wgi_mean-wgi_summary$wgi_SE*1.96
  wgi_summary$H95 <- wgi_summary$wgi_mean+wgi_summary$wgi_SE*1.96

  # cluster
  gi_summary <- base::merge(
    x = stats::aggregate(gi~k+cluster, data = cluster_o, FUN = base::mean),
    y = stats::aggregate(gi~k+cluster, data = cluster_o, FUN = get_se),
    by = c("k", "cluster"))
  base::colnames(gi_summary) <- c("k", "clusters", "gi_mean", "gi_SE")
  gi_summary$L95 <- gi_summary$gi_mean-gi_summary$gi_SE*1.96
  gi_summary$H95 <- gi_summary$gi_mean+gi_summary$gi_SE*1.96


  return(base::list(wgi_summary = wgi_summary,
              gi_summary = gi_summary,
              wgi = total_o,
              gi = cluster_o))
}


