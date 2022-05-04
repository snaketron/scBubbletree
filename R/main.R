#' Wrapper for k-means. Input: features matrix \code{x} with cells as rows and
#' features (such as PC embeddings) as columns; vector \code{ks} for the number
#' of k-means to try; B=number of bootstrapping iterations; the remaining
#' parameters are described in the function \code{kmeans} or are used for
#' multicore execution.
#'
#' @exportMethod
#'
get_k <- function(B = 20,
                  cv_prop = 1,
                  ks,
                  x,
                  n_start = 100,
                  iter_max = 50,
                  cores = 1,
                  mini_output = F,
                  approx_silhouette = T,
                  kmeans_algorithm = "MacQueen") {

  # check input param
  check_input <- function(B,
                          cv_prop,
                          ks,
                          x,
                          n_start,
                          iter_max,
                          cores,
                          mini_output,
                          approx_silhouette,
                          kmeans_algorithm) {

    # check x
    if(is.numeric(x)==F) {
      stop("x must be numeric matrix")
    }
    if(is.matrix(x)==F) {
      stop("x must be numeric matrix")
    }

    # check B
    if(is.numeric(B)==F) {
      stop("B must be a positive integer")
    }
    if(length(B)!=1) {
      stop("B must be a positive integer")
    }

    # check ks
    if(is.numeric(ks)==F) {
      stop("ks must be a positive integer or vector of positive integers")
    }
    if(any(ks<0)==T) {
      stop("ks must be a vector of positive integers")
    }
    if(any(ks>nrow(x))==T) {
      stop("ks must be a vector of positive integers < nrow(x)")
    }
    if(length(ks)==1) {
      if(all(ks<=1)) {
        stop("ks must be a positive integer > 1 or vector of positive integers")
      }
    }


    # check cv_prop
    if(is.numeric(cv_prop) == F) {
      stop("cv_prop is a number between 0 (excluding) and 1")
    }
    if(length(cv_prop) != 1) {
      stop("cv_prop is a number between 0 (excluding) and 1")
    }
    if(cv_prop<0|cv_prop>1) {
      stop("cv_prop is a number between 0 (excluding) and 1")
    }

    # check cores
    if(is.numeric(cores)==F) {
      stop("cores must be a positive integer")
    }
    if(length(cores)!=1) {
      stop("cores must be a positive integer")
    }

    # mini_output
    if(is.logical(mini_output)==F) {
      stop("mini_output is a logical parameter (TRUE or FALSE)")
    }
    if(length(mini_output)!=1) {
      stop("mini_output is a logical parameter (TRUE or FALSE)")
    }

    # approx_silhouette
    if(is.logical(approx_silhouette)==F) {
      stop("approx_silhouette is a logical parameter (TRUE or FALSE)")
    }
    if(length(approx_silhouette)!=1) {
      stop("approx_silhouette is a logical parameter (TRUE or FALSE)")
    }

    # n_start
    if(is.numeric(n_start)==F) {
      stop("n_start must be a positive integer")
    }
    if(length(n_start) != 1) {
      stop("n_start must be a positive integer")
    }
    if(n_start < 0) {
      stop("n_start must be a positive integer")
    }

    # iter_max
    if(is.numeric(iter_max)==F) {
      stop("iter_max must be a positive integer")
    }
    if(length(iter_max) != 1) {
      stop("iter_max must be a positive integer")
    }
    if(iter_max < 0) {
      stop("iter_max must be a positive integer")
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



  # compute approximate average silhouette
  get_avg_sil <- function(km,
                          df,
                          cv_prop,
                          approx_silhouette) {

    if(cv_prop < 1) {
      js <- base::sample(x = 1:nrow(df),
                         size = base::ceiling(nrow(df)*cv_prop),
                         replace = F) # replace = T => silhouette increases monotonically
    } else {
      js <- 1:nrow(df)
    }

    cs <- km$cluster[js]
    if(base::length(unique(cs))==1) {
      return(NA)
    }

    if(approx_silhouette == F) {
      ss <- cluster::silhouette(
        x = cs, stats::dist(x = df[js,], method = "euclidean"))
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
                       cv_prop = 1,
                       spaceH0 = "original",
                       kmeans_algorithm) {

    if(cv_prop < 1) {
      cs <- km$cluster
      js <- base::sample(x = 1:nrow(x),
                         size = ceiling(nrow(x)*cv_prop),
                         replace = F) # ) # in accordance with silhouette
      x <- x[js, ]
      cs <- cs[js]
    } else {
      cs <- km$cluster
    }

    n <- nrow(x)
    ii <- seq_len(n)
    kk <- length(unique(cs))

    W.k <- function(X, kk) {
      clus <- kmeans(x = X, centers = kk,
                     algorithm = kmeans_algorithm)$cluster
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




  # check input parameters
  check_input(B = B,
              cv_prop = cv_prop,
              ks = ks,
              x = x,
              n_start = n_start,
              iter_max = iter_max,
              cores = cores,
              mini_output = mini_output,
              approx_silhouette = approx_silhouette,
              kmeans_algorithm = kmeans_algorithm)


  boot_obj <- vector(mode = "list", length = B)
  for(b in 1:B) {
    cat("boot:", b, " : ")

    # clustering
    cat("1) clustering, ")
    kmeans_obj <- parallel::mclapply(X = ks,
                                     FUN = stats::kmeans,
                                     x = x,
                                     nstart = n_start,
                                     iter.max = iter_max,
                                     mc.cores = cores,
                                     algorithm = kmeans_algorithm)
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
                                     df = x,
                                     mc.cores = cores,
                                     mc.cleanup = T,
                                     cv_prop = cv_prop,
                                     approx_silhouette = approx_silhouette)


    cat("3) gap-stat, ")
    gap_stats <- parallel::mclapply(X = kmeans_obj,
                                    FUN = get_gap,
                                    x = x,
                                    B = 5,
                                    d.power = 1,
                                    cv_prop = cv_prop,
                                    mc.cores = cores,
                                    mc.cleanup = T,
                                    kmeans_algorithm = kmeans_algorithm)
    # within sum of squares
    cat("4) WSS. \n")
    boot_obj[[b]] <- list(obj = kmeans_obj,
                          wss = wss_data,
                          sil = sil_kmeans,
                          gap = gap_stats)

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

  if(mini_output) {
    return(base::structure(class = "boot_k",
                           list(boot_obj = NA,
                                wss_stats_summary = wss_stats_summary,
                                sil_stats_summary = sil_stats_summary,
                                gap_stats_summary = gap_stats_summary,
                                wss_stats = wss_stats,
                                sil_stats = sil_stats,
                                gap_stats = gap_stats)))

  }

  return(base::structure(class = "boot_k",
                         list(boot_obj = boot_obj,
                              wss_stats_summary = wss_stats_summary,
                              sil_stats_summary = sil_stats_summary,
                              gap_stats_summary = gap_stats_summary,
                              wss_stats = wss_stats,
                              sil_stats = sil_stats,
                              gap_stats = gap_stats)))
}




#'
#' @exportMethod
#'
get_bubbletree <- function(x,
                           k,
                           n_start = 100,
                           iter_max = 50,
                           B = 100,
                           N_eff = 100,
                           cores = 1,
                           seed = NA,
                           verbose = F,
                           round_digits = 2,
                           show_branch_support = T,
                           show_simple_count = F,
                           kmeans_algorithm = "MacQueen") {

  # check input param
  check_input <- function(x,
                          k,
                          n_start,
                          iter_max,
                          B,
                          N_eff,
                          cores,
                          seed,
                          verbose,
                          round_digits,
                          show_branch_support,
                          show_simple_count,
                          kmeans_algorithm) {

    # check x
    if(is.numeric(x)==F) {
      stop("x must be numeric matrix")
    }
    if(is.matrix(x)==F) {
      stop("x must be numeric matrix")
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


    # n_start
    if(is.numeric(n_start)==F) {
      stop("n_start must be a positive integer")
    }
    if(length(n_start) != 1) {
      stop("n_start must be a positive integer")
    }
    if(n_start < 0) {
      stop("n_start must be a positive integer")
    }


    # iter_max
    if(is.numeric(iter_max)==F) {
      stop("iter_max must be a positive integer")
    }
    if(length(iter_max) != 1) {
      stop("iter_max must be a positive integer")
    }
    if(iter_max < 0) {
      stop("iter_max must be a positive integer")
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


    # check verbose
    if(is.logical(verbose)==F) {
      stop("verbose is a logical parameter (TRUE or FALSE)")
    }
    if(length(verbose)!=1) {
      stop("verbose is a logical parameter (TRUE or FALSE)")
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


    # show_branch_support
    if(length(show_branch_support)!=1) {
      stop("show_branch_support is a logical parameter (TRUE or FALSE)")
    }
    if(is.logical(show_branch_support)==F) {
      stop("show_branch_support is a logical parameter (TRUE or FALSE)")
    }


    # show_simple_count
    if(length(show_simple_count)!=1) {
      stop("show_simple_count is a logical parameter (TRUE or FALSE)")
    }
    if(is.logical(show_simple_count)==F) {
      stop("show_simple_count is a logical parameter (TRUE or FALSE)")
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


  # set seed for reproducibility
  if(is.na(seed) == F) {
    set.seed(seed = seed)
  }else {
    seed <- base::sample(x = 1:10^6, size = 1)
    set.seed(seed = seed)
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
              verbose = verbose,
              round_digits = round_digits,
              show_branch_support = show_branch_support,
              show_simple_count = show_simple_count,
              kmeans_algorithm = kmeans_algorithm)


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


  # build treetree
  t <- get_dendrogram(ph = ph$main_ph,
                      cluster = km$cluster,
                      round_digits = round_digits,
                      show_branch_support = show_branch_support,
                      show_simple_count = show_simple_count)

  # collect input parameters: can be used for automated update
  input_par <- list(n_start = n_start,
                    iter_max = iter_max,
                    N_eff = N_eff,
                    B = B,
                    seed = seed,
                    round_digits = round_digits,
                    show_branch_support = show_branch_support,
                    show_simple_count = show_simple_count,
                    kmeans_algorithm = kmeans_algorithm)


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
                              cores = 1,
                              verbose = F) {

  # check input param
  check_input <- function(btd,
                          updated_bubbles,
                          ks,
                          cores = 1,
                          verbose = F) {

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

    if(is.vector(ks)==F & length(ks)==0) {
      stop("ks should be a vector of positive integers")
    }
    if(any(ks<=0)) {
      stop("ks should be a vector of positive integers")
    }
    if(any(ks<=0)) {
      stop("ks should be a vector of positive integers")
    }


    if(is.vector(updated_bubbles)==F & length(updated_bubbles)==0) {
      stop("updated_bubbles should be a vector of positive integers")
    }
    if(length(updated_bubbles) != length(ks)) {
      stop("ks and updated_bubbles should have the same length")
    }
    if(any(ks<=0)) {
      stop("ks should be a vector of positive integers")
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


    # check verbose
    if(is.logical(verbose)==F) {
      stop("verbose is a logical parameter (TRUE or FALSE)")
    }
    if(length(verbose)!=1) {
      stop("verbose is a logical parameter (TRUE or FALSE)")
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

  update_bubble <- function(A,
                            bubble,
                            k,
                            n_start,
                            iter_max,
                            kmeans_algorithm) {

    # perform k-means clustering
    km <- stats::kmeans(x = A,
                        centers = k,
                        nstart = n_start,
                        iter.max = iter_max,
                        kmeans_algorithm = kmeans_algorithm)

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
  show_simple_count <- btd$input_par$show_simple_count
  seed <- btd$input_par$seed
  kmeans_algorithm <- btd$input_par$kmeans_algorithm


  # set seed for reproducibility
  if(is.na(seed) == F) {
    set.seed(seed = seed)
  }

  # check input
  check_input(btd = btd,
              updated_bubbles = updated_bubbles,
              ks = ks,
              cores = cores,
              verbose = verbose)


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
                                iter_max = iter_max,
                                kmeans_algorithm = kmeans_algorithm)

    # update cluster naming
    btd$cluster[j] <- paste0(updated_bubbles[i], '_',
                             u_kms[[i]]$cluster)
  }

  cat("Updating dendrogram ... \n")
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
                      show_branch_support = show_branch_support,
                      show_simple_count = show_simple_count)



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
                             B = 100,
                             N_eff = 100,
                             cores = 1,
                             seed = NA,
                             verbose = F,
                             round_digits = 2,
                             show_branch_support = T,
                             show_simple_count = F) {


  # check input param
  check_input <- function(x,
                          cs,
                          B,
                          N_eff,
                          cores,
                          seed,
                          verbose,
                          round_digits,
                          show_branch_support,
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


    # check verbose
    if(is.logical(verbose)==F) {
      stop("verbose is a logical parameter (TRUE or FALSE)")
    }
    if(length(verbose)!=1) {
      stop("verbose is a logical parameter (TRUE or FALSE)")
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


    # show_branch_support
    if(length(show_branch_support)!=1) {
      stop("show_branch_support is a logical parameter (TRUE or FALSE)")
    }
    if(is.logical(show_branch_support)==F) {
      stop("show_branch_support is a logical parameter (TRUE or FALSE)")
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
              verbose = verbose,
              round_digits = round_digits,
              show_branch_support = show_branch_support,
              show_simple_count = show_simple_count)


  # pairwise distances
  cat("Generating bubbletree ... \n")
  pair_dist <- get_dist(B = B,
                        m = x,
                        c = cs,
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

  # build treetree
  t <- get_dendrogram(ph = ph$main_ph,
                      cluster = cs,
                      round_digits = round_digits,
                      show_branch_support = show_branch_support,
                      show_simple_count = show_simple_count)

  # collect input parameters: can be used for automated update
  input_par <- list(n_start = NA,
                    iter_max = NA,
                    N_eff = N_eff,
                    B = B,
                    seed = seed,
                    round_digits = round_digits,
                    show_simple_count = show_simple_count,
                    show_branch_support = show_branch_support,
                    kmeans_algorithm = NA)


  return(base::structure(class = "dummy_bubbletree",
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

  # check input param
  check_input <- function(labels, clusters) {

    # check labels
    if(length(labels)<=1) {
      stop("label is a vector has 1 or 0 element")
    }

    # check clusters
    if(length(clusters)<=1) {
      stop("clusters is a vector has 1 or 0 element")
    }

    # check labels
    if(is.vector(labels)==F) {
      stop("labels should be a vector")
    }

    # check clusters
    if(is.vector(clusters)==F) {
      stop("clusters should be a vector")
    }

    if(length(labels)!=length(clusters)) {
      stop("labels and clusters have different lengths")
    }

    if(any(is.na(labels)|is.null(labels))) {
      stop("labels contains NAs or NULLs")
    }
    if(any(is.na(clusters)|is.null(clusters))) {
      stop("clusters contains NAs or NULLs")
    }
  }

  get_gini_cluster <- function(c, l) {
    ls <- unique(l)
    l_len <- length(l)

    s <- 0
    for(i in 1:length(ls)) {
      s <- s + (sum(l == ls[i])/l_len)^2
    }

    return(s)

  }


  # check inputs
  check_input(labels = labels,
              clusters = clusters)


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
get_gini_k <- function(labels, get_k_obj) {


  # check input param
  check_input <- function(labels, get_k_obj) {

    # check labels
    if(length(labels)<=1) {
      stop("label is a vector has 1 or 0 element")
    }

    # check labels
    if(is.vector(labels)==F) {
      stop("labels should be a vector")
    }

    if(any(is.na(labels)|is.null(labels))) {
      stop("labels contains NAs or NULLs")
    }

    # check get_k_obj
    if(is.na(get_k_obj)||is.null(get_k_obj)||is.na(class(get_k_obj))||
       is.null(class(get_k_obj))||class(get_k_obj)!="boot_k") {
      stop("problem with the input get_k_obj")
    }

    if(is.list(get_k_obj$boot_obj)==F||
       is.na(get_k_obj$boot_obj)||
       is.null(get_k_obj$boot_obj)||
       length(get_k_obj$boot_obj)<=1) {
      stop("no boot_obj results in get_k_obj")
    }
  }


  # check inputs
  check_input(labels = labels,
              get_k_obj = get_k_obj)


  if(length(get_k_obj$boot_obj)==1&&
     is.na(get_k_obj$boot_obj)) {
    stop("You have to run 'get_k' with mini_output=FALSE. \n")
  }

  B <- length(get_k_obj$boot_obj)

  total_o <- vector(mode = "list", length = B*length(ks))
  cluster_o <- vector(mode = "list", length = B*length(ks))
  counter <- 1
  for(i in 1:B) {
    ks <- base::names(get_k_obj$boot_obj[[i]]$obj)

    for(j in 1:length(ks)) {
      gini <- get_gini(clusters = get_k_obj$boot_obj[[i]]$obj[[ks[j]]]$cluster,
                       labels = labels)

      # collect total gini and cluster specific gini scores
      # total
      total_o[[counter]] <- data.frame(B = i,
                                       k = as.numeric(ks[j]),
                                       total_gini = gini$total_gini)
      # cluster
      cluster_o[[counter]] <- data.frame(B = i,
                                    k = as.numeric(ks[j]),
                                    cluster = base::names(gini$cluster_gini),
                                    gini = gini$cluster_gini)
      counter <- counter + 1
    }
  }
  cluster_o <- do.call(rbind, cluster_o)
  total_o <- do.call(rbind, total_o)


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


