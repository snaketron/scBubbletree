
get_bubbletree_kmeans <- function(x,
                                  k,
                                  B = 100,
                                  N_eff = 200,
                                  n_start = 1000,
                                  iter_max = 300,
                                  kmeans_algorithm = "MacQueen",
                                  cores = 1,
                                  round_digits = 2,
                                  show_simple_count = FALSE) {
  
  # check input param
  check_input <- function(x,
                          k,
                          n_start,
                          iter_max,
                          B,
                          N_eff,
                          cores,
                          round_digits,
                          show_simple_count,
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
    
    
    
    # check k
    if(base::missing(k)) {
      stop("k input not found")
    }
    if(is.numeric(k)==FALSE) {
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
    
    
    # check B
    if(base::missing(B)) {
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
    if(base::missing(n_start)) {
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
    
    
    
    
    
    # check N_eff
    if(base::missing(N_eff)) {
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
    
    
    
    # check round_digits
    if(base::missing(round_digits)) {
      stop("round_digits input not found")
    }
    if(base::is.numeric(round_digits)==FALSE) {
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
    if(base::missing(show_simple_count)) {
      stop("show_simple_count input not found")
    }
    if(length(show_simple_count)!=1) {
      stop("show_simple_count is a logical parameter (TRUE or FALSE)")
    }
    if(is.logical(show_simple_count)==FALSE) {
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
              round_digits = round_digits,
              show_simple_count = show_simple_count,
              kmeans_algorithm = kmeans_algorithm)
  
  
  
  # perform k-means clustering
  base::message("Clustering ...")
  km <- stats::kmeans(x = x,
                      centers = k,
                      nstart = n_start,
                      iter.max = iter_max,
                      algorithm = kmeans_algorithm)
  
  
  # pairwise distances
  base::message("Bubbletree construction ...")
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
                    round_digits = round_digits,
                    show_simple_count = show_simple_count,
                    kmeans_algorithm = kmeans_algorithm,
                    update_iteration = 0)
  
  
  return(base::structure(class = "bubbletree_kmeans",
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
