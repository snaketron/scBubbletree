
get_bubbletree_dummy <- function(x,
                                 cs,
                                 B = 100,
                                 N_eff = 100,
                                 cores = 1,
                                 round_digits = 2,
                                 show_simple_count = FALSE) {


  # check input param
  check_input <- function(x,
                          cs,
                          B,
                          N_eff,
                          cores,
                          round_digits,
                          show_simple_count) {

    # check x
    if(base::missing(x)) {
      stop("x input not found")
    }

    if(methods::is(x, 'SummarizedExperiment')) {
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


    # check cs
    if(is.vector(cs)==FALSE) {
      stop("cs must be a vector")
    }
    if(length(cs)!=nrow(x)) {
      stop("length(cs) != nrow(x)")
    }
    if(length(unique(cs))<=1) {
      stop("1 or 0 clusters found in vector cs")
    }
    if(any(is.na(cs)|is.null(cs))==TRUE) {
      stop("NA or NULL elements are found in cs")
    }
    if(any(is.na(cs)|is.null(cs))==TRUE) {
      stop("NA or NULL elements are found in cs")
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
  base::message("Checking inputs ...")
  check_input(x = x,
              cs = cs,
              B = B,
              N_eff = N_eff,
              cores = cores,
              round_digits = round_digits,
              show_simple_count = show_simple_count)


  # pairwise distances
  base::message("Bubbletree construction ...")
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
                    round_digits = round_digits,
                    show_simple_count = show_simple_count,
                    kmeans_algorithm = NA)


  return(base::structure(class = "bubbletree_dummy",
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
