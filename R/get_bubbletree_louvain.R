
get_bubbletree_louvain <- function(x,
                                   r,
                                   B = 100,
                                   N_eff = 200,
                                   n_start = 20,
                                   iter_max = 100,
                                   louvain_algorithm = "original",
                                   cores = 1,
                                   round_digits = 2,
                                   show_simple_count = FALSE) {
  
  # check input param
  check_input <- function(x,
                          r,
                          n_start,
                          iter_max,
                          B,
                          N_eff,
                          cores,
                          round_digits,
                          show_simple_count,
                          louvain_algorithm) {
    
    
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
    
    
    
    # check r
    if(base::missing(r)) {
      stop("r input not found")
    }
    if(is.numeric(r)==FALSE) {
      stop("r must be a positive number (r>0) to build a bubbletree")
    }
    if(length(r)!=1) {
      stop("r must be a positive number (r>0) to build a bubbletree")
    }
    if(r<=0) {
      stop("r must be a positive number (r>0) to build a bubbletree")
    }
    if(is.infinite(r)==TRUE) {
      stop("r must be a positive number (r>0) to build a bubbletree")
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
              r = r,
              n_start = n_start,
              iter_max = iter_max,
              B = B,
              N_eff = N_eff,
              cores = cores,
              round_digits = round_digits,
              show_simple_count = show_simple_count,
              louvain_algorithm = louvain_algorithm)
  
  
  
  # perform clustering
  base::message("Clustering ...")
  
  # add cell ids if needed
  if(is.null(rownames(x))) {
    rownames(x) <- base::seq_len(length.out = base::nrow(x))
  }
  # create Knn graph
  knn <- Seurat::FindNeighbors(object = x,
                               k.param = 50)
  
  # clustering
  lc <- Seurat::FindClusters(object = knn$snn,
                             resolution = r,
                             n.start = n_start,
                             n.iter = iter_max,
                             algorithm = map_louvain_algname(louvain_algorithm),
                             verbose = FALSE)
  
  # clusters
  cs <- as.character(lc[,1])
  
  
  
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
  
  if(length(unique(cs)) <= 2) {
    
    t <- get_dendrogram(ph = ph,
                        cluster = cs,
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
                        cluster = cs,
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
                    louvain_algorithm = louvain_algorithm,
                    update_iteration = 0)
  
  
  return(base::structure(class = "bubbletree_louvain",
                         list(A = x,
                              k = length(unique(cs)),
                              r = r,
                              ph = ph,
                              pair_dist = pair_dist,
                              cluster = cs,
                              input_par = input_par,
                              tree = t$tree,
                              tree_meta = t$tree_meta)))
  
}
