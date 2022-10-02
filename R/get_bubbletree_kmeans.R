
get_bubbletree_kmeans <- function(
    x,
    k,
    B = 100,
    N_eff = 200,
    n_start = 1000,
    iter_max = 300,
    kmeans_algorithm = "MacQueen",
    cores = 1,
    round_digits = 2,
    show_simple_count = FALSE,
    verbose = TRUE) {
  
  # check inputs
  check_input_kmeans(
    x = x,
    k = k,
    n_start = n_start,
    iter_max = iter_max,
    B = B,
    N_eff = N_eff,
    cores = cores,
    round_digits = round_digits,
    show_simple_count = show_simple_count,
    kmeans_algorithm = kmeans_algorithm,
    verbose = verbose)
  
  # perform k-means clustering
  if(verbose) {
    base::message("Clustering ...")
  }
  km <- stats::kmeans(
    x = x,
    centers = k,
    nstart = n_start,
    iter.max = iter_max,
    algorithm = kmeans_algorithm)
  
  # pairwise distances
  if(verbose) {
    base::message("Bubbletree construction ...")
  }
  pair_dist <- get_dist(
    B = B,
    m = x,
    c = km$cluster,
    N_eff = N_eff,
    cores = cores)
  
  # compute hierarchical clustering dendrogram
  d <- reshape2::acast(
    data = pair_dist$pca_pair_dist,
    formula = c_i~c_j,
    value.var = "M")
  d <- stats::as.dist(d)
  hc <- stats::hclust(d, method = "average")
  ph <- ape::as.phylo(x = hc)
  
  if(k==2) {
    # build tree
    t <- get_dendrogram(
      ph = ph,
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
  input_par <- list(
    n_start = n_start,
    iter_max = iter_max,
    N_eff = N_eff,
    B = B,
    round_digits = round_digits,
    show_simple_count = show_simple_count,
    kmeans_algorithm = kmeans_algorithm,
    update_iteration = 0)
  
  return(base::structure(
    class = "bubbletree_kmeans",
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


# check input param
check_input_kmeans <- function(
    x,
    k,
    n_start,
    iter_max,
    B,
    N_eff,
    cores,
    round_digits,
    show_simple_count,
    kmeans_algorithm,
    verbose) {
  
  check_x(x = x)
  check_k(k = k)
  check_n_start(n_start = n_start)
  check_iter_max(iter_max = iter_max)
  check_B(B = B)
  check_N_eff(N_eff = N_eff)
  check_cores(cores = cores)
  check_round_digits(round_digits = round_digits)
  check_show_simple_count(show_simple_count = show_simple_count)
  check_kmeans_algorithm(kmeans_algorithm = kmeans_algorithm)
  check_verbose(verbose = verbose)
}