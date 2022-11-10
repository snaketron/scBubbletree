get_bubbletree_dummy <- function(
    x,
    cs,
    B = 100,
    N_eff = 100,
    cores = 1,
    round_digits = 2,
    show_simple_count = FALSE,
    verbose = TRUE) {
  
  # check inputs
  check_input_dummy(
    x = x,
    cs = cs,
    B = B,
    N_eff = N_eff,
    cores = cores,
    round_digits = round_digits,
    show_simple_count = show_simple_count,
    verbose = verbose)
  
  # pairwise distances
  if(verbose) {
    base::message("Bubbletree construction ...")
  }
  
  pair_dist <- get_dist(
    B = B,
    m = x,
    c = cs,
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
  ph <- ape::unroot(phy = ph)
  
  if(B==0) {
    # build tree
    t <- get_dendrogram(
      ph = ph,
      cluster = km$cluster,
      round_digits = round_digits,
      show_simple_count = show_simple_count)
  } else {
    # get branch support
    ph <- get_ph_support(main_ph = ph,
                         x = pair_dist$raw_pair_dist)
    
    # build treetree
    t <- get_dendrogram(ph = ph$main_ph,
                        cluster = cs,
                        round_digits = round_digits,
                        show_simple_count = show_simple_count)
  }
  
  
  # collect input parameters: can be used for automated update
  input_par <- list(n_start = NA,
                    iter_max = NA,
                    N_eff = N_eff,
                    B = B,
                    round_digits = round_digits,
                    show_simple_count = show_simple_count,
                    kmeans_algorithm = NA)
  
  return(base::structure(
    class = "bubbletree_dummy",
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



# check input param
check_input_dummy <- function(
    x,
    cs,
    B,
    N_eff,
    cores,
    round_digits,
    show_simple_count,
    verbose) {
  
  check_x(x = x)
  check_cs(cs = cs, x = x)
  check_B(B = B)
  check_N_eff(N_eff = N_eff)
  check_cores(cores = cores)
  check_round_digits(round_digits = round_digits)
  check_show_simple_count(show_simple_count = show_simple_count)
  check_verbose(verbose = verbose)
}
