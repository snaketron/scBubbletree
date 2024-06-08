get_bubbletree_dummy <- function(x,
                                 cs,
                                 B = 0,
                                 N_eff = 200,
                                 hclust_distance = "euclidean",
                                 hclust_method = "average",
                                 cores = 1,
                                 round_digits = 2,
                                 show_simple_count = FALSE,
                                 verbose = TRUE) {
  
  # check inputs
  check_input_dummy(x = x,
                    cs = cs,
                    B = B,
                    N_eff = N_eff,
                    cores = cores,
                    round_digits = round_digits,
                    show_simple_count = show_simple_count,
                    verbose = verbose,
                    hclust_method = hclust_method,
                    hclust_distance = hclust_distance)
  
  # pairwise distances
  if(verbose) {
    message("Bubbletree construction ...")
  }
  
  pd <- get_dist(B = B, m = x, c = cs, N_eff = N_eff, cores = cores,
                 hclust_distance = hclust_distance)
  
  # compute hierarchical clustering dendrogram
  d <- acast(data = pd$c_dist, formula = c_i~c_j, value.var = "M", 
             fun.aggregate = mean)
  d <- as.dist(d)
  hc <- hclust(d, method = hclust_method)
  ph <- as.phylo(x = hc)
  main_ph <- unroot(phy = ph)
  
  if(B==0) {
    # build tree
    t <- get_dendrogram(ph = main_ph,
                        cluster = cs,
                        round_digits = round_digits,
                        show_simple_count = show_simple_count)
    
    ph <- list(main_ph = main_ph, boot_ph = NA)
  } 
  else {
    # get branch support
    ph <- get_ph_support(main_ph = main_ph, x = main_ph$p_dist)
    
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
                    kmeans_algorithm = NA,
                    hclust_method = hclust_method,
                    hclust_distance = hclust_distance)
  
  return(structure(class = "bubbletree_dummy",
                   list(A = x,
                        k = length(unique(cs)),
                        ph = ph,
                        pair_dist = pd,
                        cluster = cs,
                        input_par = input_par,
                        tree = t$tree,
                        tree_meta = t$tree_meta)))
}



# check input param
check_input_dummy <- function(x,
                              cs,
                              B,
                              N_eff,
                              cores,
                              hclust_method,
                              hclust_distance,
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
  check_hclust_method(hclust_method = hclust_method)
  check_hclust_distance(hclust_distance = hclust_distance)
}
