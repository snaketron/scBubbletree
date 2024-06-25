
get_bubbletree_kmeans <- function(x,
                                  k,
                                  B = 200,
                                  N_eff = 100,
                                  n_start = 1000,
                                  iter_max = 300,
                                  kmeans_algorithm = "MacQueen",
                                  hclust_distance = "euclidean",
                                  hclust_method = "average",
                                  cores = 1,
                                  round_digits = 2,
                                  show_simple_count = FALSE,
                                  verbose = TRUE) {
  
  # check inputs
  check_input_kmeans(x = x,
                     k = k,
                     n_start = n_start,
                     iter_max = iter_max,
                     B = B,
                     N_eff = N_eff,
                     cores = cores,
                     round_digits = round_digits,
                     show_simple_count = show_simple_count,
                     kmeans_algorithm = kmeans_algorithm,
                     verbose = verbose,
                     hclust_method = hclust_method,
                     hclust_distance = hclust_distance)
  
  # perform k-means clustering
  if(verbose) {
    message("Clustering ...")
  }
  km <- kmeans(x = x, centers = k, nstart = n_start, iter.max = iter_max, 
               algorithm = kmeans_algorithm)
  
  # pairwise distances
  if(verbose) {
    message("Bubbletree construction ...")
  }
  pd <- get_dist(B = B, m = x, c = km$cluster, N_eff = N_eff, cores = cores,
                 hclust_distance = hclust_distance)
  
  tc <- get_tree(pd = pd, B = B, hclust_method = hclust_method, 
                 cs = km$cluster, round_digits = round_digits, 
                 show_simple_count = show_simple_count, type = "c")
  
  tp <- get_tree(pd = pd, B = B, hclust_method = hclust_method, 
                 cs = km$cluster, round_digits = round_digits, 
                 show_simple_count = show_simple_count, type = "p")
  
  # which ph to use as main?
  if(B==0) {
    ph <- tc$ph
    t <- tc$t
  } 
  else {
    ph <- tp$ph
    t <- tp$t
  }
  
  # collect input parameters: can be used for automated update
  input_par <- list(n_start = n_start,
                    iter_max = iter_max,
                    N_eff = N_eff,
                    B = B,
                    round_digits = round_digits,
                    show_simple_count = show_simple_count,
                    kmeans_algorithm = kmeans_algorithm,
                    hclust_method = hclust_method,
                    hclust_distance = hclust_distance)
  
  return(structure(class = "bubbletree_kmeans",
                   list(A = x,
                        k = k,
                        km = km,
                        ph = ph,
                        ph_data = list(ph_c = tc, ph_p = tp),
                        pair_dist = pd,
                        cluster = km$cluster,
                        input_par = input_par,
                        tree = t$tree,
                        tree_meta = t$tree_meta)))
}


# check input param
check_input_kmeans <- function(x,
                               k,
                               n_start,
                               iter_max,
                               B,
                               N_eff,
                               cores,
                               round_digits,
                               show_simple_count,
                               kmeans_algorithm,
                               verbose,
                               hclust_method,
                               hclust_distance) {
  
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
  check_hclust_method(hclust_method = hclust_method)
  check_hclust_distance(hclust_distance = hclust_distance)
}

