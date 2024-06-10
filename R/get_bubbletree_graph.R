get_bubbletree_graph <- function(x,
                                 r,
                                 B = 0,
                                 N_eff = 200,
                                 n_start = 20,
                                 iter_max = 100,
                                 algorithm = "original",
                                 knn_k = 20,
                                 hclust_method = "average",
                                 hclust_distance = "euclidean",
                                 cores = 1,
                                 round_digits = 2,
                                 show_simple_count = FALSE,
                                 verbose = TRUE) {
  
  # check inputs
  check_input_louvain(x = x,
                      r = r,
                      n_start = n_start,
                      iter_max = iter_max,
                      B = B,
                      N_eff = N_eff,
                      cores = cores,
                      round_digits = round_digits,
                      show_simple_count = show_simple_count,
                      algorithm = algorithm,
                      knn_k = knn_k,
                      verbose = verbose,
                      hclust_method = hclust_method,
                      hclust_distance = hclust_distance)
  
  # add cell ids if needed
  if(is.null(rownames(x))) {
    rownames(x) <- seq_len(length.out = nrow(x))
  }
  # create Knn graph
  knn <- FindNeighbors(object = x, k.param = knn_k)
  
  # clustering
  if(verbose) {
    message("Clustering ...")
  }
  lc <- FindClusters(object = knn$snn,
                     resolution = r,
                     n.start = n_start,
                     n.iter = iter_max,
                     algorithm = map_louvain_algname(algorithm),
                     verbose = FALSE)
  
  # clusters
  cs <- as.character(lc[,1])
  
  # pairwise distances
  if(verbose) {
    message("Bubbletree construction ...")
  }
  pd <- get_dist(B = B, m = x, c = cs, N_eff = N_eff, cores = cores,
                 hclust_distance = hclust_distance)
  
  tc <- get_tree(pd = pd, B = B, hclust_method = hclust_method, 
                 cs = cs, round_digits = round_digits, 
                 show_simple_count = show_simple_count, type = "c")
  
  tp <- get_tree(pd = pd, B = B, hclust_method = hclust_method, 
                 cs = cs, round_digits = round_digits, 
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
                    algorithm = algorithm,
                    hclust_method = hclust_method,
                    hclust_distance = hclust_distance)
  
  return(structure(class = "bubbletree_louvain",
                   list(A = x,
                        k = length(unique(cs)),
                        r = r,
                        ph = ph,
                        ph_data = list(ph_c = tc, ph_p = tp),
                        pair_dist = pd,
                        cluster = cs,
                        input_par = input_par,
                        tree = t$tree,
                        tree_meta = t$tree_meta)))
}

# check input param
check_input_louvain <- function(x,
                                r,
                                n_start,
                                iter_max,
                                B,
                                N_eff,
                                cores,
                                round_digits,
                                show_simple_count,
                                algorithm,
                                knn_k,
                                verbose,
                                hclust_method,
                                hclust_distance) {
  
  check_x(x = x)
  check_r(r = r)
  check_n_start(n_start = n_start)
  check_iter_max(iter_max = iter_max)
  check_B(B = B)
  check_N_eff(N_eff = N_eff)
  check_cores(cores = cores)
  check_round_digits(round_digits = round_digits)
  check_show_simple_count(show_simple_count = show_simple_count)
  check_louvain_algorithm(algorithm = algorithm)
  check_knn_k(knn_k = knn_k)
  check_verbose(verbose = verbose)
  check_hclust_method(hclust_method = hclust_method)
  check_hclust_distance(hclust_distance = hclust_distance)
}

