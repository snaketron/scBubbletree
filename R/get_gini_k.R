get_gini_k <- function(labels, obj) {
  
  # check inputs
  check_input_gini_k(labels = labels, obj = obj)
  
  ks <- base::names(obj$boot_obj$obj)
  
  total_o <- base::vector(mode = "list", length = base::length(ks))
  cluster_o <- base::vector(mode = "list", length = base::length(ks))
  counter <- 1
  for(j in base::seq_len(length.out = length(ks))) {
    cs <- obj$boot_obj$obj[[ks[j]]]
    
    gini <- get_gini(clusters = cs, labels = labels)
    
    # total
    total_o[[counter]] <- base::data.frame(k = length(unique(cs)),
                                           k_or_r = as.numeric(ks[j]),
                                           wgi = gini$wgi)
    # cluster
    cluster_o[[counter]] <- gini$gi
    cluster_o[[counter]]$k <- length(unique(cs))
    cluster_o[[counter]]$k_or_r <- as.numeric(ks[j])
    
    counter <- counter + 1
  }
  gi_summary <- base::do.call(rbind, cluster_o)
  wgi_summary <- base::do.call(rbind, total_o)
  
  return(base::list(wgi_summary = wgi_summary,
                    gi_summary = gi_summary))
}

check_input_gini_k <- function(labels, obj) {
  check_labels(labels = labels)
  check_obj_k_r(obj = obj)
}
