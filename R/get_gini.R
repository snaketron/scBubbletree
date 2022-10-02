get_gini <- function(labels, clusters) {
  
  # check inputs
  check_input_gini(labels = labels, clusters = clusters)
  
  cs <- base::unique(clusters)
  
  # for each cluster we get gini-index
  gi <- base::numeric(length = base::length(cs))
  base::names(gi) <- cs
  
  # cluster weights used to compute total gini
  wgi <- base::numeric(length = base::length(cs))
  base::names(wgi) <- cs
  
  for(i in base::seq_len(length.out = base::length(cs))) {
    j <- base::which(clusters == cs[i])
    wgi[i] <- base::length(j)/base::length(clusters)
    gi[i] <- 1-get_gi(c = clusters[j], l = labels[j])
  }
  
  # compute WGI
  wgi <- base::sum(gi*wgi)
  
  # convert to data.frame for better plotting
  gi <- base::data.frame(cluster = names(gi),
                         GI = as.numeric(gi))
  
  return(base::list(gi = gi, wgi = wgi))
}

check_input_gini <- function(labels, clusters) {
  check_labels(labels = labels)
  check_clusters(clusters = clusters, labels = labels)
}

get_gi <- function(c, l) {
  ls <- base::unique(l)
  l_len <- base::length(l)
  s <- 0
  for(i in base::seq_len(length.out = length(ls))) {
    s <- s + (base::sum(l == ls[i])/l_len)^2
  }
  return(s)
}
