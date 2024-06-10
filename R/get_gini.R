get_gini <- function(labels, clusters) {
  
  # check inputs
  check_input_gini(labels = labels, clusters = clusters)
  
  cs <- unique(clusters)
  
  # for each cluster we get gini-index
  gi <- numeric(length = length(cs))
  names(gi) <- cs
  
  # cluster weights used to compute total gini
  wgi <- numeric(length = length(cs))
  names(wgi) <- cs
  
  for(i in seq_len(length.out = length(cs))) {
    j <- which(clusters == cs[i])
    wgi[i] <- length(j)/length(clusters)
    gi[i] <- 1-get_gi(c = clusters[j], l = labels[j])
  }
  
  # compute WGI
  wgi <- sum(gi*wgi)
  
  # convert to data.frame for better plotting
  gi <- data.frame(cluster = names(gi),
                   GI = as.numeric(gi))
  
  return(list(gi = gi, wgi = wgi))
}

check_input_gini <- function(labels, clusters) {
  check_labels(labels = labels)
  check_clusters(clusters = clusters, labels = labels)
}

get_gi <- function(c, l) {
  ls <- unique(l)
  l_len <- length(l)
  s <- 0
  for(i in seq_len(length.out = length(ls))) {
    s <- s + (sum(l == ls[i])/l_len)^2
  }
  return(s)
}
