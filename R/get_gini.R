
get_gini <- function(labels, clusters) {
  
  
  # check input param
  check_input <- function(labels, clusters) {
    
    # check labels
    if(base::missing(labels)) {
      stop("labels is missing")
    }
    if(base::length(labels)<=1) {
      stop("labels must be a vector with more than one element")
    }
    if(base::is.character(labels)==FALSE&
       base::is.numeric(labels)==FALSE) {
      stop("labels can only contain characters or numbers")
    }
    if(base::is.vector(labels)==FALSE) {
      stop("labels must be a vector")
    }
    if(base::any(base::is.infinite(labels))==TRUE) {
      stop("labels cannot have INF/NA/NULL values")
    }
    if(base::any(base::is.na(labels))==TRUE) {
      stop("labels cannot have INF/NA/NULL values")
    }
    if(base::any(base::is.null(labels))==TRUE) {
      stop("labels cannot have INF/NA/NULL values")
    }
    
    
    # check clusters
    if(base::missing(clusters)) {
      stop("clusters is missing")
    }
    if(base::length(clusters)<=1) {
      stop("clusters must be a vector with more than one element")
    }
    if(base::is.character(clusters)==FALSE&
       base::is.numeric(clusters)==FALSE) {
      stop("clusters can only contain characters or numbers")
    }
    if(base::is.vector(clusters)==FALSE) {
      stop("clusters must be a vector")
    }
    if(base::any(base::is.infinite(clusters))==TRUE) {
      stop("clusters cannot have INF/NA/NULL values")
    }
    if(base::any(base::is.na(clusters))==TRUE) {
      stop("clusters cannot have INF/NA/NULL values")
    }
    if(base::any(base::is.null(clusters))==TRUE) {
      stop("clusters cannot have INF/NA/NULL values")
    }
    
    if(base::length(labels)!=base::length(clusters)) {
      stop("labels and clusters must be equal-length vectors")
    }
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
  
  
  # check inputs
  check_input(labels = labels,
              clusters = clusters)
  
  
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
