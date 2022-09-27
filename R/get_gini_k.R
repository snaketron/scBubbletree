
get_gini_k <- function(labels, obj) {
  
  
  # check input param
  check_input <- function(labels, obj) {
    
    # check labels
    if(base::missing(labels)) {
      stop("labels is missing")
    }
    if(base::length(labels)<=1) {
      stop("labels must be a vector with more than one element")
    }
    if(base::is.vector(labels)==FALSE) {
      stop("labels must be a vector")
    }
    if(base::any(base::is.infinite(labels))==TRUE) {
      stop("labels must be a vector")
    }
    if(base::any(base::is.na(labels))==TRUE) {
      stop("labels must be a vector")
    }
    if(base::any(base::is.null(labels))==TRUE) {
      stop("labels must be a vector")
    }
    
    
    # check obj
    if(base::missing(obj)) {
      stop("obj is missing")
    }
    if(base::any(base::is.na(obj))|base::is.null(obj)|base::is.na(class(obj))|
       base::is.null(class(obj))|
       (methods::is(obj, "boot_k")==FALSE&
        methods::is(obj, "boot_r")==FALSE)) {
      stop("problem with obj")
    }
    
    if(base::is.list(obj$boot_obj)==FALSE|
       base::any(base::is.na(obj$boot_obj))|
       base::is.null(obj$boot_obj)|
       base::length(obj)<=1) {
      stop("no boot_obj found in obj")
    }
  }
  
  
  # check inputs
  check_input(labels = labels, obj = obj)
  
  
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
