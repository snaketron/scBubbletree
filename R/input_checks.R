check_x <- function(x) {
  # check x
  if(missing(x)) {
    stop("x input not found")
  }
  if(is.numeric(x)==FALSE) {
    stop("x must be numeric matrix")
  }
  if(is.matrix(x)==FALSE) {
    stop("x must be numeric matrix")
  }
  if(any(is.infinite(x))==TRUE) {
    stop("x must be numeric matrix, infinite values not allowed")
  }
  if(any(is.na(x))==TRUE) {
    stop("x must be numeric matrix, NAs not allowed")
  }
  if(any(is.null(x))==TRUE) {
    stop("x must be numeric matrix, NULLs not allowed")
  }
  if(all(x == x[1,1])==TRUE) {
    stop("all elements in x are identical")
  }
  if(ncol(x)>nrow(x)) {
    warning("more columns (features) than rows (cells) in x")
  }
}

check_B_gap <- function(B_gap) {
  # check B_gap
  if(missing(B_gap)) {
    stop("B_gap input not found")
  }
  if(is.numeric(B_gap)==FALSE) {
    stop("B_gap must be a positive integer > 0")
  }
  if(length(B_gap)!=1) {
    stop("B_gap must be a positive integer > 0")
  }
  if(B_gap<1) {
    stop("B_gap must be a positive integer > 0")
  }
  if(is.infinite(B_gap)==TRUE) {
    stop("B_gap must be a positive integer > 0")
  }
  if(is.na(B_gap)==TRUE) {
    stop("B_gap must be a positive integer > 0")
  }
  if(B_gap%%1!=0) {
    stop("B_gap must be a positive integer > 0")
  }
}

check_rs <- function(rs) {
  # check rs
  if(missing(rs)) {
    stop("rs input not found")
  }
  if(is.numeric(rs)==FALSE) {
    stop("rs must be a positive number or vector of positive numbers")
  }
  if(is.vector(x = rs)==FALSE) {
    stop("rs must be a positive number or vector of positive numbers")
  }
  if(length(rs)<=0) {
    stop("rs must be a positive number or vector of positive numbers")
  }
  if(any(is.infinite(rs))==TRUE) {
    stop("rs must be a positive number or vector of positive numbers,
           no infinite values are allowed")
  }
  if(any(is.na(rs))==TRUE) {
    stop("rs must be a positive number or vector of positive numbers,
           no NAs are allowed")
  }
  if(any(is.null(rs))==TRUE) {
    stop("rs must be a positive number or vector of positive numbers,
           no NULLs are allowed")
  }
  if(any(rs<0)==TRUE) {
    stop("rs must be a positive number or vector of positive numbers")
  }
  if(any(duplicated(rs))==TRUE) {
    stop("rs must be a positive number or vector of positive numbers,
           duplicate r values are not allowed")
  }
}

check_cores <- function(cores) {
  # check cores
  if(missing(cores)) {
    stop("cores input not found")
  }
  if(is.numeric(cores)==FALSE) {
    stop("cores must be a positive integer")
  }
  if(length(cores)!=1) {
    stop("cores must be a positive integer")
  }
  if(is.infinite(cores)==TRUE) {
    stop("cores must be a positive integer")
  }
  if(is.na(cores)==TRUE) {
    stop("cores must be a positive integer")
  }
  if(cores<1) {
    stop("cores must be a positive integer")
  }
  if(cores%%1!=0) {
    stop("cores must be a positive integer")
  }
}

check_n_start <- function(n_start) {
  # n_start
  if(missing(x = n_start)) {
    stop("n_start input not found")
  }
  if(is.numeric(n_start)==FALSE) {
    stop("n_start must be a positive integer")
  }
  if(length(n_start) != 1) {
    stop("n_start must be a positive integer")
  }
  if(n_start < 1) {
    stop("n_start must be a positive integer")
  }
  if(is.infinite(n_start)==TRUE) {
    stop("n_start must be a positive integer")
  }
  if(is.na(n_start)==TRUE) {
    stop("n_start must be a positive integer")
  }
  if(is.null(n_start)==TRUE) {
    stop("n_start must be a positive integer")
  }
  if(n_start%%1!=0) {
    stop("n_start must be a positive integer")
  }
}

check_iter_max <- function(iter_max) {
  # iter_max
  if(missing(iter_max)) {
    stop("iter_max input not found")
  }
  if(is.numeric(iter_max)==FALSE) {
    stop("iter_max must be a positive integer")
  }
  if(length(iter_max)!=1) {
    stop("iter_max must be a positive integer")
  }
  if(iter_max<1) {
    stop("iter_max must be a positive integer")
  }
  if(is.infinite(iter_max)==TRUE) {
    stop("iter_max must be a positive integer")
  }
  if(is.na(iter_max)==TRUE) {
    stop("iter_max must be a positive integer")
  }
  if(is.null(iter_max)==TRUE) {
    stop("iter_max must be a positive integer")
  }
  if(iter_max%%1!=0) {
    stop("iter_max must be a positive integer")
  }
}

check_louvain_algorithm <- function(algorithm) {
  # algorithm
  if(missing(algorithm)) {
    stop("algorithm input not found")
  }
  if(length(algorithm)!=1) {
    stop("see ?FindClusters from R-package Seurat: algorithm must be
      one of: original, LMR, SLM or Leiden")
  }
  if(is.character(algorithm)==FALSE) {
    stop("see ?FindClusters from R-package Seurat: algorithm must be
      one of: original, LMR, SLM or Leiden")
  }
  if(algorithm %in% c("original", "LMR", "SLM", "Leiden")==FALSE) {
    stop("see ?FindClusters from R-package Seurat: algorithm must be
      one of: original, LMR, SLM or Leiden")
  }
}

check_knn_k <- function(knn_k) {
  # check knn_k
  if(missing(knn_k)) {
    stop("knn_k input not found")
  }
  if(is.numeric(knn_k)==FALSE) {
    stop("knn_k must be a positive integer")
  }
  if(length(knn_k)!=1) {
    stop("knn_k must be a positive integer")
  }
  if(is.infinite(knn_k)==TRUE) {
    stop("knn_k must be a positive integer")
  }
  if(is.na(knn_k)==TRUE) {
    stop("knn_k must be a positive integer")
  }
  if(knn_k<1) {
    stop("knn_k must be a positive integer")
  }
  if(knn_k%%1!=0) {
    stop("knn_k must be a positive integer")
  }
} 

check_ks <- function(ks, x) {
  # check ks
  if(missing(ks)==TRUE) {
    stop("ks input not found")
  }
  if(is.numeric(ks)==FALSE) {
    stop("ks must be a positive integer or vector of positive integers")
  }
  if(is.vector(x = ks)==FALSE) {
    stop("ks must be a positive integer or vector of positive integers")
  }
  if(length(ks)<=0) {
    stop("ks must be a positive integer or vector of positive integers")
  }
  if(any(is.infinite(ks))==TRUE) {
    stop("ks must be a positive integer or vector of positive integers,
           no infinite values are allowed")
  }
  if(any(is.na(ks))==TRUE) {
    stop("ks must be a positive integer or vector of positive integers,
           no NAs are allowed")
  }
  if(any(is.null(ks))==TRUE) {
    stop("ks must be a positive integer or vector of positive integers,
           no NULLs are allowed")
  }
  if(any(ks<0)==TRUE) {
    stop("ks must be a positive integer or vector of positive integers")
  }
  if(any(duplicated(ks))==TRUE) {
    stop("ks must be a positive integer or vector of positive integers,
           duplicate k values are not allowed")
  }
  if(length(ks)==1) {
    if(all(ks<1)) {
      stop("ks must be a positive integer or vector of positive integers")
    }
  }
  if(any(ks>=nrow(x))==TRUE) {
    stop("max(ks) should be smaller than nrow(x)")
  }
}

check_kmeans_algorithm <- function(kmeans_algorithm) {
  # kmeans_algorithm
  if(missing(kmeans_algorithm)) {
    stop("kmeans_algorithm input not found")
  }
  if(length(kmeans_algorithm)!=1) {
    stop("see ?kmeans: kmeans_algorithm must be one of: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")
  }
  if(is.character(kmeans_algorithm)==FALSE) {
    stop("see ?kmeans: kmeans_algorithm must be one of: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")
  }
  if(kmeans_algorithm %in% c("Hartigan-Wong", "Lloyd", "Forgy",
                             "MacQueen")==FALSE) {
    stop("see ?kmeans: kmeans_algorithm must be one of: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")
  }
}

check_verbose <- function(verbose) {
  # verbose
  if(missing(verbose)) {
    stop("verbose input not found")
  }
  if(length(verbose)!=1) {
    stop("verbose is a logical parameter (TRUE or FALSE)")
  }
  if(is.logical(verbose)==FALSE) {
    stop("verbose is a logical parameter (TRUE or FALSE)")
  }
  if(is.na(verbose)==TRUE) {
    stop("verbose is a logical parameter (TRUE or FALSE)")
  }
}

check_k <- function(k) {
  # check k
  if(missing(k)) {
    stop("k input not found")
  }
  if(is.numeric(k)==FALSE) {
    stop("k must be a positive integer (k>=2) to build a bubbletree")
  }
  if(length(k)!=1) {
    stop("k must be a positive integer (k>=2) to build a bubbletree")
  }
  if(k<=1) {
    stop("k must be a positive integer (k>=2) to build a bubbletree")
  }
  if(is.infinite(k)==TRUE) {
    stop("k must be a positive integer (k>=2) to build a bubbletree")
  }
  if(k%%1!=0) {
    stop("k must be a positive integer (k>=2) to build a bubbletree")
  }
}

check_B <- function(B) {
  # check B
  if(missing(B)) {
    stop("B input not found")
  }
  if(is.numeric(B)==FALSE) {
    stop("B must be a positive integer >= 0")
  }
  if(length(B)!=1) {
    stop("B must be a positive integer >= 0")
  }
  if(B<0) {
    stop("B must be a positive integer >= 0")
  }
  if(is.infinite(B)==TRUE) {
    stop("B must be a positive integer >= 0")
  }
  if(is.na(B)==TRUE) {
    stop("B must be a positive integer >= 0")
  }
  if(B%%1!=0) {
    stop("B must be a positive integer >= 0")
  }
}

check_N_eff <- function(N_eff) {
  # check N_eff
  if(missing(N_eff)) {
    stop("N_eff input not found")
  }
  if(is.numeric(N_eff)==FALSE) {
    stop("N_eff must be a positive integer")
  }
  if(length(N_eff)!=1) {
    stop("N_eff must be a positive integer")
  }
  if(N_eff<1) {
    stop("N_eff must be a positive integer")
  }
  if(is.infinite(N_eff)==TRUE) {
    stop("N_eff must be a positive integer")
  }
  if(is.na(N_eff)==TRUE) {
    stop("N_eff must be a positive integer")
  }
  if(is.null(N_eff)==TRUE) {
    stop("N_eff must be a positive integer")
  }
  if(N_eff%%1!=0) {
    stop("N_eff must be a positive integer")
  }
}

check_round_digits <- function(round_digits) {
  # check round_digits
  if(missing(round_digits)) {
    stop("round_digits input not found")
  }
  if(is.numeric(round_digits)==FALSE) {
    stop("round_digits must be a positive integer")
  }
  if(length(round_digits)!=1) {
    stop("round_digits must be a positive integer")
  }
  if(round_digits<0) {
    stop("round_digits must be a positive integer")
  }
  if(is.finite(round_digits)==FALSE) {
    stop("round_digits must be a positive integer")
  }
  if(round_digits%%1!=0) {
    stop("round_digits must be a positive integer")
  }
}

check_show_simple_count <- function(show_simple_count) {
  # show_simple_count
  if(missing(show_simple_count)) {
    stop("show_simple_count input not found")
  }
  if(length(show_simple_count)!=1) {
    stop("show_simple_count is a logical parameter (TRUE or FALSE)")
  }
  if(is.logical(show_simple_count)==FALSE) {
    stop("show_simple_count is a logical parameter (TRUE or FALSE)")
  }
  if(is.na(show_simple_count)==TRUE) {
    stop("show_simple_count is a logical parameter (TRUE or FALSE)")
  }
}

check_r <- function(r) {
  # check r
  if(missing(r)) {
    stop("r input not found")
  }
  if(is.numeric(r)==FALSE) {
    stop("r must be a positive number (r>0) to build a bubbletree")
  }
  if(length(r)!=1) {
    stop("r must be a positive number (r>0) to build a bubbletree")
  }
  if(r<=0) {
    stop("r must be a positive number (r>0) to build a bubbletree")
  }
  if(is.infinite(r)==TRUE) {
    stop("r must be a positive number (r>0) to build a bubbletree")
  }
}

check_cs <- function(cs, x) {
  # check cs
  if(is.vector(cs)==FALSE) {
    stop("cs must be a vector")
  }
  if(length(cs)!=nrow(x)) {
    stop("length(cs) != nrow(x)")
  }
  if(length(unique(cs))<=1) {
    stop("1 or 0 clusters found in vector cs")
  }
  if(any(is.na(cs)|is.null(cs))==TRUE) {
    stop("NA or NULL elements are found in cs")
  }
  if(any(is.na(cs)|is.null(cs))==TRUE) {
    stop("NA or NULL elements are found in cs")
  }
}

check_btd <- function(btd) {
  
  # check btd
  if(missing(btd)) {
    stop("bubbletree (btd) input not found")
  }
  if(is.null(btd)||
     is.null(class(btd))||
     (is(btd, "bubbletree_kmeans")==FALSE&
      is(btd, "bubbletree_louvain")==FALSE&
      is(btd, "bubbletree_dummy")==FALSE)) {
    stop("NA/NULL elements or wrong class detected in the bubbletree")
  }
  if(is.vector(btd$cluster)==FALSE||
     any(is.na(is.vector(btd$cluster)))|
     any(is.infinite(is.vector(btd$cluster)))|
     any(is.null(is.vector(btd$cluster)))) {
    stop("NA/NULL/Inf cluster assignments present in bubbletree")
  }
  
  
  # check btd$A
  if(any(is.na(btd$A))|is.null(btd$A)) {
    stop("btd$A must be numeric matrix")
  }
  if(is.numeric(btd$A)==FALSE) {
    stop("btd$A must be numeric matrix")
  }
  if(is.matrix(btd$A)==FALSE) {
    stop("btd$A must be numeric matrix")
  }
  if(any(is.infinite(btd$A))==TRUE) {
    stop("btd$A must be numeric matrix, infinite values not allowed")
  }
  if(any(is.na(btd$A))==TRUE) {
    stop("btd$A must be numeric matrix, NAs not allowed")
  }
  if(any(is.null(btd$A))==TRUE) {
    stop("btd$A must be numeric matrix, NULLs not allowed")
  }
  if(all(btd$A == btd$A[1,1])==TRUE) {
    stop("all elements in btd$A are identical")
  }
  if(ncol(btd$A)>nrow(btd$A)) {
    warning("more columns (features) than rows (cells) in btd$A")
  }
  if(nrow(btd$A)!=length(btd$cluster)) {
    stop("problem in btd nrow(btd$A)!=length(btd$cluster)")
  }
  
  if(any(is.na(btd$k))|is.null(btd$k)) {
    stop("problem in btd btd$k must be a positive integer (k>=2)")
  }
  if(is.numeric(btd$k)==FALSE) {
    stop("problem in btd btd$k must be a positive integer (k>=2)")
  }
  if(length(btd$k)!=1) {
    stop("problem in btd btd$k must be a positive integer (k>=2)")
  }
  if(btd$k<=1) {
    stop("problem in btd btd$k must be a positive integer (k>=2)")
  }
  if(is.infinite(btd$k)==TRUE) {
    stop("problem in btd btd$k must be a positive integer (k>=2)")
  }
  if(btd$k%%1!=0) {
    stop("problem in btd btd$k must be a positive integer (k>=2)")
  }
  
  
  if(all(is.na(btd$ph))|is.null(btd$ph)) {
    stop("problem in btd btd$ph is NA or NULL")
  }
  if(any(is.na(btd$ph$main_ph))|is.null(btd$ph$main_ph)) {
    stop("problem in btd btd$ph$main_ph is not phylo class")
  }
  if(is(btd$ph$main_ph, "phylo")==FALSE) {
    stop("problem in btd btd$ph$main_ph is not phylo class")
  }
  if(length(btd$cluster)!=nrow(btd$A)) {
    stop("problem in btd length(btd$cluster)!=nrow(btd$A)")
  }
  if(btd$k!=length(unique(btd$cluster))) {
    stop("problem in btd k != length(unique(btd$cluster))")
  }
  if(is.data.frame(btd$tree_meta)==FALSE||nrow(btd$tree_meta)<=0) {
    stop("problem in btd btd$tree_meta is not a data.frame")
  }
  if(btd$k!=nrow(btd$tree_meta)) {
    stop("problem in btd k!=nrow(btd$tree_meta)")
  }
  if(any(is.na(btd$tree_meta))) {
    stop("problem in btd NAs in btd$tree_meta")
  }
}

check_fs <- function(fs, btd) {
  # check fs
  if(missing(fs)==TRUE) {
    stop("fs input not found")
  }
  if(is.numeric(fs)==FALSE) {
    stop("fs must be a numeric vector or matrix")
  }
  if(is.vector(fs)==FALSE&
     is.matrix(fs)==FALSE) {
    stop("fs must be a numeric vector or matrix")
  }
  if(is.vector(fs)) {
    if(length(fs)!=length(btd$cluster)) {
      stop("length(fs) != length(btd$cluster)")
    }
  }
  if(is.matrix(fs)) {
    if(nrow(fs)!=length(btd$cluster)) {
      stop("nrow(fs) != length(btd$cluster)")
    }
  }
  if(any(is.infinite(fs))) {
    warning("some feature values in fs are infinite, they will be omitted")
  }
  if(any(is.na(fs))) {
    warning("some features are NAs, they will be omitted")
  }
  if(is.vector(fs)) {
    if(all(is.na(fs))|
       all(is.infinite(fs))) {
      stop("all features are NA or infinite values")
    }
  }
  if(is.matrix(fs)) {
    for(i in seq_len(length.out = ncol(fs))) {
      if(all(is.na(fs[,i]))|
         all(is.infinite(fs[,i]))) {
        stop("complete feature column is composed of NA or infinite values")
      }
    }
  }
}

check_x_axis_name <- function(x_axis_name) {
  
  # check x_axis_name
  if(missing(x_axis_name)==TRUE) {
    stop("x_axis_name input not found")
  }
  # check x_axis_name
  if(is.character(x_axis_name)==FALSE) {
    stop("x_axis_name must be a character string")
  }
  if(length(x_axis_name)!=1) {
    stop("x_axis_name must be a character string")
  }
}

check_rotate_x_axis_labels <- function(rotate_x_axis_labels) {
  # rotate_x_axis_labels
  if(missing(rotate_x_axis_labels)) {
    stop("rotate_x_axis_labels input not found")
  }
  if(length(rotate_x_axis_labels)!=1) {
    stop("rotate_x_axis_labels is a logical parameter (TRUE or FALSE)")
  }
  if(is.logical(rotate_x_axis_labels)==FALSE) {
    stop("rotate_x_axis_labels is a logical parameter (TRUE or FALSE)")
  }
  if(is.na(rotate_x_axis_labels)==TRUE) {
    stop("rotate_x_axis_labels is a logical parameter (TRUE or FALSE)")
  }
}

check_summary_function <- function(summary_function) {
  # check summary_function
  if(missing(summary_function)) {
    stop("summary_function input not found")
  }
  if(is.character(summary_function)==FALSE) {
    stop("summary_function is one of: 'mean', 'median', 'sum',
           'pct nonzero', 'pct zero'")
  }
  if(length(summary_function) != 1) {
    stop("summary_function is one of: 'mean', 'median', 'sum',
           'pct nonzero', 'pct zero'")
  }
  if(!summary_function %in% c("mean", "median", "sum",
                              "pct nonzero", "pct zero")) {
    stop("summary_function is one of: 'mean', 'median', 'sum',
           'pct nonzero', 'pct zero'")
  }
}

check_tile_text_size <- function(tile_text_size) {
  if(missing(tile_text_size)) {
    stop("tile_text_size input not found")
  }
  # check tile_text_size
  if(is.numeric(tile_text_size)==FALSE) {
    stop("tile_text_size must be a number >0")
  }
  if(length(tile_text_size)!=1) {
    stop("tile_text_size must be a number >0")
  }
  if(is.infinite(tile_text_size)) {
    stop("tile_text_size must be a number >0")
  }
  if(tile_text_size<=0) {
    stop("tile_text_size must be a number >0")
  }
}

check_tile_bw <- function(tile_bw) {
  if(missing(tile_bw)) {
    stop("tile_bw input not found")
  }
  if(length(tile_bw)!=1) {
    stop("tile_bw is a logical parameter (TRUE or FALSE)")
  }
  if(is.logical(tile_bw)==FALSE) {
    stop("tile_bw is a logical parameter (TRUE or FALSE)")
  }
  if(is.na(tile_bw)==TRUE) {
    stop("tile_bw is a logical parameter (TRUE or FALSE)")
  }
}

check_f <- function(f, btd) {
  # check f
  if(missing(f)){
    stop("f input not found")
  }
  if(is.character(f)==FALSE) {
    stop("f must be a character vector")
  }
  if(is.vector(f)==FALSE) {
    stop("f must be a character vector")
  }
  if(length(f)!=length(btd$cluster)) {
    stop("length of f is not equal to number of cells in btd")
  }
  if(any(is.na(f))|
     any(is.null(f))|
     any(is.infinite(f))) {
    stop("f must be a character vector")
  }
}

check_integrate_vertical <- function(integrate_vertical) {
  # check integrate_vertical
  if(missing(integrate_vertical)==TRUE) {
    stop("integrate_vertical input not found")
  }
  if(is.logical(integrate_vertical)==FALSE) {
    stop("integrate_vertical is a logical parameter (TRUE or FALSE)")
  }
  if(length(integrate_vertical)!=1) {
    stop("integrate_vertical is a logical parameter (TRUE or FALSE)")
  }
  if(is.na(integrate_vertical)==TRUE) {
    stop("integrate_vertical is a logical parameter (TRUE or FALSE)")
  }
}

check_labels <- function(labels) {
  # check labels
  if(missing(labels)) {
    stop("labels input not found")
  }
  if(length(labels)<=1) {
    stop("labels must be a vector with more than one element")
  }
  if(is.character(labels)==FALSE&
     is.numeric(labels)==FALSE) {
    stop("labels can only contain characters or numbers")
  }
  if(is.vector(labels)==FALSE) {
    stop("labels must be a vector")
  }
  if(any(is.infinite(labels))==TRUE) {
    stop("labels cannot have INF/NA/NULL values")
  }
  if(any(is.na(labels))==TRUE) {
    stop("labels cannot have INF/NA/NULL values")
  }
  if(any(is.null(labels))==TRUE) {
    stop("labels cannot have INF/NA/NULL values")
  }
}

check_clusters <- function(clusters, labels) {
  # check clusters
  if(missing(clusters)) {
    stop("clusters input not found")
  }
  if(length(clusters)<=1) {
    stop("clusters must be a vector with more than one element")
  }
  if(is.character(clusters)==FALSE&
     is.numeric(clusters)==FALSE) {
    stop("clusters can only contain characters or numbers")
  }
  if(is.vector(clusters)==FALSE) {
    stop("clusters must be a vector")
  }
  if(any(is.infinite(clusters))==TRUE) {
    stop("clusters cannot have INF/NA/NULL values")
  }
  if(any(is.na(clusters))==TRUE) {
    stop("clusters cannot have INF/NA/NULL values")
  }
  if(any(is.null(clusters))==TRUE) {
    stop("clusters cannot have INF/NA/NULL values")
  }
  
  if(length(labels)!=length(clusters)) {
    stop("labels and clusters must be equal-length vectors")
  }
}

check_obj_k_r <- function(obj) {
  # check obj
  if(missing(obj)) {
    stop("obj input not found")
  }
  if(any(is.na(obj))|is.null(obj)|is.na(class(obj))|
     is.null(class(obj))|
     (is(obj, "boot_k")==FALSE&
      is(obj, "boot_r")==FALSE)) {
    stop("problem with input obj")
  }
  
  if(is.list(obj$boot_obj)==FALSE|
     any(is.na(obj$boot_obj))|
     is.null(obj$boot_obj)|
     length(obj)<=1) {
    stop("no boot_obj found in obj")
  }
}

check_cluster_dend <- function(cluster) {
  # check cluster
  if(missing(cluster)) {
    stop("cluster input not found")
  }
  if(length(cluster)<=1) {
    stop("cluster must be a vector with more than one element")
  }
  if(is.character(cluster)==FALSE&
     is.numeric(cluster)==FALSE) {
    stop("cluster can only contain characters or numbers")
  }
  if(is.vector(cluster)==FALSE) {
    stop("cluster must be a vector")
  }
  if(any(is.infinite(cluster))==TRUE) {
    stop("cluster cannot have INF/NA/NULL values")
  }
  if(any(is.na(cluster))==TRUE) {
    stop("cluster cannot have INF/NA/NULL values")
  }
  if(any(is.null(cluster))==TRUE) {
    stop("cluster cannot have INF/NA/NULL values")
  }
}

check_ph_dend <- function(ph, cluster) {
  # check ph
  if(missing(ph)) {
    stop("ph input not found")
  }
  if(is(ph, "phylo")==FALSE) {
    stop("ph must be a phylo object")
  }
  if(any(cluster %in% ph$tip.label == FALSE)) {
    stop("mismatch between tip.labels of ph and cluster IDs")
  }
}

check_hclust_method <- function(hclust_method) {
  if(missing(hclust_method)) {
    stop("hclust_method input not found")
  }
  if(length(hclust_method)!=1) {
    stop("hclust_method must be one of: ward.D, single, complete, 
    average, mcquitty, median, centroid or ward.D2")
  }
  if(is.character(hclust_method)==FALSE) {
    stop("hclust_method must be one of: ward.D, single, complete, 
    average, mcquitty, median, centroid or ward.D2")
  }
  if(!hclust_method %in% c("ward.D", "ward.D2", "single", 
                          "complete", "average", "mcquitty",
                          "median", "centroid")) {
    stop("hclust_method must be one of: ward.D, single, complete, 
    average, mcquitty, median, centroid or ward.D2")
  }
}

check_hclust_distance <- function(hclust_distance) {
  
  if(missing(hclust_distance)) {
    stop("hclust_distance input not found")
  }
  if(length(hclust_distance)!=1) {
    stop("hclust_distance must be one of: euclidean or manhattan")
  }
  if(is.character(hclust_distance)==FALSE) {
    stop("hclust_distance must be one of: euclidean or manhattan")
  }
  if(!hclust_distance %in% c("euclidean", "manhattan")) {
    stop("hclust_distance must be one of: euclidean or manhattan")
  }
}

check_ratio <- function(ratio) {
  # check ratio
  if(missing(ratio)) {
    stop("ratio input not found")
  }
  if(is.numeric(ratio)==FALSE) {
    stop("ratio must be a probability")
  }
  if(length(ratio)!=1) {
    stop("ratio must be a probability")
  }
  if(is.infinite(ratio)) {
    stop("ratio must be a probability")
  }
  if(is.na(ratio)==TRUE) {
    stop("ratio must be a probability")
  }
  if(is.null(ratio)==TRUE) {
    stop("ratio must be a probability")
  }
  if(ratio<0 | ratio>1) {
    stop("ratio must be a probability")
  }
}

