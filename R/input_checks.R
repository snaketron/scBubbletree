check_x <- function(x) {
  # check x
  if(base::missing(x)) {
    stop("x input not found")
  }
  if(base::is.numeric(x)==FALSE) {
    stop("x must be numeric matrix")
  }
  if(base::is.matrix(x)==FALSE) {
    stop("x must be numeric matrix")
  }
  if(base::any(base::is.infinite(x))==TRUE) {
    stop("x must be numeric matrix, infinite values not allowed")
  }
  if(base::any(base::is.na(x))==TRUE) {
    stop("x must be numeric matrix, NAs not allowed")
  }
  if(base::any(base::is.null(x))==TRUE) {
    stop("x must be numeric matrix, NULLs not allowed")
  }
  if(base::all(x == x[1,1])==TRUE) {
    stop("all elements in x are identical")
  }
  if(base::ncol(x)>base::nrow(x)) {
    warning("more columns (features) than rows (cells) in x")
  }
}

check_B_gap <- function(B_gap) {
  # check B_gap
  if(base::missing(B_gap)) {
    stop("B_gap input not found")
  }
  if(base::is.numeric(B_gap)==FALSE) {
    stop("B_gap must be a positive integer > 0")
  }
  if(base::length(B_gap)!=1) {
    stop("B_gap must be a positive integer > 0")
  }
  if(B_gap<1) {
    stop("B_gap must be a positive integer > 0")
  }
  if(base::is.infinite(B_gap)==TRUE) {
    stop("B_gap must be a positive integer > 0")
  }
  if(base::is.na(B_gap)==TRUE) {
    stop("B_gap must be a positive integer > 0")
  }
  if(B_gap%%1!=0) {
    stop("B_gap must be a positive integer > 0")
  }
}

check_rs <- function(rs) {
  # check rs
  if(base::missing(rs)) {
    stop("rs input not found")
  }
  if(base::is.numeric(rs)==FALSE) {
    stop("rs must be a positive number or vector of positive numbers")
  }
  if(base::is.vector(x = rs)==FALSE) {
    stop("rs must be a positive number or vector of positive numbers")
  }
  if(base::length(rs)<=0) {
    stop("rs must be a positive number or vector of positive numbers")
  }
  if(base::any(base::is.infinite(rs))==TRUE) {
    stop("rs must be a positive number or vector of positive numbers,
           no infinite values are allowed")
  }
  if(base::any(base::is.na(rs))==TRUE) {
    stop("rs must be a positive number or vector of positive numbers,
           no NAs are allowed")
  }
  if(base::any(base::is.null(rs))==TRUE) {
    stop("rs must be a positive number or vector of positive numbers,
           no NULLs are allowed")
  }
  if(base::any(rs<0)==TRUE) {
    stop("rs must be a positive number or vector of positive numbers")
  }
  if(base::any(base::duplicated(rs))==TRUE) {
    stop("rs must be a positive number or vector of positive numbers,
           duplicate r values are not allowed")
  }
}

check_cores <- function(cores) {
  # check cores
  if(base::missing(cores)) {
    stop("cores input not found")
  }
  if(base::is.numeric(cores)==FALSE) {
    stop("cores must be a positive integer")
  }
  if(base::length(cores)!=1) {
    stop("cores must be a positive integer")
  }
  if(base::is.infinite(cores)==TRUE) {
    stop("cores must be a positive integer")
  }
  if(base::is.na(cores)==TRUE) {
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
  if(base::missing(x = n_start)) {
    stop("n_start input not found")
  }
  if(base::is.numeric(n_start)==FALSE) {
    stop("n_start must be a positive integer")
  }
  if(base::length(n_start) != 1) {
    stop("n_start must be a positive integer")
  }
  if(n_start < 1) {
    stop("n_start must be a positive integer")
  }
  if(base::is.infinite(n_start)==TRUE) {
    stop("n_start must be a positive integer")
  }
  if(base::is.na(n_start)==TRUE) {
    stop("n_start must be a positive integer")
  }
  if(base::is.null(n_start)==TRUE) {
    stop("n_start must be a positive integer")
  }
  if(n_start%%1!=0) {
    stop("n_start must be a positive integer")
  }
}

check_iter_max <- function(iter_max) {
  # iter_max
  if(base::missing(iter_max)) {
    stop("iter_max input not found")
  }
  if(base::is.numeric(iter_max)==FALSE) {
    stop("iter_max must be a positive integer")
  }
  if(base::length(iter_max)!=1) {
    stop("iter_max must be a positive integer")
  }
  if(iter_max<1) {
    stop("iter_max must be a positive integer")
  }
  if(base::is.infinite(iter_max)==TRUE) {
    stop("iter_max must be a positive integer")
  }
  if(base::is.na(iter_max)==TRUE) {
    stop("iter_max must be a positive integer")
  }
  if(base::is.null(iter_max)==TRUE) {
    stop("iter_max must be a positive integer")
  }
  if(iter_max%%1!=0) {
    stop("iter_max must be a positive integer")
  }
}

check_louvain_algorithm <- function(algorithm) {
  # algorithm
  if(base::missing(algorithm)) {
    stop("algorithm input not found")
  }
  if(base::length(algorithm)!=1) {
    stop("see ?FindClusters from R-package Seurat: algorithm must be
      one of: original, LMR, SLM or Leiden")
  }
  if(base::is.character(algorithm)==FALSE) {
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
  if(base::missing(knn_k)) {
    stop("knn_k input not found")
  }
  if(base::is.numeric(knn_k)==FALSE) {
    stop("knn_k must be a positive integer")
  }
  if(base::length(knn_k)!=1) {
    stop("knn_k must be a positive integer")
  }
  if(base::is.infinite(knn_k)==TRUE) {
    stop("knn_k must be a positive integer")
  }
  if(base::is.na(knn_k)==TRUE) {
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
  if(base::missing(ks)==TRUE) {
    stop("ks input not found")
  }
  if(base::is.numeric(ks)==FALSE) {
    stop("ks must be a positive integer or vector of positive integers")
  }
  if(base::is.vector(x = ks)==FALSE) {
    stop("ks must be a positive integer or vector of positive integers")
  }
  if(base::length(ks)<=0) {
    stop("ks must be a positive integer or vector of positive integers")
  }
  if(base::any(base::is.infinite(ks))==TRUE) {
    stop("ks must be a positive integer or vector of positive integers,
           no infinite values are allowed")
  }
  if(base::any(base::is.na(ks))==TRUE) {
    stop("ks must be a positive integer or vector of positive integers,
           no NAs are allowed")
  }
  if(base::any(base::is.null(ks))==TRUE) {
    stop("ks must be a positive integer or vector of positive integers,
           no NULLs are allowed")
  }
  if(base::any(ks<0)==TRUE) {
    stop("ks must be a positive integer or vector of positive integers")
  }
  if(base::any(base::duplicated(ks))==TRUE) {
    stop("ks must be a positive integer or vector of positive integers,
           duplicate k values are not allowed")
  }
  if(base::length(ks)==1) {
    if(base::all(ks<1)) {
      stop("ks must be a positive integer or vector of positive integers")
    }
  }
  if(base::any(ks>=base::nrow(x))==TRUE) {
    stop("max(ks) should be smaller than nrow(x)")
  }
}

check_kmeans_algorithm <- function(kmeans_algorithm) {
  # kmeans_algorithm
  if(base::missing(kmeans_algorithm)) {
    stop("kmeans_algorithm input not found")
  }
  if(base::length(kmeans_algorithm)!=1) {
    stop("see ?kmeans: kmeans_algorithm must be one of: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")
  }
  if(base::is.character(kmeans_algorithm)==FALSE) {
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
  if(base::missing(verbose)) {
    stop("verbose input not found")
  }
  if(length(verbose)!=1) {
    stop("verbose is a logical parameter (TRUE or FALSE)")
  }
  if(is.logical(verbose)==FALSE) {
    stop("verbose is a logical parameter (TRUE or FALSE)")
  }
  if(base::is.na(verbose)==TRUE) {
    stop("verbose is a logical parameter (TRUE or FALSE)")
  }
}

check_k <- function(k) {
  # check k
  if(base::missing(k)) {
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
  if(base::missing(B)) {
    stop("B input not found")
  }
  if(base::is.numeric(B)==FALSE) {
    stop("B must be a positive integer > 0")
  }
  if(base::length(B)!=1) {
    stop("B must be a positive integer > 0")
  }
  if(B<1) {
    stop("B must be a positive integer > 0")
  }
  if(base::is.infinite(B)==TRUE) {
    stop("B must be a positive integer > 0")
  }
  if(base::is.na(B)==TRUE) {
    stop("B must be a positive integer > 0")
  }
  if(B%%1!=0) {
    stop("B must be a positive integer > 0")
  }
}

check_N_eff <- function(N_eff) {
  # check N_eff
  if(base::missing(N_eff)) {
    stop("N_eff input not found")
  }
  if(base::is.numeric(N_eff)==FALSE) {
    stop("N_eff must be a positive integer")
  }
  if(base::length(N_eff)!=1) {
    stop("N_eff must be a positive integer")
  }
  if(N_eff<1) {
    stop("N_eff must be a positive integer")
  }
  if(base::is.infinite(N_eff)==TRUE) {
    stop("N_eff must be a positive integer")
  }
  if(base::is.na(N_eff)==TRUE) {
    stop("N_eff must be a positive integer")
  }
  if(base::is.null(N_eff)==TRUE) {
    stop("N_eff must be a positive integer")
  }
  if(N_eff%%1!=0) {
    stop("N_eff must be a positive integer")
  }
}

check_round_digits <- function(round_digits) {
  # check round_digits
  if(base::missing(round_digits)) {
    stop("round_digits input not found")
  }
  if(base::is.numeric(round_digits)==FALSE) {
    stop("round_digits must be a positive integer")
  }
  if(base::length(round_digits)!=1) {
    stop("round_digits must be a positive integer")
  }
  if(round_digits<0) {
    stop("round_digits must be a positive integer")
  }
  if(base::is.finite(round_digits)==FALSE) {
    stop("round_digits must be a positive integer")
  }
  if(round_digits%%1!=0) {
    stop("round_digits must be a positive integer")
  }
}

check_show_simple_count <- function(show_simple_count) {
  # show_simple_count
  if(base::missing(show_simple_count)) {
    stop("show_simple_count input not found")
  }
  if(length(show_simple_count)!=1) {
    stop("show_simple_count is a logical parameter (TRUE or FALSE)")
  }
  if(is.logical(show_simple_count)==FALSE) {
    stop("show_simple_count is a logical parameter (TRUE or FALSE)")
  }
  if(base::is.na(show_simple_count)==TRUE) {
    stop("show_simple_count is a logical parameter (TRUE or FALSE)")
  }
}

check_r <- function(r) {
  # check r
  if(base::missing(r)) {
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
  if(base::missing(btd)) {
    stop("bubbletree (btd) input not found")
  }
  if(length(btd)!=9) {
    stop("btd should be a list with 9 elements")
  }
  if(base::any(base::is.na(btd))||
     base::is.null(btd)||
     base::any(base::is.na(base::class(btd)))||
     base::is.null(base::class(btd))||
     (methods::is(btd, "bubbletree_kmeans")==FALSE&
      methods::is(btd, "bubbletree_louvain")==FALSE&
      methods::is(btd, "bubbletree_dummy")==FALSE)) {
    stop("NA/NULL elements or wrong class detected in the bubbletree")
  }
  if(base::is.vector(btd$cluster)==FALSE||
     base::any(base::is.na(base::is.vector(btd$cluster)))|
     base::any(base::is.infinite(base::is.vector(btd$cluster)))|
     base::any(base::is.null(base::is.vector(btd$cluster)))) {
    stop("NA/NULL/Inf cluster assignments present in bubbletree")
  }
  
  # check btd$A
  if(base::is.numeric(btd$A)==FALSE) {
    stop("btd$A must be numeric matrix")
  }
  if(base::is.matrix(btd$A)==FALSE) {
    stop("btd$A must be numeric matrix")
  }
  if(base::any(base::is.infinite(btd$A))==TRUE) {
    stop("btd$A must be numeric matrix, infinite values not allowed")
  }
  if(base::any(base::is.na(btd$A))==TRUE) {
    stop("btd$A must be numeric matrix, NAs not allowed")
  }
  if(base::any(base::is.null(btd$A))==TRUE) {
    stop("btd$A must be numeric matrix, NULLs not allowed")
  }
  if(base::all(btd$A == btd$A[1,1])==TRUE) {
    stop("all elements in btd$A are identical")
  }
  if(base::ncol(btd$A)>base::nrow(btd$A)) {
    warning("more columns (features) than rows (cells) in btd$A")
  }
  if(nrow(btd$A)!=length(btd$cluster)) {
    stop("problem in btd: nrow(btd$A)!=length(btd$cluster)")
  }
  if(is.numeric(btd$k)==FALSE) {
    stop("problem in btd: btd$k must be a positive integer (k>=2)")
  }
  if(length(btd$k)!=1) {
    stop("problem in btd: btd$k must be a positive integer (k>=2)")
  }
  if(btd$k<=1) {
    stop("problem in btd: btd$k must be a positive integer (k>=2)")
  }
  if(base::is.infinite(btd$k)==TRUE) {
    stop("problem in btd: btd$k must be a positive integer (k>=2)")
  }
  if(btd$k%%1!=0) {
    stop("problem in btd: btd$k must be a positive integer (k>=2)")
  }
  if(methods::is(btd$ph$main_ph, "phylo")==FALSE) {
    stop("problem in btd: btd$ph$main_ph is not phylo class")
  }
  if(methods::is(btd$ph$boot_ph, "multiPhylo")==FALSE) {
    stop("problem in btd: btd$ph$boot_ph is not multiPhylo class")
  }
  if(base::length(btd$cluster)!=base::nrow(btd$A)) {
    stop("problem in btd: length(btd$cluster)!=nrow(btd$A)")
  }
  if(btd$k!=length(unique(btd$cluster))) {
    stop("problem in btd: k != length(unique(btd$cluster))")
  }
  if(is.data.frame(btd$tree_meta)==FALSE||
     nrow(btd$tree_meta)<=0) {
    stop("problem in btd: btd$tree_meta is not a data.frame")
  }
  if(btd$k!=base::nrow(btd$tree_meta)) {
    stop("problem in btd: k!=nrow(btd$tree_meta)")
  }
  if(base::any(base::is.na(btd$tree_meta))) {
    stop("problem in btd: NAs in btd$tree_meta")
  }
}

check_fs <- function(fs, btd) {
  # check fs
  if(base::missing(fs)==TRUE) {
    stop("fs input not found")
  }
  if(base::is.numeric(fs)==FALSE) {
    stop("fs must be a numeric vector or matrix")
  }
  if(base::is.vector(fs)==FALSE&
     base::is.matrix(fs)==FALSE) {
    stop("fs must be a numeric vector or matrix")
  }
  if(base::is.vector(fs)) {
    if(base::length(fs)!=base::length(btd$cluster)) {
      stop("length(fs) != length(btd$cluster)")
    }
  }
  if(base::is.matrix(fs)) {
    if(base::nrow(fs)!=base::length(btd$cluster)) {
      stop("nrow(fs) != length(btd$cluster)")
    }
  }
  if(base::any(base::is.infinite(fs))) {
    warning("some feature values in fs are infinite, they will be omitted")
  }
  if(base::any(base::is.na(fs))) {
    warning("some features are NAs, they will be omitted")
  }
  if(base::is.vector(fs)) {
    if(base::all(base::is.na(fs))|
       base::all(base::is.infinite(fs))) {
      stop("all features are NA or infinite values")
    }
  }
  if(base::is.matrix(fs)) {
    for(i in base::seq_len(length.out = base::ncol(fs))) {
      if(base::all(base::is.na(fs[,i]))|
         base::all(base::is.infinite(fs[,i]))) {
        stop("complete feature column is composed of NA or infinite values")
      }
    }
  }
}

check_x_axis_name <- function(x_axis_name) {
  
  # check x_axis_name
  if(base::missing(x_axis_name)==TRUE) {
    stop("x_axis_name input not found")
  }
  # check x_axis_name
  if(base::is.character(x_axis_name)==FALSE) {
    stop("x_axis_name must be a character string")
  }
  if(base::length(x_axis_name)!=1) {
    stop("x_axis_name must be a character string")
  }
}

check_rotate_x_axis_labels <- function(rotate_x_axis_labels) {
  # rotate_x_axis_labels
  if(base::missing(rotate_x_axis_labels)) {
    stop("rotate_x_axis_labels input not found")
  }
  if(base::length(rotate_x_axis_labels)!=1) {
    stop("rotate_x_axis_labels is a logical parameter (TRUE or FALSE)")
  }
  if(base::is.logical(rotate_x_axis_labels)==FALSE) {
    stop("rotate_x_axis_labels is a logical parameter (TRUE or FALSE)")
  }
  if(base::is.na(rotate_x_axis_labels)==TRUE) {
    stop("rotate_x_axis_labels is a logical parameter (TRUE or FALSE)")
  }
}

check_summary_function <- function(summary_function) {
  # check summary_function
  if(base::missing(summary_function)) {
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
  if(base::missing(tile_text_size)) {
    stop("tile_text_size input not found")
  }
  # check tile_text_size
  if(base::is.numeric(tile_text_size)==FALSE) {
    stop("tile_text_size must be a number >0")
  }
  if(base::length(tile_text_size)!=1) {
    stop("tile_text_size must be a number >0")
  }
  if(base::is.infinite(tile_text_size)) {
    stop("tile_text_size must be a number >0")
  }
  if(tile_text_size<=0) {
    stop("tile_text_size must be a number >0")
  }
}

check_tile_bw <- function(tile_bw) {
  if(base::missing(tile_bw)) {
    stop("tile_bw input not found")
  }
  if(base::length(tile_bw)!=1) {
    stop("tile_bw is a logical parameter (TRUE or FALSE)")
  }
  if(base::is.logical(tile_bw)==FALSE) {
    stop("tile_bw is a logical parameter (TRUE or FALSE)")
  }
  if(base::is.na(tile_bw)==TRUE) {
    stop("tile_bw is a logical parameter (TRUE or FALSE)")
  }
}

check_f <- function(f, btd) {
  # check f
  if(base::missing(f)){
    stop("f input not found")
  }
  if(base::is.character(f)==FALSE) {
    stop("f must be a character vector")
  }
  if(base::is.vector(f)==FALSE) {
    stop("f must be a character vector")
  }
  if(base::length(f)!=base::length(btd$cluster)) {
    stop("length of f is not equal to number of cells in btd")
  }
  if(base::any(base::is.na(f))|
     base::any(base::is.null(f))|
     base::any(base::is.infinite(f))) {
    stop("f must be a character vector")
  }
}

check_integrate_vertical <- function(integrate_vertical) {
  # check integrate_vertical
  if(base::missing(integrate_vertical)==TRUE) {
    stop("integrate_vertical input not found")
  }
  if(base::is.logical(integrate_vertical)==FALSE) {
    stop("integrate_vertical is a logical parameter (TRUE or FALSE)")
  }
  if(base::length(integrate_vertical)!=1) {
    stop("integrate_vertical is a logical parameter (TRUE or FALSE)")
  }
  if(base::is.na(integrate_vertical)==TRUE) {
    stop("integrate_vertical is a logical parameter (TRUE or FALSE)")
  }
}

check_labels <- function(labels) {
  # check labels
  if(base::missing(labels)) {
    stop("labels input not found")
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
}

check_clusters <- function(clusters, labels) {
  # check clusters
  if(base::missing(clusters)) {
    stop("clusters input not found")
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

check_obj_k_r <- function(obj) {
  # check obj
  if(base::missing(obj)) {
    stop("obj input not found")
  }
  if(base::any(base::is.na(obj))|base::is.null(obj)|base::is.na(class(obj))|
     base::is.null(class(obj))|
     (methods::is(obj, "boot_k")==FALSE&
      methods::is(obj, "boot_r")==FALSE)) {
    stop("problem with input obj")
  }
  
  if(base::is.list(obj$boot_obj)==FALSE|
     base::any(base::is.na(obj$boot_obj))|
     base::is.null(obj$boot_obj)|
     base::length(obj)<=1) {
    stop("no boot_obj found in obj")
  }
}

check_cluster_dend <- function(cluster) {
  # check cluster
  if(base::missing(cluster)) {
    stop("cluster input not found")
  }
  if(base::length(cluster)<=1) {
    stop("cluster must be a vector with more than one element")
  }
  if(base::is.character(cluster)==FALSE&
     base::is.numeric(cluster)==FALSE) {
    stop("cluster can only contain characters or numbers")
  }
  if(base::is.vector(cluster)==FALSE) {
    stop("cluster must be a vector")
  }
  if(base::any(base::is.infinite(cluster))==TRUE) {
    stop("cluster cannot have INF/NA/NULL values")
  }
  if(base::any(base::is.na(cluster))==TRUE) {
    stop("cluster cannot have INF/NA/NULL values")
  }
  if(base::any(base::is.null(cluster))==TRUE) {
    stop("cluster cannot have INF/NA/NULL values")
  }
}

check_ph_dend <- function(ph, cluster) {
  # check ph
  if(base::missing(ph)) {
    stop("ph input not found")
  }
  if(methods::is(ph, "phylo")==FALSE) {
    stop("ph must be a phylo object")
  }
  if(base::any(cluster %in% ph$tip.label == FALSE)) {
    stop("mismatch between tip.labels of ph and cluster IDs")
  }
}
