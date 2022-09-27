
get_num_tiles <- function(btd,
                          fs,
                          summary_function = "mean",
                          round_digits = 2,
                          show_hclust = FALSE,
                          disable_hclust = FALSE,
                          tile_text_size = 3,
                          tile_bw = FALSE,
                          x_axis_name = "Feature",
                          rotate_x_axis_labels = TRUE) {
  
  
  # check input param
  check_input <- function(btd,
                          fs,
                          summary_function,
                          round_digits,
                          show_hclust,
                          disable_hclust,
                          tile_text_size,
                          tile_bw,
                          x_axis_name,
                          rotate_x_axis_labels) {
    
    
    # check btd
    if(base::any(base::is.na(btd))||
       base::is.null(btd)||
       base::any(base::is.na(base::class(btd)))||
       base::is.null(base::class(btd))||
       (methods::is(btd, "bubbletree_kmeans")==FALSE&
        methods::is(btd, "bubbletree_louvain")==FALSE&
        methods::is(btd, "bubbletree_dummy")==FALSE)) {
      stop("problem with the input bubbletree")
    }
    
    if(base::is.vector(btd$cluster)==FALSE||
       base::is.na(base::is.vector(btd$cluster))||
       base::is.null(base::is.vector(btd$cluster))) {
      stop("no clustering results in bubbletree")
    }
    
    # check fs
    if(is.numeric(fs)==FALSE) {
      stop("fs must be a numeric vector or matrix")
    }
    if(is.vector(fs)==FALSE & is.matrix(fs)==FALSE) {
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
      warning("some feature values in fs are infinite,
              they will be omitted.")
    }
    
    
    # check round_digits
    if(is.numeric(round_digits)==FALSE) {
      stop("round_digits must be an integer >=0")
    }
    if(round_digits%%1!=0) {
      stop("round_digits must be an integer >=0")
    }
    if(length(round_digits)!=1) {
      stop("round_digits must be an integer >=0")
    }
    if(is.infinite(round_digits)) {
      stop("round_digits must be an integer >=0")
    }
    if(round_digits<0) {
      stop("round_digits must be an integer >=0")
    }
    
    
    # check summary_function
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
    
    
    # check show_hclust
    if(is.logical(show_hclust)==FALSE) {
      stop("show_hclust must be a logical parameter")
    }
    if(length(show_hclust)!=1) {
      stop("show_hclust must be a logical parameter (either TRUE or FALSE)")
    }
    
    
    # check disable_hclust
    if(is.logical(disable_hclust)==FALSE) {
      stop("disable_hclust must be a logical parameter")
    }
    if(length(disable_hclust)!=1) {
      stop("disable_hclust must be a logical parameter (either TRUE or FALSE)")
    }
    
    if(disable_hclust==TRUE & show_hclust==TRUE) {
      warning("hierarchical feature clustering is disabled (disable_hclust=T),
              show_hclust=T has no effect (set disable_hclust=F to show
              hierarchical dendrogram")
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
    if(tile_text_size<0) {
      stop("tile_text_size must be an integer >=0")
    }
    
    
    # check tile_bw
    if(is.logical(tile_bw)==FALSE) {
      stop("tile_bw must be a logical parameter")
    }
    if(length(tile_bw)!=1) {
      stop("tile_bw must be a logical parameter (either TRUE or FALSE)")
    }
    
    
    # check x_axis_name
    if(is.character(x_axis_name)==FALSE) {
      stop("x_axis_name must be a character string")
    }
    if(length(x_axis_name)!=1) {
      stop("x_axis_name must be a character string")
    }
    
    # check rotate_x_axis_labels
    if(is.logical(rotate_x_axis_labels)==FALSE) {
      stop("rotate_x_axis_labels must be a logical parameter
           (either TRUE or FALSE)")
    }
    if(length(rotate_x_axis_labels)!=1) {
      stop("rotate_x_axis_labels must be a logical parameter
           (either TRUE or FALSE)")
    }
    
  }
  
  
  # check inputs
  check_input(btd = btd,
              fs = fs,
              summary_function = summary_function,
              round_digits = round_digits,
              show_hclust = show_hclust,
              disable_hclust = disable_hclust,
              tile_text_size = tile_text_size,
              tile_bw = tile_bw,
              x_axis_name = x_axis_name,
              rotate_x_axis_labels = rotate_x_axis_labels)
  
  
  
  get_summary <- function(k, a,
                          feature,
                          round_digits,
                          summary_function) {
    f <- base::data.frame(cluster = k, a = a)
    
    if(summary_function == "mean") {
      f <- stats::aggregate(a~cluster,
                            data = f,
                            FUN = base::mean,
                            na.action = stats::na.omit)
      f$value <- round(x = f$a, digits = round_digits)
    }
    if(summary_function == "median") {
      f <- stats::aggregate(a~cluster,
                            data = f,
                            FUN = stats::median,
                            na.action = stats::na.omit)
      f$value <- base::round(x = f$a, digits = round_digits)
    }
    if(summary_function == "sum") {
      f <- stats::aggregate(a~cluster,
                            data = f,
                            FUN = base::sum,
                            na.action = stats::na.omit)
      f$value <- base::round(x = f$a, digits = round_digits)
    }
    if(summary_function == "pct nonzero") {
      get_nonzero <- function(x) {
        return(base::sum(x>0)/base::length(x))
      }
      f <- stats::aggregate(a~cluster,
                            data = f,
                            FUN = get_nonzero,
                            na.action = stats::na.omit)
      f$value <- base::round(x = f$a, digits = round_digits)
    }
    if(summary_function == "pct zero") {
      get_zero <- function(x) {
        return(base::sum(x==0)/base::length(x))
      }
      f <- stats::aggregate(a~cluster,
                            data = f,
                            FUN = get_zero,
                            na.action = stats::na.omit)
      f$value <- base::round(x = f$a, digits = round_digits)
    }
    
    f$a <- NULL
    f$feature <- feature
    return(f)
  }
  
  
  if(base::is.vector(fs)) {
    fs <- base::matrix(data = fs, ncol = 1)
    if(base::is.null(base::colnames(fs))) {
      base::colnames(fs) <- "f"
    }
  }
  
  
  ws <- vector(mode = "list", length = ncol(fs))
  for(i in base::seq_len(length.out = base::ncol(fs))) {
    ws[[i]] <- get_summary(k = btd$cluster,
                           a = fs[, i],
                           feature = base::colnames(fs)[i],
                           round_digits = round_digits,
                           summary_function = summary_function)
  }
  ws <- do.call(rbind, ws)
  
  
  ws <- base::merge(x = ws, y = btd$tree_meta,
                    by.x = "cluster",
                    by.y = "label",
                    all = TRUE)
  ws <- ws[base::order(ws$tree_order, decreasing = FALSE), ]
  ws$cluster <- base::factor(x = ws$cluster, levels = base::unique(ws$cluster))
  if(base::any(base::is.na(ws$feature))) {
    ws$feature <- ws$feature[base::is.na(ws$feature)==FALSE][1]
  }
  ws$feature <- base::as.character(ws$feature)
  ws$feature <- base::factor(x = ws$feature, levels = base::colnames(fs))
  
  
  
  # reorder features based on hclust
  if(disable_hclust==FALSE) {
    if(ncol(fs)>1) {
      tree <- get_weighted_feature_dist_num(main_ph = btd$ph$main_ph,
                                            w = ws,
                                            value_var = "value")
      ws$feature <- base::factor(levels = tree$labels, x = ws$feature)
    }
  }
  
  
  w <- ggplot2::ggplot(data = ws)+
    ggplot2::theme_bw(base_size = 10)+
    ggplot2::geom_tile(ggplot2::aes(x = ws$feature, 
                                    y = ws$cluster, 
                                    fill = ws$value),
                       col = "white")+
    ggplot2::geom_text(ggplot2::aes(x = ws$feature, 
                                    y = ws$cluster, 
                                    label = ws$value),
                       col = "black",
                       size = tile_text_size)+
    ggplot2::theme(legend.position = "top",
                   legend.margin=ggplot2::margin(t=0,r=0,b=2,l=0, unit = "pt"),
                   legend.box.margin=ggplot2::margin(-10,-10,-10,-10))+
    ggplot2::xlab(label = x_axis_name)+
    ggplot2::ylab(label = "Bubble")+
    ggplot2::guides(fill = ggplot2::guide_colourbar(barwidth = 5, 
                                                    barheight=1))
  
  
  if(tile_bw==FALSE) {
    w <- w+ggplot2::scale_fill_distiller(name = "Feature",
                                         palette = "Spectral",
                                         na.value = 'white',
                                         breaks = scales::pretty_breaks(n = 3))
  } else {
    w <- w+ggplot2::scale_fill_gradient(name = "Feature",
                                        low = "#f9f9f9",
                                        high = "#848484",
                                        na.value = 'white',
                                        breaks = scales::pretty_breaks(n = 3))
  }
  
  
  if(rotate_x_axis_labels==TRUE) {
    w <- w+ggplot2::theme(axis.text.x = ggplot2::element_text(
      angle = 90, vjust = 0.5, hjust=1))
  }
  
  if(disable_hclust==FALSE) {
    if(show_hclust==TRUE) {
      w <- (w/tree$tree)+patchwork::plot_layout(heights = c(0.7, 0.3))
    }
  }
  
  return(base::list(table = ws, plot = w))
}
