get_num_tiles <- function(
    btd,
    fs,
    summary_function,
    round_digits = 2,
    show_hclust = FALSE,
    disable_hclust = FALSE,
    tile_text_size = 3,
    tile_bw = FALSE,
    x_axis_name = "Feature",
    rotate_x_axis_labels = TRUE) {
  
  # check inputs
  check_input_num_tiles(
    btd = btd,
    fs = fs,
    summary_function = summary_function,
    round_digits = round_digits,
    show_hclust = show_hclust,
    disable_hclust = disable_hclust,
    tile_text_size = tile_text_size,
    tile_bw = tile_bw,
    x_axis_name = x_axis_name,
    rotate_x_axis_labels = rotate_x_axis_labels)
  
  if(base::is.vector(fs)) {
    fs <- base::matrix(data = fs, ncol = 1)
    if(base::is.null(base::colnames(fs))) {
      base::colnames(fs) <- "f"
    }
  }
  
  ws <- vector(mode = "list", length = ncol(fs))
  for(i in base::seq_len(length.out = base::ncol(fs))) {
    ws[[i]] <- get_summary_num_tiles(
      k = btd$cluster,
      a = fs[, i],
      feature = base::colnames(fs)[i],
      round_digits = round_digits,
      summary_function = summary_function)
  }
  ws <- do.call(rbind, ws)
  
  ws <- base::merge(
    x = ws, y = btd$tree_meta,
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
      tree <- get_weighted_feature_dist_num(
        main_ph = btd$ph$main_ph,
        w = ws,
        value_var = "value")
      ws$feature <- base::factor(levels = tree$labels, x = ws$feature)
    }
  }
  
  w <- ggplot_num_tiles(
    ws = ws,
    tile_text_size = tile_text_size, 
    x_axis_name = x_axis_name, 
    tile_bw = tile_bw, 
    rotate_x_axis_labels = rotate_x_axis_labels, 
    disable_hclust = disable_hclust, 
    show_hclust = show_hclust,
    tree = tree)
  
  return(base::list(table = ws, plot = w))
}


check_input_num_tiles <- function(
    btd,
    fs,
    summary_function,
    round_digits,
    show_hclust,
    disable_hclust,
    tile_text_size,
    tile_bw,
    x_axis_name,
    rotate_x_axis_labels) {
  
  check_btd(btd = btd)
  check_fs(fs = fs, btd = btd)
  check_summary_function(summary_function = summary_function)
  check_round_digits(round_digits = round_digits)
  check_show_hclust(show_hclust)
  check_disable_hclust(disable_hclust = disable_hclust, 
                       show_hclust = show_hclust)
  check_tile_text_size(tile_text_size = tile_text_size)
  check_tile_bw(tile_bw = tile_bw)
  check_x_axis_name(x_axis_name = x_axis_name)
  check_rotate_x_axis_labels(rotate_x_axis_labels = rotate_x_axis_labels)
}

get_summary_num_tiles <- function(
    k, a,
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

ggplot_num_tiles <- function(
    ws,
    tile_text_size, 
    x_axis_name, 
    tile_bw, 
    rotate_x_axis_labels, 
    disable_hclust, 
    show_hclust,
    tree) {
  
  w <- ggplot2::ggplot(data = ws)+
    ggplot2::theme_bw(base_size = 10)+
    ggplot2::geom_tile(ggplot2::aes_string(x = "feature", y = "cluster", 
                                           fill = "value"), col = "white")+
    ggplot2::geom_text(ggplot2::aes_string(x = "feature", y = "cluster", 
                                           label = "value"), col = "black",
                       size = tile_text_size)+
    ggplot2::theme(legend.position = "top",
                   legend.margin=ggplot2::margin(t=0,r=0,b=2,l=0, unit = "pt"),
                   legend.box.margin=ggplot2::margin(-10,-10,-10,-10))+
    ggplot2::xlab(label = x_axis_name)+
    ggplot2::ylab(label = "Bubble")+
    ggplot2::guides(fill = ggplot2::guide_colourbar(barwidth = 5, barheight=1))
  
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
  if(disable_hclust==FALSE & show_hclust==TRUE) {
    w <- (w/tree$tree)+patchwork::plot_layout(heights = c(0.7, 0.3))
  }
  return(w)
}
