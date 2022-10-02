get_cat_tiles <- function(
    btd,
    f,
    integrate_vertical,
    round_digits = 2,
    show_hclust = FALSE,
    disable_hclust = FALSE,
    tile_text_size = 3,
    tile_bw = FALSE,
    x_axis_name = "Feature",
    rotate_x_axis_labels = TRUE) {
  
  
  # check inputs
  check_input_cat_tiles(
    btd = btd,
    f = f,
    integrate_vertical = integrate_vertical,
    round_digits = round_digits,
    show_hclust = show_hclust,
    disable_hclust = disable_hclust,
    tile_text_size = tile_text_size,
    tile_bw = tile_bw,
    x_axis_name = x_axis_name,
    rotate_x_axis_labels = rotate_x_axis_labels)
  
  ws <- base::data.frame(table(btd$cluster, f))
  base::colnames(ws) <- c("cluster", "feature", "freq")
  
  # compute stats
  n <- stats::aggregate(freq~cluster, data = ws, FUN = sum)
  n$n_cluster <- n$freq
  n$freq <- NULL
  ws <- base::merge(x = ws, y = n, by = "cluster")
  rm(n)
  
  n <- stats::aggregate(freq~feature, data = ws, FUN = sum)
  n$n_feature <- n$freq
  n$freq <- NULL
  ws <- base::merge(x = ws, y = n, by = "feature")
  rm(n)
  
  ws$prob_cluster <- ws$freq/ws$n_cluster
  ws$prob_feature <- ws$freq/ws$n_feature
  
  ws$percent_cluster <- ws$prob_cluster*100
  ws$percent_feature <- ws$prob_feature*100
  
  ws$norm_percent_feature <- ws$percent_feature/ws$n_cluster
  ws$norm_percent_cluster <- ws$percent_cluster/ws$n_feature
  
  if(integrate_vertical==FALSE) {
    legend <- "Bubble composition [%]"
    ws$percent <- base::round(x = ws$percent_cluster,
                              digits = round_digits)
    ws$norm_percent <- ws$norm_percent_cluster
  }
  if(integrate_vertical==TRUE) {
    legend <- "Feature composition [%]"
    ws$percent <- base::round(x = ws$percent_feature,
                              digits = round_digits)
    ws$norm_percent <- ws$norm_percent_feature
  }
  
  ws <- base::merge(
    x = ws,
    y = btd$tree_meta,
    by.x = "cluster",
    by.y = "label",
    all = TRUE)
  ws <- ws[ base::order(ws$tree_order,
                        decreasing = FALSE),]
  ws$cluster <- base::factor(x = ws$cluster,
                             levels = base::unique(ws$cluster))
  
  # reorder features based on hclust
  if(disable_hclust==FALSE) {
    if(base::length(base::unique(ws$feature))>1) {
      tree <- get_weighted_feature_dist(
        main_ph = btd$ph$main_ph, w = ws, value_var = "prob_feature")
      ws$feature <- base::as.character(ws$feature)
      ws$feature <- base::factor(levels = tree$labels, x = ws$feature)
    }
  }
  
  # draw
  w <- ggplot_cat_tiles(
    ws = ws, 
    tile_text_size = tile_text_size, 
    x_axis_name = x_axis_name, 
    tile_bw = tile_bw, 
    legend = legend,
    rotate_x_axis_labels = rotate_x_axis_labels,
    disable_hclust = disable_hclust, 
    show_hclust = show_hclust,
    tree = tree)
  
  return(base::list(table = ws, plot = w))
}

check_input_cat_tiles <- function(
    btd,
    f,
    integrate_vertical,
    round_digits,
    show_hclust,
    disable_hclust,
    tile_text_size,
    tile_bw,
    x_axis_name,
    rotate_x_axis_labels) {
  
  check_btd(btd = btd)
  check_f(f = f, btd = btd)
  check_integrate_vertical(integrate_vertical = integrate_vertical)
  check_round_digits(round_digits = round_digits)
  check_show_hclust(show_hclust = show_hclust)
  check_disable_hclust(disable_hclust = disable_hclust, 
                       show_hclust = show_hclust)
  check_tile_text_size(tile_text_size = tile_text_size)
  check_tile_bw(tile_bw = tile_bw)
  check_x_axis_name(x_axis_name = x_axis_name)
  check_rotate_x_axis_labels(rotate_x_axis_labels = rotate_x_axis_labels)
}


ggplot_cat_tiles <- function(
    ws, 
    tile_text_size, 
    x_axis_name, 
    tile_bw, 
    legend,
    rotate_x_axis_labels,
    disable_hclust, 
    show_hclust,
    tree) {
  
  w <- ggplot2::ggplot(data = ws)+
    ggplot2::theme_bw(base_size = 10)+
    ggplot2::geom_tile(ggplot2::aes_string(x = "feature", y = "cluster", 
                                           fill = "percent"), col = "white")+
    ggplot2::geom_text(ggplot2::aes_string(x = "feature", y = "cluster", 
                                           label = "percent"), col = "black", 
                       size = tile_text_size)+
    ggplot2::theme(legend.position = "top",
                   legend.margin=ggplot2::margin(0,0,0,0),
                   legend.box.margin=ggplot2::margin(-5,-5,-5,-5))+
    ggplot2::xlab(label = x_axis_name)+
    ggplot2::ylab(label = "Bubble")+
    ggplot2::guides(fill = ggplot2::guide_colourbar(
      barwidth = 4, barheight = 0.7))
  if(tile_bw==FALSE) {
    w <- w+ggplot2::scale_fill_distiller(name = legend,
                                         palette = "Spectral",
                                         na.value = 'white',
                                         limits = c(0, 100))
  } else {
    w <- w+ggplot2::scale_fill_gradient(name = legend,
                                        low = "#f9f9f9",
                                        high = "#848484",
                                        na.value = 'white',
                                        limits = c(0, 100))
  }
  if(rotate_x_axis_labels==TRUE) {
    w <- w+ggplot2::theme(axis.text.x = ggplot2::element_text(
      angle = 90, vjust = 0.5, hjust=1))
  }
  if(disable_hclust==FALSE & show_hclust==TRUE) {
    if(length(base::unique(w$feature))==1) {
      warning("Can't do hierarchical clustering:feature has 1 category\n")
    } else {
      w <- (w/tree$tree)+patchwork::plot_layout(heights = c(0.7, 0.3))
    }
  }
  return(w)
}
