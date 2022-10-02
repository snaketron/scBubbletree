get_num_violins <- function(
    btd,
    fs,
    x_axis_name = "Feature distribution",
    rotate_x_axis_labels = TRUE) {
  
  # check inputs
  check_input_num_violins(
    btd = btd,
    fs = fs,
    x_axis_name = x_axis_name,
    rotate_x_axis_labels = rotate_x_axis_labels)
  
  # aux. functions
  get_raw <- function(k, a, feature) {
    f <- base::data.frame(cluster = k, a = a)
    f$value <- f$a
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
  
  if(base::is.matrix(fs)) {
    if(base::length(base::unique(base::colnames(fs))) < base::ncol(fs)) {
      base::colnames(fs) <- base::paste0("f_", base::seq_len(
        length.out = base::ncol(fs)))
    }
  }
  
  # get summary
  # TODO: solve this inefficiency -> data.frame too large for big datasets
  ws <- base::vector(mode = "list", length = base::ncol(fs))
  for(i in base::seq_len(length.out = base::ncol(fs))) {
    ws[[i]] <- get_raw(k = btd$cluster,
                       a = fs[, i],
                       feature = base::colnames(fs)[i])
  }
  ws <- base::do.call(base::rbind, ws)
  
  # merge with raw-data + tree
  ws <- base::merge(
    x = ws,
    y = btd$tree_meta,
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
  
  j <- base::which(btd$tree_meta$c < 10)
  if(base::length(j)>0) {
    warning(base::paste0("Bubbles: ",
                         paste0(btd$tree_meta$label[j], collapse = ','),
                         " have < 10 cells -> violins might not be robust"))
  }
  
  w <- ggplot_num_violins(
    ws = ws,
    x_axis_name = x_axis_name,
    rotate_x_axis_labels = rotate_x_axis_labels)
  
  return(base::list(table = ws, plot = w))
}


# check input param
check_input_num_violins <- function(
    btd,
    fs,
    x_axis_name,
    rotate_x_axis_labels) {
  
  check_btd(btd = btd)
  check_fs(fs = fs, btd = btd)
  check_x_axis_name(x_axis_name = x_axis_name)
  check_rotate_x_axis_labels(rotate_x_axis_labels = rotate_x_axis_labels)
}


ggplot_num_violins <- function(
    ws, 
    x_axis_name,
    rotate_x_axis_labels) {
  
  w <- ggplot2::ggplot(data = ws)+
    ggplot2::theme_bw(base_size = 10)+
    ggplot2::facet_grid(.~feature, scales = "free_x")+
    ggplot2::coord_flip()+
    ggplot2::ylab(label = x_axis_name)+
    ggplot2::xlab(label = "Bubble")+
    ggplot2::theme(strip.text.x = ggplot2::element_text(
      margin = ggplot2::margin(0.01,0,0.01,0, "cm")),
      legend.margin=ggplot2::margin(t = 0,r = 0,b = 2,l = 0),
      legend.box.margin=ggplot2::margin(-10,-10,-10,-10))+
    ggplot2::geom_violin(data = ws, ggplot2::aes_string(
      x = "cluster", y = "value"), fill = NA)
  
  if(rotate_x_axis_labels==TRUE) {
    w <- w+ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                              vjust = 0.5,
                                                              hjust=1))
  }
  
  return(w)
}
