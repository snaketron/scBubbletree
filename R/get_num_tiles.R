get_num_tiles <- function(btd,
                          fs,
                          summary_function,
                          round_digits = 2,
                          tile_text_size = 3,
                          tile_bw = FALSE,
                          x_axis_name = "Feature",
                          rotate_x_axis_labels = TRUE) {
  
  # check inputs
  check_input_num_tiles(btd = btd,
                        fs = fs,
                        summary_function = summary_function,
                        round_digits = round_digits,
                        tile_text_size = tile_text_size,
                        tile_bw = tile_bw,
                        x_axis_name = x_axis_name,
                        rotate_x_axis_labels = rotate_x_axis_labels)
  
  if(is.vector(fs)) {
    fs <- matrix(data = fs, ncol = 1)
    if(is.null(colnames(fs))) {
      colnames(fs) <- "f"
    }
  }
  
  ws <- vector(mode = "list", length = ncol(fs))
  for(i in seq_len(length.out = ncol(fs))) {
    ws[[i]] <- get_summary_num_tiles(k = btd$cluster,
                                     a = fs[, i],
                                     feature = colnames(fs)[i],
                                     round_digits = round_digits,
                                     summary_function = summary_function)
  }
  ws <- do.call(rbind, ws)
  
  ws <- merge(x = ws, y = btd$tree_meta, by.x = "cluster", 
              by.y = "label", all = TRUE)
  ws <- ws[order(ws$tree_order, decreasing = FALSE), ]
  ws$cluster <- factor(x = ws$cluster, levels = unique(ws$cluster))
  if(any(is.na(ws$feature))) {
    ws$feature <- ws$feature[is.na(ws$feature)==FALSE][1]
  }
  ws$feature <- as.character(ws$feature)
  ws$feature <- factor(x = ws$feature, levels = colnames(fs))
  
  w <- ggplot_num_tiles(ws = ws,
                        tile_text_size = tile_text_size, 
                        x_axis_name = x_axis_name, 
                        tile_bw = tile_bw, 
                        rotate_x_axis_labels = rotate_x_axis_labels)
  
  return(list(table = ws, plot = w))
}


check_input_num_tiles <- function(btd,
                                  fs,
                                  summary_function,
                                  round_digits,
                                  tile_text_size,
                                  tile_bw,
                                  x_axis_name,
                                  rotate_x_axis_labels) {
  
  check_btd(btd = btd)
  check_fs(fs = fs, btd = btd)
  check_summary_function(summary_function = summary_function)
  check_round_digits(round_digits = round_digits)
  check_tile_text_size(tile_text_size = tile_text_size)
  check_tile_bw(tile_bw = tile_bw)
  check_x_axis_name(x_axis_name = x_axis_name)
  check_rotate_x_axis_labels(rotate_x_axis_labels = rotate_x_axis_labels)
}

get_summary_num_tiles <- function(k, 
                                  a,
                                  feature,
                                  round_digits,
                                  summary_function) {
  
  f <- data.frame(cluster = k, a = a)
  
  if(summary_function == "mean") {
    f <- aggregate(a~cluster, data = f, FUN = mean, na.action = na.omit)
    f$value <- round(x = f$a, digits = round_digits)
  }
  if(summary_function == "median") {
    f <- aggregate(a~cluster, data = f, FUN = median, na.action = na.omit)
    f$value <- round(x = f$a, digits = round_digits)
  }
  if(summary_function == "sum") {
    f <- aggregate(a~cluster, data = f, FUN = sum, na.action = na.omit)
    f$value <- round(x = f$a, digits = round_digits)
  }
  if(summary_function == "pct nonzero") {
    get_nonzero <- function(x) {
      return(sum(x>0)/length(x))
    }
    f <- aggregate(a~cluster, data = f, FUN = get_nonzero, na.action = na.omit)
    f$value <- round(x = f$a, digits = round_digits)
  }
  if(summary_function == "pct zero") {
    get_zero <- function(x) {
      return(sum(x==0)/length(x))
    }
    f <- aggregate(a~cluster, data = f, FUN = get_zero, na.action = na.omit)
    f$value <- round(x = f$a, digits = round_digits)
  }
  
  f$a <- NULL
  f$feature <- feature
  return(f)
}

ggplot_num_tiles <- function(ws,
                             tile_text_size, 
                             x_axis_name, 
                             tile_bw, 
                             rotate_x_axis_labels) {
  
  w <- ggplot(data = ws)+
    theme_bw(base_size = 10)+
    geom_tile(aes(x = feature, y = cluster, fill = value), col = "white")+
    geom_text(aes(x = feature, y = cluster, label = value), col = "black",
              size = tile_text_size)+
    theme(legend.position = "top",
          legend.margin=margin(t=0,r=0,b=2,l=0, unit = "pt"),
          legend.box.margin=margin(-10,-10,-10,-10))+
    xlab(label = x_axis_name)+
    ylab(label = "Bubble")+
    guides(fill = guide_colourbar(barwidth = 4, barheight = 0.7))
  
  if(tile_bw==FALSE) {
    w <- w+scale_fill_distiller(name = "Feature",
                                palette = "Spectral",
                                na.value = 'white',
                                breaks = pretty_breaks(n = 3))
  } else {
    w <- w+scale_fill_gradient(name = "Feature",
                               low = "#f9f9f9",
                               high = "#848484",
                               na.value = 'white',
                               breaks = pretty_breaks(n = 3))
  }
  if(rotate_x_axis_labels==TRUE) {
    w <- w+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  }
  return(w)
}
