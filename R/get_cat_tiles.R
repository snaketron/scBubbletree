get_cat_tiles <- function(btd,
                          f,
                          integrate_vertical,
                          round_digits = 2,
                          tile_text_size = 3,
                          tile_bw = FALSE,
                          x_axis_name = "Feature",
                          rotate_x_axis_labels = TRUE) {
  
  # check inputs
  check_input_cat_tiles(btd = btd,
                        f = f,
                        integrate_vertical = integrate_vertical,
                        round_digits = round_digits,
                        tile_text_size = tile_text_size,
                        tile_bw = tile_bw,
                        x_axis_name = x_axis_name,
                        rotate_x_axis_labels = rotate_x_axis_labels)
  
  ws <- data.frame(table(btd$cluster, f))
  colnames(ws) <- c("cluster", "feature", "freq")
  
  # compute stats
  n <- aggregate(freq~cluster, data = ws, FUN = sum)
  n$n_cluster <- n$freq
  n$freq <- NULL
  ws <- merge(x = ws, y = n, by = "cluster")
  rm(n)
  
  n <- aggregate(freq~feature, data = ws, FUN = sum)
  n$n_feature <- n$freq
  n$freq <- NULL
  ws <- merge(x = ws, y = n, by = "feature")
  rm(n)
  
  ws$prob_cluster <- ws$freq/ws$n_cluster
  ws$prob_feature <- ws$freq/ws$n_feature
  
  ws$percent_cluster <- ws$prob_cluster*100
  ws$percent_feature <- ws$prob_feature*100
  
  ws$norm_percent_feature <- ws$percent_feature/ws$n_cluster
  ws$norm_percent_cluster <- ws$percent_cluster/ws$n_feature
  
  if(integrate_vertical==FALSE) {
    legend <- "Bubble composition [%]"
    ws$percent <- round(x = ws$percent_cluster, digits = round_digits)
    ws$norm_percent <- ws$norm_percent_cluster
  }
  if(integrate_vertical==TRUE) {
    legend <- "Feature composition [%]"
    ws$percent <- round(x = ws$percent_feature, digits = round_digits)
    ws$norm_percent <- ws$norm_percent_feature
  }
  
  ws <- merge(x = ws, y = btd$tree_meta, by.x = "cluster", 
              by.y = "label", all = TRUE)
  ws <- ws[ order(ws$tree_order, decreasing = FALSE),]
  ws$cluster <- factor(x = ws$cluster, levels = unique(ws$cluster))
  
  # draw
  w <- ggplot_cat_tiles(ws = ws, 
                        tile_text_size = tile_text_size, 
                        x_axis_name = x_axis_name, 
                        tile_bw = tile_bw, 
                        legend = legend,
                        rotate_x_axis_labels = rotate_x_axis_labels)
  
  return(list(table = ws, plot = w))
}

check_input_cat_tiles <- function(btd,
                                  f,
                                  integrate_vertical,
                                  round_digits,
                                  tile_text_size,
                                  tile_bw,
                                  x_axis_name,
                                  rotate_x_axis_labels) {
  
  check_btd(btd = btd)
  check_f(f = f, btd = btd)
  check_integrate_vertical(integrate_vertical = integrate_vertical)
  check_round_digits(round_digits = round_digits)
  check_tile_text_size(tile_text_size = tile_text_size)
  check_tile_bw(tile_bw = tile_bw)
  check_x_axis_name(x_axis_name = x_axis_name)
  check_rotate_x_axis_labels(rotate_x_axis_labels = rotate_x_axis_labels)
}


ggplot_cat_tiles <- function(ws, 
                             tile_text_size, 
                             x_axis_name, 
                             tile_bw, 
                             legend,
                             rotate_x_axis_labels) {
  
  w <- ggplot(data = ws)+
    theme_bw(base_size = 10)+
    geom_tile(aes(x = feature, y = cluster, fill = percent), 
              col = "white")+
    geom_text(aes(x = feature, y = cluster, label = percent), 
              col = "black", size = tile_text_size)+
    theme(legend.position = "top",
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-5,-5,-5,-5))+
    xlab(label = x_axis_name)+
    ylab(label = "Bubble")+
    guides(fill = guide_colourbar(barwidth = 4, barheight = 0.7,
                                  title.position="top", title.hjust = 0.5))
  
  if(tile_bw==FALSE) {
    w <- w+scale_fill_distiller(name = legend, palette = "Spectral", 
                                na.value = 'white')
  } else {
    w <- w+scale_fill_gradient(name = legend,
                               low = "#f9f9f9",
                               high = "#848484",
                               na.value = 'white')
  }
  if(rotate_x_axis_labels==TRUE) {
    w <- w+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  }
  return(w)
}
