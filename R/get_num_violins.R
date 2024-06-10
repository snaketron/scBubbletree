get_num_violins <- function(btd,
                            fs,
                            x_axis_name = "Feature distribution",
                            rotate_x_axis_labels = TRUE) {
  
  # check inputs
  check_input_num_violins(btd = btd,
                          fs = fs,
                          x_axis_name = x_axis_name,
                          rotate_x_axis_labels = rotate_x_axis_labels)
  
  # aux. functions
  get_raw <- function(k, a, feature) {
    f <- data.frame(cluster = k, a = a)
    f$value <- f$a
    f$a <- NULL
    f$feature <- feature
    return(f)
  }
  
  if(is.vector(fs)) {
    fs <- matrix(data = fs, ncol = 1)
    if(is.null(colnames(fs))) {
      colnames(fs) <- "f"
    }
  }
  
  if(is.matrix(fs)) {
    if(length(unique(colnames(fs))) < ncol(fs)) {
      colnames(fs) <- paste0("f_", seq_len(length.out = ncol(fs)))
    }
  }
  
  # get summary
  # TODO: solve this inefficiency -> data.frame too large for big datasets
  ws <- vector(mode = "list", length = ncol(fs))
  for(i in seq_len(length.out = ncol(fs))) {
    ws[[i]] <- get_raw(k = btd$cluster,
                       a = fs[, i],
                       feature = colnames(fs)[i])
  }
  ws <- do.call(rbind, ws)
  
  # merge with raw-data + tree
  ws <- merge(x = ws, y = btd$tree_meta, by.x = "cluster", 
              by.y = "label", all = TRUE)
  
  ws <- ws[order(ws$tree_order, decreasing = FALSE), ]
  ws$cluster <- factor(x = ws$cluster, levels = unique(ws$cluster))
  if(any(is.na(ws$feature))) {
    ws$feature <- ws$feature[is.na(ws$feature)==FALSE][1]
  }
  ws$feature <- as.character(ws$feature)
  ws$feature <- factor(x = ws$feature, levels = colnames(fs))
  
  j <- which(btd$tree_meta$c < 10)
  if(length(j)>0) {
    warning(paste0("Bubbles: ", paste0(btd$tree_meta$label[j], collapse = ','),
                   " have < 10 cells -> violins might not be robust"))
  }
  
  w <- ggplot_num_violins(ws = ws,
                          x_axis_name = x_axis_name,
                          rotate_x_axis_labels = rotate_x_axis_labels)
  
  return(list(table = ws, plot = w))
}


# check input param
check_input_num_violins <- function(btd,
    fs,
    x_axis_name,
    rotate_x_axis_labels) {
  
  check_btd(btd = btd)
  check_fs(fs = fs, btd = btd)
  check_x_axis_name(x_axis_name = x_axis_name)
  check_rotate_x_axis_labels(rotate_x_axis_labels = rotate_x_axis_labels)
}


ggplot_num_violins <- function(ws, 
                               x_axis_name,
                               rotate_x_axis_labels) {
  
  w <- ggplot(data = ws)+
    theme_bw(base_size = 10)+
    facet_grid(.~feature, scales = "free_x")+
    coord_flip()+
    ylab(label = x_axis_name)+
    xlab(label = "Bubble")+
    theme(strip.text.x = element_text(margin = margin(0.01,0,0.01,0, "cm")),
          legend.margin=margin(t = 0,r = 0,b = 2,l = 0),
          legend.box.margin=margin(-10,-10,-10,-10))+
    geom_violin(data = ws, aes(x = cluster, y = value), fill = NA)
  
  if(rotate_x_axis_labels==TRUE) {
    w <- w+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  }
  
  return(w)
}
