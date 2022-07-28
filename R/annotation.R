


get_cat_tiles <- function(btd,
                          f,
                          integrate_vertical,
                          round_digits = 2,
                          show_hclust = F,
                          disable_hclust = F,
                          tile_text_size = 3,
                          tile_bw = F,
                          x_axis_name = "Feature",
                          rotate_x_axis_labels = T) {


  # check input param
  check_input <- function(btd,
                          f,
                          integrate_vertical,
                          round_digits,
                          show_hclust,
                          disable_hclust,
                          tile_text_size,
                          tile_bw,
                          x_axis_name,
                          rotate_x_axis_labels) {

    # check btd
    if(base::missing(btd)) {
      stop("bubbletree (btd) is missing")
    }
    if(base::any(base::is.na(btd))||base::is.null(btd)||
       base::any(base::is.na(base::class(btd)))||
       base::is.null(base::class(btd))||!base::class(btd)%in%
       c("bubbletree", "dummy_bubbletree")) {
      stop("problem with the input bubbletree")
    }

    if(base::is.vector(btd$cluster)==FALSE||
       base::is.na(base::is.vector(btd$cluster))||
       base::is.null(base::is.vector(btd$cluster))) {
      stop("no clustering results in bubbletree")
    }


    # check fs
    if(base::missing(f)){
      stop("f is missing")
    }
    if(base::is.character(f)==FALSE) {
      stop("f must be a character vector")
    }
    if(base::is.vector(f)==FALSE) {
      stop("f must be a character vector")
    }
    if(base::is.na(f)||base::is.null(f)) {
      stop("f must be a character vector")
    }
    if(base::length(f)!=base::length(btd$cluster)) {
      stop("length of f is not equal to number of cells in btd")
    }


    # check integrate_vertical
    if(base::missing(integrate_vertical)) {
      stop("integrate_vertical is missing")
    }
    if(base::is.logical(integrate_vertical)==FALSE) {
      stop("integrate_vertical must be a logical parameter")
    }
    if(base::length(integrate_vertical)!=1) {
      stop("integrate_vertical must be a logical parameter
           (either TRUE or FALSE)")
    }


    # check round_digits
    if(base::missing(round_digits)==TRUE) {
      stop("round_digits input not found")
    }
    if(base::is.numeric(round_digits)==F) {
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


    # check show_hclust
    if(base::missing(show_hclust)==TRUE) {
      stop("show_hclust input not found")
    }
    if(base::is.logical(show_hclust)==FALSE) {
      stop("show_hclust is a logical parameter (TRUE or FALSE)")
    }
    if(base::length(show_hclust)!=1) {
      stop("show_hclust is a logical parameter (TRUE or FALSE)")
    }
    if(base::is.na(show_hclust)==TRUE) {
      stop("show_hclust is a logical parameter (TRUE or FALSE)")
    }


    # check disable_hclust
    if(base::missing(disable_hclust)==TRUE) {
      stop("disable_hclust input not found")
    }
    if(base::is.logical(disable_hclust)==FALSE) {
      stop("disable_hclust is a logical parameter (TRUE or FALSE)")
    }
    if(base::length(disable_hclust)!=1) {
      stop("disable_hclust is a logical parameter (TRUE or FALSE)")
    }
    if(base::is.na(disable_hclust)==TRUE) {
      stop("disable_hclust is a logical parameter (TRUE or FALSE)")
    }

    if(disable_hclust==TRUE & show_hclust==TRUE) {
      warning("hierarchical feature clustering is disabled (disable_hclust=T),
              show_hclust=T has no effect (set disable_hclust=F to show
              hierarchical dendrogram")
    }


    # check tile_text_size
    if(base::missing(tile_text_size)) {
      stop("tile_text_size is missing")
    }
    if(base::is.numeric(tile_text_size)==FALSE) {
      stop("tile_text_size must be a number >0")
    }
    if(base::length(tile_text_size)!=1) {
      stop("tile_text_size must be a number >0")
    }
    if(base::is.infinite(tile_text_size)) {
      stop("tile_text_size must be a number >0")
    }
    if(tile_text_size<0) {
      stop("tile_text_size must be an number >=0")
    }


    # check tile_bw
    if(base::missing(tile_bw)==TRUE) {
      stop("tile_bw input not found")
    }
    if(base::is.logical(tile_bw)==FALSE) {
      stop("tile_bw is a logical parameter (TRUE or FALSE)")
    }
    if(base::length(tile_bw)!=1) {
      stop("tile_bw is a logical parameter (TRUE or FALSE)")
    }
    if(base::is.na(tile_bw)==TRUE) {
      stop("tile_bw is a logical parameter (TRUE or FALSE)")
    }


    # check x_axis_name
    if(base::missing(x_axis_name)==TRUE) {
      stop("x_axis_name input not found")
    }
    if(base::is.character(x_axis_name)==F) {
      stop("x_axis_name must be a character string")
    }
    if(base::length(x_axis_name)!=1) {
      stop("x_axis_name must be a character string")
    }


    # check rotate_x_axis_labels
    if(base::missing(rotate_x_axis_labels)==TRUE) {
      stop("rotate_x_axis_labels input not found")
    }
    if(base::is.logical(rotate_x_axis_labels)==FALSE) {
      stop("rotate_x_axis_labels is a logical parameter (TRUE or FALSE)")
    }
    if(base::length(rotate_x_axis_labels)!=1) {
      stop("rotate_x_axis_labels is a logical parameter (TRUE or FALSE)")
    }
    if(base::is.na(rotate_x_axis_labels)==TRUE) {
      stop("rotate_x_axis_labels is a logical parameter (TRUE or FALSE)")
    }

  }


  # check inputs
  check_input(btd = btd,
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

  if(integrate_vertical==F) {
    legend <- "Bubble composition [%]"
    ws$percent <- base::round(x = ws$percent_cluster,
                              digits = round_digits)
    ws$norm_percent <- ws$norm_percent_cluster
  }
  if(integrate_vertical==T) {
    legend <- "Feature composition [%]"
    ws$percent <- base::round(x = ws$percent_feature,
                              digits = round_digits)
    ws$norm_percent <- ws$norm_percent_feature
  }

  ws <- base::merge(x = ws,
                    y = btd$tree_meta,
                    by.x = "cluster",
                    by.y = "label",
                    all = T)
  ws <- ws[ base::order(ws$tree_order,
                        decreasing = F),]
  ws$cluster <- base::factor(x = ws$cluster,
                             levels = base::unique(ws$cluster))


  # reorder features based on hclust
  if(disable_hclust==F) {
    if(length(unique(ws$feature))>1) {
      tree <- get_weighted_feature_dist(main_ph = btd$ph$main_ph,
                                        w = ws,
                                        value_var = "prob_feature")
      ws$feature <- as.character(ws$feature)
      ws$feature <- factor(levels = tree$labels, x = ws$feature)
    }
  }


  w <- ggplot(data = ws)+
    theme_bw(base_size = 10)+
    geom_tile(aes(x = feature, y = cluster, fill = percent), col = "white")+
    geom_text(aes(x = feature, y = cluster, label = percent),
              col = "black", size = tile_text_size)+
    theme(legend.position = "top",
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-5,-5,-5,-5))+
    xlab(label = x_axis_name)+
    ylab(label = "Bubble")+
    guides(fill = guide_colourbar(barwidth = 4, barheight = 0.7))


  if(tile_bw==F) {
    w <- w+scale_fill_distiller(name = legend,
                         palette = "Spectral",
                         na.value = 'white')
  } else {
    w <- w+scale_fill_gradient(name = legend,
                               low = "#f9f9f9",
                               high = "#666666",
                               na.value = 'white')
  }

  if(rotate_x_axis_labels==T) {
    w <- w+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  }

  if(disable_hclust==F) {
    if(show_hclust==T) {
      if(length(unique(w$feature))==1) {
        warning("Cannot perform hierarchical clustering:feature has 1 category\n")
      } else {
        w <- (w/tree$tree)+patchwork::plot_layout(heights = c(0.7, 0.3))
      }
    }
  }

  return(list(table = ws,
              plot = w))
}




get_num_tiles <- function(btd,
                          fs,
                          summary_function = "mean",
                          round_digits = 2,
                          show_hclust = F,
                          disable_hclust = F,
                          tile_text_size = 3,
                          tile_bw = F,
                          x_axis_name = "Feature",
                          rotate_x_axis_labels = T) {


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
    if(any(is.na(btd))||is.null(btd)||any(is.na(class(btd)))||
       is.null(class(btd))||!class(btd)%in%
       c("bubbletree", "dummy_bubbletree")) {
      stop("problem with the input bubbletree")
    }

    if(is.vector(btd$cluster)==F||
       is.na(is.vector(btd$cluster))||
       is.null(is.vector(btd$cluster))) {
      stop("no clustering results in bubbletree")
    }

    # check fs
    if(is.numeric(fs)==F) {
      stop("fs must be a numeric vector or matrix")
    }
    if(is.vector(fs)==F&is.matrix(fs)==F) {
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
    if(is.numeric(round_digits)==F) {
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
    if(is.character(summary_function)==F) {
      stop("summary_function is one of: 'mean', 'median', 'sum'")
    }
    if(length(summary_function) != 1) {
      stop("summary_function is one of: 'mean', 'median', 'sum'")
    }
    if(!summary_function %in% c("mean", "median", "sum")) {
      stop("summary_function is one of: 'mean', 'median', 'sum'")
    }


    # check show_hclust
    if(is.logical(show_hclust)==F) {
      stop("show_hclust must be a logical parameter")
    }
    if(length(show_hclust)!=1) {
      stop("show_hclust must be a logical parameter (either TRUE or FALSE)")
    }


    # check disable_hclust
    if(is.logical(disable_hclust)==F) {
      stop("disable_hclust must be a logical parameter")
    }
    if(length(disable_hclust)!=1) {
      stop("disable_hclust must be a logical parameter (either TRUE or FALSE)")
    }

    if(disable_hclust==T & show_hclust==T) {
      warning("hierarchical feature clustering is disabled (disable_hclust=T),
              show_hclust=T has no effect (set disable_hclust=F to show
              hierarchical dendrogram")
    }


    # check tile_text_size
    if(is.numeric(tile_text_size)==F) {
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
    if(is.logical(tile_bw)==F) {
      stop("tile_bw must be a logical parameter")
    }
    if(length(tile_bw)!=1) {
      stop("tile_bw must be a logical parameter (either TRUE or FALSE)")
    }


    # check x_axis_name
    if(is.character(x_axis_name)==F) {
      stop("x_axis_name must be a character string")
    }
    if(length(x_axis_name)!=1) {
      stop("x_axis_name must be a character string")
    }

    # check rotate_x_axis_labels
    if(is.logical(rotate_x_axis_labels)==F) {
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
    f <- data.frame(cluster = k, a = a)

    if(summary_function == "mean") {
      f <- stats::aggregate(a~cluster,
                            data = f,
                            FUN = mean,
                            na.action = na.omit)
      f$value <- round(x = f$a, digits = round_digits)
    }
    if(summary_function == "median") {
      f <- stats::aggregate(a~cluster,
                            data = f,
                            FUN = median,
                            na.action = na.omit)
      f$value <- round(x = f$a, digits = round_digits)
    }
    if(summary_function == "sum") {
      f <- stats::aggregate(a~cluster,
                            data = f,
                            FUN = sum,
                            na.action = na.omit)
      f$value <- round(x = f$a, digits = round_digits)
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
  for(i in 1:ncol(fs)) {
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
                    all = T)
  ws <- ws[order(ws$tree_order, decreasing = F), ]
  ws$cluster <- factor(x = ws$cluster, levels = unique(ws$cluster))
  if(any(is.na(ws$feature))) {
    ws$feature <- ws$feature[is.na(ws$feature)==F][1]
  }
  ws$feature <- as.character(ws$feature)
  ws$feature <- factor(x = ws$feature, levels = base::colnames(fs))



  # reorder features based on hclust
  if(disable_hclust==F) {
    if(ncol(fs)>1) {
      tree <- get_weighted_feature_dist_num(main_ph = btd$ph$main_ph,
                                            w = ws,
                                            value_var = "value")
      ws$feature <- factor(levels = tree$labels, x = ws$feature)
    }
  }


  w <- ggplot(data = ws)+
    theme_bw(base_size = 10)+
    geom_tile(aes(x = feature, y = cluster, fill = value), col = "white")+
    geom_text(aes(x = feature, y = cluster, label = value), col = "black",
              size = tile_text_size)+
    theme(legend.position = "top",
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-10,-10,-10,-10))+
    xlab(label = x_axis_name)+
    ylab(label = "Bubble")+
    guides(fill = guide_colourbar(barwidth = 5, barheight = 1))


  if(tile_bw==F) {
    w <- w+scale_fill_distiller(name = "Feature",
                                palette = "Spectral",
                                na.value = 'white')
  } else {
    w <- w+scale_fill_gradient(name = "Feature",
                               low = "#f9f9f9",
                               high = "#666666",
                               na.value = 'white')
  }


  if(rotate_x_axis_labels==T) {
    w <- w+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  }

  if(disable_hclust==F) {
    if(show_hclust==T) {
      w <- (w/tree$tree)+patchwork::plot_layout(heights = c(0.7, 0.3))
    }
  }

  return(list(table = ws,
              plot = w))
}




get_num_violins <- function(btd,
                            fs,
                            x_axis_name = "Feature distribution",
                            rotate_x_axis_labels = T) {


  # check input param
  check_input <- function(btd,
                          fs,
                          x_axis_name,
                          rotate_x_axis_labels) {

    # check btd
    if(any(is.na(btd))||is.null(btd)||any(is.na(class(btd)))||
       is.null(class(btd))||!class(btd)%in%
       c("bubbletree", "dummy_bubbletree")) {
      stop("problem with the input bubbletree")
    }

    if(is.vector(btd$cluster)==F||
       is.na(is.vector(btd$cluster))||
       is.null(is.vector(btd$cluster))) {
      stop("no clustering results in bubbletree")
    }


    # check fs
    if(is.numeric(fs)==F) {
      stop("fs must be a numeric vector or matrix")
    }
    if(is.vector(fs)==F&is.matrix(fs)==F) {
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


    # check x_axis_name
    if(is.character(x_axis_name)==F) {
      stop("x_axis_name must be a character string")
    }
    if(length(x_axis_name)!=1) {
      stop("x_axis_name must be a character string")
    }


    # check rotate_x_axis_labels
    if(is.logical(rotate_x_axis_labels)==F) {
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


  if(base::is.vector(fs)) {
    fs <- base::matrix(data = fs, ncol = 1)
    if(base::is.null(base::colnames(fs))) {
      base::colnames(fs) <- "f"
    }
  }

  if(base::is.matrix(fs)) {
    if(base::length(base::unique(base::colnames(fs))) < base::ncol(fs)) {
      base::colnames(fs) <- base::paste0("f_", 1:base::ncol(fs))
    }
  }



  # get summary
  # TODO: solve this inefficiency -> data.frame too large for big datasets
  ws <- base::vector(mode = "list", length = base::ncol(fs))
  for(i in 1:base::ncol(fs)) {
    ws[[i]] <- get_raw(k = btd$cluster,
                       a = fs[, i],
                       feature = base::colnames(fs)[i])

  }
  ws <- base::do.call(base::rbind, ws)



  # merge with raw-data + tree
  ws <- base::merge(x = ws,
                    y = btd$tree_meta,
                    by.x = "cluster",
                    by.y = "label",
                    all = TRUE)

  ws <- ws[order(ws$tree_order, decreasing = F), ]
  ws$cluster <- factor(x = ws$cluster, levels = unique(ws$cluster))
  if(any(is.na(ws$feature))) {
    ws$feature <- ws$feature[is.na(ws$feature)==F][1]
  }
  ws$feature <- as.character(ws$feature)
  ws$feature <- factor(x = ws$feature, levels = base::colnames(fs))



  j <- base::which(btd$tree_meta$c < 10)
  if(base::length(j)>0) {
    warning(base::paste0("Bubbles: ", paste0(btd$tree_meta$label[j],
                                             collapse = ','), " have fewer than 10 cells
    -> violins might not be robust"))
  }

  w <- ggplot(data = ws)+
    theme_bw(base_size = 10)+
    facet_grid(.~feature, scales = "free_x")+
    coord_flip()+
    ylab(label = x_axis_name)+
    xlab(label = "Bubble")+
    theme(strip.text.x = element_text(margin = margin(0.01,0,0.01,0, "cm")),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-10,-10,-10,-10))+
    geom_violin(data = ws, aes(x = cluster, y = value), fill = NA)

  return(list(table = ws,
              plot = w))
}


