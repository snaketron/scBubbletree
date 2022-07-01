


#'
#' @exportMethod
#'
get_cat_feature_tiles <- function(d,
                                  a,
                                  integrate_vertical = F,
                                  round_digits = 2,
                                  rotate_x_axis_labels = T,
                                  show_hclust = F,
                                  disable_hclust = F,
                                  tile_text_size = 3,
                                  x_axis_name = "Feature") {


  if(disable_hclust==T & show_hclust==T) {
    warning("Hclust disabled, set disable_hclust=T to create dendrogram")
  }

  get_a <- function(k, a, feature) {
    f <- data.frame(label = k, a = a)
    f <- stats::aggregate(a~cluster, data = f, FUN = base::mean)
    f$value <- round(x = f$a, digits = round_digits)
    f$a <- NULL
    f$feature <- feature
    return(f)
  }

  # check if d is a btd object

  ws <- data.frame(table(d$cluster, a))
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
    legend <- "Bubble composition\n over features [%]"
    ws$percent <- round(x = ws$percent_cluster, digits = round_digits)
    ws$norm_percent <- ws$norm_percent_cluster
  }
  if(integrate_vertical==T) {
    legend <- "Feature composition\n across bubbles [%]"
    ws$percent <- round(x = ws$percent_feature, digits = round_digits)
    ws$norm_percent <- ws$norm_percent_feature
  }

  ws <- base::merge(x = ws,
                    y = d$tree_meta,
                    by.x = "cluster",
                    by.y = "label",
                    all = T)
  ws <- ws[order(ws$tree_order, decreasing = F), ]
  ws$cluster <- factor(x = ws$cluster, levels = unique(ws$cluster))


  # reorder features based on hclust
  if(disable_hclust==F) {
    if(length(unique(ws$feature))>1) {
      tree <- get_weighted_feature_dist(main_ph = d$ph$main_ph,
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
    scale_fill_distiller(name = legend,
                         palette = "Spectral",
                         # limits = c(0, 100),
                         na.value = 'white')+
    theme(legend.position = "top",
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-5,-5,-5,-5))+
    xlab(label = x_axis_name)+
    ylab(label = "Bubble")+
    guides(fill = guide_colourbar(barwidth = 4, barheight = 0.7))


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

  return(list(ws = ws, w = w))
}


#'
#' @exportMethod
#'
get_num_feature_tiles <- function(btd,
                                  fs,
                                  summary_function = "mean",
                                  round_digits = 2,
                                  show_hclust = F,
                                  disable_hclust = F,
                                  tile_text_size = 3,
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
                          x_axis_name,
                          rotate_x_axis_labels) {

    # check btd
    if(is.na(btd)||is.null(btd)||is.na(class(btd))||
       is.null(class(btd))||class(btd)!="bubbletree") {
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
    if(length(fs)!=length(btd$cluster)) {
      stop("length(fs)!=length(btd$cluster)")
    }
    if(any(is.infinite(fs))) {
      warning("some feature values were not finite, they will be omitted.")
    }


    # check round_digits
    if(is.numeric(round_digits)==F) {
      stop("round_digits must be an integer >=0")
    }
    if(is.integer(round_digits)==F) {
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


    # check x_axis_name
    if(is.character(x_axis_name)==F) {
      stop("x_axis_name must be a character string")
    }
    if(length(tile_text_size)!=1) {
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


  if(is.vector(fs)) {
    fs <- matrix(data = fs, ncol = 1)
    base::colnames(fs) <- "f"
  }

  if(is.null(base::colnames(fs))) {
    base::colnames(fs) <- paste0("f_", 1:ncol(fs))
  }

  ws <- vector(mode = "list", length = ncol(fs))
  for(i in 1:ncol(fs)) {
    ws[[i]] <- get_summary(k = d$cluster,
                           a = fs[, i],
                           feature = base::colnames(fs)[i],
                           round_digits = round_digits,
                           summary_function = summary_function)
  }
  ws <- do.call(rbind, ws)


  ws <- base::merge(x = ws, y = d$tree_meta,
                    by.x = "cluster", by.y = "label", all = T)
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
      tree <- get_weighted_feature_dist_num(main_ph = d$ph$main_ph,
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
    scale_fill_distiller(name = "Avg.", palette = "Spectral", na.value = 'white')+
    theme(legend.position = "top",
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-10,-10,-10,-10))+
    xlab(label = x_axis_name)+
    ylab(label = "Bubble")+
    guides(fill = guide_colourbar(barwidth = 5, barheight = 0.7))

  if(rotate_x_axis_labels==T) {
    w <- w+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  }

  if(disable_hclust==F) {
    if(show_hclust==T) {
      w <- (w/tree$tree)+patchwork::plot_layout(heights = c(0.7, 0.3))
    }
  }

  return(list(ws = ws, w = w))
}



#'
#' @exportMethod
#'
get_num_feature_violins <- function(btd,
                                    fs,
                                    show_cells = T,
                                    disable_hclust = F,
                                    x_axis_name = "Feature distribution",
                                    rotate_x_axis_labels = T,
                                    min_cells_for_violin = 10) {



  # check input param
  check_input <- function(btd,
                          fs,
                          show_cells,
                          disable_hclust,
                          x_axis_name,
                          rotate_x_axis_labels,
                          min_cells_for_violin) {

    # check btd
    if(is.na(btd)||is.null(btd)||is.na(class(btd))||
       is.null(class(btd))||class(btd)!="bubbletree") {
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
    if(length(fs)!=length(btd$cluster)) {
      stop("length(fs)!=length(btd$cluster)")
    }
    if(any(is.infinite(fs))) {
      warning("some feature values were not finite, they will be omitted.")
    }


    # check show_cells
    if(is.logical(show_cells)==F) {
      stop("show_cells must be a logical parameter")
    }
    if(length(show_cells)!=1) {
      stop("show_cells must be a logical parameter (either TRUE or FALSE)")
    }


    # check disable_hclust
    if(is.logical(disable_hclust)==F) {
      stop("disable_hclust must be a logical parameter")
    }
    if(length(disable_hclust)!=1) {
      stop("disable_hclust must be a logical parameter (either TRUE or FALSE)")
    }


    # check x_axis_name
    if(is.character(x_axis_name)==F) {
      stop("x_axis_name must be a character string")
    }
    if(length(tile_text_size)!=1) {
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


    # check min_cells_for_violin
    if(is.numeric(min_cells_for_violin)==F) {
      stop("min_cells_for_violin must be an integer >0")
    }
    if(is.integer(min_cells_for_violin)==F) {
      stop("min_cells_for_violin must be an integer >0")
    }
    if(length(min_cells_for_violin)!=1) {
      stop("min_cells_for_violin must be an integer >0")
    }
    if(is.infinite(min_cells_for_violin)) {
      stop("min_cells_for_violin must be an integer >0")
    }
    if(min_cells_for_violin<0) {
      stop("min_cells_for_violin must be an integer >0")
    }
  }


  # check inputs
  check_input(btd = btd,
              fs = fs,
              show_cells = show_cells,
              disable_hclust = disable_hclust,
              x_axis_name = x_axis_name,
              rotate_x_axis_labels = rotate_x_axis_labels,
              min_cells_for_violin = min_cells_for_violin)



  get_raw <- function(k, a, feature) {
    f <- data.frame(cluster = k, a = a)
    f$value <- f$a
    f$a <- NULL
    f$feature <- feature
    return(f)
  }


  get_mu <- function(k, a, feature) {
    f <- data.frame(cluster = k, a = a)
    f <- stats::aggregate(a~cluster,
                          data = f,
                          FUN = mean,
                          na.action = na.omit)
    f$value <- f$a
    f$a <- NULL
    f$feature <- feature
    return(f)
  }


  if(is.vector(fs)) {
    fs <- matrix(data = fs, ncol = 1)
    base::colnames(fs) <- "a"
  }

  if(is.null(base::colnames(fs))) {
    base::colnames(fs) <- paste0("a_", 1:ncol(fs))
  }



  # get summary
  ws <- vector(mode = "list", length = ncol(fs))
  mu_ws <- vector(mode = "list", length = ncol(fs))
  for(i in 1:ncol(fs)) {

    ws[[i]] <- get_raw(k = d$cluster,
                       a = fs[, i],
                       feature = base::colnames(fs)[i])

    mu_ws[[i]] <- get_mu(k = d$cluster,
                         a = fs[, i],
                         feature = base::colnames(fs)[i])

  }
  ws <- do.call(rbind, ws)
  mu_ws <- do.call(rbind, mu_ws)


  # merge with mu-data + tree
  mu_ws <- base::merge(x = mu_ws, y = d$tree_meta,
                    by.x = "cluster", by.y = "label", all = T)
  mu_ws <- mu_ws[order(mu_ws$tree_order, decreasing = F), ]
  mu_ws$cluster <- factor(x = mu_ws$cluster, levels = unique(mu_ws$cluster))
  if(any(is.na(mu_ws$feature))) {
    mu_ws$feature <- mu_ws$feature[is.na(mu_ws$feature)==F][1]
  }
  mu_ws$feature <- as.character(mu_ws$feature)
  mu_ws$feature <- factor(x = mu_ws$feature, levels = base::colnames(fs))



  # merge with raw-data + tree
  ws <- base::merge(x = ws, y = d$tree_meta,
                    by.x = "cluster", by.y = "label", all = T)
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
      tree <- get_weighted_feature_dist_num(main_ph = d$ph$main_ph,
                                            w = mu_ws,
                                            value_var = "value")
      # now ws not mu_ws
      ws$feature <- as.character(ws$feature)
      ws$feature <- factor(levels = tree$labels, x = ws$feature)
    }
  }

  w <- ggplot(data = ws)+
    theme_bw(base_size = 10)+
    facet_grid(.~feature, scales = "free_x")+
    coord_flip()+
    ylab(label = x_axis_name)+
    xlab(label = "Bubble")+
    theme(strip.text.x = element_text(margin = margin(0.01,0,0.01,0, "cm")),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-10,-10,-10,-10))


  if(show_cells==F) {
    w <- w+geom_violin(data = ws, aes(x = cluster, y = value), fill = NA)
  } else {
    # remove small violins n<min_cells_for_violin
    ws_stat <- data.frame(table(ws$cluster, ws$feature))
    colnames(ws_stat) <- c("cluster", "feature", "freq")
    ws_bad <- which(ws_stat$freq < min_cells_for_violin)
    if(length(ws_bad)>0) {
      ws_stat <- ws_stat[ws_bad, ]
      ws_new <- ws
      for(i in 1:nrow(ws_stat)) {
        ws_new <- ws_new[-which(ws_new$cluster == ws_stat$cluster&
                                  ws_new$feature == ws_stat$feature),]
      }
    } else {
      ws_new <- ws
    }

    w <- w + geom_violin(data = ws_new,
                         aes(x = cluster,
                             y = value),
                         fill = NA)

    w<-w+geom_jitter(data = ws_new,
                     aes(x = cluster, y = value),
                     width = 0.1, height = 0,
                     col = "darkgray", size = 0.25)
  }

  return(list(ws = ws, w = w))
}


