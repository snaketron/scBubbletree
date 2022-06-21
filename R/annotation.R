


#'
#' @exportMethod
#'
get_cat_feature_tiles <- function(d,
                                  a,
                                  integrate_vertical = F,
                                  round_digits = 2,
                                  rotate_x_axis = T,
                                  show_hclust = F,
                                  disable_hclust = F,
                                  tile_text_size = 3,
                                  x_axis_label = "Feature") {


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
    xlab(label = x_axis_label)+
    ylab(label = "Bubble")+
    guides(fill = guide_colourbar(barwidth = 4, barheight = 0.7))


  if(rotate_x_axis==T) {
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
get_num_feature_tiles <- function(d,
                                  as,
                                  plot_title = '',
                                  round_digits = 2,
                                  rotate_x_axis = T,
                                  show_hclust = F,
                                  disable_hclust = F,
                                  tile_text_size = 3,
                                  x_axis_label = "Feature") {

  if(disable_hclust==T & show_hclust==T) {
    warning("Hclust disabled, set disable_hclust=T to create dendrogram")
  }


  get_mu <- function(k, a, feature, round_digits) {
    f <- data.frame(cluster = k, a = a)
    f <- stats::aggregate(a~cluster,
                          data = f,
                          FUN = mean,
                          na.action = na.omit)
    f$value <- round(x = f$a, digits = round_digits)
    f$a <- NULL
    f$feature <- feature
    return(f)
  }


  if(is.vector(as)) {
    as <- matrix(data = as, ncol = 1)
    base::colnames(as) <- "a"
  }

  if(is.null(base::colnames(as))) {
    base::colnames(as) <- paste0("a_", 1:ncol(as))
  }

  ws <- vector(mode = "list", length = ncol(as))
  for(i in 1:ncol(as)) {
    ws[[i]] <- get_mu(k = d$cluster,
                     a = as[, i],
                     feature = base::colnames(as)[i],
                     round_digits = round_digits)
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
  ws$feature <- factor(x = ws$feature, levels = base::colnames(as))



  # reorder features based on hclust
  if(disable_hclust==F) {
    if(ncol(as)>1) {
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
    xlab(label = x_axis_label)+
    ylab(label = "Bubble")+
    ggtitle(label = plot_title)+
    guides(fill = guide_colourbar(barwidth = 5, barheight = 0.7))

  if(rotate_x_axis==T) {
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
get_num_feature_violins <- function(d,
                                    as,
                                    plot_title = '',
                                    scales = "free_x",
                                    violin_min_cells = 10,
                                    show_cells = T,
                                    disable_hclust = F,
                                    x_axis_label = "Feature distribution") {

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


  if(is.vector(as)) {
    as <- matrix(data = as, ncol = 1)
    base::colnames(as) <- "a"
  }

  if(is.null(base::colnames(as))) {
    base::colnames(as) <- paste0("a_", 1:ncol(as))
  }



  # get summary
  ws <- vector(mode = "list", length = ncol(as))
  mu_ws <- vector(mode = "list", length = ncol(as))
  for(i in 1:ncol(as)) {

    ws[[i]] <- get_raw(k = d$cluster,
                       a = as[, i],
                       feature = base::colnames(as)[i])

    mu_ws[[i]] <- get_mu(k = d$cluster,
                         a = as[, i],
                         feature = base::colnames(as)[i])

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
  mu_ws$feature <- factor(x = mu_ws$feature, levels = base::colnames(as))



  # merge with raw-data + tree
  ws <- base::merge(x = ws, y = d$tree_meta,
                    by.x = "cluster", by.y = "label", all = T)
  ws <- ws[order(ws$tree_order, decreasing = F), ]
  ws$cluster <- factor(x = ws$cluster, levels = unique(ws$cluster))
  if(any(is.na(ws$feature))) {
    ws$feature <- ws$feature[is.na(ws$feature)==F][1]
  }
  ws$feature <- as.character(ws$feature)
  ws$feature <- factor(x = ws$feature, levels = base::colnames(as))




  # reorder features based on hclust
  if(disable_hclust==F) {
    if(ncol(as)>1) {
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
    facet_grid(.~feature, scales = scales)+
    coord_flip()+
    ylab(label = x_axis_label)+
    xlab(label = "Bubble")+
    ggtitle(label = plot_title)+
    theme(strip.text.x = element_text(margin = margin(0.01,0,0.01,0, "cm")),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-10,-10,-10,-10))


  # remove small violins n<violin_min_cells
  ws_stat <- data.frame(table(ws$cluster, ws$feature))
  colnames(ws_stat) <- c("cluster", "feature", "freq")
  ws_bad <- which(ws_stat$freq < violin_min_cells)
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

  if(show_cells==T) {
    w<-w+geom_jitter(data = ws_new,
                     aes(x = cluster, y = value),
                     width = 0.1, height = 0,
                     col = "darkgray", size = 0.25)
  }

  return(list(ws = ws, w = w))
}


