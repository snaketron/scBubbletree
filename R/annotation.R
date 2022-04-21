


#'
#' @exportMethod
#'
get_cat_feature_tiles <- function(d,
                                  a,
                                  feature_composition = F,
                                  round_digits = 2,
                                  rotate_x_axis = T,
                                  show_hclust = F) {

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

  if(feature_composition==F) {
    legend <- "Bubble composition\n over features [%]"
    ws$percent <- round(x = ws$percent_cluster, digits = round_digits)
    ws$norm_percent <- ws$norm_percent_cluster
  }
  if(feature_composition==T) {
    legend <- "Feature composition\n across bubbles [%]"
    ws$percent <- round(x = ws$percent_feature, digits = round_digits)
    ws$norm_percent <- ws$norm_percent_feature
  }

  # browser()
  ws <- base::merge(x = ws,
                    y = d$tree_meta,
                    by.x = "cluster",
                    by.y = "label",
                    all = T)
  ws <- ws[order(ws$tree_order, decreasing = F), ]
  ws$cluster <- factor(x = ws$cluster, levels = unique(ws$cluster))


  # reorder features based on hclust
  if(length(unique(ws$feature))>1) {
    tree <- get_weighted_feature_dist(main_ph = d$ph$main_ph,
                                      w = ws,
                                      value_var = "prob_feature")
    ws$feature <- as.character(ws$feature)
    ws$feature <- factor(levels = tree$labels, x = ws$feature)
  }


  w <- ggplot(data = ws)+
    theme_bw(base_size = 10)+
    geom_tile(aes(x = feature, y = cluster, fill = percent), col = "white")+
    geom_text(aes(x = feature, y = cluster, label = percent),
              col = "black", size = 3)+
    scale_fill_distiller(name = legend,
                         palette = "Spectral",
                         # limits = c(0, 100),
                         na.value = 'white')+
    theme(legend.position = "top",
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-10,-10,-10,-10))+
    xlab(label = "Feature")+
    ylab(label = "Bubble")+
    guides(fill = guide_colourbar(barwidth = 4, barheight = 0.7))


  if(rotate_x_axis==T) {
    w <- w+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  }

  if(show_hclust==T) {
    if(length(unique(w$feature))==1) {
      warning("Cannot perform hierarchical clustering:feature has 1 category\n")
    } else {
      w <- (w/tree$tree)+patchwork::plot_layout(heights = c(0.7, 0.3))
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
                                  show_hclust = F) {

  get_a <- function(k, a, feature, round_digits) {
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
    ws[[i]] <- get_a(k = d$cluster,
                     a = as[, i],
                     feature = base::colnames(as)[i],
                     round_digits = round_digits)
  }
  ws <- do.call(rbind, ws)

  ws <- base::merge(x = ws,
                    y = d$tree_meta,
                    by.x = "cluster",
                    by.y = "label",
                    all = T)
  ws <- ws[order(ws$tree_order, decreasing = F), ]
  ws$cluster <- factor(x = ws$cluster, levels = unique(ws$cluster))
  if(any(is.na(ws$feature))) {
    ws$feature <- ws$feature[is.na(ws$feature)==F][1]
  }
  ws$feature <- factor(x = ws$feature,
                       levels = base::colnames(as))



  # reorder features based on hclust
  if(ncol(as)>1) {
    tree <- get_weighted_feature_dist(main_ph = d$ph$main_ph,
                                      w = ws,
                                      value_var = "value")
    ws$feature <- as.character(ws$feature)
    ws$feature <- factor(levels = tree$labels, x = ws$feature)
  }


  w <- ggplot(data = ws)+
    theme_bw(base_size = 10)+
    geom_tile(aes(x = feature, y = cluster, fill = value), col = "white")+
    geom_text(aes(x = feature, y = cluster, label = value), col = "black", size = 3)+
    scale_fill_distiller(name = "Avg.", palette = "Spectral", na.value = 'white')+
    theme(legend.position = "top",
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-10,-10,-10,-10))+
    xlab(label = "Feature")+
    ylab(label = "Bubble")+
    ggtitle(label = plot_title)+
    guides(fill = guide_colourbar(barwidth = 4, barheight = 0.7))

  if(rotate_x_axis==T) {
    w <- w+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  }

  if(show_hclust==T) {
    w <- (w/tree$tree)+patchwork::plot_layout(heights = c(0.7, 0.3))
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
                                    violin_min_cells = 10) {

  get_a <- function(k, a, feature) {
    f <- data.frame(label = k, a = a)
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

  ws <- vector(mode = "list", length = ncol(as))
  for(i in 1:ncol(as)) {
    ws[[i]] <- get_a(k = d$cluster,
                     a = as[, i],
                     feature = base::colnames(as)[i])
  }
  ws <- do.call(rbind, ws)


  ws <- base::merge(x = ws, y = d$tree_meta, by = "label", all = T)
  ws <- ws[order(ws$tree_order, decreasing = F), ]
  ws$label <- factor(x = ws$label, levels = unique(ws$label))
  if(any(is.na(ws$feature))) {
    ws$feature <- ws$feature[is.na(ws$feature)==F][1]
  }
  ws$feature <- factor(x = ws$feature,
                       levels = base::colnames(as))


  w <- ggplot(data = ws)+
    theme_bw(base_size = 10)+
    facet_grid(.~feature, scales = scales)+
    geom_jitter(aes(x = label, y = value),
                width = 0.1, height = 0, col = "darkgray", size = 0.25)+
    coord_flip()+
    ylab(label = "Distribution")+
    xlab(label = "Bubble")+
    ggtitle(label = plot_title)+
    theme(strip.text.x = element_text(margin = margin(0.01,0,0.01,0, "cm")),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-10,-10,-10,-10))

  ws_ok <- ws[is.finite(ws$value), ]
  ws_j <- which(table(ws_ok$label) >= violin_min_cells)
  if(length(ws_j) != 0) {
    ws_ok <- ws_ok[ws_ok$label %in% base::names(ws_j), ]
    w <- w + geom_violin(data = ws_ok,
                         aes(x = label,
                             y = value),
                         fill = NA)
  }

  return(list(ws = ws, w = w))
}


