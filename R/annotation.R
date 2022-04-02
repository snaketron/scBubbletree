

#'
#' @exportMethod
#'
get_annotation_tiles_char <- function(k,
                                      a,
                                      tree_meta,
                                      integrate_over_clusters = T,
                                      round_digits = 2) {

  get_a <- function(k, a, annotation) {
    f <- data.frame(label = k, a = a)
    f <- stats::aggregate(a~label,
                          data = f,
                          FUN = mean)
    f$value <- round(x = f$a, digits = round_digits)
    f$a <- NULL
    f$annotation <- annotation
    return(f)
  }

  ws <- data.frame(table(k, a))
  base::colnames(ws) <- c("label", "annotation", "freq")
  if(integrate_over_clusters==T) {
    n <- stats::aggregate(freq~label,
                          data = ws,
                          FUN = sum)
    n$n <- n$freq
    n$freq <- NULL
    ws <- base::merge(x = ws, y = n, by = "label")
    rm(n)
    legend <- "% of cluster"
  }
  if(integrate_over_clusters==F) {
    n <- stats::aggregate(freq~annotation,
                          data = ws,
                          FUN = sum)
    n$n <- n$freq
    n$freq <- NULL
    ws <- base::merge(x = ws, y = n, by = "annotation")
    rm(n)
    legend <- "% of annotation"
  }
  ws$p <- ws$freq/ws$n
  ws$percent <- round(x = ws$p*100, digits = round_digits)

  ws <- base::merge(x = ws, y = tree_meta, by = "label", all = T)
  ws <- ws[order(ws$tree_order, decreasing = F), ]
  ws$label <- factor(x = ws$label, levels = unique(ws$label))

  w <- ggplot(data = ws)+
    theme_bw()+
    geom_tile(aes(x = annotation, y = label, fill = percent),
              col = "white")+
    geom_text(aes(x = annotation, y = label, label = percent),
              col = "black", size = 3)+
    scale_fill_distiller(name = legend,
                         palette = "Spectral",
                         limits = c(0, 100),
                         na.value = 'white')+
    theme(legend.position = "top")+
    xlab(label = "Annotation")+
    ylab(label = "Cluster")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    guides(fill = guide_colourbar(barwidth = 4, barheight = 1))

  return(list(ws = ws, w = w))
}


#'
#' @exportMethod
#'
get_annotation_tiles_num <- function(k,
                                     as,
                                     tree_meta,
                                     plot_title = '',
                                     round_digits = 2) {

  get_a <- function(k, a, annotation) {
    f <- data.frame(label = k, a = a)
    f <- stats::aggregate(a~label,
                          data = f,
                          FUN = mean,
                          na.action = na.omit)
    f$value <- round(x = f$a, digits = round_digits)
    f$a <- NULL
    f$annotation <- annotation
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
    ws[[i]] <- get_a(k = k,
                     a = as[, i],
                     annotation = base::colnames(as)[i])
  }
  ws <- do.call(rbind, ws)

  ws <- base::merge(x = ws, y = tree_meta, by = "label", all = T)
  ws <- ws[order(ws$tree_order, decreasing = F), ]
  ws$label <- factor(x = ws$label, levels = unique(ws$label))
  if(any(is.na(ws$annotation))) {
    ws$annotation <- ws$annotation[is.na(ws$annotation)==F][1]
  }
  ws$annotation <- factor(x = ws$annotation,
                          levels = base::colnames(as))

  w <- ggplot(data = ws)+
    theme_bw()+
    geom_tile(aes(x = annotation, y = label, fill = value), col = "white")+
    geom_text(aes(x = annotation, y = label, label = value), col = "black", size = 3)+
    scale_fill_distiller(name = "Avg.", palette = "Spectral", na.value = 'white')+
    theme(legend.position = "top")+
    xlab(label = "Annotation")+
    ylab(label = "Cluster")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    ggtitle(label = plot_title)+
    guides(fill = guide_colourbar(barwidth = 4, barheight = 1))

  return(list(ws = ws, w = w))
}


#'
#' @exportMethod
#'
get_annotation_violins <- function(k,
                                   as,
                                   tree_meta,
                                   plot_title = '',
                                   scales = "free_x",
                                   violin_min_cells = 10) {

  get_a <- function(k, a, annotation) {
    f <- data.frame(label = k, a = a)
    f$value <- f$a
    f$a <- NULL
    f$annotation <- annotation
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
    ws[[i]] <- get_a(k = k, a = as[, i],
                     annotation = base::colnames(as)[i])
  }
  ws <- do.call(rbind, ws)


  ws <- base::merge(x = ws, y = tree_meta, by = "label", all = T)
  ws <- ws[order(ws$tree_order, decreasing = F), ]
  ws$label <- factor(x = ws$label, levels = unique(ws$label))
  if(any(is.na(ws$annotation))) {
    ws$annotation <- ws$annotation[is.na(ws$annotation)==F][1]
  }
  ws$annotation <- factor(x = ws$annotation,
                          levels = base::colnames(as))


  w <- ggplot(data = ws)+
    theme_bw()+
    facet_grid(.~annotation, scales = scales)+
    geom_jitter(aes(x = label, y = value),
                width = 0.1, height = 0, col = "darkgray", size = 0.25)+
    coord_flip()+
    ylab(label = "Distribution")+
    xlab(label = "Cluster")+
    ggtitle(label = plot_title)+
    theme(strip.text.x = element_text(margin = margin(0.01,0,0.01,0, "cm")))

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


