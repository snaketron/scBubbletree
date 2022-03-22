#' For a features matrix \code{x} with cells as rows and features
#' (such as PC embeddings) as columns and cell clusters vector \code{c},
#' this function computes the average Euclidean distance between the pairs
#' of clusters and highest density interval of the distribution of pairwise
#' distances estimated between a pair of clusters.
#'
#' \code{eff_cells} number of cells to use from each cluster (if
#' eff_cells=NA all cells are used). Improves efficiency.
#'
#' @param x matrix.
#' @param c vector.
#' @param hdi_level number.
#' @param eff_cells number
#' @return data frame (default = 0.95).
#' @examples
#' get_pair_dist(
#' x = matrix(data = rnorm(n = 100), ncol = 10),
#' c = rep(x = c("a", "b"), each = 5),
#' hdi_level = 0.95)
get_pair_dist <- function(x, c, eff_cells = NA, hdi_level = 0.95) {

  get_euc <- function(x, y) {
    for(i in 1:ncol(y)) {
      y[,i] <- (x[i]-y[,i])^2
    }
    y <- apply(X = y, MARGIN = 1, FUN = sum)
    y <- sqrt(y)
    return(y)
  }

  cs <- unique(c)
  stats <- c()
  for(i in 1:(length(cs)-1)) {
    x_i <- x[which(c == cs[i]), ]
    if(is.vector(x_i)) {
      x_i <- matrix(data = x_i, nrow = 1)
    }

    # efficiency
    if(is.na(eff_cells) == F) {
      if(nrow(x_i)>eff_cells) {
        x_i <- x_i[sample(x = 1:nrow(x_i), size = eff_cells, replace = F), ]
      }
    }

    for(j in (i+1):length(cs)) {
      x_j <- x[which(c == cs[j]), ]
      if(is.vector(x_j)) {
        x_j <- matrix(data = x_j, nrow = 1)
      }

      # efficiency
      if(is.na(eff_cells) == F) {
        if(nrow(x_j)>eff_cells) {
          x_j <- x_j[sample(x = 1:nrow(x_j), size = eff_cells, replace = F), ]
        }
      }

      w <- matrix(data = 0, nrow = nrow(x_i), ncol = nrow(x_j))
      for(k in 1:nrow(x_i)) {
        w[k, ] <- get_euc(x = x_i[k, ], y = x_j)
      }
      w <- as.vector(w)
      hdi <- getHdi(vec = as.vector(w), hdi.level = hdi_level)

      # symmetric distances
      stats <- rbind(stats, data.frame(c_i = cs[i],
                                       c_j = cs[j],
                                       M = mean(w),
                                       L = hdi[1],
                                       H = hdi[2]))
      stats <- rbind(stats, data.frame(c_i = cs[j],
                                       c_j = cs[i],
                                       M = mean(w),
                                       L = hdi[1],
                                       H = hdi[2]))

    }
  }

  return(stats)
}


#' Wrapper for k-means. Input: features matrix \code{x} with cells
#' as rows and features (such as PC embeddings) as columns; vector \code{ks}
#' for the number of k-means to try; B=number of bootstrapping iterations; the
#' remaining parameters are described in the function \code{kmeans} or are used
#' for multicore execution.
#'
#' @param x matrix
#' @param ks vector
#' @param B number (default = 100)
#' @param n_start (default = 100)
#' @param iter_max (default = 50)
#' @param cores
#' @return list of B kmeans outputs
#' @exportMethod
#'
get_kmeans_boot <- function(B = 20,
                            boot_p = 0.66,
                            ks,
                            x,
                            n_start = 100,
                            iter_max = 50,
                            cores) {


  # average silhouette for k-means
  get_avg_sil <- function(km, df, only_avg) {
    ss <- cluster::silhouette(km$cluster, stats::dist(x = df, method = "euclidean"))
    if(only_avg) {
      if(is.na(ss)) {
        return(NA)
      }
      return(mean(ss[, 3]))
    }
    return(ss)
  }



  # gat statistics
  get_gap <- function (x, km, B = 100, d.power = 1) {
    n <- nrow(x)
    ii <- seq_len(n)

    W.k <- function(X) {
      clus <- km$cluster
      0.5 * sum(vapply(split(ii, clus), function(I) {
        xs <- X[I, , drop = FALSE]
        sum(dist(xs, method = "euclidean")^d.power/nrow(xs))
      }, 0))
    }

    logW <- E.logW <- SE.sim <- numeric(1)
    logW <- log(W.k(x))

    xs <- x
    m.x <- 0
    # xs <- scale(x, center = TRUE, scale = FALSE)
    # m.x <- rep(attr(xs, "scaled:center"), each = n)
    rng.x1 <- apply(xs, 2L, range)
    logWks <- numeric(length = B)
    for (b in 1:B) {
      z1 <- apply(X = rng.x1,
                  MARGIN = 2,
                  FUN = function(M, nn) {runif(nn, min = M[1], max = M[2])},
                  nn = n)
      z <- z1 + m.x
      logWks[b] <- log(W.k(z))
    }
    E.logW <- mean(logWks)
    SE.sim <- sqrt((1 + 1/B) * var(logWks))
    return(list(gap = E.logW - logW,
                SE.sim = SE.sim,
                logW = logW,
                logWks = logWks,
                E.logW = E.logW))
  }


  if(is.numeric(boot_p) == F) {
    stop("boot_p is a number between 0 (excluding) and 1")
  }
  if(length(boot_p) != 1) {
    stop("boot_p is a number between 0 (excluding) and 1")
  }
  if(boot_p<0|boot_p>1) {
    stop("boot_p is a number between 0 (excluding) and 1")
  }


  if(is.numeric(B) == F) {
    stop("boot_p is a number >0")
  }
  if(length(B) != 1) {
    stop("boot_p is a number >0")
  }
  if(B<0) {
    stop("boot_p is a number >0")
  }

  # browser()
  boot_obj <- vector(mode = "list", length = B)
  for(b in 1:B) {
    cat("boot:", b, " : ")

    j <- sample(x = 1:nrow(x), size = ceiling(nrow(x)*boot_p), replace = T)

    # clustering
    cat("1) clustering, ")
    kmeans_obj <- parallel::mclapply(X = ks,
                                     FUN = kmeans,
                                     x = x[j,],
                                     nstart = n_start,
                                     iter.max = iter_max,
                                     mc.cores = cores)
    names(kmeans_obj) <- ks
    boot_obj[[b]] <- kmeans_obj

    # extract WSS
    wss_data <- lapply(X = kmeans_obj, FUN =  function(x) {
      return(x$tot.withinss)
    })

    # compute silhouette
    cat("2) silhouette, ")
    sil_kmeans <- parallel::mclapply(X = kmeans_obj,
                                     FUN = get_avg_sil,
                                     df = x[j,],
                                     only_avg = T,
                                     mc.cores = cores,
                                     mc.cleanup = T)


    # get_gapstat <- function (x, km, k, B = 100, d.power = 1)
    cat("3) gap-stat, ")
    gap_stats <- parallel::mclapply(X = kmeans_obj,
                                    FUN = get_gap,
                                    x = x[j,],
                                    B = 10,
                                    d.power = 1,
                                    mc.cores = cores,
                                    mc.cleanup = T)

    # within sum of squares
    cat("4) WSS. \n")
    boot_obj[[b]] <- list(obj = kmeans_obj,
                          wss = wss_data,
                          sil = sil_kmeans,
                          gap = gap_stats)

  }
  names(boot_obj) <- 1:B



  # collect clustering info data
  sil_stats <- c()
  gap_stats <- c()
  wss_stats <- c()

  for(i in 1:length(boot_obj)) {

    sil_vec <- numeric(length = length(ks))
    gap_vec <- numeric(length = length(ks))
    wss_vec <- numeric(length = length(ks))

    for(j in 1:length(ks)) {
      sil_vec[j] <- boot_obj[[i]]$sil[[j]]
      gap_vec[j] <- boot_obj[[i]]$gap[[j]]$gap
      wss_vec[j] <- boot_obj[[i]]$wss[[j]]
    }

    sil_stats <- rbind(sil_stats, data.frame(boot = i, sil = sil_vec, k = ks))
    gap_stats <- rbind(gap_stats, data.frame(boot = i, gap = gap_vec, k = ks))
    wss_stats <- rbind(wss_stats, data.frame(boot = i, wss = wss_vec, k = ks))

  }

  return(list(boot_obj = boot_obj,
              wss_stats = wss_stats,
              sil_stats = sil_stats,
              gap_stats = gap_stats))
}





# run Gini impurity calculation on kmeans outpu
get_gini_kmeans <- function(kmeans_boot_obj,
                            labels,
                            verbose = F) {

  get_gini <- function(classes, groups) {

    ginis <- numeric(length = length(unique(groups)))
    names(ginis) <- unique(groups)

    ws <- numeric(length = length(unique(groups)))
    names(ws) <- unique(groups)

    for(g in unique(groups)) {
      temp_classes <- classes[groups == g]
      n <- length(temp_classes)
      ws[g] <- n

      if(length(temp_classes)==0) {
        ginis[g] <- 0
      } else {
        gini <- 0
        for(c in unique(temp_classes)) {
          gini <- gini+sum(temp_classes==c)/n*sum(temp_classes!=c)/n
        }
        ginis[g] <- gini
      }
    }
    return(list(ginis = ginis,
                ws = ws,
                gini_avg = sum(ginis*ws/sum(ws))))
  }

  gini_boot_obj <- vector(mode = "list", length = length(kmeans_boot_obj))

  for(b in 1:length(kmeans_boot_obj)) {
    if(verbose) {
      cat("boot: ", b, " \n")
    }

    o <- kmeans_boot_obj[[b]]$obj

    gini_local <- vector(mode = "list", length = length(o))
    for(k in 1:length(o)) {

      u <- o[[k]]
      l <- data.frame(ident = names(u$cluster), cluster = as.numeric(u$cluster))
      l <- merge(x = l, y = labels, by = "ident", all.x = T)

      gini_local[[k]] <- get_gini(classes = l$label, groups = l$cluster)
    }
    gini_boot_obj[[b]] <- gini_local

  }

  return(gini_boot_obj)
}



# main method
get_bubble_tree_data <- function(x,
                                 k,
                                 n_start = 10,
                                 iter_max = 50,
                                 hdi_level = 0.95,
                                 eff_cells = NA,
                                 seed = NA) {

  # check input
  if(is.na(x) || is.null(x) || is.matrix(x)==F) {
    stop("x should be a numeric matrix")
  }

  if(is.na(k) || is.null(k) || is.numeric(k)==F || k<=0 || k>=nrow(x)) {
    stop("k should be a number between 1 and nrow(x)")
  }

  # set seed for reproducibility
  if(is.na(seed) == F) {
    set.seed(seed = seed)
  }

  # perform k-means clustering
  # km <- stats::kmeans(x = x,
  #                     centers = k,
  #                     nstart = n_start,
  #                     iter.max = iter_max)
  km <- cluster::pam(x = x,
                     k = k,
                     metric = "euclidean",
                     stand = FALSE,
                     nstart = n_start)

  # get distances between clusters
  pp_dist <- get_pair_dist(x = x,
                           # c = as.numeric(km$cluster),
                           c = as.numeric(km$clustering),
                           eff_cells = eff_cells,
                           hdi_level = hdi_level)


  # compute hierarchical clustering dendrogram
  d <- reshape2::acast(data = pp_dist, formula = c_i~c_j, value.var = "M")
  d <- stats::as.dist(d)
  hc <- stats::hclust(d, method = "average")
  ph <- treeio::as.phylo(x = hc)

  return(list(km = km,
              ph = ph,
              hc = hc,
              pp_dist = pp_dist,
              k = k,
              input_par = list(n_start = n_start,
                               iter_max = iter_max,
                               hdi_level = hdi_level,
                               eff_cells = eff_cells)))
}



get_bubble_tree <- function(btd,
                            bubble_breaks = NA) {

  # get from bubble tree data
  km <- btd$km
  km$table <- table(km$clustering)

  ph <- btd$ph

  # clustering meta data (for bubbles)
  km_meta <- data.frame(label = names(km$table),
                        n = sum(km$table),
                        c = as.numeric(km$table))
  km_meta$p <- km_meta$c/km_meta$n
  km_meta$pct <- round(x = km_meta$p*100, digits = 2)


  # build ggtree
  tree <- ggtree::ggtree(ph, linetype='solid')%<+%km_meta+
    geom_point()+
    layout_rectangular()+
    geom_tippoint(aes(size = c, fill = c), shape = 21)+
    geom_tiplab(aes(label=paste0(label, " (", c, ', ', pct, "%)")),
                color='black', size = 3.5, hjust=-0.25)+
    guides(nrow = 3,
           fill = guide_legend("cell #"),
           size = guide_legend("cell #"))+
    theme_tree(plot.margin=margin(6,100,6,6),
               legend.position = "top")


  # add legends
  if(length(bubble_breaks)==1) {
    tree <- tree+
      scale_size_continuous(range = c(1, 8),
                            limits = c(0, max(bubble_breaks)))+
      scale_fill_gradient(low = "white",
                          high = "black",
                          limits = c(0, max(bubble_breaks)))
  }
  if(length(bubble_breaks)>1) {

    # create breaks if not specified
    bubble_breaks <- ceiling(seq(from = 100, to = max(km_meta$n), length.out = 10))

    tree <- tree+
      scale_size_continuous(range = c(1, 8),
                            breaks = bubble_breaks,
                            labels = bubble_breaks,
                            limits = c(0, max(bubble_breaks)))+
      scale_fill_gradient(low = "white",
                          high = "black",
                          breaks = bubble_breaks,
                          labels = bubble_breaks,
                          limits = c(0, max(bubble_breaks)))
  }


  # merge order of tips in the tree with metadata
  q <- tree$data
  q <- q[order(q$y, decreasing = F), ]
  tips <- q$label[q$isTip==T]
  tips <- data.frame(label = tips, tree_order = 1:length(tips))
  km_meta <- merge(x = km_meta, y = tips, by = "label")
  km_meta <- km_meta[order(km_meta$tree_order, decreasing = T), ]
  rm(q, tips)

  # format output
  out <- list(tree = tree,
              tree_meta = km_meta,
              btd = btd)

  return(out)
}



get_annotation_tiles_char <- function(k,
                                      a,
                                      tree_meta,
                                      integrate_over_clusters = T) {

  get_a <- function(k, a, annotation) {
    f <- data.frame(label = k, a = a)
    f <- aggregate(a~label, data = f, FUN = mean)
    f$value <- round(x = f$a, digits = 2)
    f$a <- NULL
    f$annotation <- annotation
    return(f)
  }

  ws <- data.frame(table(k, a))
  colnames(ws) <- c("label", "annotation", "freq")
  if(integrate_over_clusters==T) {
    n <- aggregate(freq~label, data = ws, FUN = sum)
    n$n <- n$freq
    n$freq <- NULL
    ws <- merge(x = ws, y = n, by = "label")
    rm(n)
    legend <- "% of cluster"
  }
  if(integrate_over_clusters==F) {
    n <- aggregate(freq~annotation, data = ws, FUN = sum)
    n$n <- n$freq
    n$freq <- NULL
    ws <- merge(x = ws, y = n, by = "annotation")
    rm(n)
    legend <- "% of annotation"
  }
  ws$p <- ws$freq/ws$n
  ws$percent <- round(x = ws$p*100, digits = 2)

  ws <- merge(x = ws, y = tree_meta, by = "label", all = T)
  ws <- ws[order(ws$tree_order, decreasing = F), ]
  ws$label <- factor(x = ws$label, levels = unique(ws$label))

  w <- ggplot(data = ws)+
    geom_tile(aes(x = annotation, y = label, fill = percent),
              col = "white")+
    geom_text(aes(x = annotation, y = label, label = percent),
              col = "black", size = 3)+
    scale_fill_distiller(name = legend,
                         palette = "Spectral",
                         limits = c(0, 100))+
    theme(legend.position = "top")+
    xlab(label = "Annotation")+
    ylab(label = "Cluster")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  return(list(ws = ws, w = w))
}



get_annotation_tiles_num <- function(k,
                                     as,
                                     tree_meta) {

  get_a <- function(k, a, annotation) {
    f <- data.frame(label = k, a = a)
    f <- aggregate(a~label, data = f, FUN = mean)
    f$value <- round(x = f$a, digits = 2)
    f$a <- NULL
    f$annotation <- annotation
    return(f)
  }

  if(is.vector(as)) {
    as <- matrix(data = as, ncol = 1)
  }

  ws <- vector(mode = "list", length = ncol(as))
  for(i in 1:ncol(as)) {
    ws[[i]] <- get_a(k = k, a = as[, i],
                     annotation = colnames(as)[i])
  }
  ws <- do.call(rbind, ws)

  ws <- merge(x = ws, y = tree_meta, by = "label", all = T)
  ws <- ws[order(ws$tree_order, decreasing = F), ]
  ws$label <- factor(x = ws$label, levels = unique(ws$label))
  ws$annotation <- factor(x = ws$annotation, levels = colnames(as))

  w <- ggplot(data = ws)+
    geom_tile(aes(x = annotation, y = label, fill = value), col = "white")+
    geom_text(aes(x = annotation, y = label, label = value), col = "black", size = 3)+
    scale_fill_distiller(name = "Avg.", palette = "Spectral")+
    theme(legend.position = "top")+
    xlab(label = "Annotation")+
    ylab(label = "Cluster")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  return(list(ws = ws, w = w))
}



get_annotation_violins <- function(k,
                                   as,
                                   tree_meta) {

  get_a <- function(k, a, annotation) {
    f <- data.frame(label = k, a = a)
    f$value <- f$a
    f$a <- NULL
    f$annotation <- annotation
    return(f)
  }

  if(is.vector(as)) {
    as <- matrix(data = as, ncol = 1)
  }

  ws <- vector(mode = "list", length = ncol(as))
  for(i in 1:ncol(as)) {
    ws[[i]] <- get_a(k = k, a = as[, i],
                     annotation = colnames(as)[i])
  }
  ws <- do.call(rbind, ws)


  ws <- merge(x = ws, y = tree_meta, by = "label", all = T)
  ws <- ws[order(ws$tree_order, decreasing = F), ]
  ws$label <- factor(x = ws$label, levels = unique(ws$label))
  ws$annotation <- factor(x = ws$annotation, levels = colnames(as))

  w <- ggplot(data = ws)+
    facet_grid(.~annotation, scales = "free_x")+
    geom_violin(aes(x = label, y = value))+
    coord_flip()+
    xlab(label = "Value")+
    xlab(label = "Cluster")

  return(list(ws = ws, w = w))
}




