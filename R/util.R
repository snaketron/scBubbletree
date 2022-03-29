#' For a features matrix \code{x} with cells as rows and features
#' (such as PC embeddings) as columns and cell clusters vector \code{c},
#' this function computes the average Euclidean distance between the pairs
#' of clusters and highest density interval of the distribution of pairwise
#' distances estimated between a pair of clusters.
#'
#' \code{N_eff} number of cells to use from each cluster (if
#' N_eff=NA all cells are used). Improves efficiency.
#'
#' @param B number.
#' @param m matrix
#' @param c vector.
#' @param N_eff number
#' @param cores
#' @return data frame
#'
get_dend_dist <- function(B = 100,
                          m,
                          c,
                          N_eff,
                          cores,
                          hdi_level,
                          verbose = F) {

  get_pair_dist <- function(x, m, c, N_eff) {

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
    verbose_counter <- 0
    len_cs <- length(cs)

    for(i in 1:(len_cs-1)) {

      x_i <- m[which(c == cs[i]), ]
      if(is.vector(x_i)) {
        x_i <- matrix(data = x_i, nrow = 1)
      }

      # efficiency
      if(is.na(N_eff) == F) {
        if(nrow(x_i)>N_eff) {
          x_i <- x_i[sample(x = 1:nrow(x_i), size = N_eff, replace = F), ]
        }
      }

      for(j in (i+1):len_cs) {

        x_j <- m[which(c == cs[j]), ]
        if(is.vector(x_j)) {
          x_j <- matrix(data = x_j, nrow = 1)
        }

        # efficiency
        if(is.na(N_eff) == F) {
          if(nrow(x_j)>N_eff) {
            x_j <- x_j[sample(x = 1:nrow(x_j), size = N_eff, replace = F), ]
          }
        }

        # speed-up this part
        w <- matrix(data = 0, nrow = nrow(x_i), ncol = nrow(x_j))
        for(k in 1:nrow(x_i)) {
          w[k, ] <- get_euc(x = x_i[k, ], y = x_j)
        }

        # microbenchmark(jogo  = {w <- matrix(data = 0, nrow = nrow(x_i), ncol = nrow(x_j))
        #                for(k in 1:nrow(x_i)) {w[k, ] <- get_euc(x = x_i[k, ], y = x_j)}},
        #                sqrt(apply(array(apply(x_j,1,function(x){(x-t(x_i))^2}),
        #                                 c(ncol(x_i),nrow(x_i),nrow(x_j))),2:3,sum)))

        # symmetric distances
        stats <- rbind(stats, data.frame(c_i = cs[i],
                                         c_j = cs[j],
                                         M = mean(w)))
        stats <- rbind(stats, data.frame(c_i = cs[j],
                                         c_j = cs[i],
                                         M = mean(w)))

        if(verbose) {
          verbose_counter <- verbose_counter + 1
          cat(paste0("Generating dendrogram:",
                     round(x = verbose_counter/((len_cs*(len_cs-1))/2)*100,
                           digits = 0), "% \n"))
        }
      }
    }

    return(stats)
  }

  get_hdi <- function(vec, hdi_level) {
    sortedPts <- sort(vec)
    ciIdxInc <- floor(hdi_level * length(sortedPts))
    nCIs = length(sortedPts) - ciIdxInc
    ciWidth = rep(0 , nCIs)
    for (i in 1:nCIs) {
      ciWidth[i] = sortedPts[i + ciIdxInc] - sortedPts[i]
    }
    HDImin = sortedPts[which.min(ciWidth)]
    HDImax = sortedPts[which.min(ciWidth) + ciIdxInc]
    HDIlim = c(HDImin, HDImax)
    return(HDIlim)
  }

  # get distances between clusters
  ps <- parallel::mclapply(X = 1:B,
                           FUN = get_pair_dist,
                           m = m,
                           c = c,
                           N_eff = N_eff,
                           mc.cores = cores)

  ps <- do.call(rbind, ps)
  m <- aggregate(M~c_i+c_j, data = ps, FUN = mean)
  hdi <- aggregate(M~c_i+c_j, data = ps, FUN = get_hdi,
                   hdi_level = hdi_level)
  if(B==1) {
    hdi$L <- NA
    hdi$H <- NA
  }
  else {
    hdi$L <- hdi$M[, 1]
    hdi$H <- hdi$M[, 2]
  }
  hdi$M <- NULL

  o <- merge(x = m, y = hdi, by = c("c_i", "c_j"))


  return(o)
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
                            cv_clust_p = 1,
                            cv_gap_p = 0.5 ,
                            ks,
                            x,
                            n_start = 100,
                            iter_max = 50,
                            cores) {


  # average silhouette for k-means
  get_avg_sil <- function(km, df, approx = T) {
    if(approx == F) {
      ss <- cluster::silhouette(km$cluster, stats::dist(
        x = df, method = "euclidean"))
      if(is.na(ss)) {
        return(NA)
      }
      return(mean(ss[, 3]))
    } else {
      ss <- bluster::approxSilhouette(x = df, clusters = km$cluster)
      return(mean(ss$width))
    }
  }


  # gat statistics
  get_gap <- function (x, km, B = 100, d.power = 1, cv_gap_p = 1) {
    if(cv_gap_p < 0 | cv_gap_p > 1) {
      stop("cv_gap_p is a number between 0 (excluded) and 1.")
    }
    if(cv_gap_p < 1) {
      cs <- km$cluster
      js <- sample(x = 1:nrow(x), size = ceiling(nrow(x)*cv_gap_p), replace = F)
      x <- x[js, ]
      cs <- cs[js]
    } else {
      cs <- km$cluster
    }
    n <- nrow(x)
    ii <- seq_len(n)


    Wk <- function(X, cs) {
      0.5 * sum(vapply(split(ii, cs), function(I) {
        xs <- X[I, , drop = FALSE]
        sum(dist(xs, method = "euclidean")^d.power/nrow(xs))
      }, 0))
    }

    logW <- E.logW <- SE.sim <- numeric(1)
    logW <- log(Wk(x, cs=cs))

    xs <- scale(x, center = TRUE, scale = FALSE)
    m.x <- rep(attr(xs, "scaled:center"), each = n)
    rng.x1 <- apply(xs, 2L, range)
    logWks <- numeric(length = B)
    for (b in 1:B) {
      z1 <- apply(X = rng.x1,
                  MARGIN = 2,
                  FUN = function(M, nn) {
                    runif(nn, min = M[1], max = M[2])
                    },
                  nn = n)
      z <- z1 + m.x
      logWks[b] <- log(Wk(z, cs=cs))
    }
    E.logW <- mean(logWks)
    SE.sim <- sqrt((1 + 1/B) * var(logWks))
    return(list(gap = E.logW - logW,
                SE.sim = SE.sim,
                logW = logW,
                logWks = logWks,
                E.logW = E.logW))
  }


  if(is.numeric(cv_clust_p) == F) {
    stop("cv_clust_p is a number between 0 (excluding) and 1")
  }
  if(length(cv_clust_p) != 1) {
    stop("cv_clust_p is a number between 0 (excluding) and 1")
  }
  if(cv_clust_p<0|cv_clust_p>1) {
    stop("cv_clust_p is a number between 0 (excluding) and 1")
  }


  if(is.numeric(B) == F) {
    stop("cv_clust_p is a number >0")
  }
  if(length(B) != 1) {
    stop("cv_clust_p is a number >0")
  }
  if(B<0) {
    stop("cv_clust_p is a number >0")
  }

  boot_obj <- vector(mode = "list", length = B)
  for(b in 1:B) {
    cat("boot:", b, " : ")

    j <- sample(x = 1:nrow(x), size = ceiling(nrow(x)*cv_clust_p), replace = T)

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
                                     mc.cores = cores,
                                     mc.cleanup = T)


    # get_gapstat <- function (x, km, k, B = 100, d.power = 1)
    cat("3) gap-stat, ")
    gap_stats <- parallel::mclapply(X = kmeans_obj,
                                    FUN = get_gap,
                                    x = x[j,],
                                    B = 1,
                                    d.power = 1,
                                    cv_gap_p = cv_gap_p,
                                    mc.cores = cores,
                                    mc.cleanup = T)
    # within sum of squares
    cat("4) WSS. \n")
    boot_obj[[b]] <- list(obj = kmeans_obj,
                          wss = wss_data,
                          sil = sil_kmeans,
                          gap = gap_stats,
                          cell_i = j)

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



get_bubbletree_data <- function(x,
                                k,
                                n_start = 10,
                                iter_max = 50,
                                hdi_level = 0.95,
                                B = 1,
                                N_eff = 500,
                                cores,
                                seed = NA,
                                verbose = F) {


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
  km <- stats::kmeans(x = x,
                      centers = k,
                      nstart = n_start,
                      iter.max = iter_max)


  # dendrogram distance
  dend_dist <- get_dend_dist(B = B,
                             m = x,
                             c = km$cluster,
                             N_eff = N_eff,
                             cores = cores,
                             hdi_level = hdi_level,
                             verbose = verbose)



  # compute hierarchical clustering dendrogram
  d <- reshape2::acast(data = dend_dist,
                       formula = c_i~c_j, value.var = "M")
  d <- stats::as.dist(d)
  hc <- stats::hclust(d, method = "average")
  ph <- treeio::as.phylo(x = hc)

  return(list(A = x,
              km = km,
              ph = ph,
              hc = hc,
              dend_dist = dend_dist,
              k = k,
              cluster = km$cluster,
              input_par = list(n_start = n_start,
                               iter_max = iter_max,
                               hdi_level = hdi_level,
                               N_eff = N_eff,
                               B = B,
                               seed = seed)))
}


update_bubbletree_data <- function(btd,
                                   updated_bubbles,
                                   ks,
                                   cores,
                                   seed = NA,
                                   verbose = F) {

  update_bubble <- function(A, bubble, k,
                            n_start, iter_max) {

    # perform k-means clustering
    km <- stats::kmeans(x = A,
                        centers = k,
                        nstart = n_start,
                        iter.max = iter_max,
                        seed = seed)

    return(km)

  }


  # ks: vector
  # non-zero

  # updated_bubbles: vector
  # * part of btd


  # get input from previous bubbletree data
  A <- btd$A
  n_start <- btd$input_par$n_start
  iter_max <- btd$input_par$iter_max
  hdi_level <- btd$input_par$hdi_level
  N_eff <- btd$input_par$N_eff
  B <- btd$input_par$B


  # loop around updated_bubbles
  u_kms <- vector(mode = "list", length = length(updated_bubbles))
  for(i in 1:length(updated_bubbles)) {
    cat("Updating bubble:", updated_bubbles[i], "\n")
    j <- which(btd$cluster == updated_bubbles[i])

    # run update
    u_kms[[i]] <- update_bubble(A = A[j,],
                              bubble = updated_bubbles[i],
                              k = ks[i],
                              n_start = n_start,
                              iter_max = iter_max)

    # add comments for debugging
    u_kms[[i]]$comment <- list(note = "update",
                               bubble = updated_bubbles[i],
                               k = ks[i])

    # update cluster naming
    btd$cluster[j] <- paste0(updated_bubbles[i], '_', u_kms[[i]]$cluster)
  }


  # browser()
  cat("Updating dendrogram \n")
  dend_dist <- get_dend_dist(B = B,
                             m = A,
                             c = btd$cluster,
                             N_eff = N_eff,
                             cores = cores,
                             hdi_level = hdi_level,
                             verbose = verbose)

  # compute hierarchical clustering dendrogram
  d <- reshape2::acast(data = dend_dist,
                       formula = c_i~c_j,
                       value.var = "M")

  d <- stats::as.dist(d)
  hc <- stats::hclust(d, method = "average")
  ph <- treeio::as.phylo(x = hc)

  btd$ph <- ph
  btd$hc <- hc
  btd$dend_dist <- dend_dist
  dend_dist$km <- "updated"
  dend_dist$k <- "updated"

  return(list(btd = btd, u_kms = u_kms))
}




get_bubbletree_data_from_clustering <- function(x,
                                                c,
                                                hdi_level = 0.95,
                                                B = 1,
                                                N_eff = 500,
                                                cores,
                                                seed = NA,
                                                verbose = F) {


  # check input
  if(is.na(x) || is.null(x) || is.matrix(x)==F) {
    stop("x should be a numeric matrix")
  }

  # set seed for reproducibility
  if(is.na(seed) == F) {
    set.seed(seed = seed)
  }


  # dendrogram distance
  dend_dist <- get_dend_dist(B = B,
                             m = x,
                             c = c,
                             N_eff = N_eff,
                             cores = cores,
                             hdi_level = hdi_level,
                             verbose = verbose)


  # compute hierarchical clustering dendrogram
  d <- reshape2::acast(data = dend_dist,
                       formula = c_i~c_j, value.var = "M")
  d <- stats::as.dist(d)
  hc <- stats::hclust(d, method = "average")
  ph <- treeio::as.phylo(x = hc)

  return(list(A = x,
              km = NA,
              ph = ph,
              hc = hc,
              dend_dist = dend_dist,
              k = k,
              cluster = c,
              input_par = list(hdi_level = hdi_level,
                               N_eff = N_eff,
                               B = B,
                               seed = seed)))
}


get_bubbletree <- function(btd,
                           bubble_breaks = NA) {


  # get dendrogram
  ph <- btd$ph

  # get km data
  km <- btd$cluster
  km_meta <- data.frame(table(btd$cluster))
  colnames(km_meta) <- c("label", "c")
  km_meta$n <- sum(km_meta$c)
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
    theme_tree2(plot.margin=margin(6,100,6,6),
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
                                      integrate_over_clusters = T,
                                      round_digits = 2) {

  get_a <- function(k, a, annotation) {
    f <- data.frame(label = k, a = a)
    f <- aggregate(a~label, data = f, FUN = mean)
    f$value <- round(x = f$a, digits = round_digits)
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
  ws$percent <- round(x = ws$p*100, digits = round_digits)

  ws <- merge(x = ws, y = tree_meta, by = "label", all = T)
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
                         limits = c(0, 100))+
    theme(legend.position = "top")+
    xlab(label = "Annotation")+
    ylab(label = "Cluster")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  return(list(ws = ws, w = w))
}



get_annotation_tiles_num <- function(k,
                                     as,
                                     tree_meta,
                                     plot_title = '',
                                     round_digits = 2) {

  get_a <- function(k, a, annotation) {
    f <- data.frame(label = k, a = a)
    f <- aggregate(a~label, data = f, FUN = mean)
    f$value <- round(x = f$a, digits = round_digits)
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
    theme_bw()+
    geom_tile(aes(x = annotation, y = label, fill = value), col = "white")+
    geom_text(aes(x = annotation, y = label, label = value), col = "black", size = 3)+
    scale_fill_distiller(name = "Avg.", palette = "Spectral")+
    theme(legend.position = "top")+
    xlab(label = "Annotation")+
    ylab(label = "Cluster")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    ggtitle(label = plot_title)+
    guides(fill = guide_colourbar(barwidth = 4, barheight = 1))

  return(list(ws = ws, w = w))
}



get_annotation_violins <- function(k,
                                   as,
                                   tree_meta,
                                   plot_title = '',
                                   scales = "free_x") {

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
    theme_bw()+
    facet_grid(.~annotation, scales = scales)+
    geom_violin(aes(x = label, y = value))+
    coord_flip()+
    ylab(label = "Distribution")+
    xlab(label = "Cluster")+
    ggtitle(label = plot_title)+
    theme(strip.text.x = element_text(margin = margin(0.01,0,0.01,0, "cm")))


  return(list(ws = ws, w = w))
}




get_gini <- function(labels, clusters) {

  get_gini_cluster <- function(c, l) {
    ls <- unique(l)
    l_len <- length(l)

    s <- 0
    for(i in 1:length(ls)) {
      s <- s + (sum(l == ls[i])/l_len)^2
    }

    return(s)

  }

  cs <- unique(clusters)

  # for each cluster we get gini-index
  cluster_gini <- numeric(length = length(cs))
  names(cluster_gini) <- cs

  # cluster weights used to compute total gini
  cluster_weight <- numeric(length = length(cs))
  names(cluster_weight) <- cs

  for(i in 1:length(cs)) {
    j <- which(clusters == cs[i])

    cluster_weight[i] <- length(j)/length(clusters)

    cluster_gini[i] <- 1-get_gini_cluster(c = clusters[j],
                                          l = labels[j])

  }

  total_gini = sum(cluster_gini*cluster_weight)

  return(list(cluster_gini = cluster_gini,
              total_gini = total_gini))
}



get_gini_boot <- function(labels, kmeans_boot_obj) {
  B <- length(kmeans_boot_obj$boot_obj)

  total_o <- c()
  cluster_o <- c()
  for(i in 1:B) {
    # b$boot_obj[[b]]$obj$`2`$
    ks <- names(kmeans_boot_obj$boot_obj[[i]]$obj)

    for(j in 1:length(ks)) {
      cell_id <- kmeans_boot_obj$boot_obj[[i]]$cell_i
      gini <- get_gini(clusters = kmeans_boot_obj$boot_obj[[i]]$obj[[ks[j]]]$cluster,
                       labels = labels[cell_id])

      # collect total gini and cluster specific gini scores

      # total
      total_o <- rbind(total_o, data.frame(B = i,
                                           k = as.numeric(ks[j]),
                                           total_gini = gini$total_gini))
      # cluster
      cluster_o <- rbind(cluster_o, data.frame(B = i,
                                               k = as.numeric(ks[j]),
                                               cluster = names(gini$cluster_gini),
                                               gini = gini$cluster_gini))
    }
  }
  return(list(total_gini = total_o,
              cluster_gini = cluster_o))
}

