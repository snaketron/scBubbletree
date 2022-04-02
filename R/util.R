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
get_dend_dist <- function(B = 20,
                          m,
                          c,
                          N_eff,
                          cores,
                          verbose = F) {

  # get distances between clusters
  pair_dist <- parallel::mclapply(X = 1:B,
                                  FUN = get_pair_dist,
                                  m = m,
                                  c = c,
                                  N_eff = N_eff,
                                  mc.cores = cores,
                                  verbose = verbose)
  pair_dist <- base::do.call(rbind, pair_dist)

  hc_pair_dist <- get_hc_dist(pair_dist = pair_dist)
  pca_pair_dist <- get_pca_dist(pair_dist = pair_dist)

  return(list(hc_pair_dist = hc_pair_dist,
              pca_pair_dist = pca_pair_dist,
              raw_pair_dist = pair_dist))
}


get_ph_support <- function(main_ph,
                           x) {
  boot_ph <- c()
  for(i in 1:max(x$B)) {
    d <- reshape2::acast(data = x[x$B == i,],
                         formula = c_i~c_j,
                         value.var = "M")
    d <- stats::as.dist(d)
    hc <- stats::hclust(d, method = "average")
    ph <- treeio::as.phylo(x = hc)
    if(i == 1) {
      boot_ph <- ph
    } else {
      boot_ph <- c(boot_ph, ph)
    }
  }

  # compute clade proportions
  clade_b <- ape::prop.clades(phy = main_ph,
                              x = boot_ph,
                              part = NULL,
                              rooted = ape::is.rooted(main_ph))

  main_ph$node.label <- clade_b
  #paste0(clade_b, "/", max(x$B))
  return(main_ph)
  # clade_p <- round(x = clade_b/max(x$B)
}



get_bubbletree <- function(ph,
                           cluster,
                           round_digits,
                           show_branch_support) {


  # compute meta summary
  km_meta <- data.frame(table(cluster))
  base::colnames(km_meta) <- c("label", "c")
  km_meta$n <- sum(km_meta$c)
  km_meta$p <- km_meta$c/km_meta$n
  km_meta$pct <- round(x = km_meta$p*100,
                       digits = round_digits)


  # build ggtree
  tree <- ggtree::ggtree(ph, linetype='solid')%<+%km_meta+
    geom_point()+
    layout_rectangular()+
    geom_tippoint(aes(size = c, fill = c), shape = 21)+
    geom_tiplab(aes(label=paste0(label, " (", c, ', ', pct, "%)")),
                color='black', size = 3.5, hjust=-0.25)+
    theme_tree2(plot.margin=margin(6,100,6,6),
                legend.position = "top")

  if(show_branch_support) {
    tree <- tree+
      geom_nodelab(geom='label', aes(label=label, subset=isTip==F),
                   size = 3.0, hjust=-0.25)
  }

  tree <- tree+
    scale_size_continuous(range = c(1, 7),
                          limits = c(0, max(km_meta$c)))+
    scale_fill_gradient(low = "white",
                        high = "black",
                        limits = c(0, max(km_meta$c)))+
    guides(fill = guide_legend(title = "cells", nrow = 2, byrow = TRUE),
           size = guide_legend(title = "cells", nrow = 2, byrow = TRUE))


  # merge order of tips in the tree with metadata
  q <- tree$data
  q <- q[order(q$y, decreasing = F), ]
  tips <- q$label[q$isTip==T]
  tips <- data.frame(label = tips, tree_order = 1:length(tips))
  km_meta <- base::merge(x = km_meta, y = tips, by = "label")
  km_meta <- km_meta[order(km_meta$tree_order, decreasing = T), ]
  rm(q, tips)

  # format output
  t <- list(tree = tree, tree_meta = km_meta)

  return(t)
}



get_pair_dist <- function(x, m, c, N_eff, verbose) {

  get_euc <- function(x, y) {
    for(i in 1:ncol(y)) {
      y[,i] <- (x[i]-y[,i])^2
    }
    y <- base::apply(X = y,
                     MARGIN = 1,
                     FUN = sum)
    y <- sqrt(y)
    return(y)
  }

  if(verbose) {
    cat("B:", x, "\n")
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
        x_i <- x_i[base::sample(x = 1:nrow(x_i),
                                size = N_eff,
                                replace = F), ]
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
          x_j <- x_j[base::sample(x = 1:nrow(x_j),
                                  size = N_eff,
                                  replace = F), ]
        }
      }

      # speed-up this part
      w <- matrix(data = 0, nrow = nrow(x_i), ncol = nrow(x_j))
      for(k in 1:nrow(x_i)) {
        w[k, ] <- get_euc(x = x_i[k, ], y = x_j)
      }

      # symmetric distances
      stats <- rbind(stats, data.frame(c_i = cs[i],
                                       c_j = cs[j],
                                       B = x,
                                       M = mean(w)))
      stats <- rbind(stats, data.frame(c_i = cs[j],
                                       c_j = cs[i],
                                       B = x,
                                       M = mean(w)))

      if(verbose) {
        verbose_counter <- verbose_counter + 1
        cat(paste0("Generating dendrogram:",
                   base::round(x = verbose_counter/((len_cs*(len_cs-1))/2)*100,
                         digits = 0), "% \n"))
      }
    }
  }

  return(stats)
}


get_hdi <- function(vec, hdi_level) {
  sortedPts <- base::sort(vec)
  ciIdxInc <- base::floor(hdi_level * base::length(sortedPts))
  nCIs = base::length(sortedPts) - ciIdxInc
  ciWidth = rep(0 , nCIs)
  for (i in 1:nCIs) {
    ciWidth[i] = sortedPts[i + ciIdxInc] - sortedPts[i]
  }
  HDImin = sortedPts[base::which.min(ciWidth)]
  HDImax = sortedPts[base::which.min(ciWidth) + ciIdxInc]
  HDIlim = c(HDImin, HDImax)
  return(HDIlim)
}


get_hc_dist <- function(pair_dist) {
  hc_dist <- c()
  for(i in 1:max(pair_dist$B)) {
    # get pairwise distances for B
    d <- pair_dist[pair_dist$B==i, ]

    # compute HClust
    hc <- stats::hclust(d = stats::as.dist(reshape2::acast(
      data = d, formula = c_i~c_j,
      value.var = "M")), method = "average")

    # get HClust distances as data.frame and name columns appropriately
    hd <- reshape2::melt(data = as.matrix(stats::cophenetic(x = hc)))
    base::colnames(x = hd) <- c("c_i", "c_j", "M")

    # add B info and join data
    hd$B <- i
    hc_dist <- rbind(hc_dist, hd)
  }

  # compute mean HClust distances accross Bs
  m <- stats::aggregate(M~c_i+c_j,
                        data = hc_dist,
                        FUN = mean)

  # compute HDI accross Bs
  hdi <- stats::aggregate(M~c_i+c_j,
                          data = hc_dist,
                          FUN = get_hdi,
                          hdi_level = 0.9)

  # if B too low, cannot compute HDI
  if(B<50) {
    hdi$L90 <- NA
    hdi$H90 <- NA
  }
  else {
    hdi$L90 <- hdi$M[, 1]
    hdi$H90 <- hdi$M[, 2]
  }
  hdi$M <- NULL

  o <- base::merge(x = m, y = hdi,
                   by = c("c_i", "c_j"))
  return(o)
}


get_pca_dist <- function(pair_dist) {
  m <- stats::aggregate(M~c_i+c_j,
                        data = pair_dist,
                        FUN = mean)
  hdi <- stats::aggregate(M~c_i+c_j,
                          data = pair_dist,
                          FUN = get_hdi,
                          hdi_level = 0.9)
  if(B<50) {
    hdi$L90 <- NA
    hdi$H90 <- NA
  }
  else {
    hdi$L90 <- hdi$M[, 1]
    hdi$H90 <- hdi$M[, 2]
  }
  hdi$M <- NULL

  o <- base::merge(x = m, y = hdi,
                   by = c("c_i", "c_j"))
  return(o)
}
