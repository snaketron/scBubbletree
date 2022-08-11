get_dist <- function(B,
                     m,
                     c,
                     N_eff,
                     cores) {

  # get distances between clusters
  pair_dist <- parallel::mclapply(X = 1:B,
                                  FUN = get_pair_dist,
                                  m = m,
                                  c = c,
                                  N_eff = N_eff,
                                  mc.cores = cores)
  # get distances between clusters
  # pair_dist <- parallel::mclapply(X = 1:B,
  #                                 FUN = get_pair_dist_2,
  #                                 m = m,
  #                                 c = c,
  #                                 N_eff = N_eff,
  #                                 mc.cores = cores)

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
    ph <- ape::as.phylo(x = hc)
    ph <- ape::unroot(phy = ph)

    if(i == 1) {
      boot_ph <- ph
    }
    else {
      boot_ph <- c(boot_ph, ph)
    }
  }

  # compute majortiy tree
  # majority_ph <- ape::consensus(boot_ph, p = 0.5)

  # compute clade proportions
  clade_b <- ape::prop.clades(phy = main_ph,
                              x = boot_ph,
                              part = NULL,
                              rooted = ape::is.rooted(main_ph))


  # some checks
  # # tips
  # tips <- main_ph$tip.label
  # branches <- setdiff(unique(as.vector(main_ph$edge)), tips)
  # #
  # root <- unique(main_ph$edge[which(!main_ph$edge[, 1] %in% main_ph$edge[, 2]), 1])
  # branch_composition <- c()
  # for(branch in branches) {
  #   w <- get_node_descendents_in_phylo(tree = main_ph, node = branch)
  #   w <- sort(w[w %in% tips])
  #   branch_composition <- rbind(branch_composition,
  #     data.frame(branch_node = branch, descendent_tip_nodes = paste0(w, collapse = ',')))
  # }
  # branch_composition$is_root <- ifelse(test = branch_composition$branch == root, yes = T, no = F)


  # add bootstrap
  main_ph$node.label <- clade_b

  return(list(main_ph = main_ph,
              boot_ph = boot_ph))
}



get_node_descendents_in_phylo <- function(tree,
                                          node,
                                          curr = NULL) {
  if (is.null(curr)) {
    curr <- vector()
  }
  daughters <- tree$edge[which(tree$edge[, 1] == node), 2]
  curr <- c(curr, daughters)
  if (length(curr) == 0 && node <= Ntip(tree))
    curr <- node
  w <- which(daughters > Ntip(tree))
  if (length(w) > 0)
    for (i in 1:length(w)) curr <- get_node_descendents_in_phylo(
      tree, daughters[w[i]], curr)
  return(curr)
}



get_dendrogram <- function(ph,
                           cluster,
                           round_digits,
                           show_simple_count) {


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
    # geom_tippoint(aes(size = c, fill = c), shape = 21)+ # old bubble coloring
    geom_tippoint(aes(size = c), fill = "white", shape = 21)+
    theme_bw(base_size = 10)+
    theme_tree2(plot.margin=margin(6,100,6,6),
                legend.position = "top",
                legend.margin=margin(0,0,0,0),
                legend.box.margin=margin(-10,-10,-10,-10),
                legend.spacing.x = unit(0.2, 'cm'),
                legend.spacing.y = unit(0, 'cm'))

  if(show_simple_count) {
    tree <- tree+
      geom_tiplab(aes(label=paste0(label, " (",
                                   paste0(round(c/1000, digits = round_digits),
                                          'K'), ', ', pct, "%)")),
                  color='black', size = 2.8, hjust=-0.25,
                  align = T)
  } else {
    tree <- tree+
      geom_tiplab(aes(label=paste0(label, " (", c, ', ', pct, "%)")),
                  color='black', size = 2.8, hjust=-0.25,
                  align = T)
  }


  tree_data <- tree$data
  tree <- tree+
    geom_nodelab(geom='text',
                 color = "#4c4c4c", # previously red
                 aes(label=label, subset=isTip==F),
                 size = 2.8, hjust=-0.2)

  tree <- tree+
    # scale_radius
    scale_radius(range = c(1, 5),
                 limits = c(0, max(km_meta$c)))+
    guides(size = guide_legend(title = "cells", nrow = 2, byrow = TRUE))
    # scale_fill_gradient(low = "white",
    #                     high = "black",
    #                     limits = c(0, max(km_meta$c)))+
    # guides(fill = guide_legend(title = "cells", nrow = 2, byrow = TRUE),
    #        size = guide_legend(title = "cells", nrow = 2, byrow = TRUE))


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



get_pair_dist_2 <- function(x, m, c, N_eff) {

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

  cs <- unique(c)
  stats <- c()
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
                                replace = T), ]
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
                                  replace = T), ]
        }
      }


      # just in case check
      if(is.vector(x_i)) {
        x_i <- matrix(data = x_i, nrow = 1)
      }
      if(is.vector(x_j)) {
        x_j <- matrix(data = x_j, nrow = 1)
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
    }
  }

  return(stats)
}



get_pair_dist <- function(x, m, c, N_eff) {


  get_euc <- function(x,y) {
    return(base::sqrt(base::outer(base::rowSums(x^2),
                                  base::rowSums(y^2), '+') -
                        base::tcrossprod(x, 2 * y)))
  }


  cs <- unique(c)
  stats <- c()
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
                                replace = T), ]
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
                                  replace = T), ]
        }
      }


      # just in case check
      if(is.vector(x_i)) {
        x_i <- matrix(data = x_i, nrow = 1)
      }
      if(is.vector(x_j)) {
        x_j <- matrix(data = x_j, nrow = 1)
      }

      w <- get_euc(x = x_i, y = x_j)


      # symmetric distances
      stats <- rbind(stats, data.frame(c_i = cs[i],
                                       c_j = cs[j],
                                       B = x,
                                       M = mean(w)))
      stats <- rbind(stats, data.frame(c_i = cs[j],
                                       c_j = cs[i],
                                       B = x,
                                       M = mean(w)))
    }
  }

  return(stats)
}



get_se <- function(x) {
  if(length(x) == 1) {
    se <- NA
  }
  else {
    se <- stats::sd(x)/base::sqrt(base::length(x))
  }
  return(se)
}



get_hc_dist <- function(pair_dist) {
  B <- base::max(pair_dist$B)

  hc_dist <- c()
  for(i in 1:B) {
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


  m <- base::merge(
    x = stats::aggregate(M~c_i+c_j, data = hc_dist, FUN = base::mean),
    y = stats::aggregate(M~c_i+c_j, data = hc_dist, FUN = get_se),
    by = c("c_i", "c_j"))
  colnames(m) <- c("c_i", "c_j", "M", "SE")
  m$L95 <- m$M-m$SE*1.96
  m$H95 <- m$M+m$SE*1.96

  if(B<20) {
    warning("B<20: standard errors and confidence intervals might be biased.")
  }
  return(m)
}



get_pca_dist <- function(pair_dist) {
  B <- base::max(pair_dist$B)

  m <- base::merge(
    x = stats::aggregate(M~c_i+c_j, data = pair_dist, FUN = base::mean),
    y = stats::aggregate(M~c_i+c_j, data = pair_dist, FUN = get_se),
    by = c("c_i", "c_j"))
  colnames(m) <- c("c_i", "c_j", "M", "SE")
  m$L95 <- m$M-m$SE*1.96
  m$H95 <- m$M+m$SE*1.96

  if(B<20) {
    warning("B<20: standard errors and confidence intervals might be biased.")
  }

  return(m)
}



get_weighted_feature_dist_num <- function(main_ph, w, value_var) {


  if(length(unique(w$feature))<=1) {
    stop("Only one feature, cannot compute dendrogram.\n")
  }

  w_df <- reshape2::acast(data = w,
                          formula = feature~cluster,
                          value.var = value_var)

  dist_feature <- matrix(data = 0,
                         nrow = nrow(w_df),
                         ncol = nrow(w_df))
  rownames(dist_feature) <- rownames(w_df)
  colnames(dist_feature) <- rownames(w_df)


  for(i in 1:(nrow(w_df)-1)) {
    for(j in (i+1):nrow(w_df)) {
      d <- abs(w_df[i, ]-w_df[j,])
      d <- d[order(as.numeric(names(d)), decreasing = F)]

      tree_dist <- ape::cophenetic.phylo(main_ph)
      for(k1 in 1:(length(d)-1)) {
        for(k2 in (k1+1):length(d)) {
          tmp <- tree_dist[names(d)[k1], names(d)[k2]]*max(d[k1], d[k2])
          tree_dist[names(d)[k1], names(d)[k2]] <- tmp
          tree_dist[names(d)[k2], names(d)[k1]] <- tmp
        }
      }
      dist_feature[i,j] <- sum(tree_dist)
      dist_feature[j,i] <- dist_feature[i,j]
    }
  }


  for(i in 1:(nrow(w_df)-1)) {
    for(j in (i+1):nrow(w_df)) {
      d <- abs(w_df[i, ]-w_df[j,])
      d <- d[order(as.numeric(names(d)), decreasing = F)]

      tree_dist <- ape::cophenetic.phylo(main_ph)
      for(k1 in 1:(length(d)-1)) {
        for(k2 in (k1+1):length(d)) {
          tmp <- tree_dist[names(d)[k1], names(d)[k2]]*max(d[k1], d[k2])
          tree_dist[names(d)[k1], names(d)[k2]] <- tmp
          tree_dist[names(d)[k2], names(d)[k1]] <- tmp
        }
      }
      dist_feature[i,j] <- sum(tree_dist)
      dist_feature[j,i] <- dist_feature[i,j]
    }
  }

  # build hclust
  hc_dist <- stats::dist(x = dist_feature, method = "euclidean")
  hc <- stats::hclust(d = hc_dist, method = "average")
  hc <- ape::as.phylo(x = hc)

  # build ggtree
  tree <- ggtree::ggtree(hc, linetype='solid')+
    theme_dendrogram()+
    coord_flip()+
    geom_tippoint()+
    theme(legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-10,-10,-10,-10))

  tree_data <- tree$data
  tree_data <- tree_data[tree_data$isTip==T,]
  ordered_labels <- tree_data$label[order(tree_data$y, decreasing = F)]

  return(list(tree = tree,
              labels = ordered_labels))


}



get_weighted_feature_dist <- function(main_ph, w, value_var) {


  if(length(unique(w$feature))<=1) {
    stop("Only one feature, cannot compute dendrogram.\n")
  }

  w$cluster <- as.character(w$cluster)
  features <- unique(as.character(w$feature))
  tree_dist <- ape::cophenetic.phylo(main_ph)
  tree_dist <- tree_dist[order(rownames(tree_dist)),
                         order(rownames(tree_dist))]

  weighted_dist <- matrix(data = 0,
                          nrow = length(features),
                          ncol = length(features))
  rownames(weighted_dist) <- features
  colnames(weighted_dist) <- features

  for(i in 1:length(features)) {
    for(j in 1:length(features)) {
      w_i <- w[which(w$feature == features[i]), ]
      w_j <- w[which(w$feature == features[j]), ]
      w_i <- w_i[order(w_i$cluster), ]
      w_j <- w_j[order(w_j$cluster), ]

      w_d <- abs(w_i$prob_feature-w_j$prob_feature)
      names(w_d) <- w_i$cluster

      d <- 0
      for(k1 in 1:(length(w_d)-1)) {
        for(k2 in (k1+1):length(w_d)) {
          p <- min(max(w_d[k1], w_d[k2]), w_d[k1], w_d[k2])
          # p <- max(w_d[k1], w_d[k2])
          d <- d+(p*tree_dist[names(w_d)[k1], names(w_d)[k2]])

        }
      }
      weighted_dist[features[i], features[j]] <- d
    }
  }


  # build hclust
  # hc_dist <- stats::dist(x = weighted_dist, method = "euclidean")
  hc_dist <- stats::as.dist(m = weighted_dist)
  hc <- stats::hclust(d = hc_dist, method = "average")
  # hc <- stats::hclust(d = hc_dist, method = "single")
  hc <- ape::as.phylo(x = hc)

  # build ggtree
  tree <- ggtree::ggtree(hc, linetype='solid')+
    theme_dendrogram()+
    coord_flip()+
    geom_tippoint()+
    theme(legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-10,-10,-10,-10))

  tree_data <- tree$data
  tree_data <- tree_data[tree_data$isTip==T,]
  ordered_labels <- tree_data$label[order(tree_data$y, decreasing = F)]

  return(list(tree = tree,
              labels = ordered_labels))


}


# Maps input louvain_algorithm to Seurat accepted names
map_louvain_algname <- function(x) {
  if(x=="original") {
    return(1)
  }
  if(x=="LMR") {
    return(2)
  }
  if(x=="SLM") {
    return(3)
  }
  if(x=="Leiden") {
    return(4)
  }
}

