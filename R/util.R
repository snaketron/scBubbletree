# Short description:
# Main util function to compute inter-cluster distance and bubbletree
# B = nr. bootstrap iter.
# m = PCA matrix (also known as A)
# c = cluster identities
# N_eff = effective number of cells to draw from each cluster
# cores = number -> multicore execution
get_dist <- function(B,
                     m,
                     c,
                     N_eff,
                     cores) {

  # Short description:
  # For b in 1:B computes inter-cluster distances
  get_dist_point <- function(x, m, c, N_eff) {

    # Compute pairwise euclidean distances between
    # two matrices x and y
    get_euc <- function(x,y) {
      return(base::sqrt(base::outer(base::rowSums(x^2),
                                    base::rowSums(y^2), '+') -
                          base::tcrossprod(x, 2 * y)))
    }


    cs <- unique(c)
    stats <- c()
    len_cs <- length(cs)


    for(i in base::seq_len(length.out = len_cs-1)) {

      x_i <- m[which(c == cs[i]), ]
      if(is.vector(x_i)) {
        x_i <- matrix(data = x_i, nrow = 1)
      }

      # efficiency
      if(is.na(N_eff)==FALSE) {
        if(nrow(x_i)>N_eff) {
          x_i <- x_i[base::sample(x = base::seq_len(length.out=base::nrow(x_i)),
                                  size = N_eff,
                                  replace = FALSE), ]
        }
      }

      for(j in (i+1):len_cs) {

        x_j <- m[which(c == cs[j]), ]
        if(is.vector(x_j)) {
          x_j <- matrix(data = x_j, nrow = 1)
        }

        # efficiency
        if(is.na(N_eff)==FALSE) {
          if(nrow(x_j)>N_eff) {
            x_j <- x_j[base::sample(
              x = base::seq_len(length.out=base::nrow(x_j)),
              size = N_eff,
              replace = FALSE), ]
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


  # Short description:
  # computes summaries (mean + SE) of inter-cluster hclust distances
  get_hc_dist <- function(pair_dist) {
    B <- base::max(pair_dist$B)

    # For b in 1:B computes hclust distances
    get_hc_point <- function(x, pair_dist) {
      d <- pair_dist[pair_dist$B==x, ]

      # compute hierarchical clustering with average linkage
      hc <- stats::hclust(d = stats::as.dist(reshape2::acast(
        data = d, formula = c_i~c_j,
        value.var = "M")),
        method = "average")

      # get hclust distances as data.frame and name columns appropriately
      hd <- reshape2::melt(data = as.matrix(stats::cophenetic(x = hc)))
      base::colnames(x = hd) <- c("c_i", "c_j", "M")

      # add B info and join data
      hd$B <- x
      return(hd)
    }

    hc_dist <- lapply(X = base::seq_len(length.out=B),
                      FUN = get_hc_point,
                      pair_dist = pair_dist)
    hc_dist <- base::do.call(base::rbind, hc_dist)

    m <- base::merge(
      x = stats::aggregate(M~c_i+c_j, data = hc_dist, FUN = base::mean),
      y = stats::aggregate(M~c_i+c_j, data = hc_dist, FUN = get_se),
      by = c("c_i", "c_j"))
    colnames(m) <- c("c_i", "c_j", "M", "SE")
    m$L95 <- m$M-m$SE*1.96
    m$H95 <- m$M+m$SE*1.96

    return(m)
  }


  # Short description:
  # computes summaries (mean + SE) of  inter-cluster PCA distance
  get_pca_dist <- function(pair_dist) {
    B <- base::max(pair_dist$B)

    m <- base::merge(
      x = stats::aggregate(M~c_i+c_j, data = pair_dist, FUN = base::mean),
      y = stats::aggregate(M~c_i+c_j, data = pair_dist, FUN = get_se),
      by = c("c_i", "c_j"))
    colnames(m) <- c("c_i", "c_j", "M", "SE")
    m$L95 <- m$M-m$SE*1.96
    m$H95 <- m$M+m$SE*1.96

    return(m)
  }


  # get distances between clusters in B bootstrap iterations
  pair_dist <- parallel::mclapply(X = base::seq_len(length.out = B),
                                  FUN = get_dist_point,
                                  m = m,
                                  c = c,
                                  N_eff = N_eff,
                                  mc.cores = cores)

  # collect results
  pair_dist <- base::do.call(rbind, pair_dist)

  # get additional summaries
  hc_pair_dist <- get_hc_dist(pair_dist = pair_dist)
  pca_pair_dist <- get_pca_dist(pair_dist = pair_dist)

  return(list(hc_pair_dist = hc_pair_dist,
              pca_pair_dist = pca_pair_dist,
              raw_pair_dist = pair_dist))
}




get_ph_support <- function(main_ph,
                           x) {
  boot_ph <- c()
  for(i in base::seq_len(length.out = max(x$B))) {
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

  # compute clade proportions
  clade_b <- ape::prop.clades(phy = main_ph,
                              x = boot_ph,
                              part = NULL,
                              rooted = ape::is.rooted(main_ph))


  # add bootstrap
  main_ph$node.label <- clade_b

  return(list(main_ph = main_ph,
              boot_ph = boot_ph))
}





get_dendrogram <- function(ph,
                           cluster,
                           round_digits,
                           show_simple_count) {

  # compute meta summary
  km_meta <- base::data.frame(base::table(cluster))
  base::colnames(km_meta) <- c("label", "Cells")
  km_meta$n <- base::sum(km_meta$Cells)
  km_meta$p <- km_meta$Cells/km_meta$n
  km_meta$pct <- base::round(x = km_meta$p*100,
                             digits = round_digits)
  km_meta$lab_short <- paste0(km_meta$label, " (", 
                              round(km_meta$Cells/1000, digits = round_digits),
                              'K, ', km_meta$pct, "%)")
  km_meta$lab_long <- paste0(km_meta$label, " (", km_meta$Cells, ', ', 
                             km_meta$pct, "%)")
  
  # build ggtree
  tree <- ggtree::ggtree(ph, linetype='solid')%<+%km_meta+
    geom_point2(mapping = ggplot2::aes_string(subset="isTip==FALSE"),
                size = 0.5, col = "black")+
    ggtree::layout_rectangular()+
    ggtree::geom_tippoint(mapping = ggplot2::aes_string(size = "Cells"),
                  fill = "white",
                  shape = 21)+
    theme_bw(base_size = 10)+
    theme_tree2(plot.margin=margin(6,100,6,6),
                legend.position = "top",
                legend.margin=margin(0,0,0,0),
                legend.box.margin=margin(-10,-10,-10,-10),
                legend.spacing.x = unit(0.2, 'cm'),
                legend.spacing.y = unit(0, 'cm'))

  if(show_simple_count) {
    tree <- tree+
      geom_tiplab(mapping = ggplot2::aes_string(label="lab_short"),
        color='black', size = 2.75, hjust=-0.25, align = TRUE)
  } else {
    tree <- tree+
      geom_tiplab(mapping = ggplot2::aes_string(label="lab_long"),
        color='black', size = 2.75, hjust=-0.25, align = TRUE)
  }

  tree <- tree+
    geom_nodelab(geom='text',
                 color = "#4c4c4c",
                 mapping = ggplot2::aes_string(label="label", 
                                        subset="isTip==FALSE"),
                 size = 2.75,
                 hjust=-0.2)
  
  tree <- tree+
    ggplot2::scale_radius(range = c(1, 4),
                          limits = c(0, max(km_meta$Cells)))+
    ggplot2::guides(size = ggplot2::guide_legend(
      title = "Cells", nrow = 2, byrow = TRUE))
  
  # merge order of tips in the tree with metadata
  q <- tree$data
  q <- q[base::order(q$y, decreasing = FALSE), ]
  tips <- q$label[q$isTip==TRUE]
  tips <- base::data.frame(label = tips,
                           tree_order = base::seq_len(length.out=length(tips)))
  km_meta <- base::merge(x = km_meta, y = tips, by = "label")
  km_meta <- km_meta[base::order(km_meta$tree_order, decreasing = TRUE), ]
  rm(q, tips)

  # format output
  t <- base::list(tree = tree, tree_meta = km_meta)
  return(t)
}




# Short description:
# aux. function which computes standard error of num. vector x
get_se <- function(x) {
  if(length(x) == 1) {
    se <- NA
  }
  else {
    se <- stats::sd(x)/base::sqrt(base::length(x))
  }
  return(se)
}




get_weighted_feature_dist_num <- function(main_ph,
                                          w,
                                          value_var) {


  if(length(unique(w$feature))<=1) {
    stop("Only one feature, cannot compute dendrogram.\n")
  }

  w_df <- reshape2::acast(data = w,
                          formula = feature~cluster,
                          value.var = value_var)

  dist_feature <- matrix(data = 0,
                         nrow = base::nrow(w_df),
                         ncol = base::nrow(w_df))
  base::rownames(dist_feature) <- base::rownames(w_df)
  base::colnames(dist_feature) <- base::rownames(w_df)


  for(i in base::seq(from = 1, to = base::nrow(w_df)-1, by = 1)) {
    for(j in base::seq(from = i+1, to = base::nrow(w_df), by = 1)) {
      d <- base::abs(w_df[i, ]-w_df[j,])
      d <- d[base::order(base::as.numeric(base::names(d)), decreasing = FALSE)]

      tree_dist <- ape::cophenetic.phylo(main_ph)
      for(k1 in base::seq(from = 1, to = base::length(d)-1, by = 1)) {
        for(k2 in base::seq(from = k1+1, to = base::length(d), by = 1)) {
          tmp <- tree_dist[names(d)[k1], names(d)[k2]]*base::max(d[k1], d[k2])
          tree_dist[base::names(d)[k1], base::names(d)[k2]] <- tmp
          tree_dist[base::names(d)[k2], base::names(d)[k1]] <- tmp
        }
      }
      dist_feature[i,j] <- base::sum(tree_dist)
      dist_feature[j,i] <- dist_feature[i,j]
    }
  }


  for(i in base::seq(from = 1, to = base::nrow(w_df)-1, by = 1)) {
    for(j in base::seq(from = i+1, to = base::nrow(w_df), by = 1)) {
      d <- base::abs(w_df[i, ]-w_df[j,])
      d <- d[base::order(base::as.numeric(base::names(d)), decreasing = FALSE)]

      tree_dist <- ape::cophenetic.phylo(main_ph)
      for(k1 in base::seq(from = 1, to = base::length(d)-1, by = 1)) {
        for(k2 in base::seq(from = k1+1, to = base::length(d), by = 1)) {
          tmp <- tree_dist[base::names(d)[k1],
                           base::names(d)[k2]]*base::max(d[k1], d[k2])
          tree_dist[base::names(d)[k1], base::names(d)[k2]] <- tmp
          tree_dist[base::names(d)[k2], base::names(d)[k1]] <- tmp
        }
      }
      dist_feature[i,j] <- base::sum(tree_dist)
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
  tree_data <- tree_data[tree_data$isTip==TRUE,]
  ordered_labels <- tree_data$label[base::order(tree_data$y,
                                                decreasing = FALSE)]

  return(base::list(tree = tree, labels = ordered_labels))
}



get_weighted_feature_dist <- function(main_ph,
                                      w,
                                      value_var) {


  if(length(unique(w$feature))<=1) {
    stop("Only one feature, cannot compute dendrogram.\n")
  }

  w$cluster <- base::as.character(w$cluster)
  features <- base::unique(base::as.character(w$feature))
  tree_dist <- ape::cophenetic.phylo(main_ph)
  tree_dist <- tree_dist[base::order(base::rownames(tree_dist)),
                         base::order(base::rownames(tree_dist))]

  weighted_dist <- matrix(data = 0,
                          nrow = base::length(features),
                          ncol = base::length(features))
  base::rownames(weighted_dist) <- features
  base::colnames(weighted_dist) <- features

  for(i in base::seq(from = 1, to = base::length(features), by = 1)) {
    for(j in base::seq(from = 1, to = base::length(features), by = 1)) {
      w_i <- w[base::which(w$feature == features[i]), ]
      w_j <- w[base::which(w$feature == features[j]), ]
      w_i <- w_i[base::order(w_i$cluster), ]
      w_j <- w_j[base::order(w_j$cluster), ]

      w_d <- base::abs(w_i$prob_feature-w_j$prob_feature)
      base::names(w_d) <- w_i$cluster

      d <- 0
      for(k1 in base::seq(from = 1, to = base::length(w_d)-1, by = 1)) {
        for(k2 in base::seq(from = k1+1, to = base::length(w_d), by = 1)) {
          p <- base::min(base::max(w_d[k1], w_d[k2]), w_d[k1], w_d[k2])
          # p <- max(w_d[k1], w_d[k2])
          d <- d+(p*tree_dist[base::names(w_d)[k1], base::names(w_d)[k2]])

        }
      }
      weighted_dist[features[i], features[j]] <- d
    }
  }


  # build hclust
  hc_dist <- stats::as.dist(m = weighted_dist)
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
  tree_data <- tree_data[tree_data$isTip==TRUE,]
  ordered_labels <- tree_data$label[base::order(tree_data$y,
                                                decreasing = FALSE)]


  return(base::list(tree = tree, labels = ordered_labels))
}


# Short description:
# maps input louvain_algorithm to Seurat accepted names. If a
# non-matching is provided main fun. checks will produce error.
map_louvain_algname <- function(x) {

  if(is.numeric(x)) {
    return(x)
  }

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

