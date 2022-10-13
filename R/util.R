# Short description:
# Main util function to compute inter-cluster distance and bubbletree
# B = nr. bootstrap iter.
# m = PCA matrix (also known as A)
# c = cluster identities
# N_eff = effective number of cells to draw from each cluster
# cores = number -> multicore execution
get_dist <- function(
    B,
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
  future::plan(future::cluster, workers = cores)
  pair_dist <- future.apply::future_lapply(
    X = base::seq_len(length.out = B),
    FUN = get_dist_point,
    m = m,
    c = c,
    N_eff = N_eff,
    future.seed = TRUE)
  future::plan(future::sequential())

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





get_dendrogram <- function(
    ph,
    cluster,
    round_digits,
    show_simple_count) {
  
  # input checks
  check_input_get_dendrogram(
    ph = ph,
    cluster = cluster,
    round_digits = round_digits,
    show_simple_count = show_simple_count)
  
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
                  fill = "white", shape = 21)+
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
    geom_nodelab(geom='text', color = "#4c4c4c" ,size = 2.75, hjust=-0.2,
              mapping=ggplot2::aes_string(label="label",subset="isTip==FALSE"))
  
  tree <- tree+
    ggplot2::scale_radius(range = c(1, 4), limits = c(0, max(km_meta$Cells)))+
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

  return(base::list(tree = tree, tree_meta = km_meta))
}





# Short description:
# maps input louvain_algorithm to Seurat accepted names. If a
# non-matching is provided, main function input check will catch 
# this and report error.
map_louvain_algname <- function(x) {

  if(base::missing(x)) {
    stop("x is missing")
  }
  if(base::is.numeric(x)) {
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


# Short description:
# aux. function which computes standard error of num. vector x
get_se <- function(x) {
  if(base::missing(x)|base::length(x)==0) {
    stop("x is missing or length(x)==0")
  }
  if(length(x) == 1) {
    se <- NA
  }
  else {
    se <- stats::sd(x)/base::sqrt(base::length(x))
  }
  return(se)
}



# Short description:
# Checks inputs of get_dendrogram
check_input_get_dendrogram <- function(
    ph,
    cluster,
    round_digits,
    show_simple_count) {
  
  check_cluster_dend(cluster = cluster)
  check_ph_dend(ph = ph, cluster = cluster)
  check_round_digits(round_digits = round_digits)
  check_show_simple_count(show_simple_count = show_simple_count)
}
