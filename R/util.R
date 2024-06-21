# Short description:
# Main util function to compute inter-cluster distance and bubbletree
# B = nr. bootstrap iter.
# m = PCA matrix (also known as A)
# c = cluster identities
# N_eff = effective number of cells to draw from each cluster
# cores = number -> multicore execution
# hclust_distance = euclidean or manhattan
get_dist <- function(B,
                     m,
                     c,
                     N_eff,
                     cores,
                     hclust_distance) {
  
  # get centroid distances
  c_dist <- get_c_dist(m = m, c = c, hclust_distance = hclust_distance)
  p_dist <- NA
  p_dist_summary <- NA
  
  if(B>0) {
    # get distances between clusters in B bootstrap iterations
    p_dist <- bplapply(X = seq_len(length.out = B),
                       FUN = get_p_dist,
                       m = m,
                       c = c,
                       N_eff = N_eff,
                       hclust_distance = hclust_distance,
                       BPPARAM = MulticoreParam(workers = cores))
    
    # collect results
    p_dist <- do.call(rbind, p_dist)
    
    # get additional summaries
    p_dist_summary <- get_p_dist_summary(p_dist = p_dist)
  }
  
  return(list(c_dist = c_dist, 
              p_dist = p_dist, 
              p_dist_summary = p_dist_summary))
}


get_tree <- function(pd,
                     B,
                     hclust_method, 
                     cs, 
                     round_digits, 
                     show_simple_count, 
                     type = "c") {
  
  if(type == "c") {
    d <- acast(data = pd$c_dist, formula = c_i~c_j, value.var = "M", 
               fun.aggregate = mean)
  }
  else if(type == "p") {
    if(B>0) {
      d <- acast(data = pd$p_dist_summary, formula = c_i~c_j, 
                 value.var = "M", fun.aggregate = mean)
    } else {
      return(list(ph = NA, t = NA))
    }
  }
  
  d <- stats::as.dist(d)
  hc <- hclust(d, method = hclust_method)
  main_ph <- as.phylo(x = hc)
  
  if(length(unique(cs)) <= 2) {
    t <- get_dendrogram(ph = main_ph,
                        cluster = cs,
                        round_digits = round_digits,
                        show_simple_count = show_simple_count)
    
    ph <- list(main_ph = main_ph, boot_ph = NA)
  }
  else {
    main_ph <- unroot(phy = main_ph)
    
    # get branch support
    ph <- get_ph_support(main_ph = main_ph, x = pd$p_dist)
    
    # build bubbletree
    t <- get_dendrogram(ph = ph$main_ph,
                        cluster = cs,
                        round_digits = round_digits,
                        show_simple_count = show_simple_count)
  }
  
  return(list(ph = ph, t = t))
}


get_ph_support <- function(main_ph, x) {
  # if B=0, then no support values will be provided
  if(is.data.frame(x)==FALSE) {
    return(list(main_ph = main_ph, boot_ph = NA))
  }
  
  boot_ph <- c()
  for(i in seq_len(length.out = max(x$B))) {
    d <- acast(data = x[x$B == i,], formula = c_i~c_j, value.var = "M")
    d <- stats::as.dist(d)
    
    hc <- hclust(d, method = "average")
    ph <- as.phylo(x = hc)
    ph <- unroot(phy = ph)
    
    if(i == 1) {
      boot_ph <- ph
    }
    else {
      boot_ph <- c(boot_ph, ph)
    }
  }
  
  # compute clade proportions
  clade_b <- prop.clades(phy = main_ph,
                         x = boot_ph,
                         part = NULL,
                         rooted = is.rooted(main_ph))
  
  
  # add bootstrap
  main_ph$node.label <- clade_b
  
  # b = 0 for these nodes
  na_nodes <- which(is.na(main_ph$node.label))
  if(length(na_nodes)!=0) {
    main_ph$node.label[na_nodes] <- 0
  }
  
  return(list(main_ph = main_ph, boot_ph = boot_ph))
}


get_dendrogram <- function(ph,
                           cluster,
                           round_digits,
                           show_simple_count) {
  
  # input checks
  check_input_get_dendrogram(ph = ph,
                             cluster = cluster,
                             round_digits = round_digits,
                             show_simple_count = show_simple_count)
  
  # compute meta summary
  km_meta <- data.frame(table(cluster))
  colnames(km_meta) <- c("label", "Cells")
  km_meta$n <- sum(km_meta$Cells)
  km_meta$p <- km_meta$Cells/km_meta$n
  km_meta$pct <- round(x = km_meta$p*100, digits = round_digits)
  km_meta$lab_short <- paste0(km_meta$label, " (",
                              round(km_meta$Cells/1000, 
                                    digits = round_digits),
                              'K, ', km_meta$pct, "%)")
  km_meta$lab_long <- paste0(km_meta$label, " (", 
                             km_meta$Cells, ', ',
                             km_meta$pct, "%)")
  
  # build ggtree
  tree <- ggtree(ph, linetype='solid')%<+%km_meta+
    geom_point2(mapping = aes(subset=isTip==FALSE),size = 0.5, col = "black")+
    layout_rectangular()+
    geom_tippoint(aes(size = Cells),fill = "white", shape = 21)+
    theme_bw(base_size = 10)+
    theme_tree2(plot.margin=margin(6,100,6,6),
                legend.position = "top",
                legend.margin=margin(0,0,0,0),
                legend.box.margin=margin(-10,-10,-10,-10),
                legend.spacing.x = unit(0.2, 'cm'),
                legend.spacing.y = unit(0, 'cm'))
  
  if(show_simple_count) {
    tree <- tree+
      geom_tiplab(aes(label=lab_short),
                  color='black', size = 2.75, hjust=-0.25, align = TRUE)
  } else {
    tree <- tree+
      geom_tiplab(aes(label=lab_long),
                  color='black', size = 2.75, hjust=-0.25, align = TRUE)
  }
  
  tree <- tree+
    geom_nodelab(geom='text', color = "#4c4c4c" ,size = 2.75, hjust=-0.2,
                 mapping = aes(label=label,subset=isTip==FALSE))
  
  tree <- tree+
    scale_radius(range = c(1, 4), limits = c(0, max(km_meta$Cells)))+
    guides(size = guide_legend(title = "Cells", nrow = 2, byrow = TRUE))
  
  # merge order of tips in the tree with metadata
  q <- tree$data
  q <- q[order(q$y, decreasing = FALSE), ]
  tips <- q$label[q$isTip==TRUE]
  tips <- data.frame(label = tips,tree_order = seq_len(length.out=length(tips)))
  km_meta <- merge(x = km_meta, y = tips, by = "label")
  km_meta <- km_meta[order(km_meta$tree_order, decreasing = TRUE), ]
  
  return(list(tree = tree, tree_meta = km_meta))
}


# Short description:
# For b in 1:B computes pairwise inter-cluster distances using 
# bootstrapping
get_p_dist <- function(x, m, c, N_eff, hclust_distance) {
  
  cs <- unique(c)
  stats <- c()
  len_cs <- length(cs)
  with_replacement <- TRUE
  
  stats <- vector(mode = "list", length = len_cs*len_cs)
  counter <- 1
  for(i in seq_len(length.out = len_cs)) {
    x_i <- m[which(c == cs[i]), ]
    if(is.vector(x_i)) {
      x_i <- matrix(data = x_i, nrow = 1)
    }
    
    if(is.na(N_eff)==FALSE) {
      # efficiency
      if(nrow(x_i)>N_eff) {
        x_i <- x_i[sample(x = seq_len(length.out=nrow(x_i)),
                          size = N_eff,
                          replace = with_replacement), ]
      }
    }
    
    for(j in i:len_cs) {
      x_j <- m[which(c == cs[j]), ]
      if(is.vector(x_j)) {
        x_j <- matrix(data = x_j, nrow = 1)
      }
      
      if(is.na(N_eff)==FALSE) {
        # efficiency
        if(nrow(x_j)>N_eff) {
          x_j <- x_j[sample(x = seq_len(length.out=nrow(x_j)),
                            size = N_eff,
                            replace = with_replacement), ]
        }
      }
      
      # Euclidean distance
      if(hclust_distance=="euclidean") {
        w <- proxy::dist(x = x_i, y = x_j, method = "Euclidean")
      }
      # Manhattan distance
      if(hclust_distance=="manhattan") {
        w <- proxy::dist(x = x_i, y = x_j, method = "Manhattan")
      }
      
      # symmetric distances
      stats[[counter]]<-data.frame(c_i = cs[i], c_j = cs[j], B = x, M = mean(w), 
                                   n_i = nrow(x_i), n_j = nrow(x_j))
      counter <- counter + 1
      if(i!=j) {
        stats[[counter]]<-data.frame(c_i = cs[j], c_j = cs[i], B = x, M = mean(w), 
                                     n_i = nrow(x_j), n_j = nrow(x_i))
        counter <- counter + 1
      }
    }
  }
  
  stats <- do.call(rbind, stats)
  return(stats)
}


# Short description:
# Compute distance between centroids
get_c_dist <- function(m, c, hclust_distance) {
  cs <- unique(c)
  stats <- c()
  len_cs <- length(cs)
  
  stats <- vector(mode = "list", length = len_cs*len_cs)
  counter <- 1
  for(i in seq_len(length.out = len_cs)) {
    x_i <- m[which(c == cs[i]), ]
    x_i <- colMeans(x_i)
    
    for(j in i:len_cs) {
      x_j <- m[which(c == cs[j]), ]
      x_j <- colMeans(x_j)
      
      # Euclidean distance
      if(hclust_distance=="euclidean") {
        M <- sqrt(sum((x_i-x_j)^2))
      }
      # Manhattan distance
      if(hclust_distance=="manhattan") {
        M <- sum(abs((x_i-x_j)))
      }
      
      # symmetric distances
      stats[[counter]] <- data.frame(c_i = cs[i], c_j = cs[j], B = 1, M = M)
      counter <- counter + 1
      if(i!=j) {
        stats[[counter]] <- data.frame(c_i = cs[j], c_j = cs[i], B = 1, M = M)
        counter <- counter + 1
      }
    }
  }
  
  stats <- do.call(rbind, stats)
  return(stats)
}


# Short description:
# computes summaries (mean + SE) of  inter-cluster PCA distance
get_p_dist_summary <- function(p_dist) {
  B <- max(p_dist$B)
  
  m <- merge(x = aggregate(M~c_i+c_j, data = p_dist, FUN = mean),
             y = aggregate(M~c_i+c_j, data = p_dist, FUN = get_se),
             by = c("c_i", "c_j"))
  colnames(m) <- c("c_i", "c_j", "M", "SE")
  m$L95 <- m$M-m$SE*1.96
  m$H95 <- m$M+m$SE*1.96
  
  return(m)
}


# Short description:
# maps input louvain_algorithm to Seurat accepted names. If a
# non-matching is provided, main function input check will catch
# this and report error.
map_louvain_algname <- function(x) {
  
  if(missing(x)) {
    stop("x is missing")
  }
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


# Short description:
# aux. function which computes standard error of num. vector x
get_se <- function(x) {
  if(missing(x)|length(x)==0) {
    stop("x is missing or length(x)==0")
  }
  if(length(x) == 1) {
    se <- NA
  }
  else {
    se <- sd(x)/sqrt(length(x))
  }
  return(se)
}


# Short description:
# Compute pairwise euclidean distances between matrices x & y
get_euc <- function(x,y) {
  return(sqrt(outer(rowSums(x^2), rowSums(y^2), '+') - tcrossprod(x, 2 * y)))
}


# Short description: 
# Compute pairwise manhattan distances between matrices x & y
get_manh <- function(x,y) {
  get_manh_single <- function(i, x, y) {
    return(apply(X = abs(x[i,]-y), MARGIN = 1, FUN = sum))
  }
  return(do.call(rbind, lapply(X = 1:nrow(x), x = x, y = y, 
                               FUN = get_manh_single)))
}


# Short description:
# Checks inputs of get_dendrogram
check_input_get_dendrogram <- function(ph,
                                       cluster,
                                       round_digits,
                                       show_simple_count) {
  
  check_cluster_dend(cluster = cluster)
  check_ph_dend(ph = ph, cluster = cluster)
  check_round_digits(round_digits = round_digits)
  check_show_simple_count(show_simple_count = show_simple_count)
}
