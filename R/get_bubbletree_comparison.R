
compare_bubbletrees <- function(btd_1,
                                btd_2,
                                tile_bw = FALSE,
                                tile_text_size = 3,
                                ratio_heatmap = 0.5) {
  
  check_input_compare_trees(btd_1 = btd_1,
                            btd_2 = btd_2,
                            tile_text_size = tile_text_size,
                            tile_bw = tile_bw,
                            ratio_heatmap = ratio_heatmap)
  
  # create tree 1
  t1 <- ggtree(btd_1$ph$main_ph, linetype = "solid")%<+%btd_1$tree_meta+
    geom_point2(mapping = ggplot2::aes_string(subset = "isTip==FALSE"),
                size = 0.5, col = "black")+
    layout_rectangular()+
    geom_tippoint(mapping = ggplot2::aes_string(size = "Cells"),
                  fill = "white", shape = 21)+
    theme_bw(base_size = 10)+
    theme_tree(plot.margin = margin(6, 100, 6, 6),
               legend.position = "left",
               legend.margin = margin(0, 0, 0, 0),
               legend.box.margin = margin(-10, -10, -10, -10),
               legend.spacing.x = unit(0.2, "cm"),
               legend.spacing.y = unit(0, "cm"))+
    geom_tiplab(mapping = ggplot2::aes_string(label = "lab_short"),
                color = "black", size = tile_text_size,
                hjust = -0.25, align = TRUE)+
    scale_radius(range = c(1, 4), limits = c(0, max(btd_1$tree_meta$Cells)))+
    geom_nodelab(geom='text', color = "#4c4c4c", size = 2.75, hjust=-0.2,
                 mapping=ggplot2::aes_string(label="label",
                                             subset="isTip==FALSE"))
  
  # create tree 2
  t2 <- ggtree(btd_2$ph$main_ph, linetype = "solid")%<+%btd_2$tree_meta+
    geom_point2(mapping = ggplot2::aes_string(subset = "isTip==FALSE"),
                size = 0.5, col = "black")+
    geom_tiplab(mapping = ggplot2::aes_string(label = "lab_short"),
                color = "black", size = tile_text_size, vjust = 0,
                align = TRUE, offset = +5, angle = 90)+
    hexpand(.6)+
    geom_tippoint(mapping = ggplot2::aes_string(size = "Cells"),
                  fill = "white", shape = 21)+
    theme_bw(base_size = 10) +
    theme_tree(plot.margin = margin(15, 6, 6, 6),
               legend.position = "bottom",
               legend.margin = margin(0, 0, 0, 0),
               legend.box.margin = margin(-10, -10, -5, -10),
               legend.spacing.x = unit(0.2, "cm"),
               legend.spacing.y = unit(0, "cm"))+
    scale_radius(range = c(1, 4), limits = c(0, max(btd_2$tree_meta$Cells)))+
    geom_nodelab(geom='text', color = "#4c4c4c", size = 2.75, hjust=-0.2,
                 mapping=ggplot2::aes_string(label="label",
                                             subset="isTip==FALSE"))+
    coord_flip()
  
  # compute pairwise JD
  jm <- c()
  for(c1 in unique(btd_1$cluster)) {
    for(c2 in unique(btd_2$cluster)) {
      j <- get_JD(a = which(btd_1$cluster==c1), b = which(btd_2$cluster==c2))
      j$btd_1 <- c1
      j$btd_2 <- c2
      jm <- rbind(jm, j)
    }
  }
  
  
  jm$btd_1 <- factor(x = jm$btd_1, levels = rev(
    as.character(btd_1$tree_meta$label)))
  jm$btd_2 <- factor(x = jm$btd_2, levels = rev(
    as.character(btd_2$tree_meta$label)))
  
  g <- ggplot(data = jm)+
    geom_tile(aes(x = btd_2, y = btd_1, fill = JD), col = "white")+
    geom_text(aes(x = btd_2, y = btd_1, label = intersection),
              col = "black", size = tile_text_size)+
    theme_bw(base_size = 10)+
    theme(legend.position = "top",
          legend.margin=margin(5,0,2,0, unit = "pt"),
          legend.box.margin=margin(-10,-10,-10,-10))+
    guides(fill = guide_colourbar(barwidth = 5, barheight=1))
  
  if(tile_bw==FALSE) {
    g <- g + scale_fill_distiller(name = "JD",
                                  palette = "Spectral",
                                  na.value = 'white',
                                  breaks = pretty_breaks(n = 3),
                                  limits = c(0, 1))
  } else {
    g <- g + scale_fill_gradient(name = "JD",
                                 low = "#f9f9f9",
                                 high = "#848484",
                                 na.value = 'white',
                                 breaks = pretty_breaks(n = 3),
                                 limits = c(0, 1))
  }
  
  e <- plot_spacer()+theme(plot.margin = unit(c(0,118,0,0), "pt"))
  
  top_plot <- (t1|g)+plot_layout(widths = c(1-ratio_heatmap,
                                            ratio_heatmap))
  bottom_plot <- (e|t2)+plot_layout(widths = c(1-ratio_heatmap,
                                               ratio_heatmap))
  tg <- (top_plot/bottom_plot)+plot_layout(heights = c(ratio_heatmap,
                                                       1-ratio_heatmap))
  return(list(comparison = tg, m = jm))
}



check_input_compare_trees <- function(btd_1,
                                      btd_2,
                                      tile_text_size,
                                      tile_bw,
                                      ratio_heatmap) {
  
  check_tile_text_size(tile_text_size = tile_text_size)
  check_tile_bw(tile_bw = tile_bw)
  check_btd(btd = btd_1)
  check_btd(btd = btd_2)
  check_ratio(ratio = ratio_heatmap)
  
  if(length(btd_1$cluster)!=length(btd_2$cluster)) {
    stop("unequal number of cells in btd_1 and btd_2")
  }
}


# Description:
# Given two vectors with cell IDs in two clusters, this function computes 
# the jaccard distance = 1 - jaccard coefficient
get_JD <- function(a, b) {
  intersection = length(intersect(a, b))
  union = length(a) + length(b) - intersection
  return (data.frame(JD = 1-intersection/union, 
                     intersection = intersection,
                     union = union))
}