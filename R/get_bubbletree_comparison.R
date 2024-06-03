

check_input_compare_trees <- function(btd_1,
                                      btd_2,
                                      tile_text_size,
                                      tile_bw) {

  check_tile_text_size(tile_text_size = tile_text_size)
  check_tile_bw(tile_bw = tile_bw)
  check_btd(btd = btd_1)
  check_btd(btd = btd_2)

  if(length(btd_1$cluster)!=length(btd_2$cluster)) {
    stop("unequal number of cells in btd_1 and btd_2")
  }
}



compare_bubbletrees <- function(btd_1,
                                btd_2,
                                tile_bw = FALSE,
                                tile_text_size = 2.5) {


  check_input_compare_trees(btd_1 = btd_1,
                            btd_2 = btd_2,
                            tile_text_size = tile_text_size,
                            tile_bw = tile_bw)

  # create tree 1
  t1 <- ggtree(btd_1$ph$main_ph, linetype = "solid")%<+%btd_1$tree_meta+
    geom_point2(mapping = ggplot2::aes_string(subset = "isTip==FALSE"),
                size = 0.5, col = "black") + layout_rectangular() +
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
    scale_radius(range = c(1, 4), limits = c(0, max(btd_1$tree_meta$Cells)))


  # create tree 2
  t2 <- ggtree(btd_2$ph$main_ph, linetype = "solid")%<+%btd_2$tree_meta+
    geom_point2(mapping = ggplot2::aes_string(subset = "isTip==FALSE"),
                size = 0.5, col = "black") + #layout_rectangular() +
    geom_tippoint(mapping = ggplot2::aes_string(size = "Cells"),
                  fill = "white", shape = 21)+
    theme_bw(base_size = 10) +
    theme_tree(plot.margin = margin(6, 6, 6, 6),
               legend.position = "bottom",
               legend.margin = margin(0, 0, 0, 0),
               legend.box.margin = margin(-10, -10, -10, -10),
               legend.spacing.x = unit(0.2, "cm"),
               legend.spacing.y = unit(0, "cm"))+
    geom_tiplab(mapping = ggplot2::aes_string(label = "lab_short"),
                color = "black", size = tile_text_size, vjust = -0.65,
                align = TRUE, offset = -20, angle = 90)+
    scale_radius(range = c(1, 4), limits = c(0, max(btd_2$tree_meta$Cells)))+
    coord_flip()


  # co-occurrence matrix
  cm <- data.frame(btd_1 = btd_1$cluster, btd_2 = btd_2$cluster, y = 1)

  cm <- aggregate(y~btd_1+btd_2, data = cm, FUN = sum)
  cm$n <- sum(cm$y)
  cm$p <- cm$y/cm$n

  cm$btd_1 <- factor(x = cm$btd_1, levels = rev(as.character(btd_1$tree_meta$label)))
  cm$btd_2 <- factor(x = cm$btd_2, levels = rev(as.character(btd_2$tree_meta$label)))

  g <- ggplot(data = cm)+
    geom_tile(aes(x = btd_2, y = btd_1, fill = p))+
    geom_text(aes(x = btd_2, y = btd_1, label = y),
              col = "black", size = tile_text_size)+
    theme_bw(base_size = 10)+
    theme(legend.position = "top",
          legend.margin=margin(t=0,r=0,b=2,l=0, unit = "pt"),
          legend.box.margin=margin(-10,-10,-10,-10))+
    guides(fill = guide_colourbar(barwidth = 5, barheight=1))

  if(tile_bw==FALSE) {
    g <- g + scale_fill_distiller(name = "Relative abundance",
                                  palette = "Spectral",
                                  na.value = 'white',
                                  breaks = scales::pretty_breaks(n = 3))
  } else {
    g <- g + scale_fill_gradient(name = "Relative abundance",
                                 low = "#f9f9f9",
                                 high = "#848484",
                                 na.value = 'white',
                                 breaks = scales::pretty_breaks(n = 3))
  }

  e <- plot_spacer()+theme(plot.margin = unit(c(0,118,0,0), "pt"))
  tg <- ((t1|g)/(e|t2))

  return(list(tree_comparison = tg, cooccurrence = cm))
}
