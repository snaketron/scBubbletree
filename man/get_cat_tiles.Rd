\name{get_cat_tiles}



\alias{get_cat_tiles}




\title{
Visualization of categorical cell features using tile plots
}



\description{
get_cat_tiles creates tile plot to visualize the relative frequency of
categorical cell features between and within the bubbles of a bubbletree
}



\usage{
get_cat_tiles(btd,
              f,
              integrate_vertical,
              round_digits = 2,
              show_hclust = F,
              disable_hclust = F,
              tile_text_size = 3,
              x_axis_name = "Feature",
              rotate_x_axis_labels = T,
              tile_bw = F)
}


\arguments{
\item{btd}{bubbletree object}
\item{f}{character vector, categorical cell features}
\item{integrate_vertical}{logical, if integrate_vertical=TRUE: relative
frequency of the features is shown in each bubble, if integrate_vertical=FALSE:
relative frequencies of the features is shown within each bubble}
\item{round_digits}{integer, number of decimal places to keep when showing the
relative frequency of cells in each bubble}
\item{show_hclust}{logical, show hierarchical dendrogram in which different
feature classes are organized}
\item{disable_hclust}{logical, if disable_hclust=TRUE hierarchical clustering
of the feature classes will be performed}
\item{tile_text_size}{integer, size of tile labels}
\item{x_axis_name}{character, x-axis title}
\item{rotate_x_axis_labels}{logical, should the x-axis labels be shown
horizontally (rotate_x_axis_labels  = FALSE) or vertically
(rotate_x_axis_labels = TRUE)}
\item{tile_bw}{logical, tile grayscale (tile_bw = TRUE) vs. color
(tile_bw = FALSE, default)}
}


\details{
get_cat_tiles uses two main inputs:

1. bubbletree object

2. character vector of categorical cell features.

The order of the cells used to generat the bubbletree (input 1.)
should correspond to the order of cells in the vector of categorical
cell features (input  2.)

This function computes:

1. with integrate_vertical=T: relative frequencies of each feature
across the different bubbles

2. with integrate_vertical=F: within-bubble relative frequencies (composition)
of different features
}

\value{
\item{plot}{ggplot2, tile plot}
\item{table}{data.frame, raw data used to generate the plot}
}


\author{
  Simo Kitanovski <simo.kitanovski@uni-due.de>
}


\examples{
# input data

data(A)
data(f)

b <- get_bubbletree(x = A,
                    k = 8,
                    N_eff = 100,
                    B = 30)

g_v <- get_cat_tiles(b = b,
                     f = f,
                     integrate_vertical = T,
                     round_digits = 2,
                     show_hclust = F,
                     disable_hclust = F,
                     tile_text_size = 3,
                     x_axis_name = "Feature",
                     rotate_x_axis_labels = T)


g_h <- get_cat_tiles(b = b,
                    f = f,
                    integrate_vertical = F,
                    round_digits = 2,
                    show_hclust = F,
                    disable_hclust = F,
                    tile_text_size = 3,
                    x_axis_name = "Feature",
                    rotate_x_axis_labels = T)

b$tree|g_v$plot|g_h$plot
}

\seealso{get_k, get_dummy_bubbletree, get_gini, get_gini_k, update_bubbletree,
A,f,get_num_tiles,get_num_violins}
