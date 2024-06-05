\name{get_cat_tiles}



\alias{get_cat_tiles}




\title{
Visualization of categorical cell features using tile plots
}



\description{
\code{get_cat_tiles creates} tile plot to visualize the relative frequency of
categorical cell features between and within the bubbles of a bubbletree
}



\usage{
get_cat_tiles(btd,
              f,
              integrate_vertical,
              round_digits = 2,
              tile_text_size = 3,
              tile_bw = FALSE,
              x_axis_name = "Feature",
              rotate_x_axis_labels = TRUE)
}


\arguments{
\item{btd}{bubbletree object}
\item{f}{character vector, categorical cell features}
\item{integrate_vertical}{logical, if integrate_vertical=TRUE: relative
frequency of the features is shown in each bubble, if integrate_vertical=FALSE:
relative frequencies of the features is shown within each bubble}
\item{round_digits}{integer, number of decimal places to keep when showing the
relative frequency of cells in each bubble}
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

1. with \code{integrate_vertical}=T: relative frequencies of each feature
across the different bubbles

2. with \code{integrate_vertical\=F}: within-bubble relative frequencies
(composition) of different features
}

\value{
\item{plot}{ggplot2, tile plot}
\item{table}{data.frame, raw data used to generate the plot}
}


\author{
  Simo Kitanovski \email{simo.kitanovski@uni-due.de}
}


\examples{
# input data
data("d_500", package = "scBubbletree")
A <- d_500$A
f <- d_500$f


b <- get_bubbletree_graph(x = A,
                          r = 0.8,
                          N_eff = 100)


g_v <- get_cat_tiles(btd = b,
                     f = f,
                     integrate_vertical = TRUE,
                     round_digits = 2,
                     tile_text_size = 3,
                     x_axis_name = "Feature",
                     rotate_x_axis_labels = TRUE)


g_h <- get_cat_tiles(btd = b,
                    f = f,
                    integrate_vertical = FALSE,
                    round_digits = 2,
                    tile_text_size = 3,
                    x_axis_name = "Feature",
                    rotate_x_axis_labels = TRUE)

b$tree|g_v$plot|g_h$plot
}

\seealso{get_k, get_r get_bubbletree_dummy, get_bubbletree_kmeans,
get_bubbletree_graph, get_gini, get_gini_k, get_num_tile, get_num_violins,
d_500}
