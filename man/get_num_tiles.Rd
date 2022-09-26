\name{get_num_tiles}



\alias{get_num_tiles}




\title{
Visualization of numeric cell features using tile plots
}



\description{
get_num_tiles creates tile plot to visualize a summary (e.g. mean,
median or sum) of a numeric cell feature (e.g. gene expression of a specific
gene) in each bubble of a bubbletree
}



\usage{
get_num_tiles(btd,
              fs,
              summary_function = "mean",
              round_digits = 2,
              show_hclust = FALSE,
              disable_hclust = FALSE,
              tile_text_size = 3,
              tile_bw = FALSE,
              x_axis_name = "Feature",
              rotate_x_axis_labels = TRUE)
}

\arguments{
\item{btd}{bubbletree object}
\item{fs}{numeric vector or matrix, numeric cell features}
\item{summary_function}{character, "mean", "median" or "sum", "pct nonzero",
"pct zero", summaries are allowed}
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
get_num_tiles uses two main inputs:

1. bubbletree object

2. numeric vector or matrix of numeric cell features.

The order of the cells used to generat the bubbletree (input 1.)
should correspond to the order of cells in the vector/matrix of
numeric cell features (input 2.)

This function computes summaries of numeric cell feature in each bubble:
1. mean = mean of feature
2. median = median of feature
3. sum = sum of feature
4. pct nonzero = sum of cells with feature > 0
5. pct zero = sum of cells with feature = 0

Important note: NA and NULL values are omitted.
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
data("d_500", package = "scBubbletree")
A <- d_500$A
fs <- d_500$fs

b <- get_bubbletree_kmeans(x = A,
                    k = 8,
                    N_eff = 100)

g <- get_num_tiles(btd = b,
                   fs = fs,
                   summary_function = "mean",
                   round_digits = 2,
                   show_hclust = FALSE,
                   disable_hclust = FALSE,
                   tile_text_size = 3,
                   tile_bw = TRUE,
                   x_axis_name = "Gene expression",
                   rotate_x_axis_labels = TRUE)


b$tree|g$plot
}

\seealso{get_k, get_r get_bubbletree_dummy, get_bubbletree_kmeans,
get_bubbletree_louvain, get_gini, get_gini_k, get_cat_tile, get_num_violins,
d_500}
