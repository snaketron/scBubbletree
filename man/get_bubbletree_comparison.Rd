\name{compare_bubbletrees}



\alias{compare_bubbletrees}




\title{
Comparison of two bubbletrees generated from the same scRNA-seq data
}

\description{
\code{compare_bubbletrees} takes as its main input two bubbletrees generated
from the \bold{same input data} but potentially with different input parameters
(e.g. clustering method or resolutions).

It then does the following two operations:

1. computes the Jaccard distance (JD) and the intersection between paris of clusters 
from the two bubbletrees. This is visualized as a heatmap.

2. it visualizes the two bubbletrees together with the heatmap.
}

\usage{
compare_bubbletrees(btd_1,
                    btd_2,
                    tile_bw = FALSE,
                    tile_text_size = 3,
                    ratio_heatmap = 0.5)
}


\arguments{
\item{btd_1}{bubbletree object}
\item{btd_2}{bubbletree object}
\item{tile_text_size}{integer, size of tile labels (default = 3)}
\item{tile_bw}{logical, tile grayscale (tile_bw = TRUE) vs. color
(tile_bw = FALSE, default)}
\item{ratio_heatmap}{nummeric, probability (default = 0.5) that dictates the 
relative width and height of the heatmap and the bubbletrees}
}


\details{
\code{compare_bubbletrees} takes as its main input two bubbletrees generated
from the \bold{same input data} but potentially with different input parameters
(e.g. clustering method or resolutions).

It then does the following two operations:

1. computes the Jaccard distance and the intersection between paris of clusters 
from the two bubbletrees. This is visualized as a heatmap.

2. it visualizes the two bubbletrees together with the heatmap.
}

\value{
\item{comparison}{ggplot2 objects assembled by R-package \code{patchwork}}
\item{m}{data.frame object with JD and intersection for each pair of clusters 
from the two bubbletrees}
}


\author{
  Simo Kitanovski \email{simo.kitanovski@uni-due.de}
}


\examples{
# input data
data("d_500", package = "scBubbletree")
A <- d_500$A

btd_1 <- get_bubbletree_graph(x = A,
                              r = 1,
                              n_start = 20,
                              iter_max = 100,
                              algorithm = "original",
                              knn_k = 50,
                              hclust_method = "average",
                              hclust_distance = "euclidean",
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = FALSE)


btd_2 <- get_bubbletree_kmeans(x = A,
                               k = 8,
                               cores = 1,
                               round_digits = 1,
                               show_simple_count = FALSE,
                               kmeans_algorithm = "MacQueen",
                               hclust_distance = "euclidean",
                               hclust_method = "average")

btd_comparison <- compare_bubbletrees(btd_1 = btd_1,
                                      btd_2 = btd_2,
                                      tile_bw = FALSE,
                                      tile_text_size = 3,
                                      ratio_heatmap = 0.5)


# plot
btd_comparison$tree_comparison

# data.frame of heatmap data
btd_comparison$m
}

\seealso{get_k, get_bubbletree_dummy, get_bubbletree_graph,
get_bubbletree_kmeans, get_gini, get_gini_k, d_500, get_num_tiles,
get_num_violins, get_cat_tiles}
