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
              show_hclust = F,
              disable_hclust = F,
              tile_text_size = 3,
              x_axis_name = "Feature",
              rotate_x_axis_labels = T)
}


\arguments{
\item{btd}{bubbletree object}
\item{fs}{numeric vector or matrix, numeric cell features}
\item{summary_function}{character, "mean", "median" or "sum" summaries are
allowed}
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
}


\details{
get_num_tiles uses two main inputs:

1. bubbletree object

2. numeric vector or matrix of numeric cell features.

The order of the cells used to generat the bubbletree (input 1.)
should correspond to the order of cells in the vector/matrix of
numeric cell features (input  2.)

This function computes summaries of numeric cell feature in each bubble.
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
data(fs)

b <- get_bubbletree(x = A,
                    k = 8,
                    N_eff = 100,
                    B = 30)

g <- get_num_tiles(btd = b,
                   fs = fs,
                   summary_function = "mean",
                   round_digits = 2,
                   show_hclust = F,
                   disable_hclust = F,
                   tile_text_size = 3,
                   x_axis_name = "Gene expression",
                   rotate_x_axis_labels = T)


b$tree|g$plot
}

\seealso{get_k, get_dummy_bubbletree, get_gini, get_gini_k, update_bubbletree,
A,f,get_cat_tiles,get_num_violins}