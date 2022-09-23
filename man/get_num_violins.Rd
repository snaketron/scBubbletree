\name{get_num_violins}



\alias{get_num_violins}




\title{
Visualization of numeric cell features using violin plots
}



\description{
get_num_violins creates violin plot to visualize the distribution of
of numeric cell features (e.g. gene expressions) in each bubble of a
bubbletree
}



\usage{

get_num_violins(btd,
                fs,
                x_axis_name = "Feature distribution",
                rotate_x_axis_labels = TRUE)
}


\arguments{
\item{btd}{bubbletree object}
\item{fs}{numeric vector or matrix, numeric cell features}
\item{x_axis_name}{character, x-axis title}
\item{rotate_x_axis_labels}{logical, should the x-axis labels be shown
horizontally (rotate_x_axis_labels = FALSE) or vertically
(rotate_x_axis_labels = TRUE)}
}


\details{
get_num_violins uses two main inputs:

1. bubbletree object

2. numeric vector or matrix of numeric cell features.

The order of the cells used to generat the bubbletree (input 1.)
should correspond to the order of cells in the vector/matrix of
numeric cell features (input  2.)

This function visualizes densities of numeric cell feature in the different
bubble.
}

\value{
\item{plot}{ggplot2, violin plot}
\item{table}{data.frame, raw data used to generate the plot}
}


\author{
  Simo Kitanovski <simo.kitanovski@uni-due.de>
}


\examples{
# input data
data(d_500)
A <- d_500$A
fs <- d_500$fs

b <- get_bubbletree_louvain(x = A,
                            r = 0.8,
                            N_eff = 100,
                            B = 100)

g <- get_num_violins(btd = b,
                     fs = fs,
                     x_axis_name = "Feature distribution",
                     rotate_x_axis_labels = TRUE)


b$tree|g$plot
}

\seealso{get_k, get_r get_bubbletree_dummy, get_bubbletree_kmeans,
get_bubbletree_louvain, get_gini, get_gini_k, get_cat_tile, get_num_tiles,
d_500}
