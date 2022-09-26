\name{get_bubbletree_dummy}



\alias{get_bubbletree_dummy}




\title{
Build bubbletree given matrix \eqn{A} and vector \eqn{cs} of externally
generated cluster IDs
}



\description{
get_bubbletree_dummy takes two main inputs:

1. numeric matrix \eqn{A^{n\times f}}, which represents a low-dimensional
projection (obtained e.g. by PCA) of the original high-dimensional scRNA-seq
data, with \eqn{n} rows as cells and \eqn{f} columns as low-dimension features.

2. vector \eqn{cs} of cluster IDs of each cell

The function get_bubbletree_dummy performs one main operation. It organizes
the bubbles (defined by \eqn{cs}) in a hierarchical dendrogram (bubbletree)
which represents the hierarchical relationships between the clusters (bubbles).
}

\usage{

get_bubbletree_dummy(x,
                     cs,
                     B = 100,
                     N_eff = 100,
                     cores = 1,
                     round_digits = 2,
                     show_simple_count = FALSE)
}


\arguments{
\item{x}{numeric matrix (\eqn{A^{n\times f}} with \eqn{n} cells, and \eqn{f}
low-dimensional projections of the original single cell RNA-seq dataset)}
\item{cs}{vector, cluster IDs}
\item{B}{integer, number of bootstrap iterations to perform in order to
generate bubbletree}
\item{N_eff}{integer, number of cells to draw randomly from each cluster when
computing inter-cluster distances. Maximum available number of cells are used
for clusters that contain lower number of cells than N_eff}
\item{cores}{integer, number of PC cores for parallel execution}
\item{round_digits}{integer, number of decimal places to keep when showing the
relative frequency of cells in each bubble}
\item{show_simple_count}{logical, if show_simple_count=T, cell counts in each
bubble will be divided by 1,000 to improve readability. This is only useful for
samples that are composed of millions of cells.}
}


\details{
This function is similar to get_bubbletree_kmeans and get_bubbletree_louvain.
It skips the clustering step. See documentation of get_bubbletree_kmeans and
get_bubbletree_louvain.
}


\value{
\item{A}{input x matrix}
\item{k}{number of clusters}
\item{km}{NULL}
\item{ph}{boot_ph: bootstrap dendrograms \eqn{H_b}; main_ph: consensus
          dendrogram \eqn{\hat{H}}}
\item{pair_dist}{inter-cluster distances used to generate the dendrograms}
\item{cluster}{cluster assignments of each cell}
\item{input_par}{list of all input parameters}
\item{tree}{ggtree bubbletree object}
\item{tree_meta}{meta-data associated with the bubbletree}
}


\author{
  Simo Kitanovski <simo.kitanovski@uni-due.de>
}


\examples{
# input data
data(d_500, package = "scBubbletree")
A <- d_500$A
f <- d_500$f

cs <- base::sample(x = LETTERS[1:5], size = nrow(A), replace = TRUE)

db <- get_bubbletree_dummy(x = A,
                           cs = cs,
                           B = 100,
                           N_eff = 100,
                           cores = 1)


}

\seealso{get_k, get_r, get_bubbletree_kmeans, get_bubbletree_louvain,
get_gini, get_gini_k, get_num_tiles, get_num_violins, get_cat_tiles,
d_500}
