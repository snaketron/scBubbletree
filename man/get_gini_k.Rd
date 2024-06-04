\name{get_gini_k}

\alias{get_gini_k}


\title{
Gini impurity index computed for a list of clustering solutions obtained by
functions \code{get_k} or \code{get_r} and a vector of categorical cell feature
labels}

\description{
Given
The Gini impurity (GI) index allows us to quantitatively evaluate how well a
set of labels (categorical features) are split across a set of bubbles. We have
a completely perfect split (GI = 0) when each bubble is 'pure', i.e. each bubble
contains labels coming from distinct a class. In contrast to this, we have
completely imperfect split (GI = 1) when the relative frequency distribution of
the labels in each bubble is identical to the background relative frequency
distribution of the labels.

Cell type predictions are a type of categorical features that are often used
to evaluate the goodness of the clustering. \code{get_gini_k} takes as input:
1) a vector of labels for each cell (e.g. cell types) and 2) object returned
by function \code{get_k} or \code{get_r}. Then it computes for each \eqn{k}
or \eqn{r} the cluster purity and weightred gini impurity of each clustering
solution mean GI, which is another way of finding an optimal clustering
resolution.
}



\usage{
get_gini_k(labels, obj)
}


\arguments{
\item{labels}{character/factor vector of labels}
\item{obj}{object returned by functions \code{get_k} or \code{get_r}}
}


\details{
To quantify the purity of a cluster (or bubble) \eqn{i} with \eqn{n_i} number
of cells, each of which carries one of \eqn{L} possible labels (e.g. cell type),
we computed the Gini impurity index:

\eqn{GI_i=\sum_{j=1}^{L} \pi_{ij}(1-\pi_{ij}),}


with \eqn{\pi_{ij}} as the relative frequency of label \eqn{j} in cluster
\eqn{i}. In homogeneous (`pure`) clusters most cells carry a distinct label.
Hence, the \eqn{\pi}'s are close to either 1 or 0, and GI takes on a small
value close to zero. In `impure' clusters cells carry a mixture of different
labels. In this case most \eqn{\pi} are far from either 1 or 0, and GI diverges
from 0 and approaches 1. If the relative frequencies of the different labels
in cluster \eqn{i} are equal to the (background) relative frequencies of the
labels in the sample, then cluster \eqn{i} is completely `impure'.

To compute the overall Gini impurity of a bubbletree, which represents a
clustering solution with \eqn{k} bubbles, we estimated the weighted Gini
impurity (WGI) by computing the weighted (by the cluster size) average of
the GIs:

\eqn{WGI=\sum_{i=1}^{k} GI_i {n_i}/{n},}

with \eqn{n_i} as the number of cells in cluster \eqn{i} and \eqn{n=\sum_i n_i}.
}


\value{
\item{gi_summary}{GI for each bubble of a clustering solution with clustering
resolution \eqn{k} or \eqn{r}}
\item{wgi_summary}{WGI for each clustering solution with clustering resolution
\eqn{k} or \eqn{r}}
}


\author{
  Simo Kitanovski \email{simo.kitanovski@uni-due.de}
}


\examples{
# input data
data("d_500", package = "scBubbletree")
A <- d_500$A
f <- d_500$f


b_k <- get_k(x = A,
           ks = 1:5,
           B_gap = 5,
           n_start = 100,
           iter_max = 200,
           kmeans_algorithm = "MacQueen",
           cores = 1)


b_r <- get_r(x = A,
            rs = c(0.1, 0.5, 1),
            B_gap = 5,
            n_start = 20,
            iter_max = 100,
            algorithm = "original",
            cores = 1)

get_gini_k(labels = f, obj = b_k)
get_gini_k(labels = f, obj = b_r)
}

\seealso{get_k, get_r, get_gini, get_bubbletree_kmeans, get_bubbletree_graph,
get_bubbletree_dummy, d_500, get_num_tiles, get_num_violins, get_cat_tiles}
