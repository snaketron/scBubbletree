\name{get_gini}



\alias{get_gini}




\title{
Gini impurity index computed for a clustering solution and a vector of
categorical cell feature labels
}



\description{
How well is a set of categorical feature labels (e.g. cell type predictions)
partitioned accross the different clusters of a clustering solution? We can
assess this using the Gini impurity index (see details below).

Inputs are two equal-sized vectors:

1) clusters IDs

2) labels

Output:

1) cluster-specific purity -> Gini impurity (GI) index

2) clustering solution impurity  -> Weighted Gini impurity (WGI) index
}



\usage{
get_gini(labels, clusters)
}


\arguments{
\item{labels}{character or numeric vector of labels}
\item{clusters}{character or numeric vector of cluster IDs}
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
\item{gi}{Gini impurity of each bubble}
\item{wgi}{Weighted Gini impurity index of the bubbletree}
}


\author{
  Simo Kitanovski \email{simo.kitanovski@uni-due.de>}
}


\examples{
get_gini(labels = base::sample(x = LETTERS[1:4], size = 100, replace = TRUE),
         clusters = base::sample(x = letters[1:4], size = 100, replace = TRUE))
}

\seealso{get_k, get_r, get_bubbletree_kmeans, get_bubbletree_dummy,
get_bubbletree_graph, get_gini_k, d_500}
