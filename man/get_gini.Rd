\name{get_gini}



\alias{get_gini}




\title{
Gini impurity index
}



\description{
The Gini impurity index allows us to quantitatively evaluate how well a set of
labels (categorical features) are partitioned across a set of bubbles. We have a
completely perfect split (Gini impurity = 0) when each bubble is 'pure', i.e.
each bubble contains labels coming from distinct a class. In contrast to this,
we have completely imperfect split (Gini impurity = 1) when the relative
frequency distribution of the labels in each bubble is identical to the
background relative frequency distribution of the labels.
}



\usage{
get_gini(labels, clusters)
}


\arguments{
\item{labels}{character or numeric vector of labels}
\item{clusters}{character or numeric vector of cluster IDs}
}


\details{
To measure the homogeneity of a cluster (or bubble) \eqn{i} of cells in terms of
\eqn{L} distinct cell type labels we used the Gini impurity index:

\eqn{G_i=\sum_{j=1}^{L} \pi_{ij}(1-\pi_{ij})}

with \eqn{\pi_{ij}} as the relative frequency of cell type \eqn{j} in cluster
\eqn{i}. In homogeneous (`pure`) clusters most \eqn{\pi}'s are close to either
1 or 0, and the Gini index takes on a small value close to zero. In clusters
with heterogenous (`impure') composition most $\pi$ are far from either 1 or 0,
and the Gini index approaches its maximum value close to 1. To compute the Gini
impurity of a bubbletree with $k$ bubbles we compute the weighted Gini impurity
(WGI):

\eqn{WGI=\sum_{i=1}^{k} G_i * n_i/n}

with \eqn{n_i} as the number of cells in cluster \eqn{i} and \eqn{n=\sum_i n_i}.
}

\value{
\item{gi}{Gini impurity of each bubble}
\item{wgi}{Weighted Gini impurity index of the bubbletree}
}


\author{
  Simo Kitanovski <simo.kitanovski@uni-due.de>
}


\examples{
get_gini(labels = sample(x = LETTERS[1:4], size = 100, replace = T),
         clusters = sample(x = letters[1:4], size = 100, replace = T))
}

\seealso{get_k, update_bubbletree, get_bubbletree, dummy_bubbletree,
get_gini_k, A}
