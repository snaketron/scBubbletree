\name{get_k}



\alias{get_k}




\title{
Finding optimal number \eqn{k} of clusters
}



\description{
To perform k-means clustering we must specify a number \eqn{k} of clusters.
Data-driven metrics, such as the Gap statistic or the within-cluster sum of
squares (WCSS), can be used to infer appropriate \eqn{k} from the data. get_k
computes the Gap statistic and WCSS for a number of clusters \eqn{ks}.
}



\usage{

get_k(x,
     ks,
     B_gap = 20,
     n_start = 100,
     iter_max = 200,
     kmeans_algorithm = "MacQueen",
     cores = 1,
     verbose = TRUE)
}


\arguments{
\item{x}{numeric matrix \eqn{A^{n x f}} with \eqn{n} cells, and \eqn{f}
low-dimensional projections}
\item{ks}{integer vector, \eqn{k} values to consider}
\item{B_gap}{integer, number of Monte Carlo ("bootstrap") samples taken when
computing the Gap statistic (see documentation of function clusGap, R-package
cluster)}
\item{n_start, iter_max, kmeans_algorithm}{parameters for k-means clustering,
see documentation of function k-means, R-package stats}
\item{cores}{integer, number of PC cores for parallel execution}
\item{verbose}{logical, progress messages}
}


\details{
To compute the Gap statistic get_k adapts the algorithm of function clustGap
from R-package cluster (version 2.1.3). For k-means clustering get_k uses
the function kmeans implemented in R-package stats (version 4.2.0). For
additional information see the respective documentations.
}

\value{
\item{boot_obj}{The results: k-means clustering solutions, the Gap statistic
and WCSS}
\item{gap_stats_summary, wcss_stats_summary}{main results; Gap statistic
and WCSS estimates. Means, standard errors and 95\% confidence intervals are
provided for each \eqn{k}}
\item{gap_stats, wcss_stats}{intermediate results; Gap statistic and WCSS
estimates for each \eqn{k} and bootstrap iteration \eqn{b}}
}


\author{
  Simo Kitanovski <simo.kitanovski@uni-due.de>
}


\examples{
# input data
data("d_500", package = "scBubbletree")
A <- d_500$A

b <- get_k(x = A,
           ks = 1:5,
           B_gap = 10,
           n_start = 100,
           iter_max = 200,
           kmeans_algorithm = "MacQueen",
           cores = 1,
           verbose = TRUE)

b$gap_stats_summary
}

\seealso{get_r, get_bubbletree_dummy, get_bubbletree_louvain,
get_bubbletree_kmeans, get_gini, get_gini_k, d_500, get_num_tiles,
get_num_violins, get_cat_tiles}
