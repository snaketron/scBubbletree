\name{get_bubbletree_louvain}



\alias{get_bubbletree_louvain}




\title{
Louvain clustering and hierarchical grouping of \eqn{k'} clusters (bubbles)
}



\description{
get_bubbletree_louvain takes two main inputs:

1. numeric matrix \eqn{A^{n\times f}}, which represents a low-dimensional
projection (obtained e.g. by PCA) of the original high-dimensional scRNA-seq
data, with \eqn{n} rows as cells and \eqn{f} columns as low-dimension features.

2. clustering resolution \eqn{r}

The function get_bubbletree_louvain performs two main operations. First, it
performs Louvain clustering to identify groups (bubbles) of transcriptionally
similar cells; second, it organizes the bubbles in a hierarchical dendrogram
(bubbletree) which adequatly represents inter-cluster relationships.

}



\usage{

get_bubbletree_louvain(x,
                       r,
                       B = 100,
                       N_eff = 200,
                       n_start = 20,
                       iter_max = 100,
                       louvain_algorithm = "original",
                       cores = 1,
                       seed = NULL,
                       round_digits = 2,
                       show_simple_count = F)
}


\arguments{
\item{x}{numeric matrix (\eqn{A^{n\times f}} with \eqn{n} cells, and \eqn{f}
low-dimensional projections of the original single cell RNA-seq dataset)}
\item{r}{number, clustering resolution}
\item{B}{integer, number of bootstrap iterations to perform in order to
generate bubbletree}
\item{N_eff}{integer, number of cells to draw randomly from each cluster when
computing inter-cluster distances. Maximum available number of cells are used
for clusters that contain lower number of cells than N_eff}
\item{n_start, iter_max, louvain_algorithm}{parameters for Louvain clustering,
see documentation of function Louvain, R-package Seurat}
\item{cores}{integer, number of PC cores for parallel execution}
\item{seed}{number, for reproducing results}
\item{round_digits}{integer, number of decimal places to keep when showing the
relative frequency of cells in each bubble}
\item{show_simple_count}{logical, if show_simple_count=T, cell counts in each
bubble will be divided by 1,000 to improve readability. This is only useful for
samples that are composed of millions of cells.}
}


\details{
For Louvain clustering get_bubbletree uses the function FindClusters implemented
in R-package Seurat. For additional information on the clustering procedure see
the documentation of FindClusters. To organize the resulting clusters in a
hierarchical dendrogram the algorithm performs the following steps:

1. In bootrap iteration \eqn{b} from \eqn{1:B}

2. draw up to \eqn{N_{eff}} number of cells at random from each cluster without
   replacement

3. compute Euclidean distances (in space \eqn{A^{n\times f}}) between all pairs
   of cells in cluster \eqn{i} and cluster \eqn{j}

4. compute mean Euclidean distance between cluster \eqn{i} and \eqn{j} and
   populate inter-cluster distance matrix \eqn{D_{b}^{k \times k}}

5. perform hierarchical clustering with average linkage based on
   \eqn{D_{b}^{k \times k}} to generate dendrogram \eqn{H_b}

6. compute average distance matrix \eqn{\hat{D}} and use is as input to build
   consensus hierarchical dendrogram (\eqn{\hat{H}}; bubbletree) with average
   linkage

7. quantify branch robustness in \eqn{\hat{H}} count how many times each branch
   is found among bootrap dendrograms (\eqn{H_b}

8. visualize the bubbletree (\eqn{\hat{H}}) with R-package ggtree
}

\value{
\item{A}{input x matrix}
\item{k}{number of clusters}
\item{r}{clustering resolution}
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
data(A)
head(A)

b <- get_bubbletree_louvain(x = A,
                            r = 1,
                            B = 200,
                            N_eff = 100,
                            n_start = 20,
                            iter_max = 100,
                            louvain_algorithm = "original",
                            cores = 1,
                            seed = 1234,
                            round_digits = 2,
                            show_simple_count = F)

b$tree
}

\seealso{get_k, get_bubbletree_dummy, get_bubbletree_kmeans, get_gini,
get_gini_k, A, fs, f, get_num_tiles, get_num_violins, get_cat_tiles}
