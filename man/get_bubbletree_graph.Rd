\name{get_bubbletree_graph}



\alias{get_bubbletree_graph}




\title{
Louvain clustering and hierarchical grouping of \eqn{k'} clusters (bubbles)
}



\description{
\code{get_bubbletree_graph} has two main inputs:

1. numeric matrix \eqn{A^{n\times f}}, which represents a low-dimensional
projection (obtained e.g. by PCA) of the original high-dimensional scRNA-seq
data, with \eqn{n} rows as cells and \eqn{f} columns as low-dimension features.

2. clustering resolution \eqn{r}

The function \code{get_bubbletree_graph} performs two main operations. First, it
performs Louvain clustering to identify groups (bubbles) of transcriptionally
similar cells; second, it organizes the bubbles in a hierarchical dendrogram
(bubbletree) which adequatly represents inter-cluster relationships.
}



\usage{

get_bubbletree_graph(x,
                     r,
                     B = 200,
                     N_eff = 100,
                     n_start = 20,
                     iter_max = 100,
                     algorithm = "original",
                     knn_k = 20,
                     hclust_method = "average",
                     hclust_distance = "euclidean",
                     cores = 1,
                     round_digits = 2,
                     show_simple_count = FALSE,
                     verbose = TRUE)
}


\arguments{
\item{x}{numeric matrix (\eqn{A^{n\times f}} with \eqn{n} cells, and \eqn{f}
low-dimensional projections of the original single cell RNA-seq dataset)}
\item{r}{number, clustering resolution}
\item{B}{integer, number of bootstrap iterations to perform in order to
generate bubbletree. If \eqn{B=200} (default), cluster centroids are used to
compute inter-cluster distances and \eqn{N_{eff}} is ignored, i.e. all cells
are used to compute centroids.}
\item{N_eff}{integer, number of cells to draw randomly from each cluster when
computing inter-cluster distances.}
\item{n_start, iter_max}{parameters for Louvain clustering, see documentation
of function \code{FindClusters}, R-package \code{Seurat}}
\item{algorithm}{character, four clustering algorithms: 'original',
'LMR', 'SLM' and 'Leiden', see documentation of function \code{FindClusters},
R-package \code{Seurat}}
\item{knn_k}{integer, defines \eqn{k} for the k-nearest neighbor algorithm, see
documentation of function \code{FindClusters}, R-package \code{Seurat}}
\item{hclust_method}{the agglomeration method to be used (default = average).
See documentation of \code{stats::hclust}}
\item{hclust_distance}{distance measure to be used: euclidean (default) or
manhattan, see documentation of \code{stats::dist}}
\item{cores}{integer, number of PC cores for parallel execution}
\item{round_digits}{integer, number of decimal places to keep when showing the
relative frequency of cells in each bubble}
\item{show_simple_count}{logical, if \code{show_simple_count}=T, cell counts
in each bubble will be divided by 1,000 to improve readability. This is only
useful for samples that are composed of millions of cells.}
\item{verbose}{logical, progress messages}
}


\details{
For Louvain clustering \code{get_bubbletree_graph} uses the function
\code{FindClusters} implemented in R-package \code{Seurat}. For additional
information on the clustering procedure see the documentation of
\code{FindClusters}. To organize the resulting clusters in a hierarchical
dendrogram, then the following steps are performed:

1. In bootrap iteration \eqn{b} from \eqn{1:B}

2. draw up to \eqn{N_{eff}} number of cells at random from each cluster without
   replacement

3. compute distances (in space \eqn{A^{n\times f}}) between all pairs
   of cells in cluster \eqn{i} and cluster \eqn{j}

4. compute mean distance between cluster \eqn{i} and \eqn{j} and
   populate inter-cluster distance matrix \eqn{D_{b}^{k \times k}}

5. perform hierarchical clustering with user-specified agglomeration method
   based on \eqn{D_{b}^{k \times k}} to generate dendrogram \eqn{H_b}

6. quantify branch robustness in \eqn{H} by counting how many times each branch
   is found among bootrap dendrograms \eqn{H_b}
}

\value{
\item{A}{input x matrix}
\item{k}{number of clusters}
\item{r}{clustering resolution}
\item{ph}{boot_ph: bootstrap dendrograms \eqn{H_b}; main_ph: bubbletree \eqn{H}}
\item{pair_dist}{inter-cluster distances used to generate the dendrograms}
\item{cluster}{cluster assignments of each cell}
\item{input_par}{list of all input parameters}
\item{tree}{ggtree bubbletree object}
\item{tree_meta}{meta-data associated with the bubbletree}
}


\author{
  Simo Kitanovski \email{simo.kitanovski@uni-due.de}
}


\examples{
# input data
data("d_500", package = "scBubbletree")
A <- d_500$A

b <- get_bubbletree_graph(x = A,
                          r = 1,
                          B = 200,
                          N_eff = 100,
                          n_start = 20,
                          iter_max = 100,
                          algorithm = "original",
                          knn_k = 20,
                          hclust_method = "average",
                          hclust_distance = "euclidean",
                          cores = 1,
                          round_digits = 2,
                          show_simple_count = FALSE)

b$tree
}

\seealso{get_k, get_bubbletree_dummy, get_bubbletree_kmeans,
         get_bubbletree_comparison, get_gini, get_gini_k, d_500,
         get_num_tiles, get_num_violins, get_cat_tiles}

