\name{get_gini_k}



\alias{get_gini_k}




\title{
Gini impurity index computed for a number of clusters
}



\description{
The Gini impurity index allows us to quantitatively evaluate how well a set of
labels (categorical features) are split across a set of bubbles. We have a
completely perfect split (Gini impurity = 0) when each bubble is 'pure', i.e.
each bubble contains labels coming from distinct a class. In contrast to this,
we have completely imperfect split (Gini impurity = 1) when the relative
frequency distribution of the labels in each bubble is identical to the background relative frequency distribution of the labels.

Cell type predictions are a type of categorical features that are often used
to evaluate the goodness of the clustering. get_gini_k takes as input: 1) a vector
of labels for each cell (e.g. cell types) and 2) get_k object. Then it computes
for each \eqn{k} the mean Gini impurity, which is another way of finding an
optimal number of clusters \eqn{k}.
}



\usage{
get_gini(labels, clusters)
}


\arguments{
\item{labels}{character/factor vector of labels}
\item{get_k}{get_k object}
}


\details{
... comming soon ...
}


\value{
\item{cluster_gini_summary}{Gini impurity of each bubble and \eqn{k}}
\item{total_gini_summary}{Weighted Gini impurity index of the bubbletree for each \eqn{k}}
}


\author{
  Simo Kitanovski <simo.kitanovski@uni-due.de>
}


\examples{
get_gini(labels = sample(x = LETTERS[1:4], size = 100, replace = T),
         clusters = sample(x = letters[1:4], size = 100, replace = T))
}

\seealso{get_k, update_bubbletree, get_bubbletree, get_dummy_bubbletre,
get_gini_k}
