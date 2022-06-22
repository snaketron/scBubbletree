\name{get_gini}



\alias{get_gini}




\title{
Gini impurity index
}



\description{
The Gini impurity index allows us to quantitatively evaluate how well a set of
labels (categorical features) are split across a set of bubbles. We have a
completely perfect split (Gini impurity = 0) when each bubble is 'pure', i.e.
each bubble contains labels coming from distinct a class. In contrast to this,
we have completely imperfect split (Gini impurity = 1) when the relative
frequency distribution of the labels in each bubble is identical to the background relative frequency distribution of the labels.
}



\usage{
get_gini(labels, clusters)
}


\arguments{
\item{labels}{character/factor vector of labels}
\item{clusters}{character/factor vector of bubble IDs}
}


\details{
... comming soon ...
}

\value{
\item{cluster_gini}{Gini impurity of each bubble}
\item{total_gini}{Weighted Gini impurity index of the bubbletree}
}


\author{
  Simo Kitanovski <simo.kitanovski@uni-due.de>
}


\examples{
get_gini(labels = sample(x = LETTERS[1:4], size = 100, replace = T),
         clusters = sample(x = letters[1:4], size = 100, replace = T))
}

\seealso{get_k, update_bubbletree, get_bubbletree, dummy_bubbletree, get_gini_k}
