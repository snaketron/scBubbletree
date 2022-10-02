\name{d_500}
\alias{d_500}
\docType{data}
\title{Dataset: 500 PBMCs}
\description{
d_500 is a list with 3 elements:

1. A = numeric matrix \eqn{A^500x15} with n=500 rows for PBMCs and f=15
principal components.

2. f = character vector \eqn{f} of length 500. Each element in \eqn{f}
represents the predicted cell type of a specific cell.

3. fs = numeric matrix containing normalized gene expressions of 12 marker
genes in 500 cells.
}
\usage{data("d_500", package = "scBubbletree")}
\format{
Format of d_500: list
}
\details{
This data is a sample drawn from a larger dataset of 2,700 PBMCs. The original
dataset was processed as described in vignette (accessed 23, Sep, 2022):

https://satijalab.org/seurat/articles/multimodal_reference_mapping.html

See R script inst/script/get_d_500.R to see how this dataset was created.
}
\source{
https://satijalab.org/seurat/articles/multimodal_reference_mapping.html
}
\examples{
data("d_500", package = "scBubbletree")

A <- d_500$A
base::dim(A)

f <- d_500$f
base::table(f)

fs <- d_500$fs
base::dim(fs)

}
\keyword{datasets}
