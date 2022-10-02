\name{d_ccl}
\alias{d_ccl}
\docType{data}
\title{Dataset: scRNA-seq data of 3,918 cells from 5 adenocarcinoma cell lines}
\description{
d_ccl is a list with 3 elements:

1. A = numeric matrix with n=3,918 rows for cells and f=15 principal components

2. m = data.frame meta data

3. e = numeric matrix containing normalized gene expressions of 5 marker genes
}
\usage{data("d_ccl", package = "scBubbletree")}
\format{
Format of d_ccl: list
}
\details{
d_ccl is a scRNA-seq dataset containing a mixture of 3,918 cells from five
human lung adenocarcinoma cell lines (HCC827, H1975, A549, H838 and
H2228). The dataset is available here:

https://github.com/LuyiTian/sc_mixology/blob/master/data/
sincell_with_class_5cl.RData

The library has been prepared with 10x Chromium platform and sequenced with
Illumina NextSeq 500 platform. Raw data has been processed with Cellranger.
The tool demuxlet has been used to predict the identity of each cell based on
known genetic differences between the different cell lines.

See R script inst/script/get_d_ccl.R to see how this dataset was created.
}
\source{
https://github.com/LuyiTian/sc_mixology/blob/master/data/
sincell_with_class_5cl.RData
}
\references{
Tian, Luyi, et al. "Benchmarking single cell RNA-sequencing analysis
pipelines using mixture control experiments." Nature methods 16.6 (2019):
479-487
}
\examples{
data("d_ccl", package = "scBubbletree")

A <- d_ccl$A
base::dim(A)

m <- d_ccl$m
utils::head(m)

e <- d_ccl$e
base::dim(e)

}
\keyword{datasets}
