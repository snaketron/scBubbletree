# scBubbletree

[![platform](http://www.bioconductor.org/shields/availability/devel/scBubbletree.svg)](https://www.bioconductor.org/packages/devel/bioc/html/scBubbletree.html#archives)
[![](https://img.shields.io/badge/devel%20version-0.22-green.svg)](https://www.bioconductor.org/packages/scBubbletree)
#[![codecov](https://codecov.io/gh/GuangchuangYu/ggtree/branch/master/graph/badge.svg)](https://codecov.io/gh/GuangchuangYu/ggtree)
#[![Awesome](https://cdn.rawgit.com/sindresorhus/awesome/d7305f38d29fed78fa85652e3a63e154dd8e8829/media/badge.svg)](https://awesome-r.com/#awesome-r-graphic-displays)

#[![platform](http://www.bioconductor.org/shields/availability/devel/scBubbletree.svg)](https://www.bioconductor.org/packages/devel/bioc/html/scBubbletree.html#archives)
#`r badge_bioc_release("scBubbletree", "green")`
#[![codecov](https://codecov.io/gh/GuangchuangYu/ggtree/branch/master/graph/badge.svg)](https://codecov.io/gh/GuangchuangYu/ggtree)
#[![Awesome](https://cdn.rawgit.com/sindresorhus/awesome/d7305f38d29fed78fa85652e3a63e154dd8e8829/media/badge.svg)](https://awesome-r.com/#awesome-r-graphic-displays)


## Overview 
Single cell RNA sequencing has revolutionized biological research by allowing 
us to interrogate the expression of thousands of genes at single cell resolution. 
The rapid growth of scRNA-seq data has also created an unique set of challenges,
for instance, how can we perform *visual exploration* of this massive data in 
order to extract useful biological information from our scRNA-seq samples.

scBubbletree was designed to overcome challenges associated with the rapid growth 
of the scale of scRNA-seq datasets, such as *overplotting*, and to accurately 
preserve the local and global structure of the data. The workflow of scBubbletree 
is easy-to-use and allows seamless integration with popular approaches for scRNA-
seq data analysis.

## How to use scBubbletree

scBubbletree is an R-package available from Bioconductor: 

https://bioconductor.org/packages/scBubbletree/ (coming soon)

To install this package, start R and enter:

```r
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("scBubbletree")
```

Multiple case studies are provided in the directory /vignettes


## Workflow & output 

![alt text](inst/extdata/logo.png)
