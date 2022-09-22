# scBubbletree


## Overview 
Single cell RNA sequencing has revolutionized biological research by allowing 
us to interrogate the expression of thousands of genes at single cell resolution. 
The rapid growth of scRNA-seq data has also created an unique set of challenges,
for instance, how do we perform *visual exploration* of this massive data and 
extract useful biological information.

scBubbletree was designed to overcome challenges associated with the rapid growth 
of the scale of scRNA-seq datasets, such as *overplotting*, and to accurately 
preserve the local and global structure of the data. The workflow of scBubbletree 
is easy-to-use and allows seamless integrate with popular approaches for scRNA-
seq data analysis.

## How to use scBubbletree

scBubbletree is an R-package available from Bioconductor: https://bioconductor.org/packages/IgGeneUsage/

To install this package, start R and enter:

```r
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("scBubbletree")
```

Multiple case studies are provided in the directory /vignettes

