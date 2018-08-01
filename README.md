# HDclust
Clustering with Hidden Markov Model on Variable Blocks in R

This repository contains two implementations of "Clustering with Hidden Markov Model on Variable Blocks" algorithm 
(for reference, see Lin Lin and Jia Li, "Clustering with hidden Markov model on variable blocks," 
Journal of Machine Learning Research, 18(110):1-49, 2017.). The algorithm performs clustering of high dimensional data using the density modes. 
The algorithm can take into account sequencial dependence among groups of variables in the data. That is particularly useful in analyzing Flow and Mass Cytometry
data.

The package HDclust well-documented user-friendly R implementation. For more experienced users, who would 
like to utilize computational clusters, we also provide an OpenMPI C implementation available on GitHub (https://github.com/yevhenVT/HDclustC). We recommend
starting with R version and moving to C only if needed. To install the released R version from CRAN use install.packages("HDclust").
To install the development R version from GitHub use devtools::install_github("yevhenVT/HDclust", build_vignettes=TRUE). 
