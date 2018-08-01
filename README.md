# HDclust
Clustering with Hidden Markov Model on Variable Blocks

This repository contains two implementations of "Clustering with Hidden Markov Model on Variable Blocks" algorithm 
(for reference, see Lin Lin and Jia Li, "Clustering with hidden Markov model on variable blocks," 
Journal of Machine Learning Research, 18(110):1-49, 2017.). The algorithm performs clustering of high dimensional data using the density modes. 
The algorithm can take into account sequencial dependence among groups of variables in the data. That is particularly useful in analyzing Flow and Mass Cytometry
data.

The R package HDclust located in the directory R contains well-documented user-friendly implementation. For more experienced users, who would 
like to utilize computational clusters, we also provide an OpenMPI C implementation located in the directory C. We recommend
starting with R version and moving to C only if needed. To install the released version from CRAN use install.packages("HDclust").
To install the development version from GitHub use devtools::install_github("devtools::install_github("yevhenVT/HmmVb/tree/master/R/HMMVB")"). 
To install the C version run "make" inside the C directory.
For more documentation on C implementation consult C/README file. 
