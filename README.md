# CMIMN: Conditional Mutual Information for constructing Microbiome Network

\texttt(CMIMN) is an R package designed to construct microbiome networks using conditional mutual information (CMI) to identify relationships between taxa. The package applies two levels of CMI computation, using quantile thresholds to filter edges in the resulting adjacency matrices, facilitating robust microbial interaction analysis.


## Table of Contents
- [Installation](#installation)
- [Running CMIMN Package](#Running-CMIMN-Package)

## Installation
```bash
install.packages("devtools")
devtools::install_github("solislemuslab/CMIMN")
```

## Running CMIMN Package
### loading the Data
We use the American Gut data from [SpiecEasi package](https://github.com/zdk123/SpiecEasi) to run CMiNet algorithm to construct consensus microbiome network. 
First, load CMIMN and the American Gut Project data (included with the [SpiecEasi package](https://github.com/zdk123/SpiecEasi)), which is automatically loaded alongside CMIMN).

```bash
library(CMIMN)
data = amgut1.filt
```
## Parameters
- data: A numeric matrix where rows represent taxa and columns represent samples. If quantitative is TRUE, data will be log-transformed.
- q1: A numeric value representing the quantile threshold for filtering edges in the order 0 adjacency matrix.
- q2: A numeric value representing the quantile threshold for filtering edges in the order 1 adjacency matrix.
- quantitative: A logical value indicating if the data is quantitative. If TRUE, the data is log-transformed.

```bash
result <- conditional_MI(quantitative = TRUE, q1 = 0.7, q2 = 0.95)
```
