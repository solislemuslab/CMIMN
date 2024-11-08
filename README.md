# CMIMN: Conditional Mutual Information for constructing Microbiome Network

CMIMN is an R package designed to generate microbiome network. 

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
