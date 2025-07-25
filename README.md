# CMIMN: Conditional Mutual Information for constructing Microbiome Network
[![GitHub license](https://img.shields.io/github/license/solislemuslab/CMIMN?color=yellow)](https://github.com/solislemuslab/CMIMN/blob/main/LICENSE)
[![GitHub Issues](https://img.shields.io/github/issues/solislemuslab/CMIMN)](https://github.com/solislemuslab/CMIMN/issues)
![Code Size](https://img.shields.io/github/languages/code-size/solislemuslab/CMIMN?color=white)
[![GitHub Contributors](https://img.shields.io/github/contributors/solislemuslab/CMIMN)](https://github.com/solislemuslab/CMIMN/graphs/contributors)
![GitHub Last Commit](https://img.shields.io/github/last-commit/solislemuslab/CMIMN)

CMIMN is an R package designed to construct microbiome networks using conditional mutual information (CMI) to identify relationships between taxa. The package applies two levels of CMI computation, using quantile thresholds to filter edges in the resulting adjacency matrices, facilitating robust microbial interaction analysis.


## Table of Contents
- [Installation](#installation)
- [Running CMIMN Package](#Running-CMIMN-Package)
- [Example](#example)

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
### Parameters
- data: A numeric matrix where rows represent taxa and columns represent samples. If quantitative is TRUE, data will be log-transformed.
- q1: A numeric value representing the quantile threshold for filtering edges in the order 0 adjacency matrix.
- q2: A numeric value representing the quantile threshold for filtering edges in the order 1 adjacency matrix.
- quantitative: A logical value indicating if the data is quantitative. If TRUE, the data is log-transformed.

### Return Value
The function returns a list with the following components:

- G_order0: The adjacency matrix after order 0 calculation.
- G_order1: The adjacency matrix after order 1 calculation.
- Gval_order0: The CMI values for each taxa pair in the order 0 adjacency matrix.
- Gval_order1: The CMI values for each taxa pair in the order 1 adjacency matrix.
- quantile_order0: The quantile threshold used for filtering the order 0 adjacency matrix.
- quantile_order1: The quantile threshold used for filtering the order 1 adjacency matrix.
- sum_order0: The sum of edges in the order 0 adjacency matrix.
- sum_order1: The sum of edges in the order 1 adjacency matrix.

## Example 
```bash CMIMN
result <- conditional_MI( data ,q1 = 0.7, q2 = 0.95,quantitative = TRUE)
```
## Reporting Issues and Asking Questions

If you encounter a bug, experience a failed function, or have a feature request, please open an issue in the GitHub [issue tracker](https://github.com/solislemuslab/CMIMN/issues). 

## License

CMIMN is licensed under the [GNU General Public License v3.0 (GPL-3)](https://www.gnu.org/licenses/gpl-3.0.html). &copy; Solis-Lemus Lab (2024).


## Citation

If you use CMIMN in your work, we kindly ask that you cite the following paper:

```bibtex

@article{aghdam2024human,
  title={Human limits in machine learning: prediction of potato yield and disease using soil microbiome data},
  author={Aghdam, Rosa and Tang, Xudong and Shan, Shan and Lankau, Richard and Sol{\'\i}s-Lemus, Claudia},
  journal={BMC bioinformatics},
  volume={25},
  number={1},
  pages={366},
  year={2024},
  publisher={Springer}
}

@article{aghdam2025leveraging,
  title={Leveraging Bayesian Networks for Consensus Network Construction and Multi-Method Feature Selection to Decode Disease Prediction},
  author={Aghdam, Rosa and Shan, Shan and Lankau, Richard and Solis-Lemus, Claudia},
  journal={bioRxiv},
  pages={2025--04},
  year={2025},
  publisher={Cold Spring Harbor Laboratory}
}
@article{aghdam2025cminet,
  title={CMiNet: An R Package and User-Friendly Shiny App for Constructing Consensus Microbiome Networks},
  author={Aghdam, Rosa and Sol{\'\i}s-Lemus, Claudia},
  journal={bioRxiv},
  pages={2025--05},
  year={2025},
  publisher={Cold Spring Harbor Laboratory}
}
```
