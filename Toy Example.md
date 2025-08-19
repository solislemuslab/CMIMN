# Toy Example: Running CMIMN on a Small Dataset

This tutorial provides a **step-by-step toy example** showing how to run CMIMN on a small synthetic dataset.  
It illustrates how to prepare the input, apply CLR transformation, and run the algorithm with different parameter settings.

---

## 1. Install and Load Packages

```R
# Install from GitHub if not already installed
# devtools::install_github("solislemuslab/CMIMN")

library(CMIMN)
library(compositions)   #  for CLR normalization
```

---

## 2. Create a Small Toy Dataset

Here, we simulate a 5x6 count matrix with some zeros:

```R
set.seed(123)
toy_data <- matrix(sample(0:50, 30, replace = TRUE), nrow = 5, ncol = 6)
rownames(toy_data) <- paste0("Sample", 1:5)
colnames(toy_data) <- paste0("Taxon", 1:6)
toy_data
```

---

## 3. Apply CLR Transformation

CMIMN can be run on **log-transformed** or **CLR-transformed** data.  

```R
toy_data <- as.matrix(toy_data)
clr_data <- clr(toy_data + 1)   # add pseudo-count
```


---

## 4. Run CMIMN
### Run CMIMN  on  **CLR-transformed** data

```R
result <- conditional_MI(as.matrix(clr_data), q1 = 0.7, q2 = 0.95, quantitative = FALSE)
```

### Run CMIMN  on  **log-transformed** data
```R
result <- conditional_MI( toy_data ,q1 = 0.7, q2 = 0.95,quantitative = TRUE)
```

# View adjacency matrix
```R
result$G_order1
```

---

## 5. Interpretation

- The **adjacency matrix** shows inferred conditional dependencies.  
- Thresholds (`q1`, `q2`) control sparsity:
  - Lower `q1` → more edges
  - Higher `q2` → stricter filtering  
- We recommend trying multiple values to assess robustness.

---

## 6. Tips

- Always add a **pseudo-count** to avoid log(0).  
- For real microbiome data, apply appropriate normalization before CLR.  
- Use **synthetic or toy datasets** first to confirm reproducibility.

---

## References

- Friedman & Alm (2012) *PLoS Comput Biol* – network benchmarking  
- Kurtz et al. (2015) *PNAS* – compositional methods in microbiome networks
