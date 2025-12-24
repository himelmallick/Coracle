# Coracle: Conformalized Multiview Learning

---

**Coracle** is an R package for a conformalized framework for multimodal AI with **continuous outcomes**. 

It has several advantages:
- adapts to early, late, and intermediate fusion.
- provides theoretical marginal confidence guarantees and achieves valid finite-sample coverage without relying on distributional assumptions.

---

## ⚙️ Installation

You can install the development version directly from GitHub:

```r
install.packages("devtools")
devtools::install_github("himelmallick/Coracle")
library(Coracle)
```

---

## 🚀 Quick Example

Here is a minimal call to `Coracle()`:

```r
cml <- Coracle::Conformal(
  fit = NULL,           # A fitted \emph{IntegratedLearner} object from early/late fusion
  fit_coop = NULL,      # A fitted \emph{BayesCOOP} object for intermediate fusion
  data_calib,           # list with feature_table, sample_metadata and feature_metadata (calibration set)
  data_valid,           # list with feature_table, sample_metadata and feature_metadata (validation set)
  fusion_choice = c("late", "early", "intermediate"), # choice of different fusion schemes
  conf_level = 0.95     # desired marginal confidence level
)
```

### What do I get back?

A list with components:

- `df`: A dataframe containing the Coracle output for different fusion choices
- `coverage`: A list containing the coverage for different fusion choices
- `conf_level`: Desired marginal confidence level

---

## 📘 Full Tutorial

For an in-depth workflow — including real data preprocessing, baseline comparisons, and performance benchmarking — please see the full tutorial:

📄 [View the Coracle Tutorial](https://raw.githack.com/himelmallick/Coracle/master/vignettes/Coracle.html)

---

## 📚 Citation

---

## 🐞 Issues

We are happy to troubleshoot any issues with the package. Please contact the authors via email or open an issue in the [GitHub repository](https://github.com/himelmallick/Coracle/issues).

