# Bayesian Computational Modeling of Learning

This repository contains the code and materials for the seminar "Bayesian Computational Modeling of Learning", part of the Psychology Master's program at Goethe University Frankfurt (WS 2025/26), taught by Prof. Dr. Garvin Brod, Dr. Lucas Lörch and Dr. Carlo Vreden. 

## Project Overview

This project explores the reverse-engineering approach of Hierarchical Bayesian Models (HBMs) to explain human inductive biases, demonstrating how learners exploit environmental hierarchical structures to form overhypotheses that restrict the hypothesis space and enable rapid generalization from sparse data. This project uses `renv` for package dependency management and the `targets` package to manage the analysis pipeline. The presentation held in the seminar is available at this link: [jlschnatz.github.io/hbm/](https://jlschnatz.github.io/hbm/), and the written manuscript can be found in the folder [`manuscript/article.pdf`](manuscript/article.pdf).

## Reproducibility
 
First, clone the repository and navigate into the project directory using your terminal:

```bash
git clone https://github.com/jlschnatz/hbm.git
cd hbm
```

Then, to reproduce the results, figures, and presentation, follow these steps in R:
```r
# 1. Install required packages
install.packages(c("renv", "targets"))

# 2. Restore the specific R-package environment and versions
renv::restore()

# 3. Run the analysis pipeline 
targets::tar_make()
```

## Author

Jan Luca Schnatz

Department of Psychology, Goethe University Frankfurt

## License

The code and computational materials in this repository are licensed under the [MIT License](LICENSE.md). Everything else is licensed under a [CC BY-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/) License.
