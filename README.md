# reu-2021

This repository is used for the 2021 North Carolina A&T State University and Elon University Joint Research Experience for Undergraduates (REU) in Mathematical Biology.

## Authors
- Gabrial Ackall: Student, Georgia Institute of Technology
- Connor Shrader: Student, University of Central Florida
- Dr. Seongtae Kim: Mentor, North Carolina A&T State University
- Yang Xue, Graduate student mentor, North Carolina A&T State University

## Project Summary

This project investigates the effectiveness of different regression techniques in predictive modeling with a particular focus on penalized regression. We are especially interested in high dimensional settings where the number of predictor variables (p) exceeds the number of observations (n). We considered the following models:

- Wrapper methods for subset selection
  - Forward selection
  - Backward selection
  - Forward stepwise selection
  - Backward stepwise selection
- Penalized regression techniques
  - Ridge regression
  - Least absolute shrinkage and selection operation (lasso)
  - Elastic-net (E-net)
  - Smoothly clipped absolute deviation (SCAD)
  - Minimax concave penalty (MCP)
- Non-linear (machine learning) methods
  - Random forests (RF)
  - Gradient boosting models (using XGBoost)
  - Support vector machines (SVM)

We compare these different models using Monte Carlo simulations and empirical data. 

Our simulation study uses a factorial design to study the effects of different settings on the effectiveness of each model. The factors we considered were:
- The number of observations
- The number of predictors
- The standard deviation of the random error in the data
- The correlation structure among predictors
- The strength of correlation between predictors

Our empirical study uses data from [The Cancer Genome Atlas](http://www.cancer.gov/tcga) (TCGA). The data set we used was cleaned by Patrick Breheny to test the `biglasso` package for `R`. The data set can be found [here](https://myweb.uiowa.edu/pbreheny/data/bcTCGA.html).

All statistical analysis in this project was done using version 4.1.0 of `R`. 

## Repository Navigation
- To see our final report for the REU, go to `latex/final-report/final-report.pdf`.
- To see the code used to run simulations, see the files in `code/monte-carlo`.
- To see the code used for the empirical data analysis, go to `code/empirical-data`.
- To access simulation results, see `results`.
- The models fit for the empirical data were too large to save on GitHub.
- To see the code used to generate any figures, go to `code/figures`. Any figures that were saved are in `figures`.

## Acknowledgments
- This research was conducted as a part of the North Carolina A&T State University and Elon University Joint Summer REU Program in Mathematical Biology and was funded by the National Science Foundation DMS# 1851957/1851981.
- The results in our project are in part based upon data generated by the TCGA Research Network: [http://www.cancer.gov/tcga](http://www.cancer.gov/tcga)
- Dr. Yokley (Elon University), Dr. Luke (North Carolina A&T), and Yang Xue for guidance on abstracts, presentations, and the research process.

## Contact Information

For any questions about the project, please send an email to either [connorshrader@knights.ucf.edu](mailto:connorshrader@knights.ucf.edu) or [gackall@gatech.edu](mailto:gackall@gatech.edu).
