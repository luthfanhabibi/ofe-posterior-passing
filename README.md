# Gaussian process model for On-Farm Experiment (OFE) field using Bayesian posterior passing technique

## Overview

This repository contains the codebase used for our paper titled **"Assessing uncertainty of soybean yield response to seeding rates in on-farm experiments using Bayesian posterior passing technique (Link: https://doi.org/10.1016/j.eja.2025.127651)**. 

## Code Structure

### 1. Generate Data.R
Since we are not sharing our data used for the analysis, we decided to create code to generate data by using the statistics of our original dataset (means and sd) for presenting the structure of our dataset, including the setting of On-farm experimental design used in the study.

### 2. Run Stan.R
In this code, the Gaussian process model can be performed using RStan code. Firstly, we run the noninformative model and take the means and sd of the posteriors (quadcoef, firstcoef, and beta1), then directly add it into the informative model setup

### 3. Calculate Loglik for LOO.R
This code is for performing leave-one-out cross-validation (LOO-CV) through Pareto-smoothed importance sampling (PSIS) estimation approach to compare predictive performance between the models.

### 4. Visualization.R
This code is to create visualization for the yield response to the OFE treatment design.

### Citation

If you use this code or dataset in your research, please cite our paper:

```bibitex
@article{HABIBI2025127651,
title = {Assessing uncertainty of soybean yield response to seeding rates in on-farm experiments using Bayesian posterior passing technique},
journal = {European Journal of Agronomy},
volume = {168},
pages = {127651},
year = {2025},
issn = {1161-0301},
doi = {https://doi.org/10.1016/j.eja.2025.127651},
url = {https://www.sciencedirect.com/science/article/pii/S1161030125001479},
author = {Luthfan Nur Habibi and Tsutomu Matsui and Takashi S.T. Tanaka},
keywords = {Noninformative prior, Informative prior, Agronomic optimum seeding rate, Economic optimum seeding rate, Gaussian process model},
abstract = {Understanding the optimum seeding rate for soybeans is crucial to maximizing the revenue of farmers amidst rising seed costs. On-farm experimentation (OFE) is often performed over several years to gather information about the uncertainties of yield response to different seeding rates. This study aimed to testify the potential of the posterior passing technique under the Bayesian approach by incorporating the results from preceding OFE trials as the prior information of the following year's trials to reduce the uncertainty of optimum seeding rate input. OFE trials were conducted in Gifu, Japan, over two growing seasons. A Gaussian process model was used to evaluate the impact of the seeding rate on yield while accounting for spatial variations in the fields. Two types of prior distributions were tested, including noninformative (no prior knowledge) and informative (based on previous OFE trials) priors. Model established using informative priors could improve predictive performance and reduce uncertainty in yield response for subsequent trials. However, the utilization of posterior passing also needs to be cautious, as prior distribution with small variance may lead to unreliable results to the following yield response. In the current results, providing a single general optimum seeding rate is impractical, as each model contribute to a different prescription. Nonetheless, as the OFE framework is a continuous learning process, integrating the trial results with posterior passing technique offers a promising way to improve confidence in determining optimum seeding rates if there are more available datasets.}
}
```


For any questions or inquiries, please contact:
Luthfan Nur Habibi (noerhabibii@gmail.com)
