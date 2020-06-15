# Substantial underestimation of COVID-19 infection in the United States due to incomplete testing and imperfect test accuracy

## Overview
To date, coronavirus testing in the US has been extremely limited. Confirmed COVID-19 case counts underestimate the total number of infections in the population. We estimated the total COVID-19 infections – both symptomatic and asymptomatic – in the US in March 2020. We used a semi-Bayesian approach to correct for bias due to incomplete testing and imperfect test performance.

### Directory structure

**`0-config.R`** : configuration file that sets data directories, sources base functions, and loads required libraries

**`0-base-functions`** : folder containing scripts with functions used in the analysis
* `0-base-functions.R`: R script containing general functions used across the analysis
* `0-bias-corr-functions.R`: R script containing functions used in bias correction
* `0-bias-corr-functions-undertesting.R`: R script containing functions used in bias correction to estimate the percentage of underestimation due to incomplete testing vs. imperfect test accuracy
* `0-prior-functions.R`: R script containing functions to generate priors

**`1-data`** : folder containing data processing scripts

**`2-analysis`** : folder containing analysis scripts. To rerun all scripts in this subdirectory, run the bash script `0-run-analysis.sh`.

* `1-obtain-priors-state.R`: obtain priors for each state
* `2-est-expected-cases-state.R`: estimate expected cases in each state
* `3-est-expected-cases-state-perf-testing.R`: estimate expected cases in each state, estimate the percentage of underestimation due to incomplete testing vs. imperfect test accuracy
* `4-obtain-testing-protocols.R`: find testing protocols for each state.
* `5-summarize-results.R`: summarize results; obtain results for in text numerical results. 

**`3-figure-table-scripts`** : folder containing figure scripts. To rerun all scripts in this subdirectory, run the bash script `0-run-figs.sh`.

* `1-fig-testing.R`: creates plot of testing patterns by state over time
* `2-fig-cases-usa-state-bar.R`: creates bar plot of confirmed vs. estimated infections by state
* `3-fig-map-usa-state.R`: creates map of confirmed vs. estimated infections by state
* `4-fig-priors.R`: creates figure with priors for US as a whole
* `5-fig-density-usa.R`: creates figure of distribution of estimated cases in the US 
* `6-table-data-quality.R`: creates table of data quality grading from COVID Tracking Project
* `7-fig-testpos.R`: creates figure of the probability of testing positive among those tested by state

**`4-figures`** : folder containing figure files. 

**`5-results`** : folder containing analysis results objects. 

Contributors: Jade Benjamin-Chung, Sean L. Wu, Anna Nguyen, Stephanie Djajadi, Nolan N. Pokpongkiat, Anmol Seth, Andrew Mertens
