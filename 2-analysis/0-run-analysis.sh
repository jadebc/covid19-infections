#!/bin/bash

R CMD BATCH 1-obtain-priors-state.R
R CMD BATCH 2-est-expected-cases-state.R
R CMD BATCH 3-est-expected-cases-state-perf-testing.R
R CMD BATCH 4-summarize-results.R 
