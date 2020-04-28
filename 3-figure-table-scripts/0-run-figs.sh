#!/bin/bash

R CMD BATCH 1-fig-testing.R &
R CMD BATCH 2-fig-cases-usa-state-bar.R &
R CMD BATCH 3a-fig-map-usa-state.R &
R CMD BATCH 4-fig-priors.R &
R CMD BATCH 5-fig-density-state.R &
R CMD BATCH 6-table-data-quality.R &
R CMD BATCH 7-fig-testpos.R &
R CMD BATCH 8-fig-percent-undertesting-state.R &
