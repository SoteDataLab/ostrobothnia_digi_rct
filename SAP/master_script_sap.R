
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
###              r-script master_script_sap.R         ###
###                 Replication file.                 ###
###                    2025 by TH, MS                 ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: The order in which R scripts should be run (SAP).
rm(list=ls())

# R version 4.2.1.
# Install and load the following packages:
library(here)		          # relative file paths.
library(data.table)       # mutating and aggregating data
library(smd)              # standardized mean difference.
library(stargazer)        # Save as tex file.
library(ggplot2)          # Plotting data.
library(viridis)          # accessible color palettes.
library(fixest)           # OLS regression.
library(lubridate)        # Mutate dates.

writeLines(capture.output(sessionInfo()), 'sessionInfo_sap.txt')


# Create a hypothetical study population with placebo treatment.
source(file = here('scripts', '1_study_population.R'))
# Running time: <2 mins

# Create outcomes for the study population and for the full population.
source(file = here('scripts', '2_data.R'))
# Running time: 31 mins.

# Create a summary statistics table.
source(file = here('scripts', '3_tables_summary_stats.R'))
# Running time: <1 min

# Plot outcomes as a function of time relative to treatment.
source(file = here('scripts', '4_plot_outcomes.R'))
# Running time: 5 mins

# Estimate regression results.
source(file = here('scripts', '5_estimate_results.R'))
# Running time: <1 min


# End ---------------------------------------------------------------------

