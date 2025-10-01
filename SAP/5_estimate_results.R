
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
###             r-script 5_estimate_main.R            ###
###                 Replication file.                 ###
###                    2025 by MS, TH                 ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Estimate main regression results (Tables 3-5).
rm(list=ls())


# I) Define inputs and outputs ---------------------------------------------


# Install and load the following packages:
library(here)             # Relative file paths.
library(data.table)       # mutating and aggregating data
library(fixest)           # OLS regression.
library(stargazer)        # Save as tex file.

# Inputs:
input_population <- here('data', 'study_population_placebo.csv')
input_data <- here('data', 'person_by_prepost_12mon.csv')

# Outputs:
output_table_Y1.X <- here('tables', 'table_3_main_results_Y1.X.tex')
output_table_Y2.X <- here('tables', 'table_4_secondary_results_Y2.X.tex')
output_table_Y3.X  <- here('tables', 'table_5_secondary_results_Y3.X.tex')

# Import function for sharpened q-values:
source(here("scripts", "sharpened_qvals.R"))

# II) Load data and create an estimation function -------------------------


# Study population and their characteristics:
dt <- fread(input_population, na.strings = c(NA_character_, '')) 

# Analysis sample is restricted to those aged 0-70:
dt <- dt[ika %in% c(0:70)]

# Data on health care use:
phc <- fread(input_data, na.strings = c(NA_character_, ''))

# Merge health care data
dt <- merge(dt, phc, by='shnro', all.x = TRUE)

# Save some memory
rm(phc);gc()

# For placebo data only. Do not do this with real data! This is done for the
# placebo analysis to have a strong first stage.
dt[, d.1_post := D_1]
dt[, d.2_post := as.integer(D_1 > 0)]

# Estimation function
estimate <- function(data, Y = 'ed.other_post', D = 'd.1_post') {
  # INPUTS:
  # data: rows represent individuals in the analysis sample and outcomes are in
  #       columns
  # Y: outcome, Y1.1, Y1.2, Y1.3
  # D: take-up, D.1, D.2
  # OUTPUTS: regression results as a data.table.
  
  # The data:
  dt <- data[, mget(colnames(data))]
  dt[, Y := get(Y)]
  dt[, D := get(D)]
  
  # First stage:
  fs <- fixest::feols(
    D ~ treated | y1.1_pre + (occup.total_pre > 0) + ika + sukup + kunta31_12 + 
      income_percentile + kieli_k + distance.quartile + ek_kela_kansansairaus + 
      ek_kaisa_monisairas,
    data=dt)
  
  # Reduced form:
  rf <- fixest::feols(
    Y ~ treated |  y1.1_pre + (occup.total_pre > 0) + ika + sukup + kunta31_12 + 
      income_percentile + kieli_k + distance.quartile + ek_kela_kansansairaus + 
      ek_kaisa_monisairas,
    data=dt)
  
  # Two stage least squares:
  tsls <- fixest::feols(
    Y ~ 0 | y1.1_pre + (occup.total_pre > 0) + ika + sukup + kunta31_12 + 
      income_percentile + kieli_k + distance.quartile + ek_kela_kansansairaus + 
      ek_kaisa_monisairas | D_1 ~ treated, data=dt)
  
  
  # Cluster SEs:
  fs <- summary(fs, cluster = 'address.id')$coeftable
  rf <- summary(rf, cluster = 'address.id')$coeftable
  tsls <- summary(tsls, cluster = 'address.id')$coeftable
  
  # Collect results (first stage):
  effect <- fs['treated', 'Estimate']
  se <- fs['treated', 'Std. Error']
  p <- fs['treated', 'Pr(>|t|)']
  qval <- sharpened_qvals(p)
  results.fs <- data.table(effect, se, p, qval, type='fs')
  
  # Collect results (reduced form):
  effect <- rf['treated', 'Estimate']
  se <- rf['treated', 'Std. Error']
  p <- rf['treated', 'Pr(>|t|)']
  qval <- sharpened_qvals(p)
  results.rf <- data.table(effect, se, p, qval, type='rf')
  
  # Collect results (2SLS):
  effect <- tsls['fit_D_1', 'Estimate']
  se <- tsls['fit_D_1', 'Std. Error']
  p <- tsls['fit_D_1', 'Pr(>|t|)']
  qval <- sharpened_qvals(p)
  results.tsls <- data.table(effect, se, p, qval, type='tsls')
  
  results <- rbind(results.fs, results.rf, results.tsls)
  
  # CIs at the 95% level:
  results[, ':=' (ci_lower = effect - qnorm(1 - 0.05/2) * se,
                  ci_upper = effect + qnorm(1 - 0.05/2) * se)]
  
  # N:
  results[, ':=' (n_treated = dt[treated==1, uniqueN(shnro)],
                  n_control = dt[treated==0, uniqueN(shnro)])]
  
  # Outcomes:
  results[, ':=' (Y = Y, D = D)]
  
  # Control group means:
  results[, ':=' (baseline_d = dt[treated==0, mean(D)],
                  baseline_Y = dt[treated==0, mean(Y)])]
  
  # Effects relative (%) to the baseline:
  results[type=='rf', effect_rel := 100 * effect / baseline_Y]
  
  # CIs in relative terms:
  results[type=='rf', ':=' (ci_rel_lower = 100 * ci_lower / baseline_Y,
                            ci_rel_upper = 100 * ci_upper / baseline_Y)]
  
  return(results)
  
}

# Test the function
test <- estimate(data=dt)
print(test)


# Define row names in the results tables
long_row_names <- c("effect" = "Effect",
                    "se" = "SE",
                    "CI" = "CI",
                    "baseline_d" = "Control group mean" ,
                    "baseline_Y"  =  "Control group mean", 
                    "effect_rel" = "Relative effect (percent)",
                    "CI_rel" = "Relative CI (percent)", 
                    "qval" = "Sharpened q-values")


#  II) Primary outcomes ----------------------------------------------------


## 1) Estimate -------------------------------------------------------------

## have a look at the dependent variables
summary(dt[, .(d.1_post, d.2_post, y1.1_post, y1.2_post, y1.3_post)])

# We will loop over the following outcomes:
specs <- list(
  list(D='d.1_post', Y='y1.1_post'),
  list(D='d.1_post', Y='y1.2_post'),
  list(D='d.1_post', Y='y1.3_post'),
  list(D='d.2_post', Y='y1.1_post')
)

# Estimate:
results <- lapply(specs, function(spec) {
  estimate(data = dt, Y = spec$Y, D = spec$D)
})
results <- rbindlist(results)



## 2) Create table --------------------------------------------------------

### Panel a -----------------------------------------------------------------

# Pick the relevant results
pan_a <- unique(results[type=="fs", 
                   .(effect, baseline_d, se, ci_lower, ci_upper, D)])

# Format numbers
pan_a <- pan_a[, round(.SD, digits = 3), .SDcols = is.numeric, by = D]
pan_a <- pan_a[, .(D, format(.SD, nsmall = 3)), .SDcols = is.numeric]

# Add confidence interval
pan_a[, CI := paste0("[", ci_lower, ",", ci_upper, "]")]

# wrangle the table into the desired form
pan_a <- melt(pan_a, id.vars = "D", 
              measure.vars = c("effect", "baseline_d", "se", "CI"))
pan_a <- dcast(pan_a, variable ~ D, value.var = "value")

# longer row names
pan_a[, variable := long_row_names[as.character(variable)] ]


### Panel b -----------------------------------------------------------------


# Pick the relevant results
pan_b <- unique(results[type=="rf", 
                        .(effect, baseline_Y, se, ci_lower, ci_upper, 
                          Y,effect_rel, ci_rel_lower, ci_rel_upper)])

# Format numbers
pan_b <- pan_b[, round(.SD, digits = 3), .SDcols = is.numeric, by = Y]
pan_b <- pan_b[, .(Y, format(.SD, nsmall = 3)), .SDcols = is.numeric]

# Add confidence interval
pan_b[, CI := paste0("[", ci_lower, ",", ci_upper, "]")]
pan_b[, CI_rel := paste0("[", ci_rel_lower, ",", ci_rel_upper, "]")]

# Wrangle the table into the desired form
pan_b <- melt(pan_b, id.vars="Y", measure.vars=c("effect", "baseline_Y", "se", 
                                                 "CI", "effect_rel", "CI_rel"))
pan_b <- dcast(pan_b, variable ~ Y, value.var = "value")

# Longer row names
pan_b[,variable := long_row_names[as.character(variable)] ]


### Panel c -----------------------------------------------------------------


# Create panel C
pan_c <- unique(results[type == "tsls" & Y %in% c("y1.1_post", "y1.2_post"), 
                   .(effect, se, ci_lower, ci_upper, Y )])

# Format numbers
pan_c <- pan_c[, round(.SD, digits = 3), .SDcols = is.numeric, by = Y]
pan_c <- pan_c[, .(Y, format(.SD, nsmall = 3)), .SDcols = is.numeric]

# Add confidence interval
pan_c[, CI := paste0("[", ci_lower, ",", ci_upper, "]")]

# Wrangle the table into the desired form
pan_c <- melt(pan_c, id.vars = "Y", measure.vars = c("effect", "se", "CI"))
pan_c <- dcast(pan_c, variable ~ Y, value.var = "value")

# Longer row names
pan_c[,variable := long_row_names[as.character(variable)] ]


## 3) Finalize and save -------------------------------------------------------


# Add an empty fourth column to panels a and c
pan_a[, blank := ""]
pan_c[, blank := ""]

# Combine all the panels into one table
table <- rbind(as.matrix(pan_a), as.matrix(pan_b), as.matrix(pan_c))

# Get sample sizes
n_t <- unique(results$n_treated)
n_c <- unique(results$n_control)

# Add a row for the sample sizes
table <- rbind(table, c(a = "N", b = paste(n_t, "(treated)"), 
                        c = paste(n_c, "(control)"), d = ""))

# Get the p-values from the results table
p_acr <- results[type == "tsls" & Y == "y1.1_post" & D == "d.1_post", p]
p_itt <- results[type == "rf" & Y == "y1.1_post" & D == "d.1_post", p]

# Format numbers
p_acr <- format(p_acr, nsmall = 3, digits = 3)
p_itt <- format(p_itt, nsmall = 3, digits = 3)

# Add to the table
table <- rbind(table, c("p-values", paste(p_itt, "(ITT)"), paste(p_acr, "(ACR)"),""))


# Save 
stargazer::stargazer(table, out = output_table_Y1.X, type='text', 
                     summary=FALSE, rownames = F,colnames = F, header = F)


# III Hospital outcomes ----------------------------------------------------


## 1) Estimate ------------------------------------------------------------


## have a look at the dependent variables
summary(dt[, .(referrals_post, ed.other_post, 
               ed.in.person_post, hospital.visit_post)])

# We will loop over the following outcomes:
specs <- list(
  list(D='d.1_post', Y='referrals_post'),
  list(D='d.1_post', Y='ed.other_post'),
  list(D='d.1_post', Y='ed.in.person_post'),
  list(D='d.1_post', Y='hospital.visit_post')
)

# Estimate:
results <- lapply(specs, function(spec) {
  estimate(data = dt, Y = spec$Y, D = spec$D)
})
results <- rbindlist(results)



## 2) Create table --------------------------------------------------


### Panel a -----------------------------------------------------------------


# Pick the relevant results
pan_a <- unique(results[type == "rf", 
                        .(effect, baseline_Y, se, ci_lower, ci_upper, Y,
                          effect_rel, ci_rel_lower, ci_rel_upper, qval)])

# Format numbers
pan_a <- pan_a[, round(.SD, digits = 3), .SDcols = is.numeric, by = Y]
pan_a <- pan_a[, .(Y, format(.SD, nsmall = 3)), .SDcols = is.numeric]

# Add confidence interval
pan_a[, CI := paste0("[", ci_lower, ",", ci_upper, "]")]
pan_a[, CI_rel := paste0("[", ci_rel_lower, ",", ci_rel_upper, "]")]

# Wrangle the table into the desired form
pan_a <- melt(pan_a, id.vars = "Y", 
              measure.vars = c("effect", "baseline_Y", 
                               "se", "CI", "effect_rel", "CI_rel", "qval"))
pan_a <- dcast(pan_a, variable ~ Y, value.var = "value")

pan_a[, variable := long_row_names[as.character(variable)] ]

# change order
pan_a <- pan_a[, .(variable, referrals_post, ed.in.person_post, ed.other_post, 
                   hospital.visit_post )]   


### Panel b -----------------------------------------------------------------


# Pick the relevant results
pan_b <- unique(results[type == "tsls", 
                        .(effect, se, ci_lower, ci_upper, Y, qval)])

# Format numbers
pan_b <- pan_b[, round(.SD, digits = 3), .SDcols = is.numeric, by = Y]
pan_b <- pan_b[, .(Y, format(.SD, nsmall = 3)), .SDcols = is.numeric]

# Add confidence interval
pan_b[, CI := paste0("[", ci_lower, ",", ci_upper, "]")]

# Wrangle the table into the desired form
pan_b <- melt(pan_b, id.vars = "Y", measure.vars = c("effect", "se", "CI", "qval"))
pan_b <- dcast(pan_b, variable ~ Y, value.var = "value")

# Longer row names
pan_b[, variable := long_row_names[as.character(variable)] ]

# change order
pan_b <- pan_b[, .(variable, referrals_post, ed.in.person_post, ed.other_post, 
                   hospital.visit_post )]   

## 3) Finalize and save -------------------------------------------------------

# Combine all the panels into one table
table <- rbind(as.matrix(pan_a), as.matrix(pan_b))

# Sample sizes
n_t <- unique(results$n_treated)
n_c <- unique(results$n_control)

# Add a row for the sample sizes
table <- rbind(table, c(a = "", b = paste(n_t, "(treated)"), 
                        c = paste(n_c, "(control)"), d = "", f = ""))


# Save:
stargazer::stargazer(table, out = output_table_Y2.X, type='text', 
                     summary=FALSE, rownames = F,colnames = F, header = F)



# IV spillover Outcomes -------------------------------------------------

## 1) Estimate -------------------------------------------------------------


## have a look at the dependent variables
summary(dt[, .(occup.inperson_post, occup.telemed_post,
               private.inperson_post,private.telemed_post)])

# We will loop over the following outcomes:
specs <- list(
  list(D='d.1_post', Y='occup.inperson_post'),
  list(D='d.1_post', Y='occup.telemed_post'),
  list(D='d.1_post', Y='private.inperson_post'),
  list(D='d.1_post', Y='private.telemed_post')
)

# Estimate:
results <- lapply(specs, function(spec) {
  estimate(data = dt, Y = spec$Y, D = spec$D)
})
results <- rbindlist(results)



## 2) Create table --------------------------------------------------------


### Panel a -----------------------------------------------------------------


# Pick the relevant results
pan_a <- unique(results[type == "rf", 
                        .(effect, baseline_Y, se, ci_lower, ci_upper, Y, 
                          effect_rel, ci_rel_lower, ci_rel_upper, qval)])

# Format numbers
pan_a <- pan_a[, round(.SD, digits = 3), .SDcols = is.numeric, by = Y]
pan_a <- pan_a[, .(Y, format(.SD, nsmall = 3)), .SDcols = is.numeric]

# Add confidence interval
pan_a[, CI := paste0("[", ci_lower, ",", ci_upper, "]")]
pan_a[, CI_rel := paste0("[", ci_rel_lower, ",", ci_rel_upper, "]")]

# Wrangle the table into the desired form
pan_a <- melt(pan_a, id.vars = "Y", 
              measure.vars = c("effect", "baseline_Y", "se", "CI", 
                               "effect_rel", "CI_rel", "qval"))
pan_a <- dcast(pan_a, variable ~ Y, value.var = "value")

pan_a[,variable := long_row_names[as.character(variable)] ]


### Panel b -----------------------------------------------------------------


# Pick the relevant results
pan_b <- unique(results[type == "tsls", 
                        .(effect, se, ci_lower, ci_upper, Y , qval)])

# Format numbers
pan_b <- pan_b[, round(.SD, digits = 3), .SDcols = is.numeric, by = Y]
pan_b <- pan_b[, .(Y, format(.SD, nsmall = 3)), .SDcols = is.numeric]

# Add confidence interval
pan_b[, CI := paste0("[", ci_lower, ",", ci_upper, "]")]

# Wrangle the table into the desired form
pan_b <- melt(pan_b, id.vars = "Y", measure.vars = c("effect", "se", "CI", "qval"))
pan_b <- dcast(pan_b, variable ~ Y, value.var = "value")

# Longer row names
pan_b[,variable := long_row_names[as.character(variable)] ]


## 3) Finalize and save -------------------------------------------------------


# Combine all the panels into one table
table <- rbind(as.matrix(pan_a), as.matrix(pan_b))

# Sample sizes
n_t <- unique(results$n_treated)
n_c <- unique(results$n_control)

# Add a row for the sample sizes
table <- rbind(table, c(a = "", b = paste(n_t, "(treated)"),
                        c = paste(n_c, "(control)"), d = "", f = ""))

# Save:
stargazer::stargazer(table, out = output_table_Y3.X, type='text', summary=FALSE, 
                     rownames = F,colnames = F, header = F)


# End. ---------------------------------------------------------------------
