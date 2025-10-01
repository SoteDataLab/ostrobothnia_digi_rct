
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
###       r-script 3_tables_summary_stats.R           ###
###                 Replication file.                 ###
###                    2025 by MS, TH                 ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Create a summary statistics table.


# I Define inputs and outputs ---------------------------------------------


# Install and load the following packages:
library(here)             # Relative file paths.
library(data.table)       # mutating and aggregating data
library(smd)              # standardized mean difference.
library(stargazer)        # Save as tex file.

# Inputs:
input_study_pop <- here('data', 'study_population_placebo.csv')
input_total_pop <- here('data', 'total_population.csv')
input_phc_study <- here('data', 'person_by_prepost_12mon.csv')
input_phc_total <- here('data', 'person_by_prepost_12mon_total_population.csv')

# Outputs:
output_table_1 <- here('tables', 'table_1_summary_stats.tex')
output_table_2 <- here('tables', 'table_2_summary_stats.tex')

# Define variables of interest:
phc_vars <- c('y1.1_pre', 'y1.2_pre', 
              'private.inperson_pre','private.telemed_pre',
              'occup.inperson_pre', 'occup.telemed_pre')

# II Read and organize data ------------------------------------------------


## 1) study population -----------------------------------------------------


# STUDY POPULATION and their socioeconomic covariates:
dt <- fread(input_study_pop, na.strings = c(NA_character_, ''))

# Income as thousands:
dt[, kturaha_ekv := kturaha_ekv / 1000]

# Construct covariates on leading health care use and merge to dt:
phc <- fread(input_phc_study, na.strings = c(NA_character_,''), 
             select = c('shnro','y1.1_post',  phc_vars))
dt <- merge(dt, phc, by = 'shnro', all.x = TRUE)

# Analysis sample is restricted to those aged 0-70:
dt <- dt[ika %in% c(0:70)]


## 2) total population -----------------------------------------------------


# TOTAL POPULATION and their socioeconomic covariates:
dt_total <- fread(input_total_pop, na.strings = c(NA_character_, ''))

# Income as thousands:
dt_total[, kturaha_ekv := kturaha_ekv / 1000]

# Construct covariates on leading health care use and merge to dt:
phc <-fread(input_phc_total, na.strings = c(NA_character_,''), 
            select = c('shnro','y1.1_post', phc_vars))
dt_total <- merge(dt_total, phc, by = 'shnro', all.x = TRUE)

# Analysis sample is restricted to those aged 0-70:
dt_total <- dt_total[ika %in% c(0:70)]


# III Tables ---------------------------------------------------------------


## 1) Table 1 -----------------------------------------------------------------
# Characteristics and Means Comparisons of Residents at Baseline.

# The following variables will be in the table:
vars_table <- c(phc_vars,  'ika', 'female', 'language.fin', 'language.swe', 
                'relationship.or.widowed', 'living.in.city', 
                'etaisyys_terveysasemalle_km', 'educ.tertiary', 'pensioner', 
                'in.labor.market', 'kturaha_ekv', 'ek_kela_kansansairaus',
                'ek_kaisa_monisairas')

# Compute means and standard deviations:
table <- lapply(list(mean = "mean", sd = "sd"), function(x) {
  
  temp <- dt[, lapply(.SD, get(x), na.rm=T), .SDcols = vars_table, by='treated']
  temp <- melt(temp, id.vars = "treated")
  temp <- dcast(temp, variable ~ treated, value.var = "value")
  setnames(temp, old = c('0','1'), new = paste0(x, c('_control', '_treated')))
})

# Merge the two elements from the list:
table <- merge(table$mean, table$sd)

# Relative (%) differences in means:
table[, relative_diff := 100*(mean_treated - mean_control) / mean_control]

# Standardized mean differences
smd <- dt[, sapply(.SD, function(x){
  smd(x = x, g = treated, gref = 2, na.rm = T)$estimate
}), .SDcols = vars_table]

# Add it to summary table:
table[, smd := smd]

# Calculate mean in the study population:
mean_study <- dt[, sapply(.SD, mean, na.rm = T), .SDcols = vars_table]

# ... and in total population:
mean_total_p <- dt_total[, sapply(.SD, mean, na.rm = T), .SDcols = vars_table]

# Add them to the summary table too:
table[, mean_study_p := mean_study]
table[, mean_total_p := mean_total_p]

# Round and format:
table <- table[, lapply(.SD, round, digits=3), .SDcols=is.numeric, by=variable]
table <- table[, lapply(.SD, format, nsmall=3), .SDcols=is.numeric, by=variable]

# Combine columns
table[, treated_mean_sd := paste0(mean_treated,' [', sd_treated, ']')]
table[, control_mean_sd := paste0(mean_control,' [', sd_control, ']')]
table[, treated_control := paste0(relative_diff,' [', smd, ']')]

# Sample sizes:
ns <- lapply(list(treated_mean_sd = dt[treated == 1],
                  control_mean_sd = dt[treated == 0],
                  mean_study_p = dt,
                  mean_total_p = dt_total), 
             uniqueN, "shnro")
table <- rbind(table, c(variable = "N", ns), fill = T)


# Tidy variable names:
table[, variable := fcase(
  
  # A. Prior health care use
  variable == 'y1.1_pre', 'PPC: in-person visits (days)',
  variable == 'y1.2_pre', 'PPC: other contacts (days)',
  variable == 'private.inperson_pre',  'Private HC: in-person visits (days)',
  variable == 'private.telemed_pre', 'Private HC: other contacts (days)',
  variable == 'occup.inperson_pre',  'Occup. HC: in-person visits (days)',
  variable == 'occup.telemed_pre',  'Occup. HC: other contacts (days)',
  
  # B. Sociodemographic covariates
  variable == 'ika',  'Age (in years)',
  variable == 'female',  'Is female (share)',
  variable == 'language.fin',  'Language: Finnish (share)',
  variable == 'language.swe',  'Language: Swedish (share)',
  variable == 'relationship.or.widowed',  'Relationship or widowed (share)',
  variable == 'living.in.city', 'Living in a city  (share)',
  variable == 'etaisyys_terveysasemalle_km',  'Dist. to nearest trad. PPC clinic (km)',
  variable == 'educ.tertiary', 'Tertiary education (share)',
  variable == 'pensioner', 'Pensioner (share)',
  variable == 'in.labor.market', 'Employed  (share)',
  variable == 'kturaha_ekv',  'Income (thousands of euros)',

  # C. Morbidites: 
  variable == 'ek_kela_kansansairaus', 'Common chronic disease  (share)',
  variable == 'ek_kaisa_monisairas',  'Has multimorbidity (share)',
  
  # sample size
  variable == 'N', 'N')]

# Select variables
table <-table[, .(variable, treated_mean_sd, control_mean_sd, 
                  treated_control, mean_study_p, mean_total_p)]

# Save as tex:
stargazer::stargazer(table, out = output_table_1,
                     type='text', summary=FALSE, rownames = F, header = F)


## 2) Table 2 -----------------------------------------------------------------
# Characteristics of Public Digital Clinic Users vs. Traditional PPC
# Clinic Users in the Treatment Group in Ostrobothnia.


# Define the target population for Table 2:
dt_2 <- dt[treated == 1]

# For placebo data only. Do not do this with real data! This is done for the
# placebo analysis to have a strong first stage.
dt_2[, d.1_post := D_1]
dt_2[, d.2_post := as.integer(D_1 > 0)]

# Create an variable for different user groups:
dt_2[, customer := fcase(d.2_post ==1, "digi", 
                         d.2_post == 0 & y1.1_post > 1, "trad")]
dt_2[, .N, by = customer]
dt_2 <- dt_2[!is.na(customer)]

# The following variables will be in the table:
vars_table <- c(phc_vars,  'ika', 'female', 'language.fin', 'language.swe', 
                'relationship.or.widowed', 'living.in.city', 
                'etaisyys_terveysasemalle_km', 'educ.tertiary', 'pensioner', 
                'in.labor.market', 'kturaha_ekv', 'ek_kela_kansansairaus',
                'ek_kaisa_monisairas')

# Compute means and standard deviations:
table <- lapply(list(mean = "mean", sd = "sd"), function(x) {
  
  temp <- dt_2[, lapply(.SD, get(x), na.rm = T), 
               .SDcols = vars_table, by='customer']
  temp <- melt(temp, id.vars = "customer")
  temp <- dcast(temp, variable ~ customer, value.var = "value")
  setnames(temp, old = c('digi', 'trad'), new = paste0(x, c('_digi', '_trad')))
  
})

# Merge the two elements from the list:
table <- merge(table$mean, table$sd)

# Relative (%) differences in means:
table[, relative_diff := 100*(mean_digi - mean_trad )/ mean_trad]

# Standardized mean differences:
smd <- dt_2[, sapply(.SD, function(x){
  smd(x = x, g = customer, gref = 2, na.rm = T)$estimate
}), .SDcols = vars_table]

# Add it to summary table:
table[, smd := smd]

# Mean & sd in the study population:
mean_study_p <- dt[, sapply(.SD, mean, na.rm = T), .SDcols = vars_table]
sd_total_p <- dt[, sapply(.SD, sd, na.rm = T), .SDcols = vars_table]

# Add them to the summary table too:
table[, mean_study_p := mean_total_p]
table[, sd_study_p := sd_total_p]

# Round and format:
table <- table[, lapply(.SD, round, digits=3), .SDcols=is.numeric, by=variable]
table <- table[, lapply(.SD, format, nsmall=3), .SDcols=is.numeric, by=variable]

# Combine columns
table[, digi_mean_sd := paste0(mean_digi,' [', sd_digi, ']')]
table[, trad_mean_sd := paste0(mean_trad,' [', sd_trad, ']')]
table[, digi_trad := paste0(relative_diff,' [', smd, ']')]

# Sample sizes:
ns <- lapply(list(digi_mean_sd = dt_2[customer == 'digi'],
                  trad_mean_sd = dt_2[customer == 'trad'],
                  mean_study_p = dt), 
             uniqueN, "shnro")
table <- rbind(table, c(variable = "N", ns), fill = T)


# Tidy variable names:
table[, variable := fcase(
  
  # A. Prior health care use
  variable == 'y1.1_pre', 'PPC: in-person visits (days)',
  variable == 'y1.2_pre', 'PPC: other contacts (days)',
  variable == 'private.inperson_pre',  'Private HC: in-person visits (days)',
  variable == 'private.telemed_pre', 'Private HC: other contacts (days)',
  variable == 'occup.inperson_pre',  'Occup. HC: in-person visits (days)',
  variable == 'occup.telemed_pre',  'Occup. HC: other contacts (days)',
  
  # B. Sociodemographic covariates
  variable == 'ika',  'Age (in years)',
  variable == 'female',  'Is female (share)',
  variable == 'language.fin',  'Language: Finnish (share)',
  variable == 'language.swe',  'Language: Swedish (share)',
  variable == 'relationship.or.widowed',  'Relationship or widowed (share)',
  variable == 'living.in.city', 'Living in a city  (share)',
  variable == 'etaisyys_terveysasemalle_km',  'Dist. to nearest trad. PPC clinic (km)',
  variable == 'educ.tertiary', 'Tertiary education (share)',
  variable == 'pensioner', 'Pensioner (share)',
  variable == 'in.labor.market', 'Employed  (share)',
  variable == 'kturaha_ekv',  'Income (thousands of euros)',
  
  # C. Morbidites: 
  variable == 'ek_kela_kansansairaus', 'Common chronic disease  (share)',
  variable == 'ek_kaisa_monisairas',  'Has multimorbidity (share)',
  
  # sample size
  variable == 'N', 'N')]

# Select variables
table <-table[, .(variable, digi_mean_sd, trad_mean_sd, 
                  digi_trad, mean_study_p)]

# Save as tex:
stargazer::stargazer(table, out = output_table_2,
                     type='text', summary=FALSE, rownames = F, header = F)


# End ---------------------------------------------------------------------


