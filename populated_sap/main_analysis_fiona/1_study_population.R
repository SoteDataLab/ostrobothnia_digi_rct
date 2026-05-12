
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
###            r-script 1_study_population.R          ###
###                 Replication file.                 ###
###                    2026 by TH, MS                 ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Read the study population and their characteristics.
rm(list=ls())

# Install and load the following packages:
library(here)             # Relative file paths.
library(data.table)       # mutating and aggregating data


# I Define inputs and outputs -----------------------------------------------


# Input folder
path <- "~/DATAINFRA/"

# Inputs:
input_treatments <- here('data', 'POHJANMAA_RCT.csv')
input_folk <- paste0(path, "TK/raw/folk_yhdistetty_2023.csv") # NOTE: use 2024 in the final paper.
input_kela <- paste0(path, "KELA/cleaned/kela_erityiskorvausoikeudet_2024.csv")
input_kunta_hva <- paste0(path, "misc/raw/kunta_hva_2023.csv")
input_distance <- paste0(path, "misc/cleaned/hetu_terveysasemat_21_23_lakkautetut_VV032025AM.csv") # NOTE: use 2024 in the final paper.

# Outputs:
output_population <- here('data', 'study_population.csv')
output_population_total <- here('data', 'total_population.csv')


# II Study population -----------------------------------------------------

dt <- data.table::fread(input_treatments, na.strings = c(NA_character_, ''))
setnames(dt, old=c('asuinpaikantunnus_s', 'koeryhmassa'), 
         new=c('address.id', 'treated'))
dt[, kotikunta := NULL]


# III Covariates from FOLK -------------------------------------------------


# Read FOLK data:
vars <- c('shnro', 'kunta31_12', 'posti_alue', 'ika', 'sukup', 'kieli_k',
          'sivs', 'maka', 'ututku_aste', 'ptoim1', 'kturaha_ekv', 'tyke')
folk <- fread(input_folk, select=vars, 
                          na.strings = c(NA_character_, ''))

# Wellbeing services county:
hva <- fread(input_kunta_hva, encoding='UTF-8',
                         na.strings = c(NA_character_, ''),
                         select = c('kuntanro', 'hva_lyhenne'))
folk <- merge(folk, hva, by.x='kunta31_12', by.y='kuntanro', all.x = TRUE)

# Income percentile is used as covariate in regressions:
folk[, income_percentile :=
       .bincode(kturaha_ekv,
                quantile(kturaha_ekv, probs= 0:100/100, na.rm=TRUE),
                right = FALSE, include.lowest = TRUE), by='hva_lyhenne']

# Construct covariates for descriptive statistics statistics:
folk[, ':=' (female = as.integer(sukup == 2),
             language.fin = as.integer(kieli_k == 1),
             language.swe = as.integer(kieli_k == 2),
             language.other = as.integer(kieli_k == 3),
             relationship.or.widowed = as.integer(sivs %in% c(2, 5)),
             living.in.city = as.integer(maka %in% c('K1', 'K2', 'K3')),
             educ.tertiary = as.integer(ututku_aste %in% c(5, 6, 7, 8)),
             pensioner = as.integer(ptoim1 %in% c(24, 29)),
             in.labor.market = as.integer(ptoim1 %in% c(11, 12)),
             unemployment = as.integer(tyke %in% c(0:12)))]

# Missing values:
100 * colMeans(is.na(folk))
colSums(is.na(folk))

# N:
folk[, uniqueN(shnro)]


# Read distances to the nearest health station and merge:
dt.dist <- data.table::fread(
  input_distance, na.strings = c(NA_character_, ''),
  select = c('shnro', 'vuosi', 'etaisyys_terveysasemalle_km'))
dt.dist <- dt.dist[vuosi==2023] # NOTE: 2024 in the final paper.
dt.dist[, vuosi := NULL]

folk <- merge(folk, dt.dist, by='shnro', all.x=TRUE)


# If distance to nearest health station is missing, 1) use mean distance for the
# same postal code area or 2) use mean distance for the same municipality.

dt.help <- folk[, .(etaisyys_tk_postialue =
                      mean(etaisyys_terveysasemalle_km, na.rm=TRUE)), 
                by='posti_alue'
                ][!is.na(etaisyys_tk_postialue)]

folk <- merge(folk, dt.help, by='posti_alue', all.x = TRUE)

folk[is.na(etaisyys_terveysasemalle_km), 
     etaisyys_terveysasemalle_km := etaisyys_tk_postialue]

dt.help <- folk[, .(etaisyys_tk_kunta = 
                    mean(etaisyys_terveysasemalle_km, na.rm=TRUE)), 
                by='kunta31_12'
                ][!is.na(etaisyys_tk_kunta)]

folk <- merge(folk, dt.help, by='kunta31_12', all.x = TRUE)

folk[is.na(etaisyys_terveysasemalle_km), 
     etaisyys_terveysasemalle_km := etaisyys_tk_kunta]

folk[, ':=' (etaisyys_tk_postialue=NULL, 
             etaisyys_tk_kunta=NULL, posti_alue=NULL)]

# Distance quartile is used as a covariate in regressions:
folk[, distance.quartile :=
       .bincode(etaisyys_terveysasemalle_km,
                quantile(etaisyys_terveysasemalle_km, probs= seq(0, 1, by=0.25), 
                         na.rm=TRUE),
                right = FALSE, include.lowest = TRUE), by='hva_lyhenne']


# Read data on special reimbursement rights:
dt.rights <- data.table::fread(
  input_kela, na.strings = c(NA_character_, ''),
  select = c('shnro','ek_kela_kansansairaus','ek_kaisa_monisairas'))

# Merge to FOLK data:
folk <- merge(folk, dt.rights, by='shnro', all.x = TRUE)

# Inpute zeroes if no morbidity is observed:
folk[is.na(ek_kela_kansansairaus), ek_kela_kansansairaus := 0]
folk[is.na(ek_kaisa_monisairas), ek_kaisa_monisairas := 0]


## Total population --------------------------------------------------------

# Store a subset of covariates:
vars.total <-
  c('shnro', 'ika', 'sukup', 'kturaha_ekv', 'kunta31_12', 'income_percentile', 
    'kieli_k', 'female', 'language.fin', 'language.swe', 'language.other',
    'relationship.or.widowed', 'living.in.city', 
    'educ.tertiary', 'unemployment', 'pensioner', 'in.labor.market',
    'etaisyys_terveysasemalle_km', 'distance.quartile', 
    'ek_kela_kansansairaus', 'ek_kaisa_monisairas')
dt.total <- folk[, mget(vars.total)]

# Missing values:
colSums(is.na(dt.total))

# Save:
fwrite(dt.total, file=output_population_total)


## Study population --------------------------------------------------------


# Merge covariates to dt (the trial population):
dt <- merge(dt, folk, by='shnro', all.x = TRUE)

# Store a subset of covariates:
vars.ostro <- c('address.id', 'strata', 'treated')
vars.ostro <- c(vars.ostro, vars.total)
dt <- dt[, mget(vars.ostro)]

# Drop individuals who are not observed in the FOLK data:
dt[, 100 * mean(is.na(kunta31_12))]
dt[, sum(is.na(kunta31_12))]
dt <- dt[!is.na(kunta31_12)]
print(dt[, uniqueN(shnro)])

dt[, .N, by=treated]
dt[, uniqueN(address.id), by=treated]
dt[ika %in% c(0:70), .N, by='treated']
dt[ika %in% c(0:70), uniqueN(address.id), by='treated']

# Save:
fwrite(dt, file=output_population)


# End ---------------------------------------------------------------------
rm(list = ls())
gc()
