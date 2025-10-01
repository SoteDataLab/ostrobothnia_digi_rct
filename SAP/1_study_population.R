
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
###            r-script 1_study_population.R          ###
###                 Replication file.                 ###
###                    2025 by TH, MS                 ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Create a hypothetical study population with placebo treatment.
rm(list=ls())

# Install and load the following packages:
library(here)             # Relative file paths.
library(data.table)       # mutating and aggregating data


# I Define inputs and outputs -----------------------------------------------


# Input folder
path <- "~DATAINFRA/"

# Inputs:
input_folk <- paste0(path, "TK/raw/folk_yhdistetty_2023.csv") # NOTE: use 2024 in the final paper.
input_kela <- paste0(path, "KELA/cleaned/kela_erityiskorvausoikeudet_2024.csv")
input_kunta_hva <- paste0(path, "misc/raw/kunta_hva_2023.csv")
input_distance <- paste0(path, "misc/cleaned/hetu_terveysasemat_21_23_lakkautetut_VV032025AM.csv") # NOTE: use 2024 in the final paper.

# Outputs:
output_population <- here('data', 'study_population_placebo.csv')
output_population_total <- here('data', 'total_population.csv')


# II Study population -----------------------------------------------------

# For the purpose of writing SAP codes, we will create a table with a placebo 
# randomization that resembles the table on the actual randomization. 

# Read FOLK data:
vars <- c('shnro', 'petu', 'kunta31_12')
dt <- data.table::fread(input_folk, select=vars, 
                        na.strings = c(NA_character_, ''))

# Keep only people residing in Ostabothnia wellbeing services county.
# Kristiinankaupunki (287) is excluded because of outsourced PPC services.
munies_keep <- fread(input_kunta_hva, encoding='UTF-8', 
                     na.strings = c(NA_character_, ''))
munies_keep <- munies_keep[hva=='Pohjanmaan hyvinvointialue', kuntanro]
munies_keep <- setdiff(munies_keep, 287)
dt <- dt[kunta31_12 %in% munies_keep]
dt[, kunta31_12 := NULL]

# If not in family population, use person ID as family ID:
dt[, petu := as.character(petu)]
dt[is.na(petu), petu := shnro]

# Act as if petu were address:
setnames(dt, old='petu', new='address.id')


# III Household size strata. ----------------------------------------------


# Household size strata is used in randomization.

# Initialize strata that may be "too granular" (N_s < 2):
dt[, strata := .N, by = address.id]

# Compute stratum sizes and sort by household size (largest to smallest):
dt.help <-unique(dt[, .(address.id, strata)])[, .N, by='strata'][order(-strata)]
dt.help


# A function that (by looping) merges too small strata to the next stratum,
# merging from above.

aggr.strata.to.next <- function(data, data.help) {
  # INPUTS:
  # data: a data.table containing the trial data with 'strata' covariate
  # data.help: a smaller data.table containing sample sizes ('N') for each 
  #     strata, ordered so that the merging can be done from above.
  
  
  DT <- data[, mget(colnames(data))]
  DT.help <- data.help[, mget(colnames(data.help))]
  
  # Store the original strata labels before aggregation:
  strata.orig <- DT.help[, strata]
  
  # Initialize a table that will collect how the strata labels are updated:
  i <- 0
  strata.changes <- data.table(old = integer(length = 0),
                               new = integer(length = 0))
  
  # Loop over strata (from the top to the bottom) until N_s >= 2 for all strata:
  
  while(DT.help[, min(N)] < 2) {
    i <- i + 1
    
    # Aggregate if N_s < 2 for a given stratum:
    
    if(DT.help[strata==strata.orig[i], N] < 2) {
      
      # Combine a stratum that is too small to the next stratum:
      DT.help[strata==strata.orig[i+1], 
              N := N + DT.help[strata==strata.orig[i], N]]
      DT.help <- DT.help[strata != strata.orig[i]]
      
      # Store the information on which strata were merged:
      strata.changes <- rbind(
        strata.changes, 
        data.table(old = strata.orig[i], new = strata.orig[i+1]))
      
    }
    
  }
  
  # Loop over the merges and update the "strata" variable:
  for(i in 1:nrow(strata.changes)) {
    DT[strata==strata.changes[, old][i], strata := strata.changes[, new][i]]
  }
  
  return(DT)
  
}

# Merge too small strata to the next stratum:
dt <- aggr.strata.to.next(data = dt, data.help = dt.help)

# Check that the code works:
unique(dt[, .(address.id, strata)])[, .N, by='strata'][order(-strata)]



# IV Placebo randomization. -----------------------------------------------


# Set seed:
set.seed(12345)

# Household IDs and strata
dt.rand <- unique(dt[, .(address.id, strata)])
dt[, strata := NULL]


# A function that randomizes by household, stratified by household size.

randomize <- function(data.rand, data.ind) {
  # INPUTS:
  # data.rand: a data.table at the level of randomization with variable 'strata' 
  #           with N_s >= 2 for all strata.
  # data.ind: a data.table at the individual level containing study population
  # OUTPUTS:
  # the same data.table as data.ind but treatment status added (split 1:1)
  
  
  DT <- data.rand[, mget(colnames(data.rand))]
  DT <- DT[order(strata)] # important for this function
  
  treatment.rand <- 
    DT[, {
      # The number of treated given the stratum size. Randomize between
      # using the floor and ceiling function so that approximately 1/2
      # end up being in the control group:
      if( .N %% 2 == 0 ) { 
        n_treat <- 1/2 * .N 
      } else {
        floor_or_ceiling <- rbinom(1, 1, 0.5)
        if(floor_or_ceiling==0) { n_treat <- floor(1/2 * .N) }
        if(floor_or_ceiling==1) { n_treat <- ceiling(1/2 * .N) }
      }
      # Initialize a vector of treatment assignments:
      treat.fill <- rep(0, .N)
      # Assing treatment to rows with selected indices. Do this by
      # selecting a random number, ordering them from smallest to largest,
      # and taking the desired number (n_treat) of indices that have the 
      # smallest random numbers.
      rand.numbers <- runif(.N)
      ord <- order(rand.numbers)
      treat.fill[ord[seq_len(n_treat)]] <- 1
      list(treat = treat.fill)
    }, by=strata][, treat]
  
  DT[, treated := treatment.rand]
  
  # Merge treatment status to individual level data:
  DT <- merge(data.ind, DT, by='address.id', all.x = TRUE)
  
  return(DT)
  
}

dt <- randomize(data.rand=dt.rand, data.ind=dt)


# V Simulate D_1 -------------------------------------------------------

# We need to simulate the number of digital clinic contacts for each individual, 
# representing potential outcome (D_1) if the person is offered access to the 
# digital clinic.

# We want to satisfy the following constraints:
# 0.23 contacts per resident, 5.3% had at least one contact, and
# 4.4 contacts per user.
# We use the negative binomial distribution with mu=0.23 and size=0.0225.
100 * (1 - dnbinom(0, mu=0.23, size=0.0225)) # 5.29 % had at least one contact
set.seed(42)
draw <- rnbinom(nrow(dt), mu=0.23, size=0.0225)
100 - 100 * sum(draw==0) / length(draw) # 5.39 % had at least one contact
mean(draw) # 0.23 contacts per resident
mean(draw[draw > 0]) # 4.2 contacts per client

draw <- data.table(shnro = dt[, shnro], D_1 = draw)

dt <- merge(dt, draw, by='shnro', all.x = TRUE)

# The "untreated" should have zero for D_1:
dt[treated==0, D_1 := 0]


# VI Covariates from FOLK -------------------------------------------------


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
vars.ostro <- c('address.id', 'strata', 'treated', 'D_1')
vars.ostro <- c(vars.ostro, vars.total)
dt <- dt[, mget(vars.ostro)]

# Drop individuals who are not observed in the FOLK data:
dt[, 100 * mean(is.na(kunta31_12))]
dt[, sum(is.na(kunta31_12))]
dt <- dt[!is.na(kunta31_12)]
print(dt[, uniqueN(shnro)])

# Save:
fwrite(dt, file=output_population)


# End ---------------------------------------------------------------------


