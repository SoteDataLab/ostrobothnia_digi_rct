
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
###                 r-script 2_data.R                 ###
###                 Replication file.                 ###
###                   2025 by TH, MS                  ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Create outcomes for the study population and for the full population.
rm(list=ls())

# Install and load the following packages:
library(here)             # Relative file paths.
library(data.table)       # mutating and aggregating data


# I Define inputs and outputs -----------------------------------------------


# Input folder for Avohilmo:
input_folder <- "~DATAINFRA/THL/cleaned"

# Study and total population
input_population <- here('data', 'study_population_placebo.csv')
input_total_population <- here('data', 'total_population.csv')

# Health care organizations
input_soteorg <- "~DATAINFRA/SOTEorg/cleaned/sote_2025_02_nimet_250210.csv"

# ED units:
input_ed <- "~PROJECTS/pohjanmaa_rct/sap/data/ed_centers.csv"

# Primary health care
inputs <- list.files(input_folder)
inputs <- inputs[(grepl("(avohilmo(.*)202[2-4]{1})|hta", inputs))]
inputs_avosh <- inputs[grep('avosh_|tth_', inputs)]
inputs_hta <- inputs[grep('hta_', inputs)]

# Private clinics
# Inputs:
inputs_kela <- list.files("~DATAINFRA/KELA/cleaned", 
                          full.names = T)
inputs_kela <- inputs_kela[grepl('laakarinpalkkiot_202[2-5]{1}', inputs_kela)]

# Hospital visits
inputs_hilmo <- list.files(input_folder, full.names = T)
inputs_hilmo <- inputs_hilmo[grepl("hilmo_tupva202[2-4]{1}", inputs_hilmo)]

# Referrals
inputs_referral <- list.files(here(input_folder, "sidetables_avohilmo"), 
                              full.names = T)
inputs_referral <- inputs_referral[grepl("lahete", inputs_referral)]


# Have a look at all the input files
print(c(inputs_avosh, inputs_hta, inputs_hilmo, inputs_referral))

# Define outputs:
output_panel <- here('data', 'person_by_date_panel.csv')
output_pre_post <- here('data', 'person_by_prepost_12mon.csv')
output_total <- here('data', 'person_by_prepost_12mon_total_population.csv')


# Define study window:
min_date <- as.Date('2022-04-01') # NOTE: '2024-04-15' in the final paper.
max_date <- as.Date('2024-03-31') # NOTE: '2026-01-14' in the final paper.
treatment <- as.Date('2023-04-01') # NOTE: '2025-04-15' in the final paper.


# II initialize panels -------------------------------------------------------


# Read the sote organization register
soteorg <- fread(input_soteorg, na.strings = c(NA_character_, ''))

# Read the the org. ids of emergency departments 
ed_centers <- fread(input_ed, na.strings = c(NA_character_, ''))

# Keep only ED departments
soteorg_ed <- soteorg[tunniste_oid_s %in% ed_centers & vuosi == 2023 & kk == 3]

# Read in the study population:
population_study <- fread(input_population, select = 'shnro', 
                          na.strings = c(NA_character_, ''))
population_total <- fread(input_total_population, select = 'shnro', 
                          na.strings = c(NA_character_, ''))

# Initiate a person-date panel on healthcare use for the study population:
dt <- CJ(shnro = population_study[, unique(shnro)],
         date = as.Date(c(min_date : max_date), origin='1970-01-01'))

# Create variable period
dt[date >= treatment, period := 'post']
dt[date < treatment, period := 'pre']

# Initiate a person-by_prepost panel on healthcare use for total population:
dt_total <- CJ(shnro = population_total[, unique(shnro)], 
               period = c('pre', 'post'))

# III Get the data ------------------------------------------------------------


## 1. Outpatient visits on all sectors --------------------------------------

# Read all files
avosh <- lapply(inputs_avosh, function(input_name) {
  
  print(input_name)
  path <- here(input_folder, input_name)
  
  # Read the datasets and filter
  dt <- fread(path, na.strings = c(NA_character_, ''))
  dt <- dt[kaynti_alkoi_pvm_c >= min_date &
             kaynti_alkoi_pvm_c <= max_date &
             shnro %in% population_total[, unique(shnro)] & 
             kaynti_luonne == 'SH' &
             kaynti_kavijaryhma == 1 &
             kaynti_ammattiluokka %in% c('SH', 'LK')]
  
})
avosh <- rbindlist(avosh)

# Create a variable for the three different sectors
avosh[, sector := fcase(
  sektori == 1 & kaynti_palvelumuoto == 'T11', 'public',
  #sektori == 2 & kaynti_palvelumuoto == 'T11', 'private', 
  kaynti_palvelumuoto %in% c('T30', 'T31', 'T32'), "occup")]

# Drop rows where sector is missing (= private visits and missing values):
avosh <- avosh[!is.na(sector)]

# Select covariates
avosh <- avosh[,.(avohilmoid, shnro, kaynti_yhteystapa, sector, hva,
                  kaynti_alkoi_pvm_c, kaynti_ammattiluokka, kaynti_alkoi_aika, 
                  digiklinikka_hva, oid_suora_tunniste_s)]


## 2. Care needs assessments -----------------------------------------------

# Read all files
hta <- lapply(inputs_hta, function(input_name) {
  
  print(input_name)
  path <- here(input_folder, input_name)
  
  # Read the datasets and select covariates:
  dt <- fread(path, na.strings = c(NA_character_, ''))
  dt <- dt[hta_pvm_c >= min_date &
             hta_pvm_c <= max_date &
             shnro %in% population_total[, unique(shnro)] & 
             sektori == 1,
           .(avohilmoid, oid_suora_tunniste_s,hva, shnro, hta_pvm_c, hta_aika, 
             hta_ammattiluokka, digiklinikka_hva)]
  dt[, kaynti_yhteystapa :='hta']
  
})

hta <- rbindlist(hta)


# We include as separate rows those care needs assessments for which:
#  we observe no timestamp for the visit (kaynti_alkoi) OR
# - we observe that the timestamp for the visit differs from the time stamp
#   for the triage.

# Merge outpatient data to care needs assesments:
hta <- 
  merge(hta, avosh[sector=="public", .(avohilmoid, sector,
                                       kaynti_alkoi_aika, kaynti_alkoi_pvm_c)],
        by='avohilmoid', all.x = TRUE)
hta[is.na(sector), sector := 'public']

# Keep only separate care needs assesments
hta <- hta[is.na(kaynti_alkoi_pvm_c) |
             !(kaynti_alkoi_aika == hta_aika & kaynti_alkoi_pvm_c == hta_pvm_c)]

# Drop unnecessary columns
hta[, ':=' (kaynti_alkoi_aika = NULL, kaynti_alkoi_pvm_c = NULL)]

# Rename variables
setnames(hta, old = c('hta_pvm_c', 'hta_ammattiluokka', 'hta_aika'),
         new = c('kaynti_alkoi_pvm_c', 'kaynti_ammattiluokka', 
                 'kaynti_alkoi_aika'))

# Combine outpatient data with care needs assesments 
phc <- rbind(hta, avosh, fill = TRUE)

# Separate ea visits from phc
ed_visits <- phc[oid_suora_tunniste_s %in% soteorg_ed$tunniste_oid_s]
phc <- phc[!oid_suora_tunniste_s %in% soteorg_ed$tunniste_oid_s]


# Remove objects, release memory
rm(hta, avosh)
gc()

# Create variable period
phc[, period := fifelse(kaynti_alkoi_pvm_c >= treatment, 'post', 'pre')]


## 3. Reimbursed private visits -----------------------------------------------


# Read the data
private <- lapply(inputs_kela, fread, na.strings = c(NA_character_, ''))
private <- rbindlist(private)

# Keep only rows that are within the study window
private <- private[kaynti_pv >= min_date & kaynti_pv < max_date]

# Keep only the target population
private <- private[shnro %in% population_total[, unique(shnro)]]

# Select variables
private <- private[, .(shnro, kaynti_pv, hoidonantaja_koodi, toimenpide_koodi, 
                       toimenpidekust_eur, toimenpidekorv_eur, korvaustaksa_eur)]

# The share of missing values (looks good for all covariates):
print(nrow(private))
print(round(100 * colMeans(is.na(private)), digits=1))

# Create an indicator for whether the contact was remote or in-person (2022-25)
# NOTE: doctor's certificates not included.
eta <- c('E10PS', 'E10VI', 'E101L', 'E101V', 'H101E', 'H101V', 'N101E', 
         'N101V', 'E101T', 'E101P',
         'P010V', 'V010V' # 65+
         )
lasna <- c('H101L', 'N101L', '0101A', '0101B', '0101C', '0101D', 
           '0101E', '0101L',
           'K010V' # 65+
           )
private[toimenpide_koodi %in% eta, etapalvelu := 1]
private[toimenpide_koodi %in% lasna, etapalvelu := 0]

## remove rows that are not in person or telemedicine
private <- private[!is.na(etapalvelu)]

# Create variable period
private[, period := fifelse(kaynti_pv >= treatment, 'post', 'pre')]


## 3. Hospital visits --------------------------------------------------------


# Read hilmo data
hilmo <- lapply(inputs_hilmo, fread, na.strings = c(NA_character_, ''))
hilmo <- rbindlist(hilmo)

# Keep only the target population
hilmo <- hilmo[shnro %in% population_total[, unique(shnro)]]

# Keep only rows that are within the study window
hilmo <- hilmo[tupva >= min_date & tupva < max_date]

# Create variable: length of hospital stay
hilmo[, stay := lpvm - tupva ]

# If length of stay equals to 0, then define the visit as "out-patient"
hilmo[, type := fifelse(stay < 1, "out", "in" )]
hilmo[, outpatient := as.integer(type == "out")]

# Indicator for urgent care
hilmo[, urgent_in_person := 
        fifelse(yhteystapa == "R10" & 
                  (kiireellisyys == 6 & grepl("15", ea)),1,0)]
hilmo[, urgent_other := 
        fifelse(yhteystapa %in% c('R50','R51','R52','R55','R56', # telemedicine
                                  'R60', 'R71') # prof to prof interactions
                & (kiireellisyys == 6 & grepl("15", ea)),1,0)]

# Select variables
hilmo <- hilmo[, .(shnro, hilmoid, tupva, yhteystapa, ammattiluokka,hva,
                   urgent_in_person, urgent_other, outpatient, sektori)]

# Add avohilmo ED visits to the hilmo data set
ed_visits <- ed_visits[, .(shnro, tupva = kaynti_alkoi_pvm_c,
                           yhteystapa = kaynti_yhteystapa)]
ed_visits[, urgent_in_person := fifelse(yhteystapa == "R10",1,0)]
ed_visits[, urgent_other := fifelse(yhteystapa != "R10",1,0)]
hilmo <- rbind(hilmo, ed_visits, fill = T)


## 5. Referrals ---------------------------------------------------------------

# Read the data
refs <- lapply(inputs_referral, fread, na.strings = c(NA_character_, ''))
refs <- rbindlist(refs)

# Keep only rows that are within the study window
refs <- refs[lahete_pvm_c >= min_date & lahete_pvm_c < max_date]

# Aggregate to visit level
refs <- refs[,.(referrals = .N), by =.(avohilmoid, lahete_pvm_c)]

# Merge with the avohilmo data (keep only referrals that link to avohilmo)
refs <- merge(refs, phc[sector == "public"]) 

# Select covariates
refs <- refs[, .(avohilmoid, shnro, lahete_pvm_c, referrals, kaynti_yhteystapa, 
                 kaynti_ammattiluokka)]


## 6. Overview of the data ----------------------------------------------------


# Overview of digital PPC clinic utilization
#   As the SAP was written for a time period when the digital clinic was not 
#   available, the number of rows in 'phc' is very small.
t <- phc[sector == 'public' & digiklinikka_hva == 1]
t[, .(share_percent = round(100 * .N / nrow(t), digits=4), .N),
    by='kaynti_ammattiluokka'][order(-share_percent)]
t[, .(share_percent = round(100 * .N / nrow(t), digits=4), .N),
    by='kaynti_yhteystapa'][order(-share_percent)]


# Overview of traditional PPC utilization
t <- phc[sector == 'public' & digiklinikka_hva == 0]
t[, .(share_percent = round(100 * .N / nrow(t), digits=4), .N),
  by='kaynti_ammattiluokka'][order(-share_percent)]
t[, .(share_percent = round(100 * .N / nrow(t), digits=4), .N),
  by='kaynti_yhteystapa'][order(-share_percent)]


# Overview of total PPC utilization
t <- phc[sector == 'public' ]
t[, .(share_percent = round(100 * .N / nrow(t), digits=4), .N),
  by='kaynti_ammattiluokka'][order(-share_percent)]
t[, .(share_percent = round(100 * .N / nrow(t), digits=4), .N),
  by='kaynti_yhteystapa'][order(-share_percent)]

# Overview of private clinic utilization
t <- private
t[, .(share_percent = round(100 * .N / nrow(t), digits=4), .N),
          by='hoidonantaja_koodi'][order(-share_percent)]
t[, .(share_percent = round(100 * .N / nrow(t), digits=4), .N),
          by='toimenpide_koodi'][order(-share_percent)]
t[, .(share_percent = round(100 * .N / nrow(t), digits=4), .N),
     by='etapalvelu'][order(-share_percent)]

# Overview of occupational HC utilization
t <- phc[sector == 'occup' ]
t[, .(share_percent = round(100 * .N / nrow(t), digits=4), .N),
  by='kaynti_ammattiluokka'][order(-share_percent)]
t[, .(share_percent = round(100 * .N / nrow(t), digits=4), .N),
  by='kaynti_yhteystapa'][order(-share_percent)]

# Overview of Hospital visits
hilmo[, .(share_percent = round(100 * .N / nrow(hilmo), digits=4), .N),
  by='ammattiluokka'][order(-share_percent)]
hilmo[, .(share_percent = round(100 * .N / nrow(hilmo), digits=4), .N),
  by='yhteystapa'][order(-share_percent)]

# Overview of referrals
refs[, .(share_percent = round(100 * .N / nrow(refs), digits=4), .N),
      by='kaynti_ammattiluokka'][order(-share_percent)]
refs[, .(share_percent = round(100 * .N / nrow(refs), digits=4), .N),
  by='kaynti_yhteystapa'][order(-share_percent)]

## free unused memory
gc()


# IV Create primary outcomes --------------------------------------------------------

# Outcomes: Take unique person-by-contact-date observations:

## 1. Digital PPC clinic utilization. -----------------------------------------

#   Outcome D.1: the number of public digital clinic contacts.
#   This outcome includes care needs assessments, remote appointments to nurses
#   and physicians (via chat and video), and professional-to-professional 
#   interactions between nurses and physicians in digital PPC clinics.

## for study population in Pohjanmaa

# Filter 
d.1 <- phc[sector == 'public' & hva == 'PO' &
           digiklinikka_hva == 1 &  kaynti_yhteystapa %in% 
             c('hta',  # care needs assesments,
               'R10', # "in-person" visits (chat in reality)
               'R50','R51','R52','R55','R56', # telemedicine
               'R60', 'R71')] # prof-to-prof interactions

# Calculate outcome
d.1 <- d.1[, .N, by = .(shnro, kaynti_alkoi_pvm_c, period)
           ][N > 0, .(shnro, kaynti_alkoi_pvm_c, period)][, d.1 := 1]

# Merge the outcomes to the person-date panel
dt <- merge(dt, d.1, by.x = c('shnro', 'date', 'period'), 
            by.y = c('shnro', 'kaynti_alkoi_pvm_c', 'period'), all.x = T)

# Impute zeroes if no health care
dt[is.na(d.1), d.1 := 0]


## for the total population

# Filter
d.1 <- phc[sector == 'public' &
             digiklinikka_hva == 1 &  kaynti_yhteystapa %in% 
             c('hta',  # care needs assesments,
               'R10', # "in-person" visits (chat in reality)
               'R50','R51','R52','R55','R56', # telemedicine
               'R60', 'R71')] # prof-to-prof interactions

# Calculate outcome
d.1 <- d.1[, .N, by = .(shnro, kaynti_alkoi_pvm_c, period)
           ][N > 0, .(shnro, kaynti_alkoi_pvm_c, period)][, d.1 := 1]

# Aggregate to period level
d.1 <- d.1[, .(d.1 = .N), by=c('shnro', 'period')]

# Merge the outcomes to the person-period panel
dt_total <- merge(dt_total, d.1, by = c('shnro', 'period'), all.x = T)

# Impute zeroes if no health care
dt_total[is.na(d.1), d.1 := 0]

rm(d.1);gc()


## 2. Traditional PPC utilization ---------------------------------------------

#   Outcome Y1.1: in person visits in public primary care (PPC).
#   This outcome includes in-person visits to nurses and physicians in 
#   traditional PPC clinics.


## for study population in Pohjanmaa

# Filter 
y1.1 <- phc[sector == 'public' & hva == 'PO' &
              digiklinikka_hva == 0 & kaynti_yhteystapa == 'R10']

# Calculate outcome
y1.1 <- y1.1[, .N, by=c('shnro', 'kaynti_alkoi_pvm_c', 'period')
             ][N > 0, .(shnro, kaynti_alkoi_pvm_c,period) ][, y1.1 := 1]

# Merge the outcomes to the person-date panel
dt <- merge(dt, y1.1, by.x = c('shnro', 'date', 'period'),
            by.y = c('shnro', 'kaynti_alkoi_pvm_c', 'period'), all.x = T)

# Impute zeroes if no health care
dt[is.na(y1.1), y1.1 := 0]


## for the total population

## Filter
y1.1 <- phc[sector == 'public' & 
              digiklinikka_hva == 0 & kaynti_yhteystapa == 'R10']

# Calculate outcome
y1.1 <- y1.1[, .N, by=c('shnro', 'kaynti_alkoi_pvm_c', 'period')
             ][N > 0, .(shnro, kaynti_alkoi_pvm_c,period) ][, y1.1 := 1]

# Aggregate to period level
y1.1 <- y1.1[, .(y1.1 = .N), by=c('shnro', 'period')]

# Merge the outcomes to the person-period panel
dt_total <- merge(dt_total, y1.1, by = c('shnro', 'period'), all.x = T)

# Impute zeroes if no health care
dt_total[is.na(y1.1), y1.1 := 0]

rm(y1.1);gc()


# Outcome Y1.2: other contacts with traditional PPC.
#   This outcome includes care needs assessments, remote appointments to nurses
#   and physicians, and professional-to-professional interactions between
#   nurses and physicians in traditional PPC clinics.


## for study population in Pohjanmaa

# Filter 
y1.2 <- phc[sector == 'public' & hva == 'PO' & 
              digiklinikka_hva == 0 & kaynti_yhteystapa %in%
              c('hta', # care needs assesments
                'R50','R51','R52','R55','R56', # telemedicine
                'R60', 'R71')]  # prof-to-prof interactions 

# Calculate outcome           
y1.2 <- y1.2[, .N, by=c('shnro', 'kaynti_alkoi_pvm_c', 'period')
             ][N > 0, .(shnro, kaynti_alkoi_pvm_c, period)][, y1.2 := 1]

# Merge the outcomes to the person-date panel
dt <- merge(dt, y1.2, by.x=c('shnro', 'date' ,'period'),
            by.y=c('shnro', 'kaynti_alkoi_pvm_c','period'), all.x=TRUE)

# Impute zeroes if no health care
dt[is.na(y1.2), y1.2 := 0]


## for the total population

# Filter
y1.2 <- phc[sector == 'public' & 
              digiklinikka_hva == 0 & kaynti_yhteystapa %in%
              c('hta', # care needs assesments
                'R50','R51','R52','R55','R56', # telemedicine
                'R60', 'R71')]  # prof-to-prof interactions 

# Calculate outcome           
y1.2 <- y1.2[, .N, by=c('shnro', 'kaynti_alkoi_pvm_c', 'period')
             ][N > 0, .(shnro, kaynti_alkoi_pvm_c, period)][, y1.2 := 1]

# Aggregate to period level
y1.2 <- y1.2[, .(y1.2 = .N), by=c('shnro', 'period')]

# Merge the outcomes to the person-period panel
dt_total <- merge(dt_total, y1.2, by = c('shnro', 'period'), all.x = T)

# Impute zeroes if no health care
dt_total[is.na(y1.2), y1.2 := 0]

rm(y1.2);gc()


## 3. Total PPC utilization. --------------------------------------------------


# Outcome Y1.3: the total number of contacts to public primary care (PPC).
#   This outcome includes in-person visits to nurses and physicians, care needs 
#   assessments, remote appointments to nurses and physicians (via chat,
#   video, and phone), and professional-to-professional interactions between 
#   nurses and physicians in digital and traditional PPC clinics.


## for study population in Pohjanmaa

# Filter
y1.3 <- phc[sector == 'public' & hva == 'PO' & kaynti_yhteystapa %in% 
              c('hta',  # care needs assesments
                'R10', # "in-person" visits (chat in reality)
                'R50','R51','R52','R55','R56', # telemedicine
                'R60', 'R71') ] # prof-to-prof interactions

# Calculate outcome   
y1.3 <- y1.3[, .N, by=c('shnro', 'kaynti_alkoi_pvm_c','period')
             ][N > 0, .(shnro, kaynti_alkoi_pvm_c, period)][, y1.3 := 1]


# Merge the outcomes to the person-date panel
dt <- merge(dt, y1.3, by.x = c('shnro', 'date','period'),
            by.y = c('shnro', 'kaynti_alkoi_pvm_c','period'), all.x = T)

# Impute zeroes if no health care
dt[is.na(y1.3), y1.3 := 0]


## for the total population

# Filter
y1.3 <- phc[sector == 'public' & kaynti_yhteystapa %in% 
              c('hta',  # care needs assesments
                'R10', # "in-person" visits (chat in reality)
                'R50','R51','R52','R55','R56', # telemedicine
                'R60', 'R71') ] # prof-to-prof interactions

# Calculate outcome   
y1.3 <- y1.3[, .N, by=c('shnro', 'kaynti_alkoi_pvm_c','period')
             ][N > 0, .(shnro, kaynti_alkoi_pvm_c, period)][, y1.3 := 1]

# Aggregate to period level
y1.3 <- y1.3[, .(y1.3 = .N), by=c('shnro', 'period')]

# Merge the outcomes to the person-period panel
dt_total <- merge(dt_total, y1.3, by = c('shnro', 'period'), all.x = T)

# Impute zeroes if no health care
dt_total[is.na(y1.3), y1.3 := 0]

rm(y1.3);gc()


# V Create secondary outcomes --------------------------------------------



## 1. Hospital --------------------------------------------------------------


# Outcome Y2.1:
# Number of referrals from primary healthcare to hospitals

# Calculate outcome 
referrals <- refs[, .(referrals = sum(referrals)), by=c('shnro','lahete_pvm_c')] 

# Merge the outcomes to the person-date panel
dt <- merge(dt, referrals, by.x = c('shnro', 'date'),
            by.y = c('shnro', 'lahete_pvm_c'), all.x = T)

# Impute zeroes if no health care
dt[is.na(referrals), referrals := 0]



# Outcome  Y2.2
# number of in person emergency department visits

# Calculate outcome 
ed.in.person <- hilmo[urgent_in_person == 1, .N, by=c('shnro', 'tupva')
                      ][N > 0, .(shnro, tupva)
                        ][, ed.in.person := 1]

# Merge the outcomes to the person-date panel
dt <- merge(dt, ed.in.person, by.x = c('shnro', 'date'),
            by.y = c('shnro', 'tupva'), all.x = T)

# Impute zeroes if no health care
dt[is.na(ed.in.person), ed.in.person := 0]

rm(ed.in.person);gc()


# Outcome Y2.3
# number of other emergency department visits

# Calculate outcome 
ed.other <- hilmo[urgent_other == 1, .N, by = c('shnro', 'tupva')
                  ][N > 0, .(shnro, tupva)
                    ][, ed.other := 1]

# Merge the outcomes to the person-date panel
dt <- merge(dt, ed.other, by.x = c('shnro', 'date'),
            by.y = c('shnro', 'tupva'), all.x = T)

# Impute zeroes if no health care
dt[is.na(ed.other), ed.other := 0]

rm(ed.other);gc()


# Outcome Y2.4: 
# Number of outpatient hospital visits

# Calculate outcome 
hospital.visit <- hilmo[outpatient == 1 & sektori == 1  & hva == 'PO' &
                          yhteystapa %in%  c('R10', 'R80') #"in-person" visits and hospital stays
                          , .N, by=c('shnro', 'tupva')
                        ][N > 0, .(shnro, tupva)
                          ][, hospital.visit := 1]

# Merge the outcomes to the person-date panel
dt <- merge(dt, hospital.visit, by.x = c('shnro', 'date'),
            by.y = c('shnro', 'tupva'), all.x = T)

# Impute zeroes if no health care
dt[is.na(hospital.visit), hospital.visit := 0]

rm(hospital.visit);gc()


## 2. Occupational healthcare utilization ----------------------------------


# Outcome Y3.1 occup.inperson:
# In-person visits to nurses or physicians.

# Filter
occup.inperson <- phc[sector == 'occup' & kaynti_yhteystapa == 'R10']

# Calculate outcome 
occup.inperson <- 
  occup.inperson[, .N, by=c('shnro', 'kaynti_alkoi_pvm_c','period')
                 ][N > 0, .(shnro, kaynti_alkoi_pvm_c, period)
                   ][, occup.inperson := 1]

# Merge the outcomes to the person-date panel
dt <- merge(dt, occup.inperson, by.x=c('shnro', 'date','period'),
            by.y=c('shnro', 'kaynti_alkoi_pvm_c','period'), all.x = T)

# Impute zeroes if no health care
dt[is.na(occup.inperson), occup.inperson := 0]


# Aggregate to period level
occup.inperson <- 
  occup.inperson[, .(occup.inperson = .N), by=c('shnro', 'period')]

# Merge the outcomes to the person-period panel
dt_total <- merge(dt_total, occup.inperson, by = c('shnro', 'period'), all.x=T)

# Impute zeroes if no health care
dt_total[is.na(occup.inperson), occup.inperson := 0]

rm(occup.inperson);gc()


# Outcome Y3.2 occup.telemed:
# Include telemedicine conducted by nurses or physicians.

# Filter
occup.telemed <- phc[sector == 'occup' & kaynti_yhteystapa %in% 
                       c('R50','R51','R52','R55','R56')] # telemed

# Calculate outcome 
occup.telemed <- 
  occup.telemed[, .N, by=c('shnro', 'kaynti_alkoi_pvm_c','period')
                ][N > 0, .(shnro, kaynti_alkoi_pvm_c, period)
                  ][, occup.telemed := 1]

# Merge the outcomes to the person-date panel
dt <- merge(dt, occup.telemed, by.x=c('shnro', 'date','period'),
            by.y=c('shnro', 'kaynti_alkoi_pvm_c','period'), all.x=TRUE)

# Impute zeroes if no health care
dt[is.na(occup.telemed), occup.telemed := 0]


# Aggregate to period level
occup.telemed <- occup.telemed[, .(occup.telemed = .N), by=c('shnro', 'period')]

# Merge the outcomes to the person-period panel
dt_total <- merge(dt_total, occup.telemed, by = c('shnro', 'period'), all.x = T)

# Impute zeroes if no health care
dt_total[is.na(occup.telemed), occup.telemed := 0]

rm(occup.telemed);gc()


# 2.3 Outcome occup.total:
# Include in-person visits and telemedicine conducted by nurses or physicians.

# Filter
occup.total <- phc[sector == 'occup' & kaynti_yhteystapa %in%
                     c('R10', 'R50','R51','R52','R55','R56')]

# Calculate outcome 
occup.total <- occup.total[, .N, by=c('shnro', 'kaynti_alkoi_pvm_c','period')
                           ][N > 0, .(shnro, kaynti_alkoi_pvm_c, period)
                             ][, occup.total := 1]

# Merge the outcomes to the person-date panel
dt <- merge(dt, occup.total, by.x=c('shnro', 'date','period'),
            by.y=c('shnro', 'kaynti_alkoi_pvm_c','period'), all.x=TRUE)

# Impute zeroes if no health care
dt[is.na(occup.total), occup.total := 0]

# Aggregate to period level
occup.total <- occup.total[, .(occup.total = .N), by=c('shnro', 'period')]

# Merge the outcomes to the person-period panel
dt_total <- merge(dt_total, occup.total, by = c('shnro', 'period'), all.x = T)

# Impute zeroes if no health care
dt_total[is.na(occup.total), occup.total := 0]

rm(occup.total);gc()



## 3. Private clinic utilization ------------------------------------------


# Outcome Y3.3: private.inperson:
# In-person visits to nurses or physicians.

# Filter
private.inperson <- private[etapalvelu == 0]

# Calculate outcome 
private.inperson <- private.inperson[, .N, by=c('shnro', 'kaynti_pv','period')
                                     ][N > 0, .(shnro, kaynti_pv, period)
                                       ][, private.inperson := 1]

# Merge the outcomes to the person-date panel
dt <- merge(dt, private.inperson, by.x=c('shnro', 'date','period'),
            by.y=c('shnro', 'kaynti_pv','period'), all.x=TRUE)

# Impute zeroes if no health care
dt[is.na(private.inperson), private.inperson := 0]

# Aggregate to period level
private.inperson <- private.inperson[, .(private.inperson = .N), 
                                     by=c('shnro', 'period')]

# Merge the outcomes to the person-period panel
dt_total <- merge(dt_total, private.inperson, by=c('shnro', 'period'), all.x=T)

# Impute zeroes if no health care
dt_total[is.na(private.inperson), private.inperson := 0]

rm(private.inperson);gc()


# Outcome Y3.4: private.telemed:
# Include telemedicine conducted by nurses or physicians.

# Filter
private.telemed <- private[etapalvelu == 1] # telemed

# Calculate outcome 
private.telemed <- private.telemed[, .N, by=c('shnro', 'kaynti_pv','period')
                                   ][N > 0, .(shnro, kaynti_pv, period)
                                     ][, private.telemed := 1]

# Merge the outcomes to the person-date panel
dt <- merge(dt, private.telemed, by.x=c('shnro', 'date','period'),
            by.y=c('shnro', 'kaynti_pv','period'), all.x=TRUE)

# Impute zeroes if no health care
dt[is.na(private.telemed), private.telemed := 0]

# Aggregate to period level
private.telemed <- private.telemed[, .(private.telemed = .N), 
                                   by=c('shnro', 'period')]

# Merge the outcomes to the person-period panel
dt_total <- merge(dt_total, private.telemed, by = c('shnro', 'period'), all.x=T)

# Impute zeroes if no health care
dt_total[is.na(private.telemed), private.telemed := 0]

rm(private.telemed);gc()


# 1.3 Outcome private.total:
# Include in-person visits and telemedicine conducted by nurses or physicians.

# Filter
private.total <- private

# Calculate outcome 
private.total <- private.total[, .N, by=c('shnro', 'kaynti_pv','period')
                               ][N > 0, .(shnro, kaynti_pv, period)
                                 ][, private.total := 1]

# Merge the outcomes to the person-date panel
dt <- merge(dt, private.total, by.x=c('shnro', 'date','period'),
            by.y=c('shnro', 'kaynti_pv','period'), all.x=TRUE)

# Impute zeroes if no health care
dt[is.na(private.total), private.total := 0]

# Aggregate to period level
private.total <- private.total[, .(private.total = .N), by=c('shnro', 'period')]

# Merge the outcomes to the person-period panel
dt_total <- merge(dt_total, private.total, by = c('shnro', 'period'), all.x = T)

# Impute zeroes if no health care
dt_total[is.na(private.total), private.total := 0]

rm(private.total);gc()



# VI Save ------------------------------------------------------------------


## 1. Person-date panel ---------------------------------------------------

# Save
fwrite(dt, file = output_panel)

# Have a look at the final data frame
print(dt)


## 2. Pre-post data -----------------------------------------------------------

# Compute sums of contacts before and after the digital clinic launch at the
# individual level:

# List all the outcomes here:
vars <- c('d.1', 'y1.1', 'y1.2', 'y1.3',
          'private.inperson', 'private.telemed', 'private.total',
          'occup.inperson', 'occup.telemed', 'occup.total',
          'referrals', 'ed.in.person', 'ed.other', 'hospital.visit')

follow_up_ends <- max_date # the date on which the follow-up ends:(COMMENT: why not using just max date?)

dt <- dt[date <= follow_up_ends,
         lapply(.SD, sum), .SDcols=vars, by=c('shnro', 'period')]
dt <- dcast(dt, shnro ~ period, value.var = vars)


# Outcome D.2: an indicator for having any public digital clinic contact during
# the follow-up.
#   This outcome includes care needs assessments, remote appointments to nurses
#   and physicians (via chat and video), and professional-to-professional
#   interactions between nurses and physicians in digital PPC clinics.

dt[, d.2_post := as.integer(d.1_post > 0)]
dt[, d.2_pre := as.integer(d.1_pre > 0)]

# Save:
data.table::fwrite(dt, file=output_pre_post)
print(dt)


## 3. Total population data ---------------------------------------------------


# List all the outcomes here:
vars <- c('d.1', 'y1.1', 'y1.2', 'y1.3',
          'private.inperson', 'private.telemed', 'private.total',
          'occup.inperson', 'occup.telemed', 'occup.total')

# Pivot wider:
dt_total <- dcast(dt_total, shnro ~ period, value.var = vars)


# Outcome D.2: an indicator for having any public digital clinic contact during
# the follow-up.
#   This outcome includes care needs assessments, remote appointments to nurses
#   and physicians (via chat and video), and professional-to-professional
#   interactions between nurses and physicians in digital PPC clinics.
dt_total[, d.2_post := as.integer(d.1_post > 0)]
dt_total[, d.2_pre := as.integer(d.1_pre > 0)]


# Save:
fwrite(dt_total, file = output_total)



# End ---------------------------------------------------------------------


