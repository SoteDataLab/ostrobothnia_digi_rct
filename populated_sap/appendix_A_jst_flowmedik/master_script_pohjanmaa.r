
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
###          r-script master_script_pohjanmaa.r       ###
###                       2026 TH                     ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #

# Master script for analyses conducted outside Fiona.
rm(list = ls())

# Load all packages used in the project. ###
library(data.table)  # Fast data manipulation 
library(readxl)      # Read Excel files 
library(ggplot2)     # Plotting 
library(viridis)     # Colour palette 
library(stringr)     # String utilities 
library(scales)      # Axis formatting 

###
###


#   master_script_pohjanmaa.r  (this file)
#     Lists all scripts, their purpose, and loads all
#     packages used across the project.
#
#   update_translations_oirelahtoryhma.r
#     Adds the oireryhma (symptom group) column to
#     translations_oirelahtoryhma.csv, linking each
#     granular symptom onset group to its parent group.
#     Run this first if the translation mapping changes.
#     Input/output: data/raw/translations_oirelahtoryhma.csv
#
#   plot_jst_data.r
#     Plots the share of symptom groups (oireryhmät) and
#     symptom onset groups (oirelähtöryhmät) from digital
#     clinic session data. Produces bar charts and share
#     plots in Finnish, Swedish, and English.
#     Inputs:  data/raw/pohjanmaa_jst_oireryhma_*.xlsx
#              data/raw/pohjanmaa_jst_oirelahtoryhma_*.xlsx
#              data/raw/translations_oireryhma.csv
#              data/raw/translations_oirelahtoryhma.csv
#     Outputs: figures/sessiot_ajanjakso_oireryhma.pdf
#              figures/sessiot_ajanjakso_oirelahtoryhma.pdf
#              figures/oireryhmat_{fin,swe,eng}.pdf
#              figures/oirelahtoryhmat_{fin,swe,eng}.pdf
#              figures/oireryhmat_lahtodata_{fin,swe,eng}.pdf
#
#   plot_chats_daily_flowmedik_data.r
#     Reads daily digital clinic contact counts, computes
#     the mean number of contacts per day by ISO week,
#     and plots the result. Figures in Finnish, Swedish,
#     and English.
#     Input:   data/raw/chats_daily.csv
#     Outputs: figures/digiclinic_contacts_weekly_mean_per_day
#              _{fin,swe,eng}.pdf
#
#   plot_waittime_daily_flowmedik_data.r
#     Reads daily mean waiting times, converts HH:MM:SS to
#     minutes, computes the mean waiting time per day by ISO
#     week, and plots the result. Figures in Finnish, Swedish,
#     and English.
#     Input:   data/raw/waittime_daily.csv
#     Outputs: figures/digiclinic_waittime_weekly_mean
#              _{fin,swe,eng}.pdf
#
#   plot_survey_responses.r
#     Plots the share of yes answers to professional feedback
#     questions on remote service contacts, with extreme-scenario
#     bounds (all missing = no / all missing = yes). Figures in
#     Finnish, Swedish, and English.
#     Input:   data/raw/survey_responses.csv
#     Outputs: figures/survey_responses_{fin,swe,eng}.pdf



