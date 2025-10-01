
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
###            r-script 4_plot_outcomes.R             ###
###                 Replication file.                 ###
###                    2025 by TH, MS                 ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Plot outcomes as a function of time relative to treatment.
rm(list=ls())

# I Define inputs and outputs -----------------------------------------------

# Install and load the following packages:
library(here)             # Relative file paths.
library(data.table)       # mutating and aggregating data
library(ggplot2)          # Plotting data.
library(viridis)          # accessible color palettes.

# inputs:
input_population <- here('data', 'study_population_placebo.csv')
input_panel <- here('data', 'person_by_date_panel.csv')

# outputs:
output_plot.Y1.X <- here('figures', 'diff_in_cum_outcomes_Y1.X.pdf')
output_plot.Y2.X <- here('figures', 'diff_in_cum_outcomes_Y2.X.pdf')
output_plot.Y3.X <- here('figures', 'diff_in_cum_outcomes_Y3.X.pdf')

# treatment time: 
treatment_start <- as.Date('2023-04-01')  # REPLACE WITH THE TRUE TREATMENT DATE LATER



# II Get the data ---------------------------------------------------------

# Study population:
folk <- fread(input_population, na.strings = c(NA_character_, ''),
              select = c('shnro', 'treated', 'ika'))

# Analysis sample is restricted to those aged 0-70:
folk <- folk[ika %in% c(0:70)]
folk[, ika := NULL]

# Panel on health care use:
dt <- fread(input_panel, na.strings = c(NA_character_, ''))

# Dates as integer relative to treatment:
dt[, relative.time := as.integer(date) - as.integer(treatment_start)]
dt[, date := NULL]

# We are interested in time starting from the treatment
dt <- dt[relative.time >=0]

# Merge study population and visit data:
dt <- merge(dt, folk, by='shnro', all.x = TRUE)
dt <- dt[!is.na(treated)]

# Free some memory:
gc()



# III Aggregate -------------------------------------------------------------


# We will compute cumulative sums for the following outcomes:
outcomes_y1 <- c('d.1', 'y1.1', 'y1.2','y1.3')
outcomes_y2 <- c('d.1','referrals','ed.in.person', 'ed.other', 'hospital.visit')
outcomes_y3 <- c('d.1','occup.inperson', 'occup.telemed',
                 'private.inperson','private.telemed')

# Put the vectors to a list
outcome_list <- list(y1 = outcomes_y1, y2 = outcomes_y2, y3 = outcomes_y3)

## loop over all the outcomes
data <- lapply(outcome_list, function(outcomes){
  
  data <- lapply(outcomes, function(otc) {
    
    # Compute sums of contacts and cumulative sums of contacts:
    df <- dt[, .(contacts.sum = sum(get(otc))), by=c('treated', 'relative.time')]
    df[, contacts.cumsum := cumsum(contacts.sum), by='treated']
    
    # Pivot wider:
    df <- dcast(df, relative.time ~ paste0('treated_', treated), 
                value.var = 'contacts.cumsum')
    
    # Cumulative sum of contacts per capita:
    df[, treated_0 := treated_0 / folk[treated==0, uniqueN(shnro)]]
    df[, treated_1 := treated_1 / folk[treated==1, uniqueN(shnro)]]
    
    # Difference in the cumulative sum of contacts per capita:
    df[, cum.diff := treated_1 - treated_0]
    
    df[, Outcome := otc]
    
  })
  
  data <- rbindlist(data)
  gc()
  return(data)
})


# IV plot ------------------------------------------------------------------


## Y1.x --------------------------------------------------------------------

labels <- c(d.1 = 'Digital clinic contacts (D.1)',
            y1.1 = 'In-person visits (Y1.1)',
            y1.2 = 'Other traditional contacts (Y1.2)',
            y1.3 = 'Primary care contacts in\ntotal (Y1.3)')

ggplot(data$y1,
       aes(x = relative.time,y = cum.diff,color = Outcome, linetype= Outcome)) +
  geom_line(linewidth = 2) +
  geom_hline(yintercept = 0, linetype = 'dashed') + 
  geom_vline(xintercept = -0.5, linetype = 'dashed') +
  labs(y = 'Difference in cumulative contacts per capita (T-C)',
       x = 'Days relative to treatment',
       linetype = 'Outcome: ',
       col = 'Outcome: ') +
  guides(col = guide_legend(nrow = 3, byrow = T),
         linetype = guide_legend(nrow = 3, byrow = T)) +
  ylim(-0.3, 0.3) +
  scale_color_viridis(discrete = TRUE, labels = labels) + 
  scale_linetype_manual(values = 1:5, labels = labels) +
  scale_x_continuous(breaks = c(0, seq(50, 300, by=50), 365),
                     limits = c(0, 365)) + 
  theme(text = element_text(size=20),
        legend.position = 'bottom',
        legend.key.width = unit(4, 'cm'),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_line(linewidth=0.25, linetype = 'solid', 
                                        color = 'lightgrey'),
        panel.grid.minor = element_line(linewidth=0.25, 
                                        linetype = 'solid',
                                        color = 'lightgrey'),
        panel.border = element_rect(colour='black', fill=NA, linewidth=0.5))

# Save plot:
ggsave(filename = output_plot.Y1.X, width = 12, height = 10)  


## Y2.x --------------------------------------------------------------------

labels <- c(d.1 = 'Digital clinic contacts (D.1)',
            referrals = 'Referrals to hospitals (Y2.1)',
            ed.in.person = 'In-person ED contacts (Y2.2)',
            ed.other = 'Other ED contacts (Y2.3)',
            hospital.visit = 'Outpatient hospital visits (Y2.4)')

ggplot(data$y2,
       aes(x = relative.time,y = cum.diff,color = Outcome, linetype= Outcome)) +
  geom_line(linewidth = 2) +
  geom_hline(yintercept = 0, linetype = 'dashed') + 
  geom_vline(xintercept = -0.5, linetype = 'dashed') +
  labs(y = 'Difference in cumulative contacts per capita (T-C)',
       x = 'Days relative to treatment',
       linetype = 'Outcome: ',
       col = 'Outcome: ') +
  guides(col = guide_legend(nrow = 3, byrow = T),
         linetype = guide_legend(nrow = 3, byrow = T)) +
  ylim(-0.3, 0.3) +
  scale_color_viridis(discrete = TRUE, labels = labels) + 
  scale_linetype_manual(values = 1:5, labels = labels) +
  scale_x_continuous(breaks = c(0, seq(50, 300, by=50), 365),
                     limits = c(0, 365)) +
  theme(text = element_text(size=20),
        legend.position = 'bottom',
        legend.key.width = unit(4, 'cm'),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_line(linewidth=0.25, linetype = 'solid', 
                                        color = 'lightgrey'),
        panel.grid.minor = element_line(linewidth=0.25, 
                                        linetype = 'solid',
                                        color = 'lightgrey'),
        panel.border = element_rect(colour='black', fill=NA, linewidth=0.5))

# Save plot:
ggsave(filename = output_plot.Y2.X, width = 12, height = 10)  


## Y3.x --------------------------------------------------------------------

labels <- c(d.1 = 'Digital clinic contacts (D.1)',
            occup.inperson = 'Occupational HC: in-person (Y3.1)',
            occup.telemed = 'Occupational HC: other (Y3.2)',
            private.inperson = 'Private HC: in-person (Y3.3)', 
            private.telemed = 'Private HC: other (Y3.4)')

ggplot(data$y3,
       aes(x = relative.time,y = cum.diff,color = Outcome, linetype= Outcome)) +
  geom_line(linewidth = 2) +
  geom_hline(yintercept = 0, linetype = 'dashed') + 
  geom_vline(xintercept = -0.5, linetype = 'dashed') +
  labs(y = 'Difference in cumulative contacts per capita (T-C)',
       x = 'Days relative to treatment',
       linetype = 'Outcome: ',
       col = 'Outcome: ') +
  guides(col = guide_legend(nrow = 3, byrow = T),
         linetype = guide_legend(nrow = 3, byrow = T)) +
  ylim(-0.3, 0.3) +
  scale_color_viridis(discrete = TRUE, labels = labels) + 
  scale_linetype_manual(values = 1:5, labels = labels) +
  scale_x_continuous(breaks = c(0, seq(50, 300, by=50), 365),
                     limits = c(0, 365)) +
  theme(text = element_text(size=20),
        legend.position = 'bottom',
        legend.key.width = unit(4, 'cm'),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_line(linewidth=0.25, linetype = 'solid', 
                                        color = 'lightgrey'),
        panel.grid.minor = element_line(linewidth=0.25, 
                                        linetype = 'solid',
                                        color = 'lightgrey'),
        panel.border = element_rect(colour='black', fill=NA, linewidth=0.5))

# Save plot:
ggsave(filename = output_plot.Y3.X, width = 12, height = 10)  


# End ---------------------------------------------------------------------


