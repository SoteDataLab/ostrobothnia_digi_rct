
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
###   r-script plot_waittime_daily_flowmedik_data.r   ###
###                       2026 TH                     ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Read daily digiclinic waiting times, compute
# the mean waiting time per day by ISO week, and plot
# the result. Figures in Finnish, Swedish, and English.
rm(list = ls())

library(data.table)
library(ggplot2)

# Inputs / Outputs:
input_wait   <- "data/raw/waittime_daily.csv"
out_fig_fin  <- "figures/digiclinic_waittime_weekly_mean_fin.pdf"
out_fig_swe  <- "figures/digiclinic_waittime_weekly_mean_swe.pdf"
out_fig_eng  <- "figures/digiclinic_waittime_weekly_mean_eng.pdf"


### ### ### ### ### ### ### ### ### ### ### ### ###
#### 1) Read and prepare data. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

dt <- fread(input_wait, sep = ";")
dt[, date := as.Date(date)]

# Convert HH:MM:SS to decimal minutes.
dt[, waittime_mins := as.numeric(
  as.difftime(waittime, format = "%H:%M:%S", units = "mins")
)]

# ISO week and year (week starts on Monday).
dt[, iso_week  := as.integer(format(date, "%V"))]
dt[, iso_year  := as.integer(format(date, "%G"))]

# Week label: "YYYY-Www" for ordering and display.
dt[, week_label := sprintf("%d-W%02d", iso_year, iso_week)]

# Mean waiting time per observed day within each ISO week.
weekly <- dt[,
  .(
    mean_per_day = mean(waittime_mins),
    n_days       = .N,
    week_start   = min(date)
  ),
  by = .(iso_year, iso_week, week_label)
]
setorder(weekly, iso_year, iso_week)

# Factor for ordered x-axis.
weekly[, week_label := factor(
  week_label, levels = week_label
)]

# Overall mean waiting time across all contacts.
overall_str <- "13 min 29 s"


### ### ### ### ### ### ### ### ### ### ### ### ###
#### 2) Helper: build plot. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Show every 4th week label to avoid overplotting.
n_weeks   <- nrow(weekly)
label_idx <- seq(1, n_weeks, by = 4)
x_breaks  <- levels(weekly$week_label)[label_idx]

base_theme <- theme(
  text = element_text(size = 20),
  axis.text.x = element_text(
    angle = 45, hjust = 1
  ),
  panel.background = element_rect(
    fill = "white", colour = "white"
  ),
  panel.grid.major = element_line(
    linewidth = 0.25, linetype = "solid",
    color = "lightgrey"
  ),
  panel.grid.minor = element_line(
    linewidth = 0.25, linetype = "solid",
    color = "lightgrey"
  ),
  panel.border = element_rect(
    colour = "black", fill = NA, linewidth = 0.5
  )
)

make_plot <- function(x_lab, y_lab, subtitle) {
  ggplot(
    weekly,
    aes(x = week_label, y = mean_per_day, group = 1)
  ) +
    geom_line() +
    geom_point(size = 2) +
    scale_x_discrete(breaks = x_breaks) +
    scale_y_continuous(
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(x = x_lab, y = y_lab, subtitle = subtitle) +
    base_theme
}


### ### ### ### ### ### ### ### ### ### ### ### ###
#### 3) Save figures in three languages. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

labels <- list(
  fin = list(
    x = "Viikko",
    y = "Keskimääräinen odotusaika per päivä (min)",
    subtitle = paste(
      "Keskimääräinen odotusaika (kaikki kontaktit):",
      overall_str
    ),
    out = out_fig_fin
  ),
  swe = list(
    x = "Vecka",
    y = "Genomsnittlig väntetid per dag (min)",
    subtitle = paste(
      "Genomsnittlig väntetid (alla kontakter):",
      overall_str
    ),
    out = out_fig_swe
  ),
  eng = list(
    x = "Week",
    y = "Mean waiting time per day (min)",
    subtitle = paste(
      "Mean waiting time (all contacts):",
      overall_str
    ),
    out = out_fig_eng
  )
)

for (lang in labels) {
  p <- make_plot(
    x_lab = lang$x, y_lab = lang$y, subtitle = lang$subtitle
  )
  ggsave(
    lang$out, plot = p,
    width = 15, height = 10, device = "pdf"
  )
}
