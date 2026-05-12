
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
###      r-script plot_chats_daily_flowmedik_data.r   ###
###                       2026 TH                     ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Read daily digiclinic visit counts, compute
# the mean number of contacts per day by ISO week, and
# plot the result. Figures in Finnish, Swedish, and English.
rm(list = ls())

library(data.table)
library(ggplot2)

# Inputs / Outputs:
input_chats  <- "data/raw/chats_daily.csv"
out_fig_fin  <- "figures/digiclinic_contacts_weekly_mean_per_day_fin.pdf"
out_fig_swe  <- "figures/digiclinic_contacts_weekly_mean_per_day_swe.pdf"
out_fig_eng  <- "figures/digiclinic_contacts_weekly_mean_per_day_eng.pdf"


### ### ### ### ### ### ### ### ### ### ### ### ###
#### 1) Read and prepare data. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

dt <- fread(input_chats, sep = ";")
dt[, date := as.Date(date)]

# ISO week and year (week starts on Monday).
# %V = ISO week number, %G = ISO year.
dt[, iso_week  := as.integer(format(date, "%V"))]
dt[, iso_year  := as.integer(format(date, "%G"))]

# Week label: "YYYY-Www" for ordering and display.
dt[, week_label := sprintf(
  "%d-W%02d", iso_year, iso_week
)]

# Mean contacts per observed day within each ISO week.
weekly <- dt[,
  .(
    mean_per_day = mean(chats),
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

# Total contacts across all observed days.
total_chats <- sum(dt$chats)


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

n_fin <- format(total_chats, big.mark = "\u00a0", scientific = FALSE)
n_swe <- format(total_chats, big.mark = "\u00a0", scientific = FALSE)
n_eng <- format(total_chats, big.mark = ",",      scientific = FALSE)

labels <- list(
  fin = list(
    x = "Viikko",
    y = paste0(
      "Keskimääräinen chat-keskustelujen",
      " määrä per päivä"
    ),
    subtitle = paste("Kontakteja yhteensä:", n_fin),
    out = out_fig_fin
  ),
  swe = list(
    x = "Vecka",
    y = "Genomsnittligt antal chattkonversationer per dag",
    subtitle = paste("Totalt antal kontakter:", n_swe),
    out = out_fig_swe
  ),
  eng = list(
    x = "Week",
    y = "Mean number of digital clinic contacts per day",
    subtitle = paste("Total contacts:", n_eng),
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
