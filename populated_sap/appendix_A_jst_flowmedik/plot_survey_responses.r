
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
###         r-script plot_survey_responses.r          ###
###                       2026 TH                     ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Plot yes/no survey response shares from professional
# feedback on digital clinic contacts. Figures in Finnish,
# Swedish, and English.
rm(list = ls())

# Install and load the following packages:
library(data.table)
library(ggplot2)
library(viridis)
library(stringr)

# Inputs:
input <- "data/raw/survey_responses.csv"

# Outputs:
out_fin <- "figures/survey_responses_fin.pdf"
out_swe <- "figures/survey_responses_swe.pdf"
out_eng <- "figures/survey_responses_eng.pdf"


### ### ### ### ### ### ### ### ### ### ### ### ###
#### 1) Read and prepare data. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

dt <- fread(input, sep = ";", encoding = "UTF-8")

dt[, total     := answer_yes + answer_no]
dt[, share_yes := answer_yes / total]

# Extreme-scenario bounds using digicontacts as the denominator:
# lower: all missing responses were "no"
# upper: all missing responses were "yes"
dt[, share_yes_lo := answer_yes / digicontacts]
dt[, share_yes_hi := (answer_yes + (digicontacts - total)) / digicontacts]


### ### ### ### ### ### ### ### ### ### ### ### ###
#### 2) Helper function. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

plot_responses <- function(data, q_col, x_lab, yes_label,
                           wrap_width = 40) {
  # Horizontal bar chart of yes-response shares.
  # q_col:     name of the question column to use as y-axis labels
  # yes_label: language-specific label shown inside the bar
  # Rows are ordered so the first question in the data appears at top.
  d <- copy(data)
  d[, q_label := str_wrap(get(q_col), width = wrap_width)]
  # Reverse factor levels: coord_flip draws bottom-to-top,
  # so reversing puts the first row at the top.
  d[, q_label := factor(q_label, levels = rev(unique(q_label)))]

  ggplot(d, aes(x = q_label, y = share_yes)) +
    geom_col(
      fill = viridis(1), color = "black", alpha = 0.85, width = 0.6
    ) +
    geom_errorbar(
      aes(ymin = share_yes_lo, ymax = share_yes_hi),
      width = 0.25, linewidth = 1, color = "black"
    ) +
    geom_hline(
      yintercept = 1,
      linetype = "dashed", linewidth = 0.8, color = "black"
    ) +
    geom_text(
      aes(y = share_yes / 2, label = paste0(
        yes_label, " ",
        format(round(share_yes * 100, 1), nsmall = 1), " %"
      )),
      hjust = 0.5, size = 7, color = "white", fontface = "bold"
    ) +
    scale_y_continuous(
      labels = scales::percent_format(),
      limits = c(0, 1.05),
      breaks = seq(0, 1, by = 0.1),
      expand = c(0, 0)
    ) +
    labs(x = NULL, y = x_lab) +
    theme(
      text            = element_text(size = 22),
      axis.text.y     = element_text(size = 22),
      panel.background = element_rect(fill = "white", colour = "white"),
      panel.grid.major = element_line(
        linewidth = 0.25, linetype = "solid", color = "lightgrey"
      ),
      panel.grid.minor = element_line(
        linewidth = 0.25, linetype = "solid", color = "lightgrey"
      ),
      panel.border = element_rect(
        colour = "black", fill = NA, linewidth = 0.5
      )
    ) +
    coord_flip()
}


### ### ### ### ### ### ### ### ### ### ### ### ###
#### 3) Create plots. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

p_fin <- plot_responses(
  dt, q_col = "question_fin", yes_label = "Kyllä:",
  x_lab = "Kyllä-vastauksien osuus (%)"
)

p_swe <- plot_responses(
  dt, q_col = "question_swe", yes_label = "Ja:",
  x_lab = "Andel ja-svar (%)"
)

p_eng <- plot_responses(
  dt, q_col = "question_ena", yes_label = "Yes:",
  x_lab = "Share of yes answers (%)"
)


### ### ### ### ### ### ### ### ### ### ### ### ###
#### 4) Save. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

ggsave(out_fin, width = 15, height = 7, plot = p_fin)
ggsave(out_swe, width = 15, height = 7, plot = p_swe)
ggsave(out_eng, width = 15, height = 7, plot = p_eng)

# End.
rm(list = ls())
gc()
