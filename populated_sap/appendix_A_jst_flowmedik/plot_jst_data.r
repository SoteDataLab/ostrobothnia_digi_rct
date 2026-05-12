
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
###         r-script plot_pohjanmaa_jst_data.r        ###
###                       2026 TH                     ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Plot the share of symptom groups (oireryhmät) and
# symptom onset groups (oirelähtöryhmät) from digital clinic
# session data. Figures in Finnish, Swedish, and English.
rm(list = ls())

# Install and load the following packages:
library(data.table)
library(readxl)
library(ggplot2)
library(viridis)
library(stringr)

# Inputs:
input_aggr         <- "data/raw/pohjanmaa_jst_oireryhma_9-2025_1-2026.xlsx"
input_aggr_lahto   <- "data/raw/pohjanmaa_jst_oirelahtoryhma_5-2025_1-2026.xlsx"
input_transl       <- "data/raw/translations_oireryhma.csv"
input_transl_lahto <- "data/raw/translations_oirelahtoryhma.csv"

# Outputs:
out_n_oireryhma      <- "figures/sessiot_ajanjakso_oireryhma.pdf"
out_oireryhma_fin    <- "figures/oireryhmat_fin.pdf"
out_oireryhma_swe    <- "figures/oireryhmat_swe.pdf"
out_oireryhma_eng    <- "figures/oireryhmat_eng.pdf"
out_n_oirelahtoryhma <- "figures/sessiot_ajanjakso_oirelahtoryhma.pdf"
out_lahto_fin        <- "figures/oirelahtoryhmat_fin.pdf"
out_lahto_swe        <- "figures/oirelahtoryhmat_swe.pdf"
out_lahto_eng        <- "figures/oirelahtoryhmat_eng.pdf"
out_lahto_oireryhma_fin <- "figures/oireryhmat_lahtodata_fin.pdf"
out_lahto_oireryhma_swe <- "figures/oireryhmat_lahtodata_swe.pdf"
out_lahto_oireryhma_eng <- "figures/oireryhmat_lahtodata_eng.pdf"


### ### ### ### ### ### ### ### ### ### ### ### ###
#### 1) Helper functions. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


agg_by_period <- function(dt) {
  # Aggregate sessions by calendar month and return sorted,
  # labelled data.table.
  ap <- dt[, .(sessiot = sum(sessiot, na.rm = TRUE)), by = ajanjakso]
  ap[, month := as.integer(sub('_.*', '', ajanjakso))]
  ap[, year  := as.integer(sub('.*_', '', ajanjakso))]
  ap[, date  := as.Date(paste(year, month, '1', sep = '-'))]
  ap[, label := format(date, '%b %Y')]
  ap <- ap[order(date)]
  ap[, label := factor(label, levels = label)]
  ap
}

plot_by_month <- function(ap, title) {
  # Bar chart of session counts per month.
  ggplot(ap, aes(x = label, y = sessiot)) +
    geom_col(
      fill = viridis(1), color = 'black', alpha = 0.8, width = 0.8
    ) +
    geom_text(
      aes(label = format(sessiot, big.mark = ',', scientific = FALSE)),
      vjust = -0.5, size = 7
    ) +
    scale_y_continuous(
      limits = c(0, 1.20 * max(ap$sessiot)), expand = c(0, 0)
    ) +
    labs(x = NULL, y = 'Lukumäärä', title = title) +
    theme(
      text = element_text(size = 22),
      axis.text.x = element_text(size = 16),
      panel.background = element_rect(fill = 'white', colour = 'white'),
      panel.grid.major = element_line(
        linewidth = 0.25, linetype = 'solid', color = 'lightgrey'
      ),
      panel.grid.minor = element_line(
        linewidth = 0.25, linetype = 'solid', color = 'lightgrey'
      ),
      panel.border = element_rect(
        colour = 'black', fill = NA, linewidth = 0.5
      )
    )
}

plot_shares <- function(data, grp_col, transl, lang_col,
                        y_lab, title, total_n, wrap_width = 20,
                        text_size = 24, ytext_size = 20,
                        pct_size = 8) {
  # Horizontal bar chart of group shares with % labels and N label.
  # grp_col: name of the grouping column in data (string)
  # transl:  translation table with columns fin, swe, eng
  # total_n: sample size used as share denominator (shown on axis)
  big_mark <- if (lang_col == 'eng') ',' else '\u00a0'
  n_str <- format(total_n, big.mark = big_mark, scientific = FALSE)
  y_lab <- paste0(y_lab, ', N = ', n_str)
  d <- merge(
    data,
    transl[, .(fin, label = get(lang_col))],
    by.x = grp_col, by.y = 'fin', all.x = TRUE
  )
  d[is.na(label), label := get(grp_col)]
  d[, label := str_wrap(label, width = wrap_width)]

  ggplot(d, aes(x = reorder(label, share), y = share)) +
    geom_col(
      fill = viridis(1), color = 'black', alpha = 0.8, width = 0.8
    ) +
    geom_text(
      aes(label = paste0(
        format(round(share * 100, 1), nsmall = 1), ' %'
      )),
      hjust = -0.1, size = pct_size
    ) +
    geom_label(
      aes(label = paste('N:', format(sessiot, big.mark = big_mark,
                                     scientific = FALSE)),
          y = 1.20 * max(d$share)),
      size = 6, show.legend = FALSE
    ) +
    scale_y_continuous(
      labels = scales::percent_format(),
      limits = c(0, 1.28 * max(d$share)),
      expand = c(0, 0)
    ) +
    labs(x = NULL, y = y_lab, title = title) +
    theme(
      text = element_text(size = text_size),
      axis.text.y = element_text(size = ytext_size),
      panel.background = element_rect(fill = 'white', colour = 'white'),
      panel.grid.major = element_line(
        linewidth = 0.25, linetype = 'solid', color = 'lightgrey'
      ),
      panel.grid.minor = element_line(
        linewidth = 0.25, linetype = 'solid', color = 'lightgrey'
      ),
      panel.border = element_rect(
        colour = 'black', fill = NA, linewidth = 0.5
      )
    ) +
    coord_flip()
}


### ### ### ### ### ### ### ### ### ### ### ### ###
#### 2) Read data. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


dt  <- as.data.table(read_excel(input_aggr,       na = c("", "NA")))
dt2 <- as.data.table(read_excel(input_aggr_lahto, na = c("", "NA")))
dt[,  sessiot := as.numeric(sessiot)]
dt2[, sessiot := as.numeric(sessiot)]

translations  <- fread(input_transl,       encoding = "UTF-8")
translations2 <- fread(input_transl_lahto, encoding = "UTF-8")


### ### ### ### ### ### ### ### ### ### ### ### ###
#### 3) Prepare oireryhma data. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


dt_ap <- agg_by_period(dt)

plot_dt <- dt[
  !is.na(oireryhma),
  .(sessiot = sum(sessiot, na.rm = TRUE)),
  by = oireryhma
]
plot_dt[, share := sessiot / sum(sessiot)]
n_oireryhma <- sum(plot_dt$sessiot)
plot_dt[oireryhma == "Korva oireet", oireryhma := "Korvaoireet"]


### ### ### ### ### ### ### ### ### ### ### ### ###
#### 4) Prepare oirelahtoryhma data. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


dt2_ap <- agg_by_period(dt2)

plot_dt2 <- dt2[
  !is.na(oirelahtoryhma),
  .(sessiot = sum(sessiot, na.rm = TRUE)),
  by = oirelahtoryhma
]
plot_dt2[, share := sessiot / sum(sessiot)]
n_oirelahtoryhma <- sum(plot_dt2$sessiot)
plot_dt2 <- plot_dt2[order(-sessiot)][1:15]

# Aggregate oirelahtoryhma data to oireryhma level using the
# oireryhma column in translations2.
dt2_mapped <- merge(
  dt2[!is.na(oirelahtoryhma)],
  translations2[, .(oirelahtoryhma = fin, oireryhma)],
  by = "oirelahtoryhma", all.x = TRUE
)
dt2_mapped[is.na(oireryhma), oireryhma := "Muut"]

plot_dt2_oir <- dt2_mapped[
  , .(sessiot = sum(sessiot, na.rm = TRUE)),
  by = oireryhma
]
plot_dt2_oir[, share := sessiot / sum(sessiot)]
n_lahto_oireryhma <- sum(plot_dt2_oir$sessiot)
# Match the oireryhma fix applied to dt (Korva oireet -> Korvaoireet)
plot_dt2_oir[
  oireryhma == "Korva oireet", oireryhma := "Korvaoireet"
]


### ### ### ### ### ### ### ### ### ### ### ### ###
#### 5) Create plots. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


p_ap <- plot_by_month(
  dt_ap, title = 'Oirearvioiden lukumäärä kuukausittain'
)

p_fin <- plot_shares(
  plot_dt, grp_col = 'oireryhma', transl = translations,
  lang_col = 'fin', y_lab = 'Osuus (%)', total_n = n_oireryhma,
  title = 'Oireryhmien osuus tehdyistä oirearvioista (%)'
)

p_swe <- plot_shares(
  plot_dt, grp_col = 'oireryhma', transl = translations,
  lang_col = 'swe', y_lab = 'Andel (%)', total_n = n_oireryhma,
  title = paste('Symtomgruppernas andel av genomförda',
                'symtombedömningar (%)')
)

p_eng <- plot_shares(
  plot_dt, grp_col = 'oireryhma', transl = translations,
  lang_col = 'eng', y_lab = 'Share (%)', total_n = n_oireryhma,
  title = 'Symptom groups, share of assessments (%)'
)

p2_ap <- plot_by_month(
  dt2_ap, title = 'Oirearvioiden lukumäärä kuukausittain'
)

p2_fin <- plot_shares(
  plot_dt2, grp_col = 'oirelahtoryhma', transl = translations2,
  lang_col = 'fin', y_lab = 'Osuus (%)',
  total_n = n_oirelahtoryhma,
  title = paste('15 yleisimmän oirelähtöryhmän osuus',
                'tehdyistä oirearvioista (%)')#,
  #wrap_width = 25, text_size = 22, ytext_size = 18, pct_size = 7
)

p2_swe <- plot_shares(
  plot_dt2, grp_col = 'oirelahtoryhma', transl = translations2,
  lang_col = 'swe', y_lab = 'Andel (%)',
  total_n = n_oirelahtoryhma,
  title = 'Topp 15 symtomgrupper, andel av bedömningar (%)'#,
  #wrap_width = 25, text_size = 22, ytext_size = 18, pct_size = 7
)

p2_eng <- plot_shares(
  plot_dt2, grp_col = 'oirelahtoryhma', transl = translations2,
  lang_col = 'eng', y_lab = 'Share (%)',
  total_n = n_oirelahtoryhma,
  title = 'Top 15 symptom groups, share of assessments (%)',
  #wrap_width = 25, text_size = 22, ytext_size = 18, pct_size = 7
)

p3_fin <- plot_shares(
  plot_dt2_oir, grp_col = 'oireryhma', transl = translations,
  lang_col = 'fin', y_lab = 'Osuus (%)',
  total_n = n_lahto_oireryhma,
  title = 'Oireryhmien osuus tehdyistä oirearvioista (%)'
)

p3_swe <- plot_shares(
  plot_dt2_oir, grp_col = 'oireryhma', transl = translations,
  lang_col = 'swe', y_lab = 'Andel (%)',
  total_n = n_lahto_oireryhma,
  title = 'Symtomgruppernas andel av genomförda symtombedömningar (%)'
)

p3_eng <- plot_shares(
  plot_dt2_oir, grp_col = 'oireryhma', transl = translations,
  lang_col = 'eng', y_lab = 'Share (%)',
  total_n = n_lahto_oireryhma,
  title = 'Symptom groups, share of assessments (%)'
)


### ### ### ### ### ### ### ### ### ### ### ### ###
#### 6) Save. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

ggsave(out_n_oireryhma,         width = 15, height = 8,  plot = p_ap)
ggsave(out_oireryhma_fin,       width = 15, height = 17, plot = p_fin)
ggsave(out_oireryhma_swe,       width = 15, height = 17, plot = p_swe)
ggsave(out_oireryhma_eng,       width = 15, height = 17, plot = p_eng)
ggsave(out_n_oirelahtoryhma,    width = 15, height = 8,  plot = p2_ap)
ggsave(out_lahto_fin,           width = 15, height = 17, plot = p2_fin)
ggsave(out_lahto_swe,           width = 15, height = 17, plot = p2_swe)
ggsave(out_lahto_eng,           width = 15, height = 17, plot = p2_eng)
ggsave(out_lahto_oireryhma_fin, width = 15, height = 17, plot = p3_fin)
ggsave(out_lahto_oireryhma_swe, width = 15, height = 17, plot = p3_swe)
ggsave(out_lahto_oireryhma_eng, width = 15, height = 17, plot = p3_eng)

# End.
rm(list = ls())
gc()
