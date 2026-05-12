
# Setup -------------------------------------------------------------------

library(ggplot2)
library(readxl)

dt <- read_excel("M:/Prediction_data_cleaned_excel.xls")

ci_low <- -13
ci_high <- 23
effect <- 5
mean <- mean(dt$Substitution_rate, na.rm = T)


# Plot --------------------------------------------------------------------

plot <- ggplot(dt, aes(x = Substitution_rate)) +
  labs(y = "Density",
       x = "Substitution rate (percent)") +
  geom_vline(xintercept = c(ci_low, ci_high), linetype = "dashed", linewidth = 1) + 
  geom_vline(xintercept = effect, linetype = "solid", linewidth = 1) + 
  geom_vline(xintercept = mean, linetype = "solid", color = "red", linewidth = 1.5) +
  geom_histogram(aes(y = after_stat(density)), 
                 breaks = seq(0, 100, by = 100/17),
                 fill = "#3693fc", 
                 color = "steelblue",
                 boundary = 10,
                 alpha = 0.75) +
  scale_x_continuous(breaks = seq(-20, 100, by = 20),
                     limits = c(-20,100)) +
  scale_y_continuous(breaks = seq(0, 0.025, by = 0.005),
                     limits = c(0, 0.026),
                     labels = function(x) ifelse(abs(x) < 1e-10, "0", sub("^0", "", as.character(x))),
                     expand = c(0, 0)) +
  annotate("rect",
           xmin = ci_low, xmax = ci_high,
           ymin = -Inf, ymax = Inf,
           fill = "grey40", alpha = 0.4) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    plot.margin = margin(5, 5, 5, 5),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 12,
                                margin = margin(r = 10)),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 12,
                                margin = margin(t = 5))
  )

## Pdf for overleaf
ggsave("substitution_rate.pdf", 
       plot = plot, 
       width = 180, 
       height = 120, 
       units = "mm", 
       device = cairo_pdf)

## Png for powerpoint
ggsave("substitution_rate.png", 
       plot = plot, 
       width = 180, 
       height = 120, 
       units = "mm", 
       device = png)
