library(ggplot2)

reu_border <- theme(
  panel.grid = element_blank(),
  panel.background = element_rect(fill = "white"),
  panel.border = element_rect(color = "black", fill = NA, size = 0.2)
)

reu_blank <- theme(
  panel.grid = element_blank(),
  panel.background = element_rect(fill = "white"),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank()
)