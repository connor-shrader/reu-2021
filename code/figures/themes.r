library(ggplot2)

reu <- theme(
  # Set legend background to white underneath the lines
  legend.key = element_rect(fill = "white"),
  text = element_text(size = 16)
)

reu_border <- reu + theme(
  panel.grid = element_blank(),
  panel.background = element_rect(fill = "white"),
  panel.border = element_rect(color = "black", fill = NA, size = 0.2)
)

reu_blank <- reu + theme(
  panel.grid = element_blank(),
  panel.background = element_rect(fill = "white"),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank()
)