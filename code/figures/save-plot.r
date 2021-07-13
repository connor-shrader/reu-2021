save_plot <- function(plot, filename, path) {
  ggsave(
    filename = paste(filename, ".png", sep = ""),
    path = path,
    plot = plot,
    type = "cairo-png",
    width = 10,
    height = 6,
    unit = "in"
  )
  
  ggsave(
    filename = paste(filename, ".eps", sep = ""),
    path = path,
    plot = plot,
    width = 10,
    height = 6,
    unit = "in"
  )
}