save_plot <- function(plot, filename, path, png_only = FALSE) {
  ggsave(
    filename = paste(filename, ".png", sep = ""),
    path = path,
    plot = plot,
    type = "cairo-png",
    width = 10,
    height = 6,
    unit = "in"
  )
  
  if (!png_only) {
    ggsave(
      filename = paste(filename, ".eps", sep = ""),
      path = path,
      plot = plot,
      width = 10,
      height = 6,
      unit = "in"
    )
  }
}