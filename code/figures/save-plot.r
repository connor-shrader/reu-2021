save_plot <- function(plot, filename, path, png_only = FALSE, width = 10, height = 6) {
  ggsave(
    filename = paste(filename, ".png", sep = ""),
    path = path,
    plot = plot,
    type = "cairo-png",
    width = width,
    height = height,
    unit = "in"
  )
  
  if (!png_only) {
    ggsave(
      filename = paste(filename, ".eps", sep = ""),
      path = path,
      plot = plot,
      device = cairo_ps,
      width = width,
      height = height,
      unit = "in"
    )
  }
}