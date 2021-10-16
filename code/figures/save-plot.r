save_plot <- function(plot, filename, path, png_only = FALSE, 
                      cairo = TRUE, width = 10, height = 6) {
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
    if (cairo) {
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
    else {
      ggsave(
        filename = paste(filename, ".eps", sep = ""),
        path = path,
        plot = plot,
        device = "eps",
        width = width,
        height = height,
        unit = "in"
      )
    }
  }
}