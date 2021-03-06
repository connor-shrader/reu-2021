library(ggplot2)
library(rstudioapi)

setwd(dirname(getActiveDocumentContext()$path))
source("themes.r")

length <- 400

x <- seq(-1, 5, length.out = length)
y <- seq(-1, 5, length.out = length)

dat <- expand.grid(x= x, y = y)
dat$z <- (8 - (-1 * dat$x + 5 * dat$y))^2 + (0 - (-3 * dat$x + 3 * dat$y))^2


circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

c <- circleFun(diameter = 2, npoints = 400)

# ridge
ridge <- ggplot(
  data = dat,
  mapping = aes(x = x, y = y, z = z, color = z)) +
  
  # Axes
  geom_vline(aes(xintercept = 0)) +
  geom_hline(aes(yintercept = 0)) +
  
  # RSS contours
  geom_contour(breaks = c(2, 9.125, 16.25)) +
  
  # Diamond
  geom_polygon(
    data = c,
    mapping = aes(x = x, y = y),
    fill = "red",
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  
  # Points for the OLS and ridge solutions
  geom_point(
    data = data.frame(x = c(0.255, 2), y = c(0.967, 2)),
    mapping = aes(x = x, y = y),
    size = 2,
    inherit.aes = FALSE
  ) + 
  
  # Axis and point labels
  annotate(
    "text", 
    label = "beta[1]", 
    parse = TRUE, 
    x = 2, 
    y = -0.2, 
    size = 6
  ) +
  
  annotate(
    "text", 
    label = "beta[2]", 
    parse = TRUE, 
    x = -0.2, 
    y = 2, 
    size = 6,
    angle = 90,
  ) +
  
  annotate(
    "text", 
    label = "hat(beta)^OLS", 
    parse = TRUE, 
    x = 2.30, 
    y = 2.10, 
    size = 6
  ) +
  
  annotate(
    "text", 
    label = "hat(beta)^Ridge", 
    parse = TRUE, 
    x = 0.555, 
    y = 1.067, 
    size = 6
  ) +
  
  
  # Pre-defined theme
  reu_blank +
  
  # Keeps x and y coordinates the same length.
  coord_fixed() 



# lasso
lasso <- ggplot(
  data = dat,
  mapping = aes(x = x, y = y, z = z, color = z)) +
  
  # Axes
  geom_vline(aes(xintercept = 0)) +
  geom_hline(aes(yintercept = 0)) +
  
  # RSS contours
  geom_contour(breaks = c(2, 10, 18)) +
  
  # Diamond
  geom_polygon(
    data = data.frame(x = c(1, 0, -1, 0), y = c(0, 1, 0, -1)),
    mapping = aes(x = x, y = y),
    fill = "red",
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  
  # Points for the OLS and lasso solutions
  geom_point(
    data = data.frame(x = c(0, 2), y = c(1, 2)),
    mapping = aes(x = x, y = y),
    size = 2,
    inherit.aes = FALSE
  ) + 
  
  # Axis and point labels
  annotate(
    "text", 
    label = "beta[1]", 
    parse = TRUE, 
    x = 2, 
    y = -0.2, 
    size = 6
  ) +
  
  annotate(
    "text", 
    label = "beta[2]", 
    parse = TRUE, 
    x = -0.2, 
    y = 2, 
    size = 6,
    angle = 90
  ) +
  
  annotate(
    "text", 
    label = "hat(beta)^OLS", 
    parse = TRUE, 
    x = 2.30, 
    y = 2.10, 
    size = 6
  ) +
  
  annotate(
    "text", 
    label = "hat(beta)^Lasso", 
    parse = TRUE, 
    x = 0.30, 
    y = 1.10, 
    size = 6
  ) +
  
  
  # Pre-defined theme
  reu_blank +
  
  # Keeps x and y coordinates the same length.
  coord_fixed() 



# Save files
ggsave(
  filename = "lasso-diagram.png",
  path = "../../figures",
  plot = lasso,
  type = "cairo-png",
  width = 10,
  height = 6,
  unit = "in"
)

ggsave(
  filename = "lasso-diagram.eps",
  path = "../../figures",
  plot = lasso,
  width = 10,
  height = 6,
  unit = "in"
)

ggsave(
  filename = "ridge-diagram.png",
  path = "../../figures",
  plot = ridge,
  type = "cairo-png",
  width = 10,
  height = 6,
  unit = "in"
)

ggsave(
  filename = "ridge-diagram.eps",
  path = "../../figures",
  plot = ridge,
  width = 10,
  height = 6,
  unit = "in"
)