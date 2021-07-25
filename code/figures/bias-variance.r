library(ggplot2)
library(rstudioapi)
library(scales)

setwd(dirname(getActiveDocumentContext()$path))
source("themes.r")

set.seed(1)
x <- sample(seq(0, 3, length.out = 101), 50)
eps <- rnorm(50, mean = 0, sd = 0.2)
y <- -(x - 0.5) * (x - 1.5) * (x - 2.5) + 1 + eps

dat <- data.frame(x = x, y = y)

linear <- lm(y ~ x, data = dat)
cubic <- lm(y ~ poly(x, 3), data = dat)
interpolation <- lm(y ~ poly(x, 20), data = dat)

lin_predict <- function(x) {
  predict(linear, newdata = data.frame(x = x))
}

cub_predict <- function(x) {
  predict(cubic, newdata = data.frame(x = x))
}

inter_predict <- function(x) {
  predict(interpolation, newdata = data.frame(x = x))
}

library(ggplot2)

bias_variance <- ggplot(data = dat, mapping = aes(x = x, y = y)) +
  geom_point(size = 2) + 
  geom_function(fun = lin_predict, color = hue_pal()(3)[1]) +
  geom_function(fun = cub_predict, color = hue_pal()(3)[2]) +
  geom_function(fun = inter_predict, color = hue_pal()(3)[3]) +
  reu_border

ggsave(
  filename = "bias-variance.png",
  path = "./images",
  plot = bias_variance,
  type = "cairo-png",
  width = 10,
  height = 6,
  unit = "in"
)

ggsave(
  filename = "bias-variance.eps",
  path = "./images",
  plot = bias_variance,
  width = 10,
  height = 6,
  unit = "in"
)
