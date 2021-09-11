library(ggplot2)
library(rstudioapi)

setwd(dirname(getActiveDocumentContext()$path))
source("themes.r")

set.seed(1)
x <- sample(seq(0, 4, length.out = 101), 50)
eps <- rnorm(50, mean = 0, sd = 0.5)
y <- x + eps

dat <- data.frame(x = x, y = y)

model <- lm(y ~ x, data = dat)
coefs <- coef(model)

dat$yhat <- coefs[1] + coefs[2] * dat$x

library(ggplot2)

ols <- ggplot(data = dat, mapping = aes(x = x, y = y)) +
  geom_segment(aes(x = x, y = y, xend = x, yend = yhat), color = "red") +
  geom_point() + 
  geom_abline(slope = coefs[2], intercept = coefs[1], color = "blue") +
  reu_border

ggsave(
  filename = "ols.png",
  path = "../../figures",
  plot = ols,
  type = "cairo-png",
  width = 10,
  height = 6,
  unit = "in"
)

ggsave(
  filename = "ols.eps",
  path = "../../figures",
  plot = ols,
  width = 10,
  height = 6,
  unit = "in"
)
