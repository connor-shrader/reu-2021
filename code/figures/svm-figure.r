library(ggplot2)
library(rstudioapi)
library(scales)

setwd(dirname(getActiveDocumentContext()$path))
source("themes.r")
source("save-plot.r")

set.seed(6)

x1 <- runif(n = 20, min = 0, max = 1)
y1 <- x1 + 1 + rnorm(n = 20, mean = 0, sd = 0.25)

x2 <- runif(n = 20, min = 1, max = 2)
y2 = x2 - 1 + rnorm(n = 20, mean = 0, sd = 0.25)

class1 <- data.frame(x = x1, y = y1, class = 1, support = FALSE)
class2 <- data.frame(x = x2, y = y2, class = 2, support = FALSE)

class1 <- rbind(class1, data.frame(x = c(0.5, 1), y = c(1, 5/3), class = 1, support = TRUE))
class1 <- class1[class1$y <= 2 & class1$x >= 0, ]
class2 <- rbind(class2, data.frame(x = 1.5, y = 1, class = 2, support = TRUE))
class2 <- class2[class2$x <= 2 & class2$y >= 0, ]

dat <- rbind(class1, class2)

dat$class <- as.factor(dat$class)

dat$stroke <- ifelse(dat$support, 1.5, 0)

sv_plot <- ggplot(data = dat, mapping = aes(x = x, y = y, color = class)) +
  geom_function(fun = function(x) 4/3 * x + 1/3, xlim = c(0, 23/16), linetype = "dashed", color = "black") +
  geom_function(fun = function(x) 4/3 * x - 1, xlim = c(3/4, 2), linetype = "dashed", color = "black") +
  geom_function(fun = function(x) 4/3 * x - 1/3, xlim = c(1/4, 59/32), color = "black") +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(mapping = aes(x = x, y = y, fill = class, color = support, stroke = stroke), shape = 21, size = 3) +
  scale_color_manual(values = c("transparent", "#000000")) +
  
  annotate(
    "text", 
    label = "x", 
    x = 1, 
    y = -0.1, 
    size = 6
  ) +
  
  annotate(
    "text", 
    label = "y", 
    x = -0.1, 
    y = 1, 
    size = 6,
    angle = 90
  ) +
  reu_blank +
  theme(
    legend.position = "none"
  )

save_plot(plot = sv_plot,
          filename = "svm",
          path = "../../figures")

              