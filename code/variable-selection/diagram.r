par(mfrow = c(1, 2))

distance <- function(v1, v2)
{
  sqrt((v1[1] - v2[1])^2 + (v1[2] - v2[2])^2)
}

ellipse <- function(x, y)
{
  distance(c(x, y), a) + distance(c(x, y), b)
}

length <- 300

x <- seq(-6, 6, length = length)
y <- seq(-6, 6, length = length)

a <- c(1.2, 3.6)
b <- c(0.8, 2.4)
z <- outer(x, y, Vectorize(ellipse))
contour(x, y, z,
        levels = 1.5,
        xlim = c(-2, 4),
        ylim = c(-2, 6),
        drawlabels = FALSE,
        xlab = expression(beta[1]),
        ylab = expression(beta[2])
)

a <- c(1.4, 4.2)
b <- c(0.6, 1.8)
z <- outer(x, y, Vectorize(ellipse))
contour(x, y, z,
        levels = 3,
        xlim = c(-2, 4), 
        ylim = c(-2, 6), 
        add = TRUE,
        drawlabels = FALSE
)

a <- c(1.6, 4.8)
b <- c(0.4, 1.2)
z <- outer(x, y, Vectorize(ellipse))
contour(x, y, z, 
        levels = 4.57, 
        xlim = c(-2, 4), 
        ylim = c(-2, 6), 
        add = TRUE,
        drawlabels = FALSE
)

x <- c(-1, 0, 1, 0)
y <- c(0, 1, 0, -1)
polygon(x, y, col = "red", border = "red")

abline(v = 0, lty = "dashed", col = "gray")
abline(h = 0, lty = "dashed", col = "gray")

points(c(1, 0), c(3, 1), pch = 16)

x <- seq(-6, 6, length = length)
y <- seq(-6, 6, length = length)

a <- c(1.2, 3.6)
b <- c(0.8, 2.4)
z <- outer(x, y, Vectorize(ellipse))
contour(x, y, z,
        levels = 1.4,
        xlim = c(-2, 4),
        ylim = c(-2, 6),
        drawlabels = FALSE,
        xlab = expression(beta[1]),
        ylab = expression(beta[2])
)

a <- c(1.4, 4.2)
b <- c(0.6, 1.8)
z <- outer(x, y, Vectorize(ellipse))
contour(x, y, z,
        levels = 2.95,
        xlim = c(-2, 4), 
        ylim = c(-2, 6), 
        add = TRUE,
        drawlabels = FALSE
)

a <- c(1.6, 4.8)
b <- c(0.4, 1.2)
z <- outer(x, y, Vectorize(ellipse))
contour(x, y, z, 
        levels = 4.32, 
        xlim = c(-2, 4), 
        ylim = c(-2, 6), 
        add = TRUE,
        drawlabels = FALSE
)

t <- seq(0, 2 * pi, length = length)
x <- lapply(t, sin)
y <- lapply(t, cos)

polygon(x, y, col = "red", border = "red")

abline(v = 0, lty = "dashed", col = "gray")
abline(h = 0, lty = "dashed", col = "gray")

points(c(1, 0.3), c(3, 0.95), pch = 16)

