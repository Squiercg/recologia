library(ggplot2)

str(diamonds)

set.seed(1410)
dsmall <- diamonds[sample(nrow(diamonds), 100), ]

qplot(carat,price, data = dsmall)
ggsave("figura01.jpg")

qplot(carat, price, data = dsmall, colour = color)
ggsave("figura02.jpg")

qplot(carat, price, data = dsmall, shape = cut)
ggsave("figura03.jpg")

qplot(carat, price, data = dsmall, geom = c("point", "smooth"))
qplot(carat, price, data = dsmall, geom = c("point", "linear"))
?geom

qplot(carat, price, data = dsmall, geom = c("point", "smooth"),se=FALSE)

qplot(carat, price, data = dsmall, geom = c("point", "smooth"),span = 0.2)


qplot(color,price, data = dsmall, geom = c("boxplot"))

qplot(carat, data = diamonds, geom ="histogram")

qplot(carat, data = diamonds, geom = "histogram", binwidth = 1,xlim = c(0,3))

qplot(color, data = diamonds, geom = "bar")

qplot(carat, data = diamonds, facets = color ~ .,geom = "histogram", binwidth = 0.1, xlim = c(0, 3))

qplot(carat, price, data = dsmall,xlab = "Price ($)", ylab = "Weight (carats)",main = "Price-weight relationship")
