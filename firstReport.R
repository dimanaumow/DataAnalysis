#1.1
data(iris)
help(iris)
View(iris)
plot(iris$Sepal.Length, iris$Sepal.Width, pch=19, col=iris$Species)
legend('topright', legend=levels(iris$Species), fill=1:3)
help(mean)
tapply(iris$Sepal.Length, iris$Species, mean)


#1.3
iris$Sepal.Area <- iris$Sepal.Length * iris$Sepal.Width


#1.4
numbers <- 1:1000
numbers <- numbers * 2
numbers[seq(5, length(numbers), by = 5)] <- 0
numbers <- c(-2, -1, numbers, 2001, 2002)
length(numbers)


#1.5
data(DNase, package = "datasets")
filtered_DNase <- DNase[seq(1, nrow(DNase), by = 2), ]

ggplot(filtered_DNase, aes(x = conc, y = density, colour = factor(Run))) +
  geom_line() +
  geom_point(shape = 19) +
  labs(colour = "Run")


#1.6
X <- rnorm(200, mean = 4, sd = 2)
X <- data.frame(X)

histogram <- ggplot(X, aes(x = X, y = ..density..)) +
  geom_histogram() +
  labs(title = "Histogram of X", x = "X", y = "density")
histogram
x <- seq(-5, 15, by = 0.2)
y <- dnorm(x, 4, 2)
d <- data.frame(x, y)