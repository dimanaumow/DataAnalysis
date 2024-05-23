#2.1

n <- 850 #number of observations
observations <- rnorm(n, mean = 1, sd = 2)
unbiased_variance <- sum((observations - mean(observations))^2) / (n - 1)
mle_variance <- sum((observations - mean(observations))^2) / n
mle_std_dev <- sqrt(mle_variance)

cat(sprintf("Wariancja, estymator nieobciążony: %f\n", unbiased_variance))
cat(sprintf("Wariancja, estymator ML: %f\n", mle_variance))
cat(sprintf("Odchylenie, estymator ML: %f\n", mle_std_dev))


#2.2
n <- 5000 #number of observations
sigma <- 2 #standart_derivation
true_variance <- sigma^2
observations <- rnorm(n, mean = 0, sd = sigma)
matrix <- matrix(observations, nrow = 10, ncol = 500)
S <- apply(matrix, 2, var)
S1 <- S * (10 - 1) / 10
S2 <- S * (10 - 1) / (10 + 1)

bias_S <- mean(S) - true_variance
bias_S1 <- mean(S1) - true_variance
bias_S2 <- mean(S2) - true_variance

std_S <- sd(S)
std_S1 <- sd(S1)
std_S2 <- sd(S2)

RMSE_S <- sqrt(mean((S - true_variance)^2))
RMSE_S1 <- sqrt(mean((S1 - true_variance)^2))
RMSE_S2 <- sqrt(mean((S2 - true_variance)^2))

RMSE_mean_S <- sqrt(mean((S - true_variance)^2) * 1 / 10)
RMSE_mean_S1 <- sqrt(mean((S1 - true_variance)^2) * 1 / 10)
RMSE_mean_S2 <- sqrt(mean((S2 - true_variance)^2) * 1 / 10)


S_data <- data.frame('var_values' = S)
ggplot(S_data) + geom_histogram(aes(x=var_values), bins=30) +      # tak tworzymy histogram
  geom_point(aes(x=true_variance,y=0), col='red', size=3) +           # tak dodajemy dużą czerwoną kropkę
  geom_vline(xintercept = true_variance, col='red')                   # a tak pionową czerwoną linię


#2.3

sample_sd <- function(N, n) {
  samples <- matrix(rnorm(N * n, sd = 2), nrow = N)
  sds <- apply(samples, 1, sd)
  return(sds)
}

n <- 2:100
sd_estimates <- sapply(n, sample_sd, N = 100)
sd_to_plot <- data.frame('SD'=c(sd_estimates), 'n'=rep(n, each=100))


plot <- ggplot(sd_to_plot, aes(x = n, y = SD)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  labs(title = "Estimated Standard Deviation vs. Sample Size",
       x = "Sample Size",
       y = "Estimated Standard Deviation")
plot


#2.4
data("iris")
setosa_data <- iris[iris$Species == 'setosa', ]

mean_sepal_width <- mean(setosa_data$Sepal.Width)
sd_sepal_width <- sd(setosa_data$Sepal.Width)

mean_sepal_length <- mean(setosa_data$Sepal.Length)
sd_sepal_length <- sd(setosa_data$Sepal.Length)

correlation_sepal <- cor(setosa_data$Sepal.Width, setosa_data$Sepal.Length)
correlation_petal <- cor(setosa_data$Petal.Length, setosa_data$Sepal.Length)


ggplot(setosa_data, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point() +
  labs(title = "Sepal Width vs. Sepal Length for Setosa",
       x = "Sepal Length",
       y = "Sepal Width") +
  theme_minimal()

ggplot(setosa_data, aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point() +
  labs(title = "Petal Length vs. Sepal Length for Setosa",
       x = "Sepal Length",
       y = "Petal Length") +
  theme_minimal()


#2.5
n <- 100000
random_matrix <- matrix(runif(2 * n), nrow = 2)

is_in_circle <- function(point) {
  x <- point[1]
  y <- point[2]
  return(x^2 + y^2 <= 1)
}


#Oblicza jaka czesc wewnatrz kola
calculate_proportion <- function(points_matrix) {
  inside <- apply(points_matrix, 2, is_in_circle)
  mean(inside)
}


estimate_area <- calculate_proportion(random_matrix)