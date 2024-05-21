#3.1
Zadluzenie.gmin <- read.delim("E:/Study/MIMUW/6 semestr/SAD/Repositories/DataAnalysis/data/Zadluzenie gmin.csv")
summary(Zadluzenie.gmin)
summary(Zadluzenie.gmin$Zadłużenie.gmin)
sd(Zadluzenie.gmin$Zadłużenie.gmin)
mean(Zadluzenie.gmin$Zadłużenie.gmin)

ggplot(data = Zadluzenie.gmin, aes(x = Zadluzenie.gmin$Zadłużenie.gmin)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Municipal Debt", x = "Debt", y = "Frequency")

Zadluzenie.gmin <- Zadluzenie.gmin[Zadluzenie.gmin$Region != "Ostrowice", ]


#3.2
qqplot_zadluzenia <- ggplot(data = Zadluzenie.gmin, aes(sample = Zadluzenie.gmin$Zadłużenie.gmin)) + 
  stat_qq() + 
  stat_qq_line(colour = "red") + 
  ggtitle("Wykres kwantylowy zadłużenia gmin") +
  xlab("Kwantyle teoretyczne") +
  ylab("Kwantyle obserwowane")


qqplot_zadluzenia


#3.3
data(iris)
iris_versicolor <- subset(iris, iris$Species == "versicolor")

ggplot(iris_versicolor, aes(sample = iris_versicolor$Sepal.Width)) + 
  stat_qq() + 
  stat_qq_line(colour = "red") + 
  ggtitle("Wykres kwantylowy dla Sepal.Width gatunku Versicolor") +
  xlab("Kwantyle teoretyczne") +
  ylab("Kwantyle obserwowane")
#Z wykresu wynika, ze dane maja rozklad normalny

mean_sepal_width <- mean(iris_versicolor$Sepal.Width)
sd_sepal_width <- sd(iris_versicolor$Sepal.Length)
n <- nrow(iris_versicolor)
alpha <- 0.05
t_critical <- qt(1 - alpha/2, df = n-1)

#Budujemy przedzial ufnosci dla sredniej szerokosci dzialki na podstawie tego 
#ze ma ona rozklad podobny do normalnego

margin_error <- t_critical * sd_sepal_width / sqrt(n - 1)
ci_lower <- mean_sepal_width - margin_error
ci_upper <- mean_sepal_width + margin_error

# Wyniki
cat("Średnia Sepal.Width dla Versicolor:", mean_sepal_width, "\n")
cat("Przedział ufności (95%): (", ci_lower, ", ", ci_upper, ")\n")


#3.4
mean_sepal_width <- mean(iris_versicolor$Sepal.Width)
sd_sepal_width <- sd(iris_versicolor$Sepal.Length)
n <- nrow(iris_versicolor)
alpha <- 0.05
z <- qnorm(1 - alpha/2)

margin_error_norm <- z * sd_sepal_width / sqrt(n)
ci_lower_norm <- mean_sepal_width - margin_error_norm
ci_upper_norm <- mean_sepal_width + margin_error_norm

# Wydrukowanie wyników
cat("Asymptotyczny przedział ufności (95%): (", ci_lower_norm, ", ", ci_upper_norm, ")\n")


#3.5
set.seed(123)

a <- 5  # Zakres maksymalny dla rozkładu jednostajnego
n <- 10  # Liczba obserwacji w próbie
m <- 1000  # Liczba prób

# Generowanie prób i obliczanie estymatora
samples <- replicate(m, runif(n, 0, a))
estimates <- replicate(m, max(runif(n, 0, a)))

hist(estimates, breaks = 30, col = 'blue', main = "Histogram estymatorów a",
     xlab = "Estymowane wartości a", ylab = "Częstość")

calculate_ci <- function(sample) {
  max_xn <- max(sample)
  ci_lower <- max_xn
  ci_upper <- max_xn / (a^(1/n))
  c(ci_lower, ci_upper)
}

# Obliczanie przedziałów ufności dla wszystkich prób
ci_bounds <- apply(samples, 2, calculate_ci)

# Sprawdzanie, ile przedziałów zawiera wartość a
contains_a <- apply(ci_bounds, 2, function(ci) a >= ci[1] && a <= ci[2])
count_contains_a <- sum(contains_a)

# Wyniki
cat("Liczba prób, dla których przedział ufności zawiera wartość a:", count_contains_a, "\n")
