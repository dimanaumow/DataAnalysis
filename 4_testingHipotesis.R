#4.1
contingency_table<- matrix(c(11, 17, 62, 20), nrow = 2)
chisq.test(contingency_table, correct=F)


#4.2
Zadluzenie.gmin <- read.delim("E:/Study/MIMUW/6 semestr/SAD/Repositories/DataAnalysis/data/Zadluzenie gmin.csv", colClasses = c('factor', 'factor', 'factor', 'numeric', 'factor', 'factor'))

library(stringr)
#nowa kolumna z wyciętym początkiem kodu
Zadluzenie.gmin$Kod.Skrót <- str_sub(Zadluzenie.gmin$Kod.Teryt, 0, 2)

slownik <- c('02' = 'Dolnośląskie', '04' = 'Kujawsko-pomorskie',
             '06' = 'Lubelskie', '08' = 'Lubuskie',
             '10' = 'Łódzkie', '12' = 'Małopolskie',
             '14' = 'Mazowieckie', '16' = 'Opolskie', 
             '18' = 'Podkarpackie', '20' = 'Podlaskie',
             '22' = 'Pomorskie', '24' = 'Śląskie',
             '26' = 'Świętokrzyskie', '28' = 'Warmińsko-mazurskie',
             '30' = 'Wielkopolskie',  '32' = 'Zachodniopomorskie')

#nowa kolumna z nazwą województwa
Zadluzenie.gmin$Nazwa.wojewodztwa <- slownik[Zadluzenie.gmin$Kod.Skrót]

library(ggplot2)
ggplot() + geom_histogram(aes(x=Zadluzenie.gmin$Zadłużenie.gmin))

#wyrzucamy outliery:
Zadluzenie.gmin[Zadluzenie.gmin$Zadłużenie.gmin>100,]
Zadluzenie.gmin <- Zadluzenie.gmin[-(2474:2478),]



#4.3

#Wykonujemy test jednoprobkowy t-Studenta przy hipotezach: 
#H_0: srednie zadluzenie jest rowne 25%
#H_1: srednie zadluzenie jest mniejsze niz 25% 

mazowieckie <- Zadluzenie.gmin[Zadluzenie.gmin$Nazwa.wojewodztwa == "Mazowieckie", ]
n <- length(mazowieckie$'Zadłużenie.gmin')
mean_zero <- 25
mean_mazowieckie <- mean(mazowieckie$'Zadłużenie.gmin')
sd_mazowieckie <- sd(mazowieckie$'Zadłużenie.gmin')
t <- (mean_mazowieckie - mean_zero) / (sd_mazowieckie)*sqrt(n)

stopnie <- n - 1
p_value <- pt(t, stopnie)
t.test(mazowieckie$'Zadłużenie.gmin', mu = mean_zero, alternative = "less")
#poniewaz p-value jest duze nie mozemy odrzucic H_0


#4.4

#Testujemy rowonosc wariancji sredniego zadluzenia dwoch wojewodztw: Lodzkie i Pomorskie
#H_0: varincja i tam i tam wynosi 226
#H_1: variancja nie jest rowna 225
lodzkie <- Zadluzenie.gmin[Zadluzenie.gmin$Nazwa.wojewodztwa == 'Łódzkie', ]
pomorskie <- Zadluzenie.gmin[Zadluzenie.gmin$Nazwa.wojewodztwa == 'Pomorskie', ]
lodzkieChisq <- (nrow(lodzkie)-1)*var(lodzkie$Zadłużenie.gmin)/226 #trochę inaczej niż na slajdach, bo na slajdach obciążony, a tu nie
lodzkie_p <- 2*min(pchisq(lodzkieChisq, nrow(lodzkie)-1), 1-pchisq(lodzkieChisq,  nrow(lodzkie)-1))
pomorskieChisq <- (nrow(pomorskie) - 1)*var(pomorskie$Zadłużenie.gmin)/226
pomorskie_p <- 2*min(pchisq(pomorskieChisq, nrow(pomorskie)-1), 1-pchisq(pomorskieChisq,  nrow(pomorskie)-1))

# Dla porownania:
library(EnvStats)
varTest(lodzkie$Zadłużenie.gmin, sigma= 226)
varTest(pomorskie$Zadłużenie.gmin, sigma=226)

#p-wartości wysokie, nie ma powodu, żeby odrzucić H_0



#4.5

#Testujemy, czy przeciętna gmina z Pomorskiego jest zadłużona bardziej niż z Łódzkiego

Sp <- ((nrow(pomorskie)-1)*var(pomorskie$Zadłużenie.gmin) + (nrow(lodzkie)-1)*var(lodzkie$Zadłużenie.gmin))/(nrow(pomorskie) + nrow(lodzkie)-2)
X <- (mean(pomorskie$Zadłużenie.gmin) - mean(lodzkie$Zadłużenie.gmin))/sqrt(Sp*(1/nrow(lodzkie) + 1/nrow(pomorskie)))
my_p_value <- 2*pt(-abs(X), nrow(pomorskie) + nrow(lodzkie)-2)

#dla porównania
test_result <- t.test(pomorskie$Zadłużenie.gmin, lodzkie$Zadłużenie.gmin, var.equal=T)
test_result$p.value

#p-wartość bardzo niska, ODRZUCAMY H_0 (czyli różne średnie)

#4.6
malopolskie <- Zadluzenie.gmin[Zadluzenie.gmin$Nazwa.wojewodztwa == 'Małopolskie', ]
slaskie <- Zadluzenie.gmin[Zadluzenie.gmin$Nazwa.wojewodztwa == 'Śląskie', ]
t_test_result <- t.test(malopolskie$Zadłużenie.gmin, slaskie$Zadłużenie.gmin, var.equal = FALSE)

