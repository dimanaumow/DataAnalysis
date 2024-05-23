#6.1
wine <- read.delim("E:/Study/MIMUW/6 semestr/SAD/Repositories/DataAnalysis/data/wine.csv")

#skalujemy dane
?scale
skalowanie <- scale(wine[,-1])
wine[,2:ncol(wine)]<-apply(wine[,2:ncol(wine)],2,function(x) x-mean(x))
wine[,2:ncol(wine)]<-apply(wine[,2:ncol(wine)],2,function(x) x/sd(x))

wine$Quality <- as.factor(wine$Quality)
wine$Quality


#6.2

#predyktory
x <- c(0.42, 0.03, -0.90, 0.15, -1.25, -0.15, -0.01, 0.73, 0.90, -0.82, -0.69)
distances <- apply(wine[,-1], 1, function(y) sqrt(sum((x-y)^2)))

#liczba sasiadow
k <- 3

najblizsze_wiersze <- order(distances)[1:k]
najblizsze_klasy <- wine[najblizsze_wiersze, 1]
czestosc_klas <- table(najblizsze_klasy)
czestosc_klas
najczestsza_klasa <- which.max(czestosc_klas)
najczestsza_klasa
najczestsza_klasa <- levels(wine$Quality)[najczestsza_klasa]



#6.3

#Realizyjemy podzial na zbior testowy i treningowy: 
indeksy_testowe <- sample(1:nrow(wine), 480, replace=F)
zbior_testowy <- wine[indeksy_testowe, ]
zbior_treningowy <- wine[-indeksy_testowe, ]  # Indeksowanie ujemne wiele ułatwia!

?knn
library(class)
wynik <- knn(zbior_treningowy[,-1], zbior_testowy[,-1], zbior_treningowy[,1], k=3)
mean(wynik==zbior_testowy[,1])

#sprawdzamy dla roznych k od 1 do 15
sapply(1:15, function(x) mean(zbior_testowy[,1]==knn(zbior_treningowy[,-1], zbior_testowy[,-1], zbior_treningowy[,1], k = x)))


#6.5
classifikation <- knn(zbior_treningowy[,-1], zbior_testowy[,-1], zbior_treningowy[,1], k=2)
confusion <- table(zbior_testowy[,1], classifikation)
confusion

precision <- diag(confusion)/colSums(confusion)
precision

recall <- diag(confusion)/rowSums(confusion)
recall
p1 <- sum(confusion[lower.tri(confusion)])/sum(confusion)
p2 <- sum(confusion[upper.tri(confusion)])/sum(confusion)
