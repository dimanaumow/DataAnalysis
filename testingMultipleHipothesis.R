#5.1
proba1 <- replicate(1000, rnorm(n = 10, mean =  0.5, sd = 1))
proba2 <- replicate(1000, rnorm(n = 10, mean = -0.5, sd = 1))

#wykonujemy t.test na wszystkich probach
t_p_values <- sapply(1:ncol(proba1), 
                     function(i) t.test(proba1[,i], proba2[,i], var.equal = TRUE)$p.value)

#wykonujemy test Wilcoxa na wszystkich probach
w_p_values <- sapply(1:ncol(proba1), 
                     function(i) wilcox.test(proba1[,i], proba2[,i])$p.value)

library(ggplot)
dane_do_wykresu <- data.frame('Test'=rep(c('Student', 'Wilcox'), each=1000), 'p_wartosc' = c(t_p_values, w_p_values))
ggplot(dane_do_wykresu) + geom_histogram(aes(x=p_wartosc, fill=Test), binwidth=0.01) + theme_minimal()

#gdzie p-valuje wieksze
sum(t_p_values < w_p_values)

#tworzenie obszrow krytycznych dla obu testow
alpha <- 0.05
student_odrzucil <- t_p_values < alpha
wilcox_odrzucil <- w_p_values < alpha
mean(student_odrzucil)
mean(wilcox_odrzucil)



#5.2
dane <- read.table("E:/Study/MIMUW/6 semestr/SAD/Repositories/DataAnalysis/data/Zarobki.tsv", header = TRUE, sep = "\t")
kraj_A <- dane[dane$Kraj == "A", "Zarobki"]
kraj_B <- dane[dane$Kraj == "B", "Zarobki"]


SA<-mean(kraj_A)
SB<-mean(kraj_B)

ggplot()+geom_histogram(aes(dane$Zarobki, fill = dane$Kraj),binwidth = 100)
?outer
#jak procentowo A bogatszy niz B
mean(outer(kraj_A,kraj_B,FUN='>'))

t.test(kraj_A,kraj_B,alternative="less")
wilcox.test(kraj_A,kraj_B,alternative = "greater")



#5.3
install.packages("ISLR")
library(ISLR)

data(Khan)
x <- Khan$xtrain[Khan$ytrain %in% c(2, 4), ]
y <- Khan$ytrain[Khan$ytrain %in% c(2, 4)]
p_values <- numeric(ncol(x))


# test Welsha 
for (i in 1:ncol(x)) {
  p_values[i] <- t.test(x[y == 2, i], x[y == 4, i])$p.value
}

# p-wartości metodą Bonferroniego
p_bonferroni <- p.adjust(p_values, method = "bonferroni")

# p-wartości metodą Holma
p_holm <- p.adjust(p_values, method = "holm")

# p-wartości metodą Benjamini-Hochberg
p_bh <- p.adjust(p_values, method = "BH")

ggplot()+geom_histogram(aes(x =p_bonferroni ))
ggplot()+geom_histogram(aes(x = p_bh ))

gene_exp<-Khan$xtrain
t_class<-Khan$ytrain
grupa1<-gene_exp[t_class==2,]
grupa2<-gene_exp[t_class==4,]
p.value<-sapply(1:2308,function(x) t.test(grupa1[,x],grupa2[,x])$p.value)



#5.5
library(ggplot2)
X <- c(rnorm(1000, 1, 1), rnorm(9000, 0, 1))
X <- matrix(X, nrow=10)  # domyślnie macierz jest uzupełniana kolumnowo - 100 pierwszych kolumn odpowiada H1, pozostałe H0
p.values <- apply(X, 2, function(x) t.test(x, alternative='greater')$p.value)
ggplot() + geom_histogram(aes(p.values)) + theme_minimal()

# Idea Bonferroniego lub Holm: jeśli przyjmujemy H1 gdy q-wartość <=0.05, to prawdopodobieństwo popełnienia jakiegokolwiek 
# błędu I rodzaju (pojawienia się przynajmniej jednego False Positive) jest mniejsze niż 0.05. 
bonferroni <- p.adjust(p.values, method='bonferroni')
ggplot() + geom_histogram(aes(bonferroni)) + theme_minimal()

holm <- p.adjust(p.values, method='holm')
ggplot() + geom_histogram(aes(holm)) + theme_minimal()

# Idea korekcji BH: jeśli przyjmujemy H1 gdy q-wartość <=0.05, to FDR powinien być <= 0.05. 
bh <- p.adjust(p.values, method='BH')
ggplot() + geom_histogram(aes(bh)) + theme_minimal()

q.values <- data.frame('p.value' = p.values, 'bonferroni' = bonferroni, 'holm' = holm, 'bh' = bh)

power <- apply(q.values[1:100,], 2, function(x) mean(x <= 0.05))  # = moc testu a.k.a. czulosc, sensitivity, TPR
# Benjamini-Hohberg ma większą moc niż pozostałe korekcje. Oryginalny test ma największą moc.
FDR <- apply(q.values, 2, function(x) sum(x[101:1000] <= 0.05)/sum(x <= 0.05))  # false discovery rate
# Benjamini-Hohberg ma z grubsza taki FDR jak zadany próg q-wartości. Mówimy, że korekcja BH "kontroluje" FDR. 
# Oryginalny test ma największy FDR (mamy około 40% fałszywych "odkryć").
FPR <- apply(q.values, 2, function(x) sum(x[101:1000]<= 0.05) / 900)  # false positive rate, błąd I rodzaju
# Wszystkie korekcje dają lepszą specyficzność niż oryginalne p-wartości (dają mniejsze prawdopodobieństwo błędu I rodzaju).
# Przy FPR warto zwrócić uwagę że dla p-wartości jest ono bliskie 0.05. Mówimy, że p-wartość "kontroluje" FPR.

accuracy <- apply(q.values, 2, function(x) (sum(x[1:100]<=0.05)+sum(x[101:1000]>=0.05))/1000)
precision <- apply(q.values, 2, function(x) sum(x[1:100]<=0.05)/sum(x<=0.05))
# Q-wartości powinny dawać lepszą precyzję (większą pewność, że wynik oznaczony jako H1 rzeczywiście odpowiada H1), 
# kosztem mniejszej mocy i czasami nieco słabszej ogólnej trafności wyników (mierzonej za pomocą accuracy) 
