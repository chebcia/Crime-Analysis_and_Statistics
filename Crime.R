library(e1071)
Crime <- nowe
Crime=Crime[,-c(1,4)]

# function to save to csv 
 saveit <-function(df, output) 
 {
   df=format(df,scientific = F)
   write.csv(df, file = output)
 }

#Analiza zmiennych 
 
#średnia 
means<-sapply(Crime, function(x) mean(x))
saveit(means, "srednie")

#odchylenie standardowe
sds <- sapply(Crime, function(x) sd(x))
saveit(sds, "odchylenia")

#wspołczynnik zmiennosci
cv <- sapply(Crime, function(x) sd(x) / mean(x))
saveit(cv, "cv")

#sprawdzenie czy są wartości poniżej 0.1
for(i in 1:14)
{
  if(cv[i]<0.1)
  {
    print(cv[i])
    
  }
}
#kwartyle
kwartyle=sapply(Crime, function(x)   quantile(x, type = 2, probs = c(0.00,0.25, 0.5, 0.75,1.00) ))
saveit(kwartyle, "kwartyle")
#IQR
iqrs <- sapply(Crime, function(x) IQR(x, na.rm = TRUE))
saveit(iqrs, "iqrs")
#skosnosc
skes <- sapply(Crime, function(x) skewness(x,na.rm = TRUE))
saveit(skes, "skes")
#kurtoza
kur <- sapply(Crime, function(x) kurtosis(x,na.rm = TRUE))
saveit(kur, "kurthosis")

#obliczenie korelacji pomiędzy zmiennymi - pearson
cor(Crime)

library(heatmaply)
heatmaply_cor(
  cor(Crime),
  xlab = "Features", 
  ylab = "Features",
  k_col = 2, 
  k_row = 2
)

pvalues=rcorr.adjust(Crime, type = c("pearson"), 
             use=c("complete."))[3]
p=as.data.frame(pvalues[1])


library("Rcmdr")
#PCA 
PCA <-Crime[,c(1,2,4:8,12)]

library(dplyr)
library(tidyr)
library(ggplot2)
library(psych)
library(GGally) 

ggpairs(PCA)
corpca=cor(PCA) 
corpca=data.frame(corpca)
saveit(corpca, "pca.csv")

prc1 <- principal(PCA, nfactors=8, rotate="none") 
prc1$values
prc1$loadings 

plot(prc1$values, type="b", main = "Wykres osypiska", xlab="Składowe", ylab="Kryterium Keisera") 
abline(h=1, col="red") 

plot(cumsum(prc1$values)/8, type="b", main = "Procentowe wyjaśnienie wariancji", xlab="Składowe", ylab="Wyjaśniona wariancja") 

prc2 <- principal(PCA, nfactors=1, rotate="none")
biplot(prc2) 
prc2$loadings  


prc3 <- principal(PCA, nfactors=1, rotate="varimax") 
prc3
biplot(prc3) 
prc3$loadings
