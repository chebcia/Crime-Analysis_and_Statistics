Crime <- nowe
Crime=Crime[,-c(1)]

# analiza - średnia
means<-sapply(Crime, function(x) mean(x))
means=format(means,scientific = F)

sds <- sapply(Crime, function(x) sd(x))
sds=format(sds,scientific = F)

cv <- sapply(Crime, function(x) sd(x) / mean(x))
cv=format(cv,scientific = F)
cv=cv[2:21]

#sprawdzenie czy są wartości poniżej 0.1
for(i in 1:20)
{
  if(cv[i]<0.1)
  {
    print(cv[i])
    
  }
}

Crimepop <- Crime[,-c(13,1)]

kwartyle=sapply(Crime, function(x)   quantile(x, type = 2, probs = c(0.00,0.25, 0.5, 0.75,1.00) ))


mat = matrix(ncol = 19, nrow = 19)
df=data.frame(mat)


for(i in 1:ncol(Crimepop))
{
  for(j in 1:ncol(Crimepop))
  {
    df[i,j] = cor(Crimepop[,i],Crimepop[,j],use="pairwise.complete.obs", method = "pearson")
   }
 }
breaksList = seq(-0.4, 1, by = 0.1)

palette_primary_color <- "#9C27B0"
palette_text <- "#FFFFFF"

rownames(df) <- names(Crimepop)
colnames(df) <- names(Crimepop)
library(pheatmap)
library(RColorBrewer)
pheatmap(as.matrix(tab), border="black",
         color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(length(breaksList)), # Defines the vector of colors for the legend (it has to be of the same lenght of breaksList)
         main="Korelacja pomiedzy zmiennymi objasniajacymi", scale = 
           "none", angle_col = 90)

col=colnames(Crime[,-c(4)])

ggplot(data = tab, aes(x=col, y=col, fill=tab))) + 
  geom_title()

col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(as.matrix(tab), col=col, symm=TRUE)

library(heatmaply)
heatmaply_cor(
  cor(Crime[,-c(4,3)]),
  xlab = "Features", 
  ylab = "Features",
  k_col = 2, 
  k_row = 2
)

scale_rows = function(x){
  m = apply(x, 1, mean, na.rm = T)
  s = apply(x, 1, sd, na.rm = T)
  return((x - m) / s)
}

tab=scale_rows(tab)


df2=data.frame(mat)
for(i in 1:ncol(Crimepop))
{
  for(j in 1:ncol(Crimepop))
  {
    if((i ==j))
    {
      df2[i,j] <- 0
    } 
    else 
    {
      df2[i,j]=t.test(Crimepop[,i],Crimepop[,j], paired = TRUE, alternative = "two.sided")[3]
      
    }
  }

}

for(i in 5:15)
{
  
  print(cor(Crime[,i], Crime[4]))
}



pvalues=rcorr.adjust(Crimepop, type = c("pearson"), 
             use=c("complete."))[3]
p=as.data.frame(pvalues[1])




#PCA 
PCA <-Crime[,c(1,2,5,7,9,11, 12,17)]
mat = matrix(ncol = 8, nrow = 8)
PCAdf=data.frame(mat)


for(i in 1:ncol(PCA))
{
  for(j in 1:ncol(PCA))
  {
    PCAdf[i,j] = cor(PCA[,i],PCA[,j],use="pairwise.complete.obs", method = "pearson")
  }
}

write.csv(PCAdf, file = "dane.csv")
pcaval=princomp(PCA , cor = TRUE, scores = TRUE)
k<-unclass(loadings(pcaval))
write.csv(k, file = "k.csv")
                
summ <- summary(pcaval)

library(dplyr)
library(tidyr)
library(ggplot2)
library(psych)
library(GGally) 


ggpairs(PCA)
cor(PCA) 

prc1 <- principal(PCA[,-c(9:10)], nfactors=8, rotate="none") 
prc1$values
prc1$loadings 

plot(prc1$values, type="b") 
abline(h=1, col="red") 

plot(cumsum(prc1$values)/6, type="b") 

prc2 <- principal(PCA[,-c(9:10)], nfactors=2, rotate="none")
biplot(prc2) 
prc2$loadings  


prc3 <- principal(PCA[,-c(9:10)], nfactors=2, rotate="varimax") 
biplot(prc3) 
prc3$loadings

#średnie
means = data.frame(means)
write.csv(means, file = "means.csv")

sds=data.frame(sds)
write.csv(sds, file = "sds.csv")




iqrs <- sapply(Crime, function(x) IQR(x, na.rm = TRUE))
iqrs=data.frame(iqrs)
write.csv(iqrs, file = "iqrs.csv")
cv=data.frame(cv)
write.csv(cv, file = "cvs.csv")
skes <- sapply(Crime, function(x) skewness(x,na.rm = TRUE))
library(e1071)
skes=data.frame(skes)
write.csv(skes, file = "skeis.csv")
kur <- sapply(Crime, function(x) kurtosis(x,na.rm = TRUE))
kur=data.frame(kur)
write.csv(kur, file = "kur.csv")


kwartyle=data.frame(kwartyle)
write.csv(kwartyle, file = "kwa.csv")
write.csv(df, file = "df.csv")


ggplot(Crime, aes(x=Ludność)) +
  geom_density(alpha=.2, fill="#FF6666") + ggtitle("Wykres gęstości dla zmiennej ludności" )

tab=cor(Crime[,-c(4)])
tab = data.frame(tab)
write.csv(tab, file = "tab.csv")
