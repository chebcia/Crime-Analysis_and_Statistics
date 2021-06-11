# Wczytanie pakietow
library(readr)
library(corrplot) # corrplot
library(dplyr)
library(cluster)
library(fpc)
library(tidyr)
library(ggplot2)
library(GGally)
library(ggrepel)
library(moments) # skosnosc
library(heatmaply) # heatmapa
library(psych) # principal

# Weryfikacja sciezki do pliku
getwd()

# Wczytanie danych
rawData <- read.csv("./data/data.csv")

# Sprawdzenie czesci wczytanych danych
head(rawData, 3)

# Opis danych
# Pochodzenie danych - Eurostat.
# Przedzial czasowy - Dane zostaly wybrane dla 2017 roku dla wszystkich zmiennych za wyjatkiem zmiennej x8.
# Dla zmiennej x8 dostepne byly wylacznie dane dla lat 2016 oraz 2018. 
# W celu uzyskania przyblizonych wynikow dla roku 2017 obliczono srednia z lat 2016 oraz 2018.

# Opis zmiennych
# y     - Zanieczyszczenie powietrza pylami/czastkami <10??m w tonach
# x1    - Produkcja energii elektrycznej z ogniw fotowoltaicznych (TOE)
# x2    - Produkcja energii elektrycznej ze stalych paliw kopalnianych (Tysiac ton ekwiwalentu ropy naftowej, TOE)
# x3    - Produkcja energii elektrycznej z olejów napedowych (TOE)
# x4    - Produkcja energii elektrycznej brutto (TOE)
# x5    - Krajowe wydatki na ochrone srodowiska (mln Euro)
# x6    - PKB na mieszkanca w SSN (standard sily nabywczej)
# x7    - Dlugosc autostrad w kraju (w km)
# x8    - Produkcja smieci (tony)
# x9    - Chroniona powierzchnia kraju (obszary ladowe, wartosci wyrazone jako procent powierzchni kraju)
# x10   - Wskazniki recyklingu odpadów opakowaniowych wedlug rodzaju opakowania (wartosc procentowa ogólnej liczby opakowan przeznaczonych do recyklingu)
# x11   - Powierzchnia kraju (w kilometrach kwadratowych)

# Poprawa blednie wczytanej nazwy kolumny
rawData = rawData %>% rename(Country = ď..Country)
numericData <- rawData[, 2:13]

# Prezentacja wczytanych danych
head(rawData)
summary(rawData)
str(rawData)

# Weryfikacja korelacji dla poszczegolnych zmiennych
# W domylnym ulozeniu
corrplot(cor(rawData[, 2:13]), order = "original")
# W kolejnosci malejacej dla glownych komponentow
corrplot(cor(rawData[, 2:13]), order = "FPC")

### Powyzej jest ok ###

# standaryzacja
numericData.scale <- scale(numericData)
numericData.centerScale <- attr(numericData.scale, "scaled:center") 
numericData.sScale <- attr(numericData.scale, "scaled:scale") 

# Kmeans i ch
set.seed(42)
numericData.kmch <- kmeansruns(numericData.scale, krange = 2:10, criterion = "ch", runs = 10)
plot(numericData.kmch$crit, type = "b")
# Wykres wskazuje na 2

# Kmeans i asw
numericData.kmasw <- kmeansruns(numericData.scale, krange = 2:10, criterion = "asw", runs = 10)
plot(numericData.kmasw$crit, type = "b")
# Wykres wskazuje na 2

# Pam i ch
numericData.pamch <- pamk(numericData.scale, krange = 2:10, criterion = "ch")
plot(numericData.pamch$crit, type = "b")
# Wykres wskazuje na 4

# Liczebnosc grup:
table(numericData.pamch$pamobject$clustering)
# 4 grupy - 10, 2, 6 oraz 5 elementow

# Rozklady zmiennych
rawData %>% 
  mutate(cluster = as.factor(numericData.pamch$pamobject$clustering)) %>% 
  pivot_longer(2:13) %>% 
  ggplot(aes(x = cluster, y = value, fill = cluster)) +
  geom_boxplot() +
  facet_wrap(vars(name), scales = "free")

# Pam i asw
numericData.pamasw <- pamk(numericData.scale, krange = 2:10, criterion = "asw")
plot(numericData.pamasw$crit, type = "b")
# Wykres wskazuje na 2

# Liczebnosc grup:
table(numericData.pamasw$pamobject$clustering)
# 2 grupy - 17 oraz 6 elementow

# Rozklady zmiennych
rawData %>% 
  mutate(cluster = as.factor(numericData.pamasw$pamobject$clustering)) %>% 
  pivot_longer(2:13) %>% 
  ggplot(aes(x = cluster, y = value, fill = cluster)) +
  geom_boxplot() +
  facet_wrap(vars(name), scales = "free")

# Grupowanie hierarchiczne
# Wyznaczenie macierzy dystansu
distMatrix <- dist(numericData.scale, method = "euclidean")

# Grupowanie z dystansem euklidesowym metoda Warda
numericData.hclust <- hclust(distMatrix, method = "ward.D2")
plot(numericData.hclust, main = "Ward")
# Podzial na 3 grupy

# Obciecie dla 3 grup
numericData.hclust.3 <- cutree(numericData.hclust, k = 3)
numericData.hclust
table(numericData.hclust.3) 
# 3 grupy - 15, 2 oraz 6 elementow

# Grupowanie hierarchiczne z dystansem manhattan metoda Warda
distMatrix2 <- dist(numericData.scale, method = "manhattan")
plot(hclust(distMatrix2, method = "ward.D2"))
# Podzial na 3 grupy

# W sumie tu nie jest konieczna standaryzacja danych
pca1 <- principal(numericData.scale, nfactors = 12, rotate = "none")
pca1

# Wykres osypiska
plot(pca1$values, type = "b", xlab = "Liczba skladowych", ylab = "Wartosci wlasne skladowych")
abline(h = 1, lty = 2, col = "red")
# Przelamanie na wykresie przy wartosci 4

# SS loadings - wartosc wlasna ponizej 1 od piatej wartosci (PC5)
# konieczny wybor 4 skladowych
pca2 <- principal(numericData.scale, nfactors = 4, rotate = "none")
# pca2
pca2$loadings
# Z pierwsza skladowa mocno skorelowane zmienne x1, x4, x5, x7, x10
# Z druga tylko x8 i x11
# Z trzecia wylacznie x9

# Wartosci skladowych dla poszczegolnych obserwacji
countries <- pca2$scores %>% 
  as.data.frame() %>% 
  mutate(country = rawData$Country)

# Analiza PCA + grupowanie k-medoidow
countries %>% mutate(cluster = as.factor(numericData.pamch$pamobject$clustering)) %>% 
  ggplot(aes(PC1, PC2, fill = cluster)) + geom_point() + 
  geom_text_repel(aes(label = country, color = cluster))

# Prezentacja na wykresie typu biplot wybranych skladowych
biplot(pca2)
# Ze wzgledu na ilosc elementow wykres nie jest zbyt czytelny
# Z tego powodu na wykresie przedstawiane sa wylacznie dwie pierwsze skladowe
biplot(principal(numericData.scale, nfactors = 2, rotate = "none"))
# Zmienne x1, x2, x4, x5, x6, x7, x8 i x10 sa dodatnio skorelowane z pierwsza skladowa
# Zmienne y, x3, x9 i x11 sa ujemnie skorelowane z pierwsza skladowa
# Zmienne y, x1, x2, x3, x4, x8, x10, x11 sa dodatnio skorelowane z druga skladowa
# Zmienne x5, x6, x7, x9 sa ujemnie skorelowane z druga skladowa

# Within SS dla 1-10 skupisk na podst. wykresu osypiska
repl <- rep(0, 10)
for(i in 1:10)
  repl[i] <- kmeans(numericData.scale, centers = i, nstart = 10)$tot.withinss
plot(repl, type = "b")
# Przelamanie na wykresie przy wartosci 2


#############
# Srednia
valMean<-round(sapply(rawData[, 2:13], function(y) mean(y)), 2)
# valMean

# Odchylenie standardowe
valSd <- round(sapply(rawData[, 2:13], function(x) sd(x)), 2)
# valSd

# Wspolczynnik zmiennosci
valCv <- round(sapply(rawData[, 2:13], function(x) sd(x) / mean(x)), 2)
valCv
# Wszystkie wartosci maja wsp zmiennosci > 0.1

# Kwantyle
quant = round(sapply(rawData[, 2:13], function(x) quantile(x, type = 2, probs = c(0.00, 0.25, 0.5, 0.75, 1.00))), 2)
quant

# IQR
iqrs <- round(sapply(rawData[, 2:13], function(x) IQR(x)), 2)
iqrs

# Skosnosc
skwns <- round(sapply(rawData[, 2:13], function(x) skewness(x)), 2)
skwns

# Kurtoza
krt <- round(sapply(rawData[, 2:13], function(x) kurtosis(x)), 2)
krt

# Heatmapa
heatmaply_cor(
  cor(rawData[, 2:13]),
  xlab = "Features", 
  ylab = "Features",
  k_col = 2, 
  k_row = 2
)


# Interpretacje
# Wnioski
# Podsumowanie
