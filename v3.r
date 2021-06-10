# Wczytanie pakietow

library(readr)
library(psych)
library(corrplot)
library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(ggrepel)
library(cluster)
library(fpc)
library(moments) # skosnosc
library(heatmaply) # heatmapa

# Weryfikacja sciezki do pliku
getwd()

# Wczytanie danych
#rawData <- read.csv("./data.csv")
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

###

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

### Ale to ponizej do zmiany jeszcze ###
# (To co powyzej tez w miare potrzeb)

heatmaply_cor(
  cor(rawData[, 2:13]),
  xlab = "Features", 
  ylab = "Features",
  k_col = 2, 
  k_row = 2
)


# standaryzacja
rawData.s <- scale(rawData[, -c(1)])
rawData.center <- attr(rawData.s, "scaled:center") 
rawData.scale <- attr(rawData.s, "scaled:scale") 

# Analiza skupien 
# Wybor liczby skupien

# Wykres osypiska - a tego akurat nie rozumiem do konca
set.seed(1)
x <- 1:10
for(i in 1:10)
  x[i] <- kmeans(rawData.s, centers = i, nstart = 10)$tot.withinss
plot(x, type = "b")

# Wykres wskazuje na 2

# kmeans i ch
set.seed(10)
rawData.km1 <- kmeansruns(rawData.s, krange = 2:10, criterion = "ch", runs = 10)
plot(rawData.km1$crit, type = "b")

# Wykres wskazuje na 2

# kmeans i asw

set.seed(20)
rawData.km2 <- kmeansruns(rawData.s, krange = 2:10, criterion = "asw", runs = 10)
plot(rawData.km2$crit, type = "b")

# Wykres wskazuje na 2

# pam i ch

rawData.pam1 <- pamk(rawData.s, krange = 2:10, criterion = "ch")
plot(rawData.pam1$crit, type = "b")

# Wykres wskazuje na 2

# pam i asw

rawData.pam2 <- pamk(rawData.s, krange = 2:10, criterion = "asw")
plot(rawData.pam2$crit, type = "b")

# Wykres wskazuje na 2

# Grupowanie hierarchiczne z dystansem euklidesowym

d <- dist(rawData.s, method = "euclidean")
plot(hclust(d, method = "ward.D2"))

# 2 grupy/tez 3?


# Grupowanie hierarchiczne z dystansem manhattan

d <- dist(rawData.s, method = "manhattan")
plot(hclust(d, method = "ward.D2"))
# 2 grupy/W sumie to 3 chyba lepsza opcja by bylo



# To jest do zmiany:
# Klasteryzacja

# ustalenie grupy:

rawData.pam1$pamobject$clustering

# Liczebnosc grup:

table(rawData.pam1$pamobject$clustering)

# Rozklady zmiennych
rawData %>% 
  mutate(cluster = as.factor(rawData.pam1$pamobject$clustering)) %>% 
  pivot_longer(2:13) %>% 
  ggplot(aes(x = cluster, y = value, fill = cluster)) +
  geom_boxplot() +
  facet_wrap(vars(name), scales = "free")

# Interpretacja:


rawData.s %>% 
  as.data.frame() %>% 
  mutate(cluster = as.factor(rawData.pam1$pamobject$clustering)) %>%
  group_by(cluster) %>% 
  summarise_all(mean) %>% 
  pivot_longer(-1) %>% 
  ggplot(aes(x = name, y = value, color = cluster)) +
  geom_line(aes(group = cluster)) +
  geom_point() +
  theme_minimal() +
  coord_flip()

# Interpretacja:


# Analiza skupien - PCA
rawData.pca <- principal(rawData.s, nfactors = 12, rotate = "none")

# Analiza wynikow
rawData.pca

# Interpretacja:

# Zmniejszenie liczby factorow
rawData.pca2 <- principal(rawData.s, nfactors = 2, rotate = "none")

# Analiza wynikow
rawData.pca2$loadings

# Interpretacja:


biplot(rawData.pca2)

# Interpretacja:

rawData2 <- rawData.pca2$scores %>% 
  as.data.frame() %>% 
  mutate(Country = rawData$kraj)

# Interpretacja

# Podsumowanie
