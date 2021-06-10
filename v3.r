# Wczytanie pakietow

library(readr) # Wczytywanie danych
library(psych) # principal
library(corrplot) # wykres korelacji
library(dplyr) # pakiet z tidyverse -
library(tidyr) # do uporzÄ…dkowywania danych
library(ggplot2) # Å‚adne wykresy
library(GGally) # dodatek do ggplot2
library(ggrepel) # dodatek do edycji ggplot2
library(cluster) # dla funkcji pam
library(fpc) # dla funkcji kmeansruns, pamk

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
rawData = rawData %>% rename(Country = ï..Country)

# Prezentacja wczytanych danych
head(rawData)
summary(rawData)
str(rawData)

# Weryfikacja korelacji dla poszczegolnych zmiennych
# W domylnym ulozeniu
corrplot(cor(rawData[, 2:13]), order = "original")
# W kolejnosci malejacej dla glownych komponentow
corrplot(cor(rawData[, 2:13]), order = "FPC")

# Odwrócenie kierunku korelacji dla zmiennych, ktore powinny wplywac negatywnie na jakosc powietrza
rawData$x2 <- max(rawData$x2) - rawData$x2
rawData$x3 <- max(rawData$x3) - rawData$x3
rawData$x4 <- max(rawData$x4) - rawData$x4
rawData$x7 <- max(rawData$x7) - rawData$x7
rawData$x8 <- max(rawData$x8) - rawData$x8
rawData$x11 <- max(rawData$x11) - rawData$x11

# Weryfikacja korelacji dla poszczegolnych zmiennych
# W domylnym ulozeniu
corrplot(cor(rawData[, 2:13]), order = "original")
# W kolejnosci malejacej dla glownych komponentow
corrplot(cor(rawData[, 2:13]), order = "FPC")

### Powyzej jest ok ###


### Ale to ponizej do zmiany jeszcze ###
# (To co powyzej tez w miare potrzeb)




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