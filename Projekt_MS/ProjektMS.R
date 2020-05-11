library(tidyverse)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(caTools)


Autko <- read.table("./data/auto-mpg.data", quote="\"", comment.char="")
names(Autko)[1] <- "mpg"
names(Autko)[2] <- "cylinders"
names(Autko)[3] <- "displacement"
names(Autko)[4] <- "horsepower"
names(Autko)[5] <- "weight"
names(Autko)[6] <- "acceleration"
names(Autko)[7] <- "model_year"
names(Autko)[8] <- "origin"
names(Autko)[9] <- "car_name"

Autko2<-Autko

# pamietajcie ze komentujemy #
#wczensiej poprawi³am nazwe kolumny zeby by³a z podkresleniem bo inaczej to takie b³edy
names(Autko)[9] <- "car_name"
# pó¿niej zamieni³am factory na string zeby by³ tekst nie? :)
Autko$car_name<-as.character(Autko$car_name)
#oddzielenie marki i modelu funkcja z pakietu dplyr czyli duzego pakietu tidyverse, 
#funkcja seperate s³uzy do rozdzielania stringów, 
#doda³am na "merge" bo tak napisali na stacku bo mielismy wiele whitespaców a tak rozdzieli³ do pierwszej :)
Autko <- separate(Autko, col = c("car_name"), into = c("brand", "model"), sep = " ", extra = "merge")


#zamiana na factory nie moze byc numeric
Autko$cylinders = Autko$cylinders %>%
  factor(labels = sort(unique(Autko$cylinders)))
#rok te¿
Autko$model_year = Autko$model_year %>%
  factor(labels = sort(unique(Autko$model_year)))
# i origin
Autko$origin = Autko$origin %>%
  factor(labels = sort(unique(Autko$origin)))


#poniewa¿ nie ma nigdzie indziej "?" mozemy zamienic na wartosci N/A
Autko[Autko == "?"] <- NA
#musimy zmienic na numerics bo by³y factory przynajmniej u mnie najpierw na character by nie tracic wartosci
Autko$horsepower<-as.numeric(as.character(Autko$horsepower))
#obliczamy œredni¹ aby j¹ pó¿niej zast¹piæ
srednia<-mean(Autko$horsepower, na.rm = TRUE)
#zastepujemy nulle
Autko$horsepower <- ifelse(is.na(Autko$horsepower), mean(Autko$horsepower, na.rm=TRUE), Autko$horsepower)






