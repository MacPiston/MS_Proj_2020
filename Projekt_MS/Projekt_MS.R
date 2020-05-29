library(tidyverse)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(caTools)

Autko<- read_csv("Autko.csv")


names(Autko)[1] <- "mpg"
names(Autko)[2] <- "cylinders"
names(Autko)[3] <- "displacement"
names(Autko)[4] <- "horsepower"
names(Autko)[5] <- "weight"
names(Autko)[6] <- "acceleration"
names(Autko)[7] <- "model_year"
names(Autko)[8] <- "origin"
names(Autko)[9] <- "car_name"



# pamietajcie ze komentujemy #
#wczensiej poprawiłam nazwe kolumny zeby była z podkresleniem bo inaczej to takie błedy
names(Autko)[9] <- "car_name"
# póżniej zamieniłam factory na string zeby był tekst nie? :)
Autko$car_name<-as.character(Autko$car_name)
#oddzielenie marki i modelu funkcja z pakietu dplyr czyli duzego pakietu tidyverse, 
#funkcja seperate słuzy do rozdzielania stringów, 
#dodałam na "merge" bo tak napisali na stacku bo mielismy wiele whitespaców a tak rozdzielił do pierwszej :)
Autko <- separate(Autko, col = c("car_name"), into = c("brand", "model"), sep = " ", extra = "merge")


#zamiana na factory nie moze byc numeric
Autko$cylinders = Autko$cylinders %>%
  factor(labels = sort(unique(Autko$cylinders)))
#rok też
Autko$model_year = Autko$model_year %>%
  factor(labels = sort(unique(Autko$model_year)))
# i origin
Autko$origin = Autko$origin %>%
  factor(labels = sort(unique(Autko$origin)))


#ponieważ nie ma nigdzie indziej "?" mozemy zamienic na wartosci N/A
Autko[Autko == "?"] <- NA
#musimy zmienic na numerics bo były factory przynajmniej u mnie najpierw na character by nie tracic wartosci
Autko$horsepower<-as.numeric(as.character(Autko$horsepower))
#zastepujemy nulle
Autko$horsepower <- ifelse(is.na(Autko$horsepower), mean(Autko$horsepower, na.rm=TRUE), Autko$horsepower)


#zamiana brandów na odpowiednie nazwy
Autko$brand[Autko$brand == "chevroelt"] <- "chevrolet"
Autko$brand[Autko$brand == "maxda"] <- "mazda"
Autko$brand[Autko$brand == "vokswagen"] <- "volkswagen"
Autko$brand[Autko$brand == "toyouta"] <- "toyota"
Autko$brand[Autko$brand == "vw"] <- "volkswagen"
Autko$brand[Autko$brand == "mercedes-benz"] <- "mercedes"


Autko$brand = Autko$brand %>%
  factor(labels = sort(unique(Autko$brand)))

#Analiza zbioru
#wyliczenie najpopularniejszych marek 
namesOccurence <- Autko %>% group_by(brand) %>% tally() %>% rename(Number_of_Occurences = n)
#Usuniecie dwoch pustych rekordow
namesOccurence <- namesOccurence[3:nrow(namesOccurence),]

srednia<-namesOccurence %>% group_by(brand) %>% summarise(srednia = mean(namesOccurence$Number_of_Occurences))
srednia<-as.numeric(srednia[1,2])

#wykres najpopularniejszych samochodów (top 20), czarną linią przerywaną zaznaczono średnią wystąpień
namesOccurence  %>% top_n(20, Number_of_Occurences) %>% arrange(desc(Number_of_Occurences)) %>% ggplot(aes(x=brand, y=Number_of_Occurences)) +
geom_bar(stat='identity') +
coord_flip() +  
ggtitle("Najpopularniejsze samochody")+
xlab("Marki samochodów")+
ylab("Ilość") + 
theme_test() +
geom_hline(yintercept = srednia, color = "black",linetype=4)+
theme(plot.title = element_text(size = 15,  face= 'bold', margin = ))+
  theme(legend.title =element_text(size = 40, face= 'bold'), legend.position = "bottom")+
  theme(axis.title.x = element_text( face="bold"))+
  theme(axis.title.y= element_text( face="bold")) 
#jak widać najpopularniejszymi markami jest ford i chevrolet 

#wykres mpg
Autko %>% group_by(brand) %>% 
  summarise(sredniam = mean(mpg, na.rm = TRUE))  %>%
  ggplot(aes(x=brand, y=sredniam))+geom_bar(stat='identity')  + coord_flip()+
  xlab("Marki samochodów") +ylab("średnia spalania")+
  ggtitle("Średnia spalań")+
  theme(plot.title = element_text(size = 15,  face= 'bold', margin = ))+
  theme(legend.title =element_text(size = 40, face= 'bold'), legend.position = "bottom")+
  theme(axis.title.x = element_text( face="bold"))+
  theme(axis.title.y= element_text( face="bold"))





Autko2<-Autko%>% group_by(model_year) %>%summarise(ilosc = n())
sredniak<-mean(Autko2$ilosc, na.rm =TRUE)


Autko%>% group_by(model_year) %>%summarise(ilosc = n()) %>% ggplot(aes(x=model_year,y= ilosc ), lty=5)+
  geom_line(group=1)+
  geom_point(size=2)+
  theme_bw()+
  theme(legend.position = "none")+  xlab("Rok") +ylab("Ilość")+
  geom_hline(yintercept = sredniak, linetype="dotted", color="red", size=2)+
  
  annotate(geom="text", x= 10,y=sredniak-0.5,
           label="średnia ilość aut",color= "black", size=4)+
  ggtitle("Ilość aut w danym roku")+
  geom_text(label="") +
  theme(axis.title.x = element_text( face="bold"))+
  theme(axis.title.y= element_text( face="bold")) +
  theme(plot.title = element_text(size = 15,  face= 'bold' ))


Autko %>% group_by(model_year)%>% summarise(srednia = mean(horsepower), srednia2 = mean(displacement)) %>%
  ggplot(aes(x=model_year))+
  geom_line(aes(x = model_year, y=srednia, group = 1),lty=2, size=0.8)+
  geom_line(aes(x= model_year, y=srednia2, group = 1),size=1.2)+
  scale_color_gradient(low = 'blue', high = 'red')+
  theme_bw()+
  annotate(geom="text", x=6,y=230,
           label="displacement",color= "black", size=4 )+
  annotate(geom="text", x=3,y=150,
           label="horsepower",color= "black", size=4 ) +
  ggtitle("Średnia dla horsepower i displacement w danym roku")+
  theme(legend.position = "none")+
  labs(x="Rok",y="") +
  theme(axis.title.x = element_text( face="bold"))+
  theme(axis.title.y= element_text( face="bold")) +
  theme(plot.title = element_text(size = 15,  face= 'bold' ))


#DODANIE NOWEJ KOLUMNY MPGOPT - OKRESLAJACEJ OCHYLENIE OD SREDNIEJ MPG
Autko$mpgopt<- round((Autko$mpg - mean(Autko$mpg))/sd(Autko$mpg), 2)
Autko$typ <- ifelse(Autko$mpgopt < 0, "pod", "nad")
#wykres dla powyżej i poniżej średniej mpg
Autko %>%group_by(brand) %>% ggplot(aes(x=brand, y=mpgopt, label=mpgopt)) + 
  geom_bar(stat='identity', aes(fill=typ), width=.5)  +
  scale_fill_manual(name="Według mpg", 
  labels = c("Powyżej średniej", "Poniżej średniej"), 
  values = c("nad"="#00ba38", "pod"="#f8766d")) + 
  labs(subtitle="Normalised mileage from 'mtcars'", 
  title= "Diverging Bars") + 
  coord_flip()


#wyliczenie procentów
namesOccurence$procent <- round(namesOccurence$Number_of_Occurences / sum(namesOccurence$Number_of_Occurences), digits = 2)


#wykres kołowy
pie <- namesOccurence %>% filter(procent>0) %>%
  ggplot(aes(x = "", y=procent ,fill = factor(brand))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="marki", 
       x=NULL, 
       y=NULL, 
       title="Wykres kołowy dla najczęściej występowanych marek")

pie + coord_polar(theta = "y", start=0) +  geom_text(aes(x = 1.3, label = procent), position = position_stack(vjust = 0.5), size=2) 



#machine learning
set.seed(100)
#wybieranie testu i trainingu
indexes <- sample(nrow(Autko), (0.9*nrow(Autko)), replace = FALSE)
trainData <- Autko[indexes, ]
testData <- Autko[-indexes, ]


library(ggplot2)
library(ggdendro)
theme_set(theme_bw())


#dist() is used to compute distance between sample
#hclust() performs the hierarchical clustering
# plot() function can plot the output directly as a tree

#dendrogram

Autko2<- testData[,-c(2,7,8,9,10)]
mean_data <- apply(Autko2,2,mean)
std<- apply(Autko2, 2,sd)
#można to robić manulanie  (x - mean(x)) / sd(x) ale lepsze jest scaling
Autko2<-scale(Autko2, mean_data, std)
Autko2

distance <- dist(Autko2)
distance
hc<-hclust(distance)
plot(hc, labels = testData$brand)



