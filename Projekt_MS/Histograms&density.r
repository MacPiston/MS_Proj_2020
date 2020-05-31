wynik <- function(x)
{
  min <- min(x, na.rm = TRUE)
  max <- max(x, na.rm = TRUE)
  zakres <- as.numeric(max - min)
  ilosc <- as.numeric(nrow(Autko))
  pierwiastek <- sqrt(ilosc)
  pierwiastek <- ceiling(pierwiastek)
  szer <- zakres / pierwiastek
  szer <-szer[1]
  pkt = seq(min, max, by = szer)
  rezultat <- c(min, max, szer, pkt)
  return(rezultat)
}
#dane zapisane w wektorze
mpg<-wynik(Autko$mpg)
dis<-wynik(Autko$displacement)
kg<-wynik(Autko$weight)
pow<-wynik(Autko$horsepower)
acc<-wynik(Autko$acceleration)
#przedziały
przedzialmpg <- cut(Autko$mpg, mpg[-c(1,2,3)], right = FALSE, include.lowest = TRUE)
przedzialdis <- cut(Autko$displacement, dis[-c(1,2,3)], right = FALSE, include.lowest = TRUE)
przedzialpow <- cut(Autko$horsepower, pow[-c(1,2,3)], right = FALSE, include.lowest = TRUE)
przedzialkg <- cut(Autko$weight, kg[-c(1,2,3)], right = FALSE, include.lowest = TRUE)
przedzialacc <- cut(Autko$acceleration, acc[-c(1,2,3)], right = FALSE, include.lowest = TRUE)
# szeregi rozdzielcze
szeregmpg <- table(przedzialmpg)
szeregdis <- table(przedzialdis)
szeregpow <- table(przedzialpow)
szeregkg <- table(przedzialkg)
szeregacc <- table(przedzialacc)
#histogramy
ggplot(Autko, aes(x=mpg)) + geom_histogram(breaks =  mpg[-c(1,2,3)] , aes(y=..density..),  colour="black", fill="white") + geom_density(alpha=.2, fill="blue") + labs(title = "Histogram mpg")
ggplot(Autko, aes(x=displacement)) + geom_histogram(breaks = dis[-c(1,2,3)], aes(y=..density..),  colour="black", fill="white") + geom_density(alpha=.2, fill="blue") + labs(title = "Histogram displacement")
ggplot(Autko, aes(x=horsepower)) +  geom_histogram(breaks = pow[-c(1,2,3)], aes(y=..density..),  colour="black", fill="white") + geom_density(alpha=.2, fill="blue") + labs(title = "Histogram horsepower")
ggplot(Autko, aes(x=weight)) + geom_histogram(breaks = kg[-c(1,2,3)], aes(y=..density..),  colour="black", fill="white") + geom_density(alpha=.2, fill="blue") + labs(title = "Histogram weight")
ggplot(Autko, aes(x=acceleration)) + geom_histogram(breaks = acc[-c(1,2,3)], aes(y=..density..),  colour="black", fill="white") + geom_density(alpha=.2, fill="blue") + labs(title = "Histogram acceleration")


