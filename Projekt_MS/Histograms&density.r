# wartoœci minimalne i maksymalne
minmpg <- min(Autko$mpg)
maxmpg <- max(Autko$mpg)
mindis <- min(Autko$displacement)
maxdis <- max(Autko$displacement)
minpow <- min(Autko$horsepower)
maxpow <- max(Autko$horsepower)
minkg <- min(Autko$weight)
maxkg <- max(Autko$weight)
minacc <- min(Autko$acceleration)
maxacc <- max(Autko$acceleration)

# zakresy wartoœci
zakresmpg <- maxmpg - minmpg
zakresdis <- maxdis - mindis
zakrespow <- maxpow - minpow
zakreskg <- maxkg - minkg
zakresacc <- maxacc - minacc

# iloœæ danych
ilosc <- as.numeric(nrow(Autko))

pierwiastek <- sqrt(ilosc)
pierwiastek <- ceiling(pierwiastek)

# szerokoœæ
szermpg <- zakresmpg / pierwiastek
szerdis <- zakresdis / pierwiastek
szerpow <- zakrespow / pierwiastek
szerkg <- zakreskg/ pierwiastek
szeracc <- zakresacc / pierwiastek

szermpg<-szermpg[1]
szerdis <- szerdis[1]
szerpow <- szerpow[1]
szerkg <- szerkg[1]
szeracc <- szeracc[1]

# punkty
pktmpg = seq(minmpg, maxmpg, by = szermpg)
pktdis = seq(mindis, maxdis, by = szerdis)
pktpow = seq(minpow, maxpow, by = szerpow)
pktkg = seq(minkg, maxkg, by = szerkg)
pktacc = seq(minacc, maxacc, by = szeracc)

# przedzia³y
przedzialmpg <- cut(Autko$mpg, pktmpg, right = FALSE, include.lowest = TRUE)
przedzialdis <- cut(Autko$dis, pktdis, right = FALSE, include.lowest = TRUE)
przedzialpow <- cut(Autko$mpg, pktpow, right = FALSE, include.lowest = TRUE)
przedzialkg <- cut(Autko$mpg, pktkg, right = FALSE, include.lowest = TRUE)
przedzialacc <- cut(Autko$mpg, pktacc, right = FALSE, include.lowest = TRUE)

# szeregi rozdzielcze
szeregmpg <- table(przedzialmpg)
szeregdis <- table(przedzialdis)
szeregpow <- table(przedzialpow)
szeregkg <- table(przedzialkg)
szeregacc <- table(przedzialacc)

# histogramy + wykresy gęstości
ggplot(Autko, aes(x=mpg)) + geom_histogram(breaks = pktmpg, aes(y=..density..),  colour="black", fill="white") + geom_density(alpha=.2, fill="blue") + labs(title = "Histogram mpg")
ggplot(Autko, aes(x=displacement)) + geom_histogram(breaks = pktdis, aes(y=..density..),  colour="black", fill="white") + geom_density(alpha=.2, fill="blue") + labs(title = "Histogram displacement")
ggplot(Autko, aes(x=horsepower)) + geom_histogram(breaks = pktpow, aes(y=..density..),  colour="black", fill="white") + geom_density(alpha=.2, fill="blue") + labs(title = "Histogram horsepowe")
ggplot(Autko, aes(x=weight)) + geom_histogram(breaks = pktkg, aes(y=..density..),  colour="black", fill="white") + geom_density(alpha=.2, fill="blue") + labs(title = "Histogram weight")
ggplot(Autko, aes(x=acceleration)) + geom_histogram(breaks = pktacc, aes(y=..density..),  colour="black", fill="white") + geom_density(alpha=.2, fill="blue") + labs(title = "Histogram acceleration")

