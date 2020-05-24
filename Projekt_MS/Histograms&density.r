# warto�ci minimalne i maksymalne
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

# zakresy warto�ci
zakresmpg <- maxmpg - minmpg
zakresdis <- maxdis - mindis
zakrespow <- maxpow - minpow
zakreskg <- maxkg - minkg
zakresacc <- maxacc - minacc

# ilo�� danych
puste <- colSums(is.na(Autko))
wszystko <- as.numeric(nrow(Autko))
ilosc <- as.numeric(wszystko - puste)

pierwiastek <- sqrt(ilosc)
pierwiastek <- ceiling(pierwiastek)

# szeroko��
szermpg <- zakresmpg / pierwiastek
szerdis <- zakresdis / pierwiastek
szerpow <- zakrespow / pierwiastek
szerkg <- zakreskg/ pierwiastek
szeracc <- zakresacc / pierwiastek

# punkty
pktmpg = seq(minmpg, maxmpg, by = szermpg)
pktdis = seq(mindis, maxdis, by = szerdis)
pktpow = seq(minpow, maxpow, by = szerpow)
pktkg = seq(minkg, maxkg, by = szerkg)
pktacc = seq(minacc, maxacc, by = szeracc)

# przedzia�y
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

# histogramy
histogrammpg = hist(x = Autko$mpg, breaks = pktmpg, col = "peachpuff", border = "black", prob = TRUE, main = "Histogram mpg", xlab = "mpg")
histogramdis = hist(x = Autko$displacement, breaks = pktdis, col = "peachpuff", border = "black", prob = TRUE, main = "Histogram displacement", xlab = "displacement")
histogrampow = hist(x = Autko$horsepower, breaks = pktpow, col = "peachpuff", border = "black", prob = TRUE, main = "Histogram horsepower", xlab = "horsepower")
histogramkg = hist(x = Autko$weight, breaks = pktkg, col = "peachpuff", border = "black", prob = TRUE, main = "Histogram weight", xlab = "weight")
histogramacc = hist(x = Autko$acceleration, breaks = pktacc, col = "peachpuff", border = "black", prob = TRUE, main = "Histogram acceleration", xlab = "acceleration")

# wykresy g�sto�ci
densitympg = lines(density(Autko$mpg), lwd = 2, col = "red")
densitydis = lines(density(Autko$displacement), lwd = 2, col = "red")
densitypow = lines(density(Autko$horsepower), lwd = 2, col = "red")
densitykg = lines(density(Autko$weight), lwd = 2, col = "red")
densityacc = lines(density(Autko$acceleration), lwd = 2, col = "red")


