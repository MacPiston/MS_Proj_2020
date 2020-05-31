#CZY AUTA O POJEMNOŚCI WIĘKSZEJ NIŻ 250 MAJĄ WIĘKSZA MOC? Test dwóch średnich
# Moce dla obu rodzajów
horsepower.over250 <- Autko$horsepower[Autko$displacement > 250]
horsepower.under250 <- Autko$horsepower[Autko$displacement <= 250]

#Przeprowadzenie testu
horsepower_displacement.test <- t.test(horsepower.over250, horsepower.under250, paired = F)

#p-value
horsepower_displacement_pvalue <- horsepower_displacement.test$p.value
horsepower_displacement_przedzial95 <- round(horsepower_displacement.test$conf.int, 2)

#WNIOSEK: Auta o pojemności większej niż 250 na 95% mają większą moc, różnica jest w przedziale (64, 76.2)