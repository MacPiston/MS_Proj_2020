#CZY AUTA Z 8 CYLINDRAMI PALĄ WIĘCEJ (MPG JEST MNIEJSZE) NIŻ TE Z 6? Test dwóch średnich
# Spalania dla obu rodzajów samochodów
mpg.8 <- Autko$mpg[Autko$cylinders == 8]
mpg.6 <- Autko$mpg[Autko$cylinders == 6]

#Przeprowadzenie testu
mpg_8vs6.test <- t.test(mpg.8, mpg.6, paired = F)
mpg_8vs6_pvalue <- mpg_8vs6.test$p.value
mpg_8vs6_przedzial95 <- round(mpg_8vs6.test$conf.int, 2)

#WNIOSEK: Na 95% auta 8-mio cylindrowe palą więcej niż auta 6-cio cylindrowe; różnica jest w przedziale (4.03, 6.01)