#CZY AUTA AMERYKAŃSKIE (origin == 1) PRZYSPIESZAJĄ LEPIEJ OD JAPOŃSKICH (origin == 3)?
#Przyspieszenia dla obu typów
acceleration.american <- Autko$acceleration[Autko$origin == 1]
acceleration.japan <- Autko$acceleration[Autko$origin == 3]

#Przeprowadzenie testu
acceleration_am_vs_jap.test <- t.test(acceleration.american, acceleration.japan, paired = F)
acceleration_am_vs_jap_pvalue <- acceleration_am_vs_jap.test$p.value
acceleration_am_vs_jap_przedzial95 <- round(acceleration_am_vs_jap.test$conf.int, 2)

#WNIOSEK: Na 95% auta amerykańskie przyspieszają wolniej niż japońskie, różnica jest w przedziale [0.58, 1.69]