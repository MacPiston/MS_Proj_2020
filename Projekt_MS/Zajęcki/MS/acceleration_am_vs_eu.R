#CZY AUTA AMERYKAŃSKIE (origin == 1) PRZYSPIESZAJĄ LEPIEJ OD EUROPEJSKICH (origin == 2)?
#Przyspieszenia dla obu typów
acceleration.american <- Autko$acceleration[Autko$origin == 1]
acceleration.european <- Autko$acceleration[Autko$origin == 2]

#Przeprowadzenie testu
acceleration_am_vs_eu.test <- t.test(acceleration.american, acceleration.european, paired = F)
acceleration_am_vs_eu_pvalue <- acceleration_am_vs_eu.test$p.value
acceleration_am_vs_eu_przedzial95 <- round(acceleration_am_vs_eu.test$conf.int, 2)

#WNIOSEK: Na 95% auta amerykańskie nie przyspieszają lepiej od europejskich; różnica jest jest w przedziale [0.95 2.55]