#CZY AUTA JAPOŃSKIE (origin == 3) PRZYSPIESZAJĄ LEPIEJ OD EUROPEJSKICH (origin == 2)?
#Przyspieszenia dla obu typów
acceleration.japan <- Autko$acceleration[Autko$origin == 3]
acceleration.european <- Autko$acceleration[Autko$origin == 2]

#Przeprowadzenie testu
acceleration_jap_vs_eu.test <- t.test(acceleration.japan, acceleration.european, paired = F)
acceleration_jap_vs_eu_pvalue <- acceleration_jap_vs_eu.test$p.value
acceleration_jap_vs_eu_przedzial95 <- round(acceleration_jap_vs_eu.test$conf.int, 2)

#WNIOSEK: Nie można jednoznacznie stwierdzić które auta przyspieszają szybciej, ponieważ otrzymany przedział różnic to [-1.46 0.23]