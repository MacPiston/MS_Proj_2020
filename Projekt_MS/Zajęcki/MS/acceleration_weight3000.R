# CZY AUTA Z WAGĄ WIĘKSZĄ NIŻ 3000 PRZYSPIESZAJĄ DŁUŻEJ?
# Przyspieszenia dla obu przedziałów wagi
acceleration.over3000 <- Autko$acceleration[Autko$weight >= 3000]
acceleration.under3000 <- Autko$acceleration[Autko$weight < 3000]

# Przeprowadzenie testu
acceleration_weight.test <- t.test(acceleration.over3000, acceleration.under3000, paired = F)
acceleration_weight_pvalue <- acceleration_weight.test$p.value
acceleration_weight_przedzial95 <- round(acceleration_weight.test$conf.int, 2)

# WNIOSEK: Na 95% samochody ważące ponad 3000 mają gorsze przyspieszenie, różnica jest w przedziale [0.95 2.06]