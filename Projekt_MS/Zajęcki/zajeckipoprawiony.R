przedzialy <- function(x)
{
  srednia = mean(x, na.rm =  TRUE)
  sd <- sd(x, na.rm = TRUE)
  przedz90norm  <- round(srednia+c(-1, 1)*sd/sqrt(398)*qnorm(.95), 2)
  przedz95norm <- round(srednia+c(-1, 1)*sd/sqrt(398)*qnorm(.975), 2)
  przedz99norm <- round(srednia+c(-1, 1)*sd/sqrt(398)*qnorm(.995), 2)
  przedz90war <- round(sqrt(sd*398/qchisq(c(1-.05,.05), 397)), 2)
  przedz95war <- round(sqrt(sd*398/qchisq(c(1-.025,.025), 397)), 2)
  przedz99war <- round(sqrt(sd*398/qchisq(c(1-.005,.005), 397)), 2)
  przedz90t <- round(srednia+c(-1, 1)*sd/sqrt(398)*qt(.95, 397), 2)
  przedz95t <- round(srednia+c(-1, 1)*sd/sqrt(398)*qt(.975, 397), 2)
  przedz99t <- round(srednia+c(-1, 1)*sd/sqrt(398)*qt(.995, 397), 2)
  
  rezultat <- c(przedz90norm,przedz95norm, przedz99norm,  przedz90war,przedz95war, 
                przedz99war, przedz90t, przedz95t, przedz99t)
  
  return(rezultat)
}

accp <- przedzialy(Autko$acceleration)
mpgp <- przedzialy(Autko$mpg)
disp <- przedzialy(Autko$displacement)
horsep <- przedzialy(Autko$horsepower)
kgp <- przedzialy(Autko$weight)

tmp <- data.frame(mpgp, accp , disp, horsep, kgp)
row.names(tmp) <- c("początek przedziału dla ufności 90% dla wartości oczekiwanej"  , "koniec  przedziału dla ufności 90% dla wartości oczekiwanej", 
                    "początek przedziału dla ufności 95% dla wartości oczekiwanej"  , "koniec  przedziału dla ufności 95% dla wartości oczekiwanej",
                    "początek przedziału dla ufności 99% dla wartości oczekiwanej"  , "koniec  przedziału dla ufności 99% dla wartości oczekiwanej",
                    "początek przedziału dla ufności 90% dla wariancji"  , "koniec  przedziału dla ufności 90% dla wariancji",
                    "początek przedziału dla ufności 95% dla wariancji"  , "koniec  przedziału dla ufności 95% dla wariancji",
                    "początek przedziału dla ufności 99% dla wariancji"  , "koniec  przedziału dla ufności 99% dla wariancji",
                    "początek przedziału dla ufności 90% dla chyba sredniej sprawdz maciek"  , "koniec  przedziału dla ufności 90% dla chyba sredniej sprawdz maciek",
                    "początek przedziału dla ufności 95% dla chyba sredniej sprawdz maciek"  , "koniec  przedziału dla ufności 95% dla chyba sredniej sprawdz maciek",
                    "początek przedziału dla ufności 99% dla chyba sredniej sprawdz maciek"  , "koniec  przedziału dla ufności 99% dla chyba sredniej sprawdz maciek")

