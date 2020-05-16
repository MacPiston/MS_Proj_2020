#średnia z próby (spalanie)
weight_mu <- mean(Autko$weight)
#odchylenie standardowe (spalanie)
weight_sigma <- sd(Autko$weight)

#NORMALNE PRZEDZIAŁY UFNOŚCI DLA WARTOŚCI OCZEKIWANEJ
#przedział ufności 90%
weight_przedz90norm <- round(weight_mu+c(-1, 1)*sigma/sqrt(398)*qnorm(.95), 2)
#przedział ufności 95%
weight_przedz95norm <- round(weight_mu+c(-1, 1)*sigma/sqrt(398)*qnorm(.975), 2)
#przedział ufności 99%
weight_przedz99norm <- round(weight_mu+c(-1, 1)*sigma/sqrt(398)*qnorm(.995), 2)
#interpertacja: przy zbiorze 398 aut średnia waga w tej populacji na x% jest w danym przedziale

#PRZEDZIAŁY UFNOŚCI DLA WARIANCJI
#przedział ufności 90%
weight_przedz90war <- round(sqrt(sigma*398/qchisq(c(1-.05,.05), 397)), 2)
#przedział ufności 95%
weight_przedz95war <- round(sqrt(sigma*398/qchisq(c(1-.025,.025), 397)), 2)
#przedział ufności 99%
weight_przedz99war <- round(sqrt(sigma*398/qchisq(c(1-.005,.005), 397)), 2)

#T PRZEDZIAŁY UFNOŚCI
#przedział ufności 90%
weight_przedz90t <- round(weight_mu+c(-1, 1)*sigma/sqrt(398)*qt(.95, 397), 2)
#przedział ufności 95%
weight_przedz95t <- round(weight_mu+c(-1, 1)*sigma/sqrt(398)*qt(.975, 397), 2)
#przedział ufności 99%
weight_przedz99t <- round(weight_mu+c(-1, 1)*sigma/sqrt(398)*qt(.995, 397), 2)