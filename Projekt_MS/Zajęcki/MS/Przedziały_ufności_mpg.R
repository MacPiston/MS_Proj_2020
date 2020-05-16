#średnia z próby (spalanie)
mpg_mu <- mean(Autko$mpg)
#odchylenie standardowe (spalanie)
mpg_sigma <- sd(Autko$mpg)

#NORMALNE PRZEDZIAŁY UFNOŚCI DLA WARTOŚCI OCZEKIWANEJ
#przedział ufności 90%
mpg_przedz90norm <- round(mpg_mu+c(-1, 1)*sigma/sqrt(398)*qnorm(.95), 2)
#przedział ufności 95%
mpg_przedz95norm <- round(mpg_mu+c(-1, 1)*sigma/sqrt(398)*qnorm(.975), 2)
#przedział ufności 99%
mpg_przedz99norm <- round(mpg_mu+c(-1, 1)*sigma/sqrt(398)*qnorm(.995), 2)
#interpertacja: przy zbiorze 398 aut średnie spalanie w tej populacji na x% jest w danym przedziale

#PRZEDZIAŁY UFNOŚCI DLA WARIANCJI
#przedział ufności 90%
mpg_przedz90war <- round(sqrt(sigma*398/qchisq(c(1-.05,.05), 397)), 2)
#przedział ufności 95%
mpg_przedz95war <- round(sqrt(sigma*398/qchisq(c(1-.025,.025), 397)), 2)
#przedział ufności 99%
mpg_przedz99war <- round(sqrt(sigma*398/qchisq(c(1-.005,.005), 397)), 2)

#T PRZEDZIAŁY UFNOŚCI
#przedział ufności 90%
mpg_przedz90t <- round(mpg_mu+c(-1, 1)*sigma/sqrt(398)*qt(.95, 397), 2)
#przedział ufności 95%
mpg_przedz95t <- round(mpg_mu+c(-1, 1)*sigma/sqrt(398)*qt(.975, 397), 2)
#przedział ufności 99%
mpg_przedz99t <- round(mpg_mu+c(-1, 1)*sigma/sqrt(398)*qt(.995, 397), 2)
