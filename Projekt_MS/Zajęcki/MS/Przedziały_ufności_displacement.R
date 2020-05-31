#średnia z próby (pojemność)
displacement_mu <- mean(Autko$displacement)
#odchylenie standardowe (pojemność)
displacement_sigma <- sd(Autko$displacement)

#NORMALNE PRZEDZIAŁY UFNOŚCI DLA WARTOŚCI OCZEKIWANEJ
#przedział ufności 90%
displacement_przedz90norm <- round(displacement_mu+c(-1, 1)*sigma/sqrt(398)*qnorm(.95), 2)
#przedział ufności 95%
displacement_przedz95norm <- round(displacement_mu+c(-1, 1)*sigma/sqrt(398)*qnorm(.975), 2)
#przedział ufności 99%
displacement_przedz99norm <- round(displacement_mu+c(-1, 1)*sigma/sqrt(398)*qnorm(.995), 2)
#interpertacja: przy zbiorze 398 aut średnia pojemność w tej populacji na x% jest w danym przedziale

#PRZEDZIAŁY UFNOŚCI DLA WARIANCJI
#przedział ufności 90%
displacement_przedz90war <- round(sqrt(sigma*398/qchisq(c(1-.05,.05), 397)), 2)
#przedział ufności 95%
displacement_przedz95war <- round(sqrt(sigma*398/qchisq(c(1-.025,.025), 397)), 2)
#przedział ufności 99%
displacement_przedz99war <- round(sqrt(sigma*398/qchisq(c(1-.005,.005), 397)), 2)

#T PRZEDZIAŁY UFNOŚCI
#przedział ufności 90%
displacement_przedz90t <- round(displacement_mu+c(-1, 1)*sigma/sqrt(398)*qt(.95, 397), 2)
#przedział ufności 95%
displacement_przedz95t <- round(displacement_mu+c(-1, 1)*sigma/sqrt(398)*qt(.975, 397), 2)
#przedział ufności 99%
displacement_przedz99t <- round(displacement_mu+c(-1, 1)*sigma/sqrt(398)*qt(.995, 397), 2)