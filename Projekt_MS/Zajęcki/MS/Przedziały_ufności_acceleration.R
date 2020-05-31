#średnia z próby (acceleration)
acceleration_mu <- mean(Autko$acceleration)
#odchylenie standardowe (acceleration)
acceleration_sigma <- sd(Autko$acceleration)

#NORMALNE PRZEDZIAŁY UFNOŚCI DLA WARTOŚCI OCZEKIWANEJ
#przedział ufności 90%
acceleration_przedz90norm <- round(acceleration_mu+c(-1, 1)*sigma/sqrt(398)*qnorm(.95), 2)
#przedział ufności 95%
acceleration_przedz95norm <- round(acceleration_mu+c(-1, 1)*sigma/sqrt(398)*qnorm(.975), 2)
#przedział ufności 99%
acceleration_przedz99norm <- round(acceleration_mu+c(-1, 1)*sigma/sqrt(398)*qnorm(.995), 2)
#interpertacja: przy zbiorze 398 aut średnie przyspieszenie w tej populacji na x% jest w danym przedziale

#PRZEDZIAŁY UFNOŚCI DLA WARIANCJI
#przedział ufności 90%
acceleration_przedz90war <- round(sqrt(sigma*398/qchisq(c(1-.05,.05), 397)), 2)
#przedział ufności 95%
acceleration_przedz95war <- round(sqrt(sigma*398/qchisq(c(1-.025,.025), 397)), 2)
#przedział ufności 99%
acceleration_przedz99war <- round(sqrt(sigma*398/qchisq(c(1-.005,.005), 397)), 2)

#T PRZEDZIAŁY UFNOŚCI
#przedział ufności 90%
acceleration_przedz90t <- round(acceleration_mu+c(-1, 1)*sigma/sqrt(398)*qt(.95, 397), 2)
#przedział ufności 95%
acceleration_przedz95t <- round(acceleration_mu+c(-1, 1)*sigma/sqrt(398)*qt(.975, 397), 2)
#przedział ufności 99%
acceleration_przedz99t <- round(acceleration_mu+c(-1, 1)*sigma/sqrt(398)*qt(.995, 397), 2)
