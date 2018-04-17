test_kolmogomorowa_lillieforsa <-function(dane, wspolczynnik_ufnosci){
  n=length(dane)
  dane_rozkladu_normalnego=seq(1,n,by=1)
  rozklad_normalny=pnorm(dane_rozkladu_normalnego,mean(dane_rozkladu_normalnego),sd(dane_rozkladu_normalnego))
  
}