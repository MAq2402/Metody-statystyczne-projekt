#FUNKCJE ZADANIE 1
moda_szereg_rozdzielczy <-function(h){
  max_left=max(h$counts)
  i=which(h$counts==max_left)
  dl=h$breaks[i+1]-h$breaks[i]
  ds_mode=h$breaks[i]+((h$counts[i]-h$counts[i-1])*dl)/((h$counts[i]-h$counts[i-1])+(h$counts[i]-h$counts[i+1]))
  return(ds_mode)
}
mediana_szereg_rozdzielczy <-function(h, data){
  i=max(which(h$breaks<median(data)))
  dl=h$breaks[i+1]-h$breaks[i]
  ds_median=h$breaks[i]+((dl/h$counts[i])*((n/2)-sum(h$counts[1:i-1])))
  return(ds_median)
}
kwantyl_Q1_szereg_rozdzielczy <-function(h, data){
  i=max(which(h$breaks<quantile(data,0.25)))
  dl=h$breaks[i+1]-h$breaks[i]
  Q1=h$breaks[i]+((dl/h$counts[i])*((n/4)-sum(h$counts[1:i-1])))
  return(Q1)
}
kwantyl_Q3_szereg_rozdzielczy <-function(h, data){
  i=max(which(h$breaks<quantile(data,0.75)))
  dl=h$breaks[i+1]-h$breaks[i]
  Q3=h$breaks[i]+((dl/h$counts[i])*((3*n/4)-sum(h$counts[1:i-1])))
  return(Q3)
}
#FUNKCJE ZADANIE 2

test_kolmogomorowa_lillieforsa <-function(dane){
  dane = sort(dane)
  n=length(dane)
  srednia=mean(dane)
  odchylenie_standardowe=sd(dane)
  standaryzowane_dane=(dane-srednia)/odchylenie_standardowe
  standaryzowane_dane=sort(standaryzowane_dane)
  standaryzowane_dane
  dystrybuanta_standaryzowanych_danych=pnorm(standaryzowane_dane,mean(standaryzowane_dane), sd(standaryzowane_dane))
  dystrybuanta_standaryzowanych_danych
  dystrybuanta = pnorm(dane,srednia,odchylenie_standardowe)
  d_plus=abs((seq(1,n,by=1)/n)-dystrybuanta)
  d_minus=abs(dystrybuanta-(seq(0,n-1,by=1)/n))
  d=max(max(d_plus),max(d_minus))
<<<<<<< HEAD
  d
  wartosc_krytyczna_d=0.886/sqrt(n)
  wartosc_krytyczna_d
  if(d<wartosc_krytyczna_d)
    return(TRUE)
  else
    return(FALSE) 
}

szczegolowy_miary <-function(file_name)
{
  data=unname(unlist(read.table(paste(file_name,".txt", sep=""))))
  sink(file = paste(file_name, "_szczegolowy_wyniki.txt", sep=""), append = FALSE, type = c("output"), split = FALSE)
  n=length(data)
  cat(n,"\n")
  #obliczanie ?redniej
  cat(mean(data),"\n")
  #obliczanie mody
  #obliczanie mediany
  cat(median(data),"\n")
  #obliczanie kwantylu Q1
  cat(quantile(data,0.25),"\n")
  #obliczanie kwantylu Q3
  cat(quantile(data,0.75),"\n")
  #obliczanie rozst?pu
  r=max(data)-min(data)
  cat(r,"\n")
  #obliczanie wriancji nieobci¹¿onej
  cat(var(data),"\n")
  #obliczanie wriancji obci¹¿onej
  cat((var(data)*(n-1)/n),"\n")
  #obliczanie odch. stand.nieobci¹¿one
  s = sd(data)
  cat(s,"\n")
  #obliczanie odch. stand. obci¹¿one
  s = sd(data)*(n-1)/n
  cat(s,"\n")
  #obliczanie odch. przec. od ?redniej
  d1=sum(abs(data-mediana))/n
  cat(d1,"\n")
  #obliczanie odch. przec. od mediany
  mediana=median(data)
  d2=sum(abs(data-mediana))/n
  cat(d2,"\n")
  #obliczanie odch. ?wiartkowego
  Q=(quantile(data,0.75)-quantile(data,0.25))/2
  cat(Q,"\n")
  #obliczanie wsp??. zmienno?ci
  V=s/mean(data)*100
  cat(V,"\n")
  #obliczanie wska?. asymetrii
  #obliczanie wsp??. sko?no?ci
  srednia=mean(data)
  M3=sum((data-srednia)^3)/n
  A=M3/(s^3)
  cat(A,"\n")
  #obliczanie kurtozy
  M4=sum((data-srednia)^4)/n
  K=M4/(s^4)
  cat(K,"\n")
  #obliczanie ekces
  eksces=K-3
  cat(eksces,"\n")
}

rozdzielczy_miary <-function(file_name, przedzialy)
{
  data=unname(unlist(read.table(paste(file_name,".txt", sep=""))))
  sink(file = paste(file_name, "_rozdzielczy_wyniki.txt", sep=""), append = FALSE, type = c("output"), split = FALSE)
  h=hist(data, breaks=przedzialy)
  hist(data, breaks=przedzialy, col="tan1",xlab="Wydajno??",ylab="",
       main="stara hala")
  n=length(data)
  h
  #obliczanie ?redniej
  srednia=sum(h$mids*h$counts)/n
  cat(srednia,"\n")
  #obliczanie mody
  moda=moda_szereg_rozdzielczy(h)
  cat(moda,"\n")
  #obliczanie mediany
  mediana=mediana_szereg_rozdzielczy(h, data)
  cat(mediana,"\n")
  #obliczanie kwantylu Q1
  Q1=kwantyl_Q1_szereg_rozdzielczy(h, data)
  cat(Q1,"\n")
  #obliczanie kwantylu Q3
  Q3=kwantyl_Q3_szereg_rozdzielczy(h, data)
  cat(Q3,"\n")
  #obliczanie wriancji obci¹¿onej
  s2=sum(((h$mids-srednia)^2)*h$counts)/n
  cat(s2,"\n")
  #obliczanie wriancji nieobci¹¿onej
  s2_gwiazdka=sum(((h$mids-srednia)^2)*h$counts)/(n-1)
  cat(s2_gwiazdka,"\n")
  #obliczanie odch. stand. obci¹¿one
  s=sqrt(s2)
  cat(s,"\n")
  #obliczanie odch. stand. nieobci¹¿one
  s_gwiazdka=sqrt(s2_gwiazdka)
  cat(s_gwiazdka,"\n")
  #obliczanie odch. przec. od ?redniej
  d1=sum((abs(h$mids-srednia))*h$counts)/n
  cat(d1,"\n")
  #obliczanie odch. przec. od mediany
  d2=sum((abs(h$mids-srednia))*h$counts)/n
  cat(d2,"\n")
  #obliczanie odch. ?wiartkowego
  Q=(Q3-Q1)/2
  cat(Q,"\n")
  #obliczanie wsp??. zmienno?ci
  V=s/srednia*100
  cat(V,"\n")
  #obliczanie wska?. asymetrii
  Ws=srednia-moda
  cat(Ws,"\n")
  #obliczanie wsp??. sko?no?ci
  M3=sum(((h$mids-srednia)^3)*h$counts)/n
  A=M3/(s^3)
  cat(A,"\n")
  #obliczanie kurtozy
  M4=sum(((h$mids-srednia)^4)*h$counts)/n
  K=M4/(s^4)
  cat(K,"\n")
  #obliczanie ekces
  eksces=K-3
  cat(eksces,"\n")
}
  
=======
  return(d)
}
>>>>>>> 7bd3b45c569ef80835a9850633143f6fb2c90d16
