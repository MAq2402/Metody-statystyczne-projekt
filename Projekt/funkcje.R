#FUNKCJE DLA SZEREGOW ROZDZIELCZYCH
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


test_kolmogomorowa_lillieforsa <-function(dane){
  n=length(dane)
  srednia=mean(dane)
  odchylenie_standardowe=sd(dane)
  standaryzowane_dane=(dane-srednia)/odchylenie_standardowe
  standaryzowane_dane=sort(standaryzowane_dane)
  standaryzowane_dane
  dystrybuanta_standaryzowanych_danych=pnorm(standaryzowane_dane,mean(standaryzowane_dane), sd(standaryzowane_dane))
  dystrybuanta_standaryzowanych_danych
  d_plus=abs((seq(1,n,by=1)/n)-dystrybuanta_standaryzowanych_danych)
  d_minus=abs((seq(0,n-1,by=1)/n)-dystrybuanta_standaryzowanych_danych)
  d=max(max(d_plus),max(d_minus))
  d
  wartosc_krytyczna_d=0.886/sqrt(n)
  wartosc_krytyczna_d
  if(d<wartosc_krytyczna_d)
    return(TRUE)
  else
    return(FALSE) 
}