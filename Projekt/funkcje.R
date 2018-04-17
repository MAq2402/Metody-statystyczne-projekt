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