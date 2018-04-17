data=unname(unlist(read.table("C:/Users/Michal/Desktop/Rprojekt/nowa_hala.txt")))
h=hist(data)
n=length(data)
h
#obliczanie ?redniej
ds_mean=sum(h$mids*h$counts)/n
ds_mean
#obliczanie mody
max_left=max(h$counts)
i=which(h$counts==max_left)
dl=h$breaks[i+1]-h$breaks[i]
ds_mode=h$breaks[i]+((h$counts[i]-h$counts[i-1])*dl)/((h$counts[i]-h$counts[i-1])+(h$counts[i]-h$counts[i+1]))
ds_mode
#obliczanie mediany
i=max(which(h$breaks<median(data)))
dl=h$breaks[i+1]-h$breaks[i]
ds_median=h$breaks[i]+((dl/h$counts[i])*((n/2)-sum(h$counts[1:i-1])))
ds_median
#obliczanie kwantylu Q1
i=max(which(h$breaks<quantile(data,0.25)))
dl=h$breaks[i+1]-h$breaks[i]
Q1=h$breaks[i]+((dl/h$counts[i])*((n/4)-sum(h$counts[1:i-1])))
Q1
#obliczanie kwantylu Q3
i=max(which(h$breaks<quantile(data,0.75)))
dl=h$breaks[i+1]-h$breaks[i]
Q3=h$breaks[i]+((dl/h$counts[i])*((3*n/4)-sum(h$counts[1:i-1])))
Q3
#obliczanie wriancji
s2=sum(((h$mids-ds_mean)^2)*h$counts)/n
s2
#obliczanie odch. stand.
s=sqrt(s2)
s
#obliczanie odch. przec. od ?redniej
d1=sum((abs(h$mids-ds_mean))*h$counts)/n
d1
#obliczanie odch. przec. od mediany
d2=sum((abs(h$mids-ds_median))*h$counts)/n
d2
#obliczanie odch. ?wiartkowego
Q=(Q3-Q1)/2
Q
#obliczanie wsp??. zmienno?ci
V=s/ds_mean*100
V
#obliczanie wska?. asymetrii
Ws=ds_mean-ds_mode
Ws
#obliczanie wsp??. sko?no?ci
M3=sum(((h$mids-ds_mean)^3)*h$counts)/n
A=M3/(s^3)
A
#obliczanie kurtozy
M4=sum(((h$mids-ds_mean)^4)*h$counts)/n
K=M4/(s^4)
K
#obliczanie ekces
eksces=K-3
eksces
