data=unname(unlist(read.table("stara_hala.txt")))
n=length(data)
n
#obliczanie ?redniej
mean(data)
#obliczanie mody
#obliczanie mediany
median(data)
#obliczanie kwantylu Q1
quantile(data,0.25)
#obliczanie kwantylu Q3
quantile(data,0.75)
#obliczanie rozst?pu
r=max(data)-min(data)
r
#obliczanie wriancji
var(data)
#obliczanie odch. stand.
s = sd(data)
#obliczanie odch. przec. od ?redniej
d1=sum(abs(data-mediana))/n
d1
#obliczanie odch. przec. od mediany
mediana=median(data)
d2=sum(abs(data-mediana))/n
d2
mad(data)
#obliczanie odch. ?wiartkowego
Q=(quantile(data,0.75)-quantile(data,0.25))/2
Q
#obliczanie wsp??. zmienno?ci
V=s/mean(data)*100
V
#obliczanie wska?. asymetrii
#obliczanie wsp??. sko?no?ci
srednia=mean(data)
M3=sum((data-srednia)^3)/n
A=M3/(s^3)
A
#obliczanie kurtozy
M4=sum((data-srednia)^4)/n
K=M4/(s^4)
K
#obliczanie ekces
eksces=K-3
eksces