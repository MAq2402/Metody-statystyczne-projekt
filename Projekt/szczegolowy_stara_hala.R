dane=unname(unlist(read.table("stara_hala.txt")))
n=length(dane)
n
#obliczanie ?redniej
mean(dane)
#obliczanie mody
#obliczanie mediany
median(dane)
#obliczanie kwantylu Q1
quantile(dane,0.25)
#obliczanie kwantylu Q3
quantile(dane,0.75)
#obliczanie rozst?pu
r=max(dane)-min(dane)
r
#obliczanie wriancji
var(dane)
#obliczanie odch. stand.
s = sd(dane)
#obliczanie odch. przec. od ?redniej
d1=sum(abs(dane-mediana))/n
d1
#obliczanie odch. przec. od mediany
mediana=median(dane)
d2=sum(abs(dane-mediana))/n
d2
mad(dane)
#obliczanie odch. ?wiartkowego
Q=(quantile(dane,0.75)-quantile(dane,0.25))/2
Q
#obliczanie wsp??. zmienno?ci
V=s/mean(dane)*100
V
#obliczanie wska?. asymetrii
#obliczanie wsp??. sko?no?ci
srednia=mean(dane)
M3=sum((dane-srednia)^3)/n
A=M3/(s^3)
A
#obliczanie kurtozy
M4=sum((dane-srednia)^4)/n
K=M4/(s^4)
K
#obliczanie ekces
eksces=K-3
eksces

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
max(d_plus)
max(d_minus)

