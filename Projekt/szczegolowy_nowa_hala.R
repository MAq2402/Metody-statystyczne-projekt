data=unname(unlist(read.table("nowa_hala.txt")))
sink(file = "nowa_hala_wyniki.txt", append = FALSE, type = c("output"), split = FALSE)
n=length(data)
print(n)
#obliczanie ?redniej
print(mean(data))
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
#obliczanie wriancji nieobci??onej
var(data)
#obliczanie wriancji obci??onej
var(data)*(n-1)/n
#obliczanie odch. stand.nieobci??one
s = sd(data)
s
#obliczanie odch. stand. obci??one
s = sd(data)*(n-1)/n
s
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