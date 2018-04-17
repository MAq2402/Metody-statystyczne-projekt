source('funkcje.R', local=TRUE)
data=unname(unlist(read.table("nowa_hala.txt")))
przedzialy=c(14,20,26,32,38,44,50,56)
h=hist(data, breaks=przedzialy)
hist(data, breaks = przedzialy, col="tan1",xlab="Wydajno??",ylab="",
     main="nowa hala")
n=length(data)
h
#obliczanie ?redniej
srednia=sum(h$mids*h$counts)/n
srednia
#obliczanie mody
moda=moda_szereg_rozdzielczy(h)
moda
#obliczanie mediany
mediana=mediana_szereg_rozdzielczy(h, data)
mediana
#obliczanie kwantylu Q1
Q1=kwantyl_Q1_szereg_rozdzielczy(h, data)
Q1
#obliczanie kwantylu Q3
Q3=kwantyl_Q3_szereg_rozdzielczy(h, data)
Q3
#obliczanie wriancji
s2=sum(((h$mids-srednia)^2)*h$counts)/n
s2
#obliczanie wriancji z gwiazdk?
s2_gwiazdka=sum(((h$mids-srednia)^2)*h$counts)/(n-1)
s2_gwiazdka
#obliczanie odch. stand.
s=sqrt(s2)
s
#obliczanie odch. stand. z gwiazdk?
s_gwiazdka=sqrt(s2_gwiazdka)
s_gwiazdka
#obliczanie odch. przec. od ?redniej
d1=sum((abs(h$mids-srednia))*h$counts)/n
d1
#obliczanie odch. przec. od mediany
d2=sum((abs(h$mids-srednia))*h$counts)/n
d2
#obliczanie odch. ?wiartkowego
Q=(Q3-Q1)/2
Q
#obliczanie wsp??. zmienno?ci
V=s/srednia*100
V
#obliczanie wska?. asymetrii
Ws=srednia-moda
Ws
#obliczanie wsp??. sko?no?ci
M3=sum(((h$mids-srednia)^3)*h$counts)/n
A=M3/(s^3)
A
#obliczanie kurtozy
M4=sum(((h$mids-srednia)^4)*h$counts)/n
K=M4/(s^4)
K
#obliczanie ekces
eksces=K-3
eksces

