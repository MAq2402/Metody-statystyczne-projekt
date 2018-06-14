dane_stara_hala=unname(unlist(read.table("stara_hala.txt")))
dane_nowa_hala=unname(unlist(read.table("nowa_hala.txt")))
w1=var(dane_nowa_hala)
w2=var(dane_stara_hala)
n1=length(dane_nowa_hala)
n2=length((dane_stara_hala))
wartosc_testu_fishera=w1/w2
wartosc_testu_fishera
wartosc_krytyczna_rozkladu_fishera=qf(0.95, length(dane_nowa_hala)-1, length(dane_stara_hala)-1)
wartosc_krytyczna_rozkladu_fishera
z1=w1/(n1-1)
z2=w2/(n2-1)
c=(abs(mean(dane_stara_hala)-mean(dane_nowa_hala)))/sqrt(z2+z1)
t1=qt(0.95, length(dane_nowa_hala)-1)
t2=qt(0.95, length(dane_stara_hala)-1)
wartosc_krytyczna_c=(z1*t1+z2*t2)/(z1+z2)

c
wartosc_krytyczna_c

