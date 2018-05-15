source("funkcje.R")
dane_stara_hala=unname(unlist(read.table("stara_hala.txt")))
dane_nowa_hala=unname(unlist(read.table("nowa_hala.txt")))
wartosc_krytyczna_stara=0.886/sqrt(length(dane_stara_hala))
wartosc_krytyczna_nowa = 0.886/sqrt(length(dane_nowa_hala))

d_stara=test_kolmogomorowa_lillieforsa(dane_stara_hala)
d_stara
wartosc_krytyczna_stara


d_nowa = test_kolmogomorowa_lillieforsa(dane_nowa_hala)
d_nowa
wartosc_krytyczna_nowa