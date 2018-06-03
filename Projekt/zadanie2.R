source("funkcje.R")
dane_stara_hala=unname(unlist(read.table("stara_hala.txt")))
dane_nowa_hala=unname(unlist(read.table("nowa_hala.txt")))
wartosc_krytyczna_stara_hala=0.886/sqrt(length(dane_stara_hala))
wartosc_krytyczna_nowa_hala = 0.886/sqrt(length(dane_nowa_hala))
d_stara_hala=test_kolmogomorowa_lillieforsa(dane_stara_hala)
d_nowa_hala = test_kolmogomorowa_lillieforsa(dane_nowa_hala)

d_stara_hala
wartosc_krytyczna_stara_hala

d_nowa_hala
wartosc_krytyczna_nowa_hala