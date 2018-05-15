dane=unname(unlist(read.table("stara_hala.txt")))  
srednia = mean(dane)
wariancja = var(dane)
odchylenie_standardowe = sd(dane)
odchylenie_standardowe
sqrt(wariancja)
kwantyl_rozkladu_normalnego = 1.96 #dla alpha = 0,05 , z jakiejs tabelki
n = length(dane)
poczatek_przedzialu = srednia - kwantyl_rozkladu_normalnego*odchylenie_standardowe/sqrt(n) 
koniec_przedzialu = srednia + kwantyl_rozkladu_normalnego*odchylenie_standardowe/sqrt(n)
d=srednia- poczatek_przedzialu
d
d2 =koniec_przedzialu-srednia
d2
wzgledna_precyzja_oszaczowania = (d/srednia) * 100
wzgledna_precyzja_oszaczowania
help(rug)
