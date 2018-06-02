dane=unname(unlist(read.table("nowa_hala.txt")))  
srednia = mean(dane)
srednia
odchylenie_standardowe = sd(dane)
odchylenie_standardowe
kwantyl_rozkladu_normalnego = 1.96 #dla alpha = 0,05 , z jakiejs tabelki
n = length(dane)
poczatek_przedzialu = odchylenie_standardowe/(1+ kwantyl_rozkladu_normalnego/sqrt(2*n)) 
koniec_przedzialu = odchylenie_standardowe/(1 - kwantyl_rozkladu_normalnego/sqrt(2*n)) 
poczatek_przedzialu
koniec_przedzialu
d = 0.5*(koniec_przedzialu - poczatek_przedzialu)
d
wzgledna_precyzja_oszaczowania = (d/odchylenie_standardowe) * 100
wzgledna_precyzja_oszaczowania
help(rug)