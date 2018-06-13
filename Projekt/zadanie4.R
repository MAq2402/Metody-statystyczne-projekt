dane=unname(unlist(read.table("nowa_hala.txt")))  
srednia = mean(dane)
odchylenie_standardowe = sd(dane)
kwantyl_rozkladu_normalnego = qnorm(0.975) #dla alpha = 0,05
n = length(dane)
poczatek_przedzialu = odchylenie_standardowe/(1+ kwantyl_rozkladu_normalnego/sqrt(2*n)) 
koniec_przedzialu = odchylenie_standardowe/(1 - kwantyl_rozkladu_normalnego/sqrt(2*n)) 
poczatek_przedzialu
koniec_przedzialu
d = 0.5*(koniec_przedzialu - poczatek_przedzialu)
wzgledna_precyzja_oszaczowania = (d/odchylenie_standardowe) * 100
wzgledna_precyzja_oszaczowania