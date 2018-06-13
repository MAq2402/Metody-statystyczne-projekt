dane=unname(unlist(read.table("stara_hala.txt")))  
srednia = mean(dane)
odchylenie_standardowe = sd(dane)
kwantyl_rozkladu_normalnego = qnorm(0.975) #dla alfa=0.05
n = length(dane)
poczatek_przedzialu = srednia - kwantyl_rozkladu_normalnego*odchylenie_standardowe/sqrt(n) 
koniec_przedzialu = srednia + kwantyl_rozkladu_normalnego*odchylenie_standardowe/sqrt(n)
poczatek_przedzialu
koniec_przedzialu
d=srednia - poczatek_przedzialu
wzgledna_precyzja_oszaczowania = (d/srednia) * 100
wzgledna_precyzja_oszaczowania
