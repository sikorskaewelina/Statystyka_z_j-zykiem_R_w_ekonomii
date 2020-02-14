library(ggplot2)
library(tidyverse)
library(zoo)

tab<-read.csv(file="ceny.csv",sep=";",dec=",",encoding = "UTF-8")

View(tab)
names(tab)
names(tab)[1:8]

names(tab)[3:16]<-paste0("szynka_wieprzowa_gotowana",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(tab)[17:30]<-paste0("jabłka_za_1kg",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(tab)[31:44]<-paste0("pomarańcze_za_1kg",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(tab)[45:58]<-paste0("marchew_za_1kg",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(tab)[59:72]<-paste0("herbata_czarna_liściasta_za_100g",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(tab)[73:86]<-paste0("koszula_męska_z_elanobawełny_długi_rękaw",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(tab)[87:100]<-paste0("rajstopy_damskie_gładkie_15den",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(tab)[101:114]<-paste0("bateria_zlewozmywaka",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(tab)[115:128]<-paste0("węgiel_kamienny",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(tab)[129:142]<-paste0("gazeta_regionalna",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))

View(tab)

woj<-rep(c("Polska","dolnoslaskie","kujawsko-pomorskie","lubelskie","lubuskie","lodzkie","malopolskie",
           "mazowieckie","opolskie","podkarpackie","podlaskie","pomorskie","slaskie","swietokrzyskie","warminsko-mazurskie","wielkopolskie","zachodniopomorskie"),12*140)
mies<-rep(c("sty","lut","mar","kwi","maj","cze","lip","sie","wrz","paz","lis","gru"),each=168)
szynka_wieprzowa_gotowana<-c(tab[,3],tab[,4],tab[,5],tab[,6],tab[,7],tab[,8],tab[,9],tab[,10],tab[,11],tab[,12],tab[,13],tab[,14],tab[,15],tab[,16])
jabłka_za_1kg<-c(tab[,17],tab[,18],tab[,19],tab[,20],tab[,21],tab[,22],tab[,23],tab[,24],tab[,25],tab[,26],tab[,27],tab[,28],tab[,29],tab[,30])
pomarańcze_za_1kg<-c(tab[,31],tab[,32],tab[,33],tab[,34],tab[,35],tab[,36],tab[,37],tab[,38],tab[,39],tab[,40],tab[,41],tab[,42],tab[,43],tab[,44])
marchew_za_1kg<-c(tab[,45],tab[,46],tab[,47],tab[,48],tab[,49],tab[,50],tab[,51],tab[,52],tab[,53],tab[,54],tab[,55],tab[,56],tab[,57],tab[,58])
herbata_czarna_liściasta_za_100g<-c(tab[,59],tab[,60],tab[,61],tab[,62],tab[,63],tab[,64],tab[,65],tab[,66],tab[,67],tab[,68],tab[,69],tab[,70],tab[,71],tab[,72])
koszula_męska_z_elanobawełny_długi_rękaw<-c(tab[,73],tab[,74],tab[,75],tab[,76],tab[,77],tab[,78],tab[,79],tab[,80],tab[,81],tab[,82],tab[,83],tab[,84],tab[,85],tab[,86])
rajstopy_damskie_gładkie_15den<-c(tab[,87],tab[,88],tab[,89],tab[,90],tab[,91],tab[,92],tab[,93],tab[,94],tab[,95],tab[,96],tab[,97],tab[,98],tab[,99],tab[,100])
bateria_zlewozmywaka<-c(tab[,101],tab[,102],tab[,103],tab[,104],tab[,105],tab[,106],tab[,107],tab[,108],tab[,109],tab[,110],tab[,111],tab[,112],tab[,113],tab[,114])
węgiel_kamienny<-c(tab[,115],tab[,116],tab[,117],tab[,118],tab[,119],tab[,120],tab[,121],tab[,122],tab[,123],tab[,124],tab[,125],tab[,126],tab[,127],tab[,128])
gazeta_regionalna<-c(tab[,129],tab[,130],tab[,131],tab[,132],tab[,133],tab[,134],tab[,135],tab[,136],tab[,137],tab[,138],tab[,139],tab[,140],tab[,141],tab[,142])

tab[,2]
tab[,2]<-as.character(tab[,2])
woj<-rep(tab[,2],14)
woj1<-rep(woj,12)
length(woj1)
rok<-rep(c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"),each=17)
rok1<-rep(rok,12)
length(rok1)
mies<-rep(c("sty","lut","mar","kwi","maj","cze","lip","sie","wrz","paź","lis","gru"),each=238)
mon<-factor(mies, levels=c("sty","lut","mar","kwi","maj","cze","lip","sie","wrz","paź","lis","gru"), ordered = TRUE)

j=0
szynka_wieprzowa_gotowana<-c(tab[,j+3],tab[,j+4],tab[,j+5],tab[,j+6],tab[,j+7],tab[,j+8],tab[,j+9],tab[,j+10],tab[,j+11],tab[,j+12],tab[,j+13],tab[,j+14],tab[,j+15],tab[,j+16])
jabłka_za_1kg<-c(tab[,j+17],tab[,j+18],tab[,j+19],tab[,j+20],tab[,j+21],tab[,j+22],tab[,j+23],tab[,j+24],tab[,j+25],tab[,j+26],tab[,j+27],tab[,j+28],tab[,j+29],tab[,j+30])
pomarańcze_za_1kg<-c(tab[,j+31],tab[,j+32],tab[,j+33],tab[,j+34],tab[,j+35],tab[,j+36],tab[,j+37],tab[,j+38],tab[,j+39],tab[,j+40],tab[,j+41],tab[,j+42],tab[,j+43],tab[,j+44])
marchew_za_1kg<-c(tab[,j+45],tab[,j+46],tab[,j+47],tab[,j+48],tab[,j+49],tab[,j+50],tab[,j+51],tab[,j+52],tab[,j+53],tab[,j+54],tab[,j+55],tab[,j+56],tab[,j+57],tab[,j+58])
herbata_czarna_liściasta_za_100g<-c(tab[,j+59],tab[,j+60],tab[,j+61],tab[,j+62],tab[,j+63],tab[,j+64],tab[,j+65],tab[,j+66],tab[,j+67],tab[,j+68],tab[,j+69],tab[,j+70],tab[,j+71],tab[,j+72])
koszula_męska_z_elanobawełny_długi_rękaw<-c(tab[,j+73],tab[,j+74],tab[,j+75],tab[,j+76],tab[,j+77],tab[,j+78],tab[,j+79],tab[,j+80],tab[,j+81],tab[,j+82],tab[,j+83],tab[,j+84],tab[,j+85],tab[,j+86])
rajstopy_damskie_gładkie_15den<-c(tab[,j+87],tab[,j+88],tab[,j+89],tab[,j+90],tab[,j+91],tab[,j+92],tab[,j+93],tab[,j+94],tab[,j+95],tab[,j+96],tab[,j+97],tab[,j+98],tab[,j+99],tab[,j+100])
bateria_zlewozmywaka<-c(tab[,j+101],tab[,j+102],tab[,j+103],tab[,j+104],tab[,j+105],tab[,j+106],tab[,j+107],tab[,j+108],tab[,j+109],tab[,j+110],tab[,j+111],tab[,j+112],tab[,j+113],tab[,j+114])
węgiel_kamienny<-c(tab[,j+115],tab[,j+116],tab[,j+117],tab[,j+118],tab[,j+119],tab[,j+120],tab[,j+121],tab[,j+122],tab[,j+123],tab[,j+124],tab[,j+125],tab[,j+126],tab[,j+127],tab[,j+128])
gazeta_regionalna<-c(tab[,j+129],tab[,j+130],tab[,j+131],tab[,j+132],tab[,j+133],tab[,j+134],tab[,j+135],tab[,j+136],tab[,j+137],tab[,j+138],tab[,j+139],tab[,j+140],tab[,j+141],tab[,j+142])

j=140
szynka_wieprzowa_gotowana<-c(szynka_wieprzowa_gotowana,tab[,j+3],tab[,j+4],tab[,j+5],tab[,j+6],tab[,j+7],tab[,j+8],tab[,j+9],tab[,j+10],tab[,j+11],tab[,j+12],tab[,j+13],tab[,j+14],tab[,j+15],tab[,j+16])
jabłka_za_1kg<-c(jabłka_za_1kg,tab[,j+17],tab[,j+18],tab[,j+19],tab[,j+20],tab[,j+21],tab[,j+22],tab[,j+23],tab[,j+24],tab[,j+25],tab[,j+26],tab[,j+27],tab[,j+28],tab[,j+29],tab[,j+30])
pomarańcze_za_1kg<-c(pomarańcze_za_1kg,tab[,j+31],tab[,j+32],tab[,j+33],tab[,j+34],tab[,j+35],tab[,j+36],tab[,j+37],tab[,j+38],tab[,j+39],tab[,j+40],tab[,j+41],tab[,j+42],tab[,j+43],tab[,j+44])
marchew_za_1kg<-c(marchew_za_1kg,tab[,j+45],tab[,j+46],tab[,j+47],tab[,j+48],tab[,j+49],tab[,j+50],tab[,j+51],tab[,j+52],tab[,j+53],tab[,j+54],tab[,j+55],tab[,j+56],tab[,j+57],tab[,j+58])
herbata_czarna_liściasta_za_100g<-c(herbata_czarna_liściasta_za_100g,tab[,j+59],tab[,j+60],tab[,j+61],tab[,j+62],tab[,j+63],tab[,j+64],tab[,j+65],tab[,j+66],tab[,j+67],tab[,j+68],tab[,j+69],tab[,j+70],tab[,j+71],tab[,j+72])
koszula_męska_z_elanobawełny_długi_rękaw<-c(koszula_męska_z_elanobawełny_długi_rękaw,tab[,j+73],tab[,j+74],tab[,j+75],tab[,j+76],tab[,j+77],tab[,j+78],tab[,j+79],tab[,j+80],tab[,j+81],tab[,j+82],tab[,j+83],tab[,j+84],tab[,j+85],tab[,j+86])
rajstopy_damskie_gładkie_15den<-c(rajstopy_damskie_gładkie_15den,tab[,j+87],tab[,j+88],tab[,j+89],tab[,j+90],tab[,j+91],tab[,j+92],tab[,j+93],tab[,j+94],tab[,j+95],tab[,j+96],tab[,j+97],tab[,j+98],tab[,j+99],tab[,j+100])
bateria_zlewozmywaka<-c(bateria_zlewozmywaka,tab[,j+101],tab[,j+102],tab[,j+103],tab[,j+104],tab[,j+105],tab[,j+106],tab[,j+107],tab[,j+108],tab[,j+109],tab[,j+110],tab[,j+111],tab[,j+112],tab[,j+113],tab[,j+114])
węgiel_kamienny<-c(węgiel_kamienny,tab[,j+115],tab[,j+116],tab[,j+117],tab[,j+118],tab[,j+119],tab[,j+120],tab[,j+121],tab[,j+122],tab[,j+123],tab[,j+124],tab[,j+125],tab[,j+126],tab[,j+127],tab[,j+128])
gazeta_regionalna<-c(gazeta_regionalna,tab[,j+129],tab[,j+130],tab[,j+131],tab[,j+132],tab[,j+133],tab[,j+134],tab[,j+135],tab[,j+136],tab[,j+137],tab[,j+138],tab[,j+139],tab[,j+140],tab[,j+141],tab[,j+142])

j=11*140
szynka_wieprzowa_gotowana<-c(szynka_wieprzowa_gotowana,tab[,j+3],tab[,j+4],tab[,j+5],tab[,j+6],tab[,j+7],tab[,j+8],tab[,j+9],tab[,j+10],tab[,j+11],tab[,j+12],tab[,j+13],tab[,j+14],tab[,j+15],tab[,j+16])
jabłka_za_1kg<-c(jabłka_za_1kg,tab[,j+17],tab[,j+18],tab[,j+19],tab[,j+20],tab[,j+21],tab[,j+22],tab[,j+23],tab[,j+24],tab[,j+25],tab[,j+26],tab[,j+27],tab[,j+28],tab[,j+29],tab[,j+30])
pomarańcze_za_1kg<-c(pomarańcze_za_1kg,tab[,j+31],tab[,j+32],tab[,j+33],tab[,j+34],tab[,j+35],tab[,j+36],tab[,j+37],tab[,j+38],tab[,j+39],tab[,j+40],tab[,j+41],tab[,j+42],tab[,j+43],tab[,j+44])
marchew_za_1kg<-c(marchew_za_1kg,tab[,j+45],tab[,j+46],tab[,j+47],tab[,j+48],tab[,j+49],tab[,j+50],tab[,j+51],tab[,j+52],tab[,j+53],tab[,j+54],tab[,j+55],tab[,j+56],tab[,j+57],tab[,j+58])
herbata_czarna_liściasta_za_100g<-c(herbata_czarna_liściasta_za_100g,tab[,j+59],tab[,j+60],tab[,j+61],tab[,j+62],tab[,j+63],tab[,j+64],tab[,j+65],tab[,j+66],tab[,j+67],tab[,j+68],tab[,j+69],tab[,j+70],tab[,j+71],tab[,j+72])
koszula_męska_z_elanobawełny_długi_rękaw<-c(koszula_męska_z_elanobawełny_długi_rękaw,tab[,j+73],tab[,j+74],tab[,j+75],tab[,j+76],tab[,j+77],tab[,j+78],tab[,j+79],tab[,j+80],tab[,j+81],tab[,j+82],tab[,j+83],tab[,j+84],tab[,j+85],tab[,j+86])
rajstopy_damskie_gładkie_15den<-c(rajstopy_damskie_gładkie_15den,tab[,j+87],tab[,j+88],tab[,j+89],tab[,j+90],tab[,j+91],tab[,j+92],tab[,j+93],tab[,j+94],tab[,j+95],tab[,j+96],tab[,j+97],tab[,j+98],tab[,j+99],tab[,j+100])
bateria_zlewozmywaka<-c(bateria_zlewozmywaka,tab[,j+101],tab[,j+102],tab[,j+103],tab[,j+104],tab[,j+105],tab[,j+106],tab[,j+107],tab[,j+108],tab[,j+109],tab[,j+110],tab[,j+111],tab[,j+112],tab[,j+113],tab[,j+114])
węgiel_kamienny<-c(węgiel_kamienny,tab[,j+115],tab[,j+116],tab[,j+117],tab[,j+118],tab[,j+119],tab[,j+120],tab[,j+121],tab[,j+122],tab[,j+123],tab[,j+124],tab[,j+125],tab[,j+126],tab[,j+127],tab[,j+128])
gazeta_regionalna<-c(gazeta_regionalna,tab[,j+129],tab[,j+130],tab[,j+131],tab[,j+132],tab[,j+133],tab[,j+134],tab[,j+135],tab[,j+136],tab[,j+137],tab[,j+138],tab[,j+139],tab[,j+140],tab[,j+141],tab[,j+142])

dane<-data.frame(woj=woj1, rok=rok1, mies=mies, mon=mon, szynka_wieprzowa_gotowana=szynka_wieprzowa_gotowana, jabłka_za_1kg=jabłka_za_1kg,pomarańcze_za_1kg=pomarańcze_za_1kg, 
                 marchew_za_1kg=marchew_za_1kg, herbata_czarna_liściasta_za_100g=herbata_czarna_liściasta_za_100g, koszula_męska_z_elanobawełny_długi_rękaw=koszula_męska_z_elanobawełny_długi_rękaw,
                 rajstopy_damskie_gładkie_15den=rajstopy_damskie_gładkie_15den, bateria_zlewozmywaka=bateria_zlewozmywaka, węgiel_kamienny=węgiel_kamienny, gazeta_regionalna=gazeta_regionalna)

dane[is.na(dane)] <- 0

dim(dane)
View(dane)

#średnie ceny produktów w poszczególnych latach
for(i in 5:14){
  print(tapply(dane[,i],dane[,2],mean))
}

#odchylenie standardowe w poszczególnych latach
for(i in 5:14){
  print(tapply(dane[,i],dane[,2],sd))
}

#średnie ceny produktów z lat 2006-2019 w poszczególnych województwach
sr_szynka_wieprzowa_gotowana<-tapply(dane[,5],dane[,1],mean)
sr_jabłka_za_1kg<-tapply(dane[,6],dane[,1],mean)
sr_pomarańcze_za_1kg<-tapply(dane[,7],dane[,1],mean,na.rm=T)
sr_marchew_za_1kg<-tapply(dane[,8],dane[,1],mean)
sr_herbata_czarna_liściasta_za_100g<-tapply(dane[,9],dane[,1],mean)
sr_koszula_męska_z_elanobawełny_długi_rękaw<-tapply(dane[,10],dane[,1],mean)
sr_rajstopy_damskie_gładkie_15den<-tapply(dane[,11],dane[,1],mean)
sr_bateria_zlewozmywaka<-tapply(dane[,12],dane[,1],mean)
sr_węgiel_kamienny<-tapply(dane[,13],dane[,1],mean)
sr_gazeta_regionalna<-tapply(dane[,14],dane[,1],mean)

#zestawienie średnich cen produktów z lat 2006-2019 w poszczególnych województwach
srednie<-data.frame(woj=as.factor(sort(tab[,1])), sr_szynka_wieprzowa_gotowana=sr_szynka_wieprzowa_gotowana, sr_jabłka_za_1kg=sr_jabłka_za_1kg, sr_pomarańcze_za_1kg=sr_pomarańcze_za_1kg, 
                    sr_marchew_za_1kg=sr_marchew_za_1kg, sr_herbata_czarna_liściasta_za_100g=sr_herbata_czarna_liściasta_za_100g, sr_koszula_męska_z_elanobawełny_długi_rękaw=sr_koszula_męska_z_elanobawełny_długi_rękaw,
                    sr_rajstopy_damskie_gładkie_15den=sr_rajstopy_damskie_gładkie_15den, sr_bateria_zlewozmywaka=sr_bateria_zlewozmywaka, sr_węgiel_kamienny=sr_węgiel_kamienny, sr_gazeta_regionalna=sr_gazeta_regionalna)
View(srednie)

#koszyki ze średnimi cenami z lat 2006-2019
koszyk_dolnoslaskie<-data.frame(sum(srednie[1,2:11]))
koszyk_kujawsko_pomorskie<-data.frame(sum(srednie[2,2:11]))
koszyk_lodzkie<-data.frame(sum(srednie[3,2:11]))
koszyk_lubelskie<-data.frame(sum(srednie[4,2:11]))
koszyk_lubuskie<-data.frame(sum(srednie[5,2:11]))
koszyk_malopolskie<-data.frame(sum(srednie[6,2:11]))
koszyk_mazowieckie<-data.frame(sum(srednie[7,2:11]))
koszyk_opolskie<-data.frame(sum(srednie[8,2:11]))
koszyk_podkarpackie<-data.frame(sum(srednie[9,2:11]))
koszyk_podlaskie<-data.frame(sum(srednie[10,2:11]))
koszyk_polska<-data.frame(sum(srednie[11,2:11]))
koszyk_pomorskie<-data.frame(sum(srednie[12,2:11]))
koszyk_slaskie<-data.frame(sum(srednie[13,2:11]))
koszyk_swietokrzyskie<-data.frame(sum(srednie[14,2:11]))
koszyk_warminsko_mazurskie<-data.frame(sum(srednie[15,2:11]))
koszyk_wielkopolskie<-data.frame(sum(srednie[16,2:11]))
koszyk_zachodniopomorskie<-data.frame(sum(srednie[17,2:11]))

#wykresy średnich
#szynka_wieprzowa_gotowana 
ggplot(data = srednie, 
       aes(x = woj, 
           y = sr_szynka_wieprzowa_gotowana)) + 
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) +
  ggtitle("Średnia cena szynki wieprzowej gotowanej (w zł)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = srednie, 
       aes(x = woj, 
           y = sr_szynka_wieprzowa_gotowana)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#jabłka_za_1kg
ggplot(data = srednie, 
       aes(x = woj, 
           y = sr_jabłka_za_1kg)) + 
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) +
  ggtitle("Średnia cena kilograma jabłek (w zł)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = srednie, 
       aes(x = woj, 
           y = sr_jabłka_za_1kg)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#pomarańcze_za_1kg
ggplot(data = srednie, 
       aes(x = woj, 
           y = sr_pomarańcze_za_1kg)) + 
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) +
  ggtitle("Średnia cena kilograma pomarańczy (w zł)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = srednie, 
       aes(x = woj, 
           y = sr_pomarańcze_za_1kg)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#marchew_za_1kg
ggplot(data = srednie, 
       aes(x = woj, 
           y = sr_marchew_za_1kg)) + 
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) +
  ggtitle("Średnia cena kilograma marchwi (w zł)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = srednie, 
       aes(x = woj, 
           y = sr_marchew_za_1kg)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#herbata_czarna_liściasta_za_100g
ggplot(data = srednie, 
       aes(x = woj, 
           y = sr_herbata_czarna_liściasta_za_100g)) + 
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) +
  ggtitle("Średnia cena 100g herbaty czarnej liściastej (w zł)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = srednie, 
       aes(x = woj, 
           y = sr_herbata_czarna_liściasta_za_100g)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#koszula_męska_z_elanobawełny_długi_rękaw
ggplot(data = srednie, 
       aes(x = woj, 
           y = sr_koszula_męska_z_elanobawełny_długi_rękaw)) + 
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) +
  ggtitle("Średnia cena koszuli męskiej z elanobawełny, długi rękaw (w zł)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = srednie, 
       aes(x = woj, 
           y = sr_koszula_męska_z_elanobawełny_długi_rękaw)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#rajstopy_damskie_gładkie_15den
ggplot(data = srednie, 
       aes(x = woj, 
           y = sr_rajstopy_damskie_gładkie_15den)) + 
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) +
  ggtitle("Średnia cena rajstop damskich gładkich 15 den (w zł)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = srednie, 
       aes(x = woj, 
           y = sr_rajstopy_damskie_gładkie_15den)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#bateria_zlewozmywaka
ggplot(data = srednie, 
       aes(x = woj, 
           y = sr_bateria_zlewozmywaka)) + 
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) +
  ggtitle("Średnia cena baterii zlewozmywaka (w zł)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = srednie, 
       aes(x = woj, 
           y = sr_bateria_zlewozmywaka)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#węgiel_kamienny
ggplot(data = srednie, 
       aes(x = woj, 
           y = sr_węgiel_kamienny)) + 
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) +
  ggtitle("Średnia cena węgla kamiennego (w zł)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = srednie, 
       aes(x = woj, 
           y = sr_węgiel_kamienny)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#gazeta_regionalna
ggplot(data = srednie, 
       aes(x = woj, 
           y = sr_gazeta_regionalna)) + 
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) +
  ggtitle("Średnia cena gazety regionalnej (w zł)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = srednie, 
       aes(x = woj, 
           y = sr_gazeta_regionalna)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#odchylenie standardowe w poszczególnych województwach
sd_szynka_wieprzowa_gotowana<-tapply(dane[,5],dane[,1],sd)
sd_jabłka_za_1kg<-tapply(dane[,6],dane[,1],sd)
sd_pomarańcze_za_1kg<-tapply(dane[,7],dane[,1],sd,na.rm=T)
sd_marchew_za_1kg<-tapply(dane[,8],dane[,1],sd)
sd_herbata_czarna_liściasta_za_100g<-tapply(dane[,9],dane[,1],sd)
sd_koszula_męska_z_elanobawełny_długi_rękaw<-tapply(dane[,10],dane[,1],sd)
sd_rajstopy_damskie_gładkie_15den<-tapply(dane[,11],dane[,1],sd)
sd_bateria_zlewozmywaka<-tapply(dane[,12],dane[,1],sd)
sd_węgiel_kamienny<-tapply(dane[,13],dane[,1],sd)
sd_gazeta_regionalna<-tapply(dane[,14],dane[,1],sd)

#zestawienie odchyleń standardowych
odchylenia<-data.frame(srednie,sd_szynka_wieprzowa_gotowana=sd_szynka_wieprzowa_gotowana, sd_jabłka_za_1kg=sd_jabłka_za_1kg,
                       sd_pomarańcze_za_1kg=sd_pomarańcze_za_1kg, sd_marchew_za_1kg=sd_marchew_za_1kg, sd_herbata_czarna_liściasta_za_100g=sd_herbata_czarna_liściasta_za_100g, sd_koszula_męska_z_elanobawełny_długi_rękaw=sd_koszula_męska_z_elanobawełny_długi_rękaw,
                       sd_rajstopy_damskie_gładkie_15den=sd_rajstopy_damskie_gładkie_15den, sd_bateria_zlewozmywaka=sd_bateria_zlewozmywaka, sd_węgiel_kamienny=sd_węgiel_kamienny, sd_gazeta_regionalna=sd_gazeta_regionalna)

#wykresy odchyleń
#szynka_wieprzowa_gotowana
ggplot(odchylenia, aes(x=woj, y=sr_szynka_wieprzowa_gotowana)) + 
  geom_errorbar(aes(ymin=sr_szynka_wieprzowa_gotowana-sd_szynka_wieprzowa_gotowana, 
                    ymax=sr_szynka_wieprzowa_gotowana+sd_szynka_wieprzowa_gotowana), 
                width=.1,
                col="blue") +
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = odchylenia, 
       aes(x = woj, 
           y = sr_szynka_wieprzowa_gotowana)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  geom_errorbar(aes(ymin=sr_szynka_wieprzowa_gotowana - sd_szynka_wieprzowa_gotowana, ymax=sr_szynka_wieprzowa_gotowana + sd_szynka_wieprzowa_gotowana),
                width=.2,
                col="darkblue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#jabłka_za_1kg
ggplot(odchylenia, aes(x=woj, y=sr_jabłka_za_1kg)) + 
  geom_errorbar(aes(ymin=sr_jabłka_za_1kg-sd_jabłka_za_1kg, 
                    ymax=sr_jabłka_za_1kg+sd_jabłka_za_1kg), 
                width=.1,
                col="blue") +
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = odchylenia, 
       aes(x = woj, 
           y = sr_jabłka_za_1kg)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  geom_errorbar(aes(ymin=sr_jabłka_za_1kg - sd_jabłka_za_1kg, ymax=sr_jabłka_za_1kg + sd_jabłka_za_1kg),
                width=.2,
                col="darkblue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#pomarańcze_za_1kg 
ggplot(odchylenia, aes(x=woj, y=sr_pomarańcze_za_1kg)) + 
  geom_errorbar(aes(ymin=sr_pomarańcze_za_1kg-sd_pomarańcze_za_1kg, 
                    ymax=sr_pomarańcze_za_1kg+sd_pomarańcze_za_1kg), 
                width=.1,
                col="blue") +
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = odchylenia, 
       aes(x = woj, 
           y = sr_pomarańcze_za_1kg)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  geom_errorbar(aes(ymin=sr_pomarańcze_za_1kg - sd_pomarańcze_za_1kg, ymax=sr_pomarańcze_za_1kg + sd_pomarańcze_za_1kg),
                width=.2,                    # Width of the error bars
                col="darkblue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#marchew_za_1kg
ggplot(odchylenia, aes(x=woj, y=sr_marchew_za_1kg)) + 
  geom_errorbar(aes(ymin=sr_marchew_za_1kg-sd_marchew_za_1kg, 
                    ymax=sr_marchew_za_1kg+sd_marchew_za_1kg), 
                width=.1,
                col="blue") +
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = odchylenia, 
       aes(x = woj, 
           y = sr_marchew_za_1kg)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  geom_errorbar(aes(ymin=sr_marchew_za_1kg - sd_marchew_za_1kg, ymax=sr_marchew_za_1kg + sd_marchew_za_1kg),
                width=.2,
                col="darkblue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#herbata_czarna_liściasta_za_100g
ggplot(odchylenia, aes(x=woj, y=sr_herbata_czarna_liściasta_za_100g)) + 
  geom_errorbar(aes(ymin=sr_herbata_czarna_liściasta_za_100g-sd_herbata_czarna_liściasta_za_100g, 
                    ymax=sr_herbata_czarna_liściasta_za_100g+sd_herbata_czarna_liściasta_za_100g), 
                width=.1,
                col="blue") +
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = odchylenia, 
       aes(x = woj, 
           y = sr_herbata_czarna_liściasta_za_100g)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  geom_errorbar(aes(ymin=sr_herbata_czarna_liściasta_za_100g - sd_herbata_czarna_liściasta_za_100g, ymax=sr_herbata_czarna_liściasta_za_100g + sd_herbata_czarna_liściasta_za_100g),
                width=.2,
                col="darkblue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#koszula_męska_z_elanobawełny_długi_rękaw
ggplot(odchylenia, aes(x=woj, y=sr_koszula_męska_z_elanobawełny_długi_rękaw)) + 
  geom_errorbar(aes(ymin=sr_koszula_męska_z_elanobawełny_długi_rękaw-sd_koszula_męska_z_elanobawełny_długi_rękaw, 
                    ymax=sr_koszula_męska_z_elanobawełny_długi_rękaw+sd_koszula_męska_z_elanobawełny_długi_rękaw), 
                width=.1,
                col="blue") +
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = odchylenia, 
       aes(x = woj, 
           y = sr_koszula_męska_z_elanobawełny_długi_rękaw)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  geom_errorbar(aes(ymin=sr_koszula_męska_z_elanobawełny_długi_rękaw - sd_koszula_męska_z_elanobawełny_długi_rękaw, ymax=sr_koszula_męska_z_elanobawełny_długi_rękaw + sd_koszula_męska_z_elanobawełny_długi_rękaw),
                width=.2,
                col="darkblue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#rajstopy_damskie_gładkie_15den
ggplot(odchylenia, aes(x=woj, y=sr_rajstopy_damskie_gładkie_15den)) + 
  geom_errorbar(aes(ymin=sr_rajstopy_damskie_gładkie_15den-sd_rajstopy_damskie_gładkie_15den, 
                    ymax=sr_rajstopy_damskie_gładkie_15den+sd_rajstopy_damskie_gładkie_15den), 
                width=.1,
                col="blue") +
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = odchylenia, 
       aes(x = woj, 
           y = sr_rajstopy_damskie_gładkie_15den)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  geom_errorbar(aes(ymin=sr_rajstopy_damskie_gładkie_15den - sd_rajstopy_damskie_gładkie_15den, ymax=sr_rajstopy_damskie_gładkie_15den + sd_rajstopy_damskie_gładkie_15den),
                width=.2,
                col="darkblue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#bateria_zlewozmywaka
ggplot(odchylenia, aes(x=woj, y=sr_bateria_zlewozmywaka)) + 
  geom_errorbar(aes(ymin=sr_bateria_zlewozmywaka-sd_bateria_zlewozmywaka, 
                    ymax=sr_bateria_zlewozmywaka+sd_bateria_zlewozmywaka), 
                width=.1,
                col="blue") +
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = odchylenia, 
       aes(x = woj, 
           y = sr_bateria_zlewozmywaka)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  geom_errorbar(aes(ymin=sr_bateria_zlewozmywaka - sd_bateria_zlewozmywaka, ymax=sr_bateria_zlewozmywaka + sd_bateria_zlewozmywaka),
                width=.2,
                col="darkblue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#węgiel_kamienny
ggplot(odchylenia, aes(x=woj, y=sr_węgiel_kamienny)) + 
  geom_errorbar(aes(ymin=sr_węgiel_kamienny-sd_węgiel_kamienny, 
                    ymax=sr_węgiel_kamienny+sd_węgiel_kamienny), 
                width=.1,
                col="blue") +
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = odchylenia, 
       aes(x = woj, 
           y = sr_węgiel_kamienny)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  geom_errorbar(aes(ymin=sr_węgiel_kamienny - sd_węgiel_kamienny, ymax=sr_węgiel_kamienny + sd_węgiel_kamienny),
                width=.2,
                col="darkblue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#gazeta_regionalna
ggplot(odchylenia, aes(x=woj, y=sr_gazeta_regionalna)) + 
  geom_errorbar(aes(ymin=sr_gazeta_regionalna-sd_gazeta_regionalna, 
                    ymax=sr_gazeta_regionalna+sd_gazeta_regionalna), 
                width=.1,
                col="blue") +
  geom_point(color = 'red', 
             size = 1, 
             shape = 15) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = odchylenia, 
       aes(x = woj, 
           y = sr_gazeta_regionalna)) +
  geom_bar(stat = 'identity', fill="lightblue", colour = "red") +
  geom_errorbar(aes(ymin=sr_gazeta_regionalna - sd_gazeta_regionalna, ymax=sr_gazeta_regionalna + sd_gazeta_regionalna),
                width=.2,
                col="darkblue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#analiza cen dla Polski (lata 2006-2019)
dim(dane)
View(dane)

polska <- dane[dane$woj == "POLSKA",]
dim(polska)
View(polska)
str(polska)

polska$data <- as.yearmon(paste(polska$mies, polska$rok)) 
polska<-polska[order(polska$mies),]
polska2006<-polska[polska$rok==2006,]
polska2007<-polska[polska$rok==2007,]
polska2008<-polska[polska$rok==2008,]
polska2009<-polska[polska$rok==2009,]
polska2010<-polska[polska$rok==2010,]
polska2011<-polska[polska$rok==2011,]
polska2012<-polska[polska$rok==2012,]
polska2013<-polska[polska$rok==2013,]
polska2014<-polska[polska$rok==2014,]
polska2015<-polska[polska$rok==2015,]
polska2016<-polska[polska$rok==2016,]
polska2017<-polska[polska$rok==2017,]
polska2018<-polska[polska$rok==2018,]
polska2019<-polska[polska$rok==2019,]

#zmiany cen produktów w czasie dla Polski
#szynka_wieprzowa_gotowana
ggplot(data = polska, 
       aes(x = data, 
           y = szynka_wieprzowa_gotowana,
           color =rok)) + 
  geom_point(size = 5, 
             shape = 15) +
  ggtitle("Zmiany cen szynki wieprzowej gotowanej w Polsce w latach 2006-2019") +
  scale_x_yearmon() +
  theme(axis.text.x = element_text(angle = 90, hjust = .5))

#jabłka_za_1kg
ggplot(data = polska, 
       aes(x = data, 
           y = jabłka_za_1kg,
           color =rok)) + 
  geom_point(size = 5, 
             shape = 15) +
  ggtitle("Zmiany cen kilograma jabłek w Polsce w latach 2006-2019") +
  scale_x_yearmon() +
  theme(axis.text.x = element_text(angle = 90, hjust = .5))

#pomarańcze_za_1kg
ggplot(data = polska, 
       aes(x = data, 
           y = pomarańcze_za_1kg,
           color =rok)) + 
  geom_point(size = 5, 
             shape = 15) +
  ggtitle("Zmiany cen kilograma pomarańczy w Polsce w latach 2006-2019") +
  scale_x_yearmon() +
  theme(axis.text.x = element_text(angle = 90, hjust = .5))

#marchew_za_1kg
ggplot(data = polska, 
       aes(x = data, 
           y = marchew_za_1kg,
           color =rok)) + 
  geom_point(size = 5, 
             shape = 15) +
  ggtitle("Zmiany cen kilograma marchwi w Polsce w latach 2006-2019") +
  scale_x_yearmon() +
  theme(axis.text.x = element_text(angle = 90, hjust = .5))

#herbata_czarna_liściasta_za_100g
ggplot(data = polska, 
       aes(x = data, 
           y = herbata_czarna_liściasta_za_100g,
           color =rok)) + 
  geom_point(size = 5, 
             shape = 15) +
  ggtitle("Zmiany cen 100g herbaty czarnej liściastej w Polsce w latach 2006-2019") +
  scale_x_yearmon() +
  theme(axis.text.x = element_text(angle = 90, hjust = .5))

#koszula_męska_z_elanobawełny_długi_rękaw
ggplot(data = polska, 
       aes(x = data, 
           y = koszula_męska_z_elanobawełny_długi_rękaw,
           color =rok)) + 
  geom_point(size = 5, 
             shape = 15) +
  ggtitle("Zmiany cen koszuli męskiej z elanobawełny długi rękaw w Polsce w latach 2006-2019") +
  scale_x_yearmon() +
  theme(axis.text.x = element_text(angle = 90, hjust = .5))

#rajstopy damskie gładkie 15den
ggplot(data = polska, 
       aes(x = data, 
           y = rajstopy_damskie_gładkie_15den,
           color =rok)) + 
  geom_point(size = 5, 
             shape = 15) +
  ggtitle("Zmiany cen rajstop damskich gładkich 15den w Polsce w latach 2006-2019") +
  scale_x_yearmon() +
  theme(axis.text.x = element_text(angle = 90, hjust = .5))

#bateria_zlewozmywaka
ggplot(data = polska, 
       aes(x = data, 
           y = bateria_zlewozmywaka,
           color =rok)) + 
  geom_point(size = 5, 
             shape = 15) +
  ggtitle("Zmiany cen baterii zlewozmywaka w Polsce w latach 2006-2019") +
  scale_x_yearmon() +
  theme(axis.text.x = element_text(angle = 90, hjust = .5))

#węgiel_kamienny
ggplot(data = polska, 
       aes(x = data, 
           y = węgiel_kamienny,
           color =rok)) + 
  geom_point(size = 5, 
             shape = 15) +
  ggtitle("Zmiany cen węgla kamiennego w Polsce w latach 2006-2019") +
  scale_x_yearmon() +
  theme(axis.text.x = element_text(angle = 90, hjust = .5))

#gazeta_regionalna
ggplot(data = polska, 
       aes(x = data, 
           y = gazeta_regionalna,
           color =rok)) + 
  geom_point(size = 5, 
             shape = 15) +
  ggtitle("Zmiany cen gazety regionalnej w Polsce w latach 2006-2019") +
  scale_x_yearmon() +
  theme(axis.text.x = element_text(angle = 90, hjust = .5))