
###############################################  Thesis 2020   ##############################################

                                         ###### Demographics ########

#############################################################################################################

Total_For <- as.data.frame(read_excel("C:/Users/46766/Dropbox/Big R Map/0000030A_20210424-003849.xlsx", skip = 1))[1:160, ]

Total_For$Total_Change<-Total_For$`2019`-Total_For$`2000`

## Top countries

Top<-Vectorize(Top<-function(x){

  return(which(Total_For$Total_Change== Rfast::nth(Total_For$Total_Change, x,descending = TRUE)))

  })

## Proportions

sum(Total_For$Total_Change[Top((1:40))])/sum(Total_For$Total_Change)

## Top countries

Country <- as.data.frame(read_excel("C:/Users/46766/Dropbox/Big R Map/0000030A_20210322-101626.xlsx", skip = 1))[seq(1, 160),] # Data on no.1 ppl from country X.

Country$Diff<-Country[, 21]-Country[, 2]

Country<-Country[, c(1, 23)]

#Top countries as of yr 2019.

Names<-NULL

for (i in seq(1, 40)) {
  
  Names<-c(Names, Country[which(Country[, 2] == Rfast::nth(Country[, 2], i, descending = TRUE)), 1])

  }

Names # Check the character vector.

## Swedish case

Swedenlength<-cbind(as.data.frame(read_excel("C:/Users/46766/Dropbox/Big R Map/UF0506M1_20210420-114544.xlsx",skip = 1))[seq(1,5),],
                    as.data.frame(read_excel("C:/Users/46766/Dropbox/Big R Map/000000XG_20210420-114609.xlsx", skip = 1))[seq(1,5),][, 3])

## Education backgrounds

Educationallength<- as.data.frame(read_excel("C:/Users/46766/Dropbox/Big R Map/UF0506LI_20210419-231931.xlsx", skip = 1))[1:476,]

Educationallength[78+95*(1-1), seq(3, 22)]<-as.numeric(Swedenlength[1, seq(3,22)])
Educationallength[78+95*(2-1), seq(3, 22)]<-as.numeric(Swedenlength[2, seq(3,22)])
Educationallength[78+95*(3-1), seq(3, 22)]<-as.numeric(Swedenlength[3, seq(3,22)])
Educationallength[78+95*(4-1), seq(3, 22)]<-as.numeric(Swedenlength[4, seq(3,22)])
Educationallength[78+95*(5-1), seq(3, 22)]<-as.numeric(Swedenlength[5, seq(3,22)])

#### Weighted average function

Educ_Score<-function(x){
  
  Total<-sum(c(sum(as.numeric(Educationallength[x+95*(1-1), seq(3, 22)])),
               sum(as.numeric(Educationallength[x+95*(2-1), seq(3, 22)])),
               sum(as.numeric(Educationallength[x+95*(3-1), seq(3, 22)])),
               sum(as.numeric(Educationallength[x+95*(4-1), seq(3, 22)])),
               sum(as.numeric(Educationallength[x+95*(5-1), seq(3, 22)]))
  ))
  
  Weighted_average<-sum(c((sum(as.numeric(Educationallength[c(x+95*(1-1)), seq(3, 22)]))/Total)*1,
                          ((sum(as.numeric(Educationallength[c(x+95*(2-1)), seq(3, 22)]))/Total)*2),
                          (sum(as.numeric(Educationallength[c(x+95*(3-1)), seq(3, 22)]))/Total)*3,
                          (sum(as.numeric(Educationallength[c(x+95*(4-1)), seq(3, 22)]))/Total)*4),
                        (sum(as.numeric(Educationallength[c(x+95*(5-1)), seq(3, 22)]))/Total)*5)
  
  return(Weighted_average)
  
}

# Human capital estimates - World bank 2020.

HCI_frame<-data.frame(c(Names, "Sweden"), rep(0, 41),rep(0, 41), rep(0, 41), rep(0, 41))

hci <- as.data.frame(read_excel("C:/Users/46766/Dropbox/Big R Map/hci_data_september_2020.xlsx", sheet = "HCI 2020 - MaleFemale"))

colnames(HCI_frame)<-c("Country names", "HCI", "Region", "Lifeexp", "Educ")

#

Lifeexp <- read_excel("C:/Users/46766/Dropbox/Big R Map/API_SP.DYN.LE00.IN_DS2_en_excel_v2_2163736.xls", skip = 3)

# Asia=1, EU=2, Africa=3, SA=4, NA=5, Oce=6

HCI_frame[1, seq(2, 5)]<- c(0.5,"Asia" ,Lifeexp$`2018`[Lifeexp$`Country Name` =="Syrian Arab Republic"], Educ_Score(which(Educationallength$...2 =="Syrien")[1]))#Syria
HCI_frame[2, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Iraq"], "Asia",Lifeexp$`2018`[Lifeexp$`Country Name` =="Iraq"], Educ_Score(which(Educationallength$...2 =="Irak")[1])) #Iraq
HCI_frame[3, seq(2, 5)]<-c(0.27, "Africa",Lifeexp$`2018`[Lifeexp$`Country Name` =="Somalia"],Educ_Score(which(Educationallength$...2 =="Somalia")[1]))
HCI_frame[4, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Afghanistan"], "Asia", Lifeexp$`2018`[Lifeexp$`Country Name` =="Afghanistan"], Educ_Score(which(Educationallength$...2 =="Afghanistan")[1])) #Afghanistan
HCI_frame[5, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Poland"], "Europe",Lifeexp$`2018`[Lifeexp$`Country Name` =="Poland"], Educ_Score(which(Educationallength$...2 =="Polen")[1])) #Poland
HCI_frame[6, seq(2, 5)]<-c(0.4, "Africa",Lifeexp$`2018`[Lifeexp$`Country Name` =="Eritrea"], Educ_Score(which(Educationallength$...2 =="Eritrea")[1])) #Eritrea
HCI_frame[7, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Thailand"], "Asia",Lifeexp$`2018`[Lifeexp$`Country Name` =="Thailand"], Educ_Score(which(Educationallength$...2 =="Thailand")[1])) #Thailand
HCI_frame[8, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "India"], "Asia", Lifeexp$`2018`[Lifeexp$`Country Name` =="India"],Educ_Score(which(Educationallength$...2 =="Indien")[1])) #India
HCI_frame[9, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Iran, Islamic Rep."], "Asia", Lifeexp$`2018`[Lifeexp$`Country Name` =="Iran, Islamic Rep."],Educ_Score(which(Educationallength$...2 =="Iran")[1])) #Iran                    
HCI_frame[10, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "China"], "Asia", Lifeexp$`2018`[Lifeexp$`Country Name` =="China"],Educ_Score(which(Educationallength$...2 =="Kina")[1])) #China
HCI_frame[11, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Romania"], "Europe", Lifeexp$`2018`[Lifeexp$`Country Name` =="Romania"],Educ_Score(which(Educationallength$...2 =="Rumänien")[1])) #Romania
HCI_frame[12, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Turkey"], "Europe", Lifeexp$`2018`[Lifeexp$`Country Name` =="Turkey"], Educ_Score(which(Educationallength$...2 =="Turkiet")[1])) #Turkey
HCI_frame[13, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Pakistan"], "Asia", Lifeexp$`2018`[Lifeexp$`Country Name` =="Pakistan"],Educ_Score(which(Educationallength$...2 =="Pakistan")[1])) #Pakistan
HCI_frame[14, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Russian Federation"], "Europe", Lifeexp$`2018`[Lifeexp$`Country Name` =="Russian Federation"],Educ_Score(which(Educationallength$...2 =="Ryssland")[1])) #Russia
HCI_frame[15, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "United Kingdom"], "Europe", Lifeexp$`2018`[Lifeexp$`Country Name` =="United Kingdom"],Educ_Score(which(Educationallength$...2 =="Storbritannien och Nordirland")[1])) #GBR
HCI_frame[16, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Serbia"], "Europe", Lifeexp$`2018`[Lifeexp$`Country Name` =="Serbia"], Educ_Score(which(Educationallength$...2 =="Serbien")[1])) #Serbien
HCI_frame[17, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Lithuania"], "Europe", Lifeexp$`2018`[Lifeexp$`Country Name` =="Lithuania"],Educ_Score(which(Educationallength$...2 =="Litauen")[1])) #Lithuania
HCI_frame[18, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Germany"], "Europe", Lifeexp$`2018`[Lifeexp$`Country Name` =="Germany"],Educ_Score(which(Educationallength$...2 =="Tyskland")[1])) #Germany
HCI_frame[19, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Kosovo"], "Europe", Lifeexp$`2018`[Lifeexp$`Country Name` =="Serbia"]-5,Educ_Score(which(Educationallength$...2 =="Serbien")[1])-0.3) #Germany
HCI_frame[20, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Philippines"], "Oceania", Lifeexp$`2018`[Lifeexp$`Country Name` =="Philippines"],Educ_Score(which(Educationallength$...2 =="Filippinerna")[1])) #Philippines
HCI_frame[21, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Ethiopia"], "Africa", Lifeexp$`2018`[Lifeexp$`Country Name` =="Ethiopia"],Educ_Score(which(Educationallength$...2 =="Etiopien")[1])) #Ethiopia
HCI_frame[22, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Vietnam"], "Asia", Lifeexp$`2018`[Lifeexp$`Country Name` =="Vietnam"],Educ_Score(which(Educationallength$...2 =="Vietnam")[1])) #Vietnam
HCI_frame[23, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Ukraine"], "Europe", Lifeexp$`2018`[Lifeexp$`Country Name` =="Ukraine"],Educ_Score(which(Educationallength$...2 =="Ukraina")[1])) #Vietnam
HCI_frame[24, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Greece"], "Europe", Lifeexp$`2018`[Lifeexp$`Country Name` =="Greece"],Educ_Score(which(Educationallength$...2 =="Grekland")[1]))#Greece
HCI_frame[25, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Bangladesh"], "Asia", Lifeexp$`2018`[Lifeexp$`Country Name` =="Bangladesh"], Educ_Score(which(Educationallength$...2 =="Bangladesh")[1])) #Bangladesh
HCI_frame[26, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Bosnia and Herzegovina"], "Europe", Lifeexp$`2018`[Lifeexp$`Country Name` =="Bosnia and Herzegovina"], Educ_Score(which(Educationallength$...2 =="Bosnien och Hercegovina")[1])) #Bosnia
HCI_frame[27, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Lebanon"], "Asia", Lifeexp$`2018`[Lifeexp$`Country Name` =="Lebanon"],Educ_Score(which(Educationallength$...2 =="Libanon")[1])) #Lebanon
HCI_frame[28, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "United States"], "North America", Lifeexp$`2018`[Lifeexp$`Country Name` =="United States"],Educ_Score(which(Educationallength$...2 =="USA")[1])) #USA
HCI_frame[29, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Netherlands"], "Europe", Lifeexp$`2018`[Lifeexp$`Country Name` =="Netherlands"], Educ_Score(which(Educationallength$...2 =="Nederländerna")[1])) #Nederländerna
HCI_frame[30, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Spain"], "Europe", Lifeexp$`2018`[Lifeexp$`Country Name` =="Spain"], Educ_Score(which(Educationallength$...2 =="Spanien")[1])) #Spanien
HCI_frame[31, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Italy"], "Europe", Lifeexp$`2018`[Lifeexp$`Country Name` =="Italy"], Educ_Score(which(Educationallength$...2 =="Italien")[1])) #Italien
HCI_frame[32, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "West Bank and Gaza"], "Asia", Lifeexp$`2018`[Lifeexp$`Country Name` =="West Bank and Gaza"], 2.2) #Italien
HCI_frame[33, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Morocco"], "Africa", Lifeexp$`2018`[Lifeexp$`Country Name` =="Morocco"], Educ_Score(which(Educationallength$...2 =="Marocko")[1])) #Marocko
HCI_frame[34, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Egypt, Arab Rep."], "Africa", Lifeexp$`2018`[Lifeexp$`Country Name` =="Egypt, Arab Rep."], Educ_Score(which(Educationallength$...2 =="Egypten")[1])) #Marocko
HCI_frame[35, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Brazil"], "South America", Lifeexp$`2018`[Lifeexp$`Country Name` =="Brazil"], Educ_Score(which(Educationallength$...2 =="Brasilien")[1])) #Marocko
HCI_frame[36, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Albania"], "Europe", Lifeexp$`2018`[Lifeexp$`Country Name` =="Albania"], Educ_Score(which(Educationallength$...2 =="Albanien")[1])) #Marocko
HCI_frame[37, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Croatia"], "Europe", Lifeexp$`2018`[Lifeexp$`Country Name` =="Croatia"], Educ_Score(which(Educationallength$...2 =="Kroatien")[1])) #Kroatien
HCI_frame[38, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Latvia"], "Europe", Lifeexp$`2018`[Lifeexp$`Country Name` =="Latvia"], Educ_Score(which(Educationallength$...2 =="Lettland")[1])) #Kroatien
HCI_frame[39, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Bulgaria"], "Europe", Lifeexp$`2018`[Lifeexp$`Country Name` =="Bulgaria"], Educ_Score(which(Educationallength$...2 =="Bulgarien")[1])) #Kroatien
HCI_frame[40, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "North Macedonia"], "Europe", Lifeexp$`2018`[Lifeexp$`Country Name` =="North Macedonia"], Educ_Score(which(Educationallength$...2 =="Nordmakedonien")[1])) #Kroatien
HCI_frame[41, seq(2, 5)]<-c(hci$`HUMAN CAPITAL INDEX 2020`[hci$`Country Name` == "Sweden"], "Sweden", Lifeexp$`2018`[Lifeexp$`Country Name` =="Sweden"], Educ_Score(which(Educationallength$...2 =="Sverige")[1])) #Sweden

plot_frame<-data.frame("Names"= HCI_frame$`Country names`[1:41],"Education"=HCI_frame$Educ[1:41]
                       ,"Region"=factor(HCI_frame$Region)
                       ,"HCI"=HCI_frame$HCI[1:41]
                       , "Volume"=c(Total_For$Total_Change[Top(seq(1, 40))], 50000))

# plotting the regions.

Lab_names<-HCI_frame$Region
Lab_names[20]

Label_names<-c("Syria", "Iraq", "Somalia", "Afghanistan", "Poland", "Eritrea", "Thailand", "India", "Iran", "China",
               "Romania", "Turkey", "Pakistan", "Russia", "Great Britain", "Serbia", "Lithuania", "Germany", "Kosovo", "Philippines", "Ethiopia", "
               Vietnam", "Ukraine", "Greece", "Bangladesh", "Bosnia", "Lebanon", "USA", "Netherlands", "Spain", "Italy", "Palestina", "Morocco", "Egypt", 
               "Brazil", "Albania", "Croatia", "Northern Macedonia", "Bulgaria", "Latvia", "Sweden")

ggplot(plot_frame, aes(x=as.numeric(Education), y=as.numeric(HCI), label=Label_names))+geom_point(aes(col=Region,size=Volume))+theme_bw()+geom_text(check_overlap = TRUE)+xlim(1,4)+ylim(0.2,0.9)+scale_color_manual(values=c("#2171B5", "#FF519C", "#20FF70","lightblue", "purple", "black", "Yellow"))+xlab("Education")+ylab("HCI")

## Fitting of missing data points for HCI

## Missing: 1, 3, 6

y_fit<-HCI_frame$HCI[-c(1, 3, 6)]

x_fit<-HCI_frame$Lifeexp[-c(1, 3, 6)]

fit_miss<-lm(y_fit~x_fit)

new<-data.frame(x_fit=c(Lifeexp$`2018`[Lifeexp$`Country Name` =="Syrian Arab Republic"],
                        Lifeexp$`2018`[Lifeexp$`Country Name` =="Somalia"],
                        Lifeexp$`2018`[Lifeexp$`Country Name` =="Eritrea"]
                        ))

#############################################################################################################

                                                   ## Year 2002

##############################################################################################################


read_excel("C:/Users/46766/Dropbox/Thesis_2021/Thesis data/Foreign 2002.xls")

For_2002 <- as.data.frame(read_excel("C:/Users/46766/Dropbox/Thesis_2021/Thesis data/Foreign 2002.xls", skip = 6))[-c(1:3),]

Remove<-c(30,38,48,62,seq(67,76),86,95,108,110,116, seq(140,149),160,167,seq(214,223),227,244,257,269,285,seq(287,296),306,314,323,339,354)
Remove<-Remove-3

For_2002<-For_2002[-c(Remove),]

# Col: c(6, 11, 15) = Eu, c(19)=Africa, c(21)=NA, c(23)=SA, c(25)=Asia, c(29)=Oceania

# Reminder: not sorted similarly

For_M_2002<-matrix(data=0, ncol=6, nrow=290, byrow=TRUE)
colnames(For_M_2002)<-c("EU", "Africa", "NA", "SA", "Asia", "Oce")

for (i in seq(1, 290)) {
  
  For_M_2002[i,1]<-sum(as.numeric(For_2002[i, c(6,11,15,28)]))/as.numeric(For_2002[i, 3])
  
} 

for (j in seq(1, 290)) {
  
  For_M_2002[j,2]<-sum(as.numeric(For_2002[j, 19]))/as.numeric(For_2002[j, 3])
  
}

for (z in seq(1, 290)) {
  
  For_M_2002[z,3]<-sum(as.numeric(For_2002[z, 21]))/as.numeric(For_2002[z, 3])
  
} 

for (v in seq(1, 290)) {
  
  For_M_2002[v,4]<-sum(as.numeric(For_2002[v, 23]))/as.numeric(For_2002[v, 3])
  
} 

for (w in seq(1, 290)) {
  
  For_M_2002[w,5]<-(sum(as.numeric(For_2002[w, 25]))-sum(as.numeric(For_2002[w, 28])))/as.numeric(For_2002[w, 3])
  
} 

for (r in seq(1, 290)) {
  
  For_M_2002[r,6]<-sum(as.numeric(For_2002[r, 29]))/as.numeric(For_2002[r, 3])
  
}

For_M_2002<-cbind(For_2002$...2,For_M_2002)

For_M_2002<-cbind(seq(1, 290), For_M_2002)



## Translations - wrong order


New_v<-c(
  
  as.character("Botkyrka"),
  as.character("Danderyd"),
  as.character("Ekera"),
  as.character("Haninge"),
  as.character("Huddinge"),
  as.character("Jarfalla"),
  as.character("Lidinga"),
  as.character("Nacka"),
  as.character("Norrtalje"),
  as.character("Nykvarn"),
  as.character("Nynashamn"),
  as.character("Salem"),
  as.character("Sigtuna"),
  as.character("Sollentuna"),
  as.character("Solna"),
  as.character("Stockholm"),
  as.character("Sundbyberg"),
  as.character("Sadertalje"),
  as.character("Tyresa"),
  as.character("Taby"),
  as.character("Uppland"),
  as.character("Upplands-Bro"),
  as.character("Vallentuna"),
  as.character("Vaxholm"),  
  as.character("Varmda"),
  
  as.character("asteraker"),
  as.character("Enkaping"),
  as.character("Heabo"),
  as.character("Knivsta"),
  as.character("Tierp"),
  as.character("Uppsala"),
  as.character("alvkarleby"),
  as.character("asthammar"),
  as.character("Eskilstuna"),
  as.character("Flen"),
  as.character("Gnesta"),
  as.character("Katrineholm"),
  as.character("Nykaping"),
  as.character("Oxelasund"),
  as.character("Strangnas"),
  as.character("Trosa"),
  as.character("Vingaker"),
  as.character("Boxholm"),
  as.character("Finspang"),
  as.character("Kinda"),
  as.character("Linkaping"),
  as.character("Mjalby"),
  as.character("Motala"),
  as.character("Norrkaping"),
  as.character("Saderkaping"),

  as.character("Vadstena"),
  as.character("Valdemarsvik"),
  as.character("Ydre"),
  as.character("atvidaberg"),
  as.character("adeshag"),
  as.character("Aneby"),
  as.character("Eksja") , 
  as.character("Gislaved"),
  as.character("Gnasja"),
  as.character("Habo"),
  as.character("Jankaping"),
  as.character("Mullsja"),
  as.character("Nassja"),
  as.character("Savsja"),
  as.character("Tranas"),
  as.character("Vaggeryd"),
  as.character("Vetlanda"),
  as.character("Varnamo"),
  as.character("Alvesta"),
  as.character("Lessebo"),
  as.character("Ljungby"),
  as.character("Makaryd"),
  as.character("Tingsryd"),
  as.character("Uppvidinge"),
  as.character("Vaxja"), 
  
  as.character("almhult"),
  as.character("Borgholm"),
  as.character("Emmaboda"), 
  as.character("Hultsfred"),
  as.character("Hagsby"),  
  as.character("Kalmar"),
  as.character("Mansteras"),
  as.character("Marbylanga"),
  as.character("Nybro"),
  as.character("Oskarshamn"),
  as.character("Torsas"),
  as.character("Vimmerby"),
  as.character("Vastervik"),
  as.character("Gotland"),
  as.character("Karlshamn"),
  as.character("Karlskrona"),
  as.character("Olofstram"),
  as.character("Ronneby"),
  as.character("Salvesborg"),
  as.character("Bjuv"),
  as.character("Bromalla"),
  as.character("Burlav"),
  as.character("Bastad"),
  as.character("Eslov"),
  as.character("Helsingborg"),
  
  as.character("Hassleholm"),
  as.character("Haganas"),
  as.character("Harby"),
  as.character("Haar"),
  as.character("Klippan"),
  as.character("Kristianstad"),
  as.character("Kavlinge"),
  as.character("Landskrona"),
  as.character("Lomma"),
  as.character("Lund"),
  as.character("Malma"),
  as.character("Osby"),
  as.character("Perstorp"),
  as.character("Simrishamn"),
  as.character("Sjabo"),
  as.character("Skurup"),
  as.character("Staffanstorp"),
  as.character("Svalav"),
  as.character("Svedala"),
  as.character("Tomelilla"),
  as.character("Trelleborg"),
  as.character("Vellinge"),
  as.character("Ystad"),
  as.character("astorp"),
  as.character("angelholm"),
  as.character("arkelljunga"),
  as.character("astra Gainge"),
  as.character("Falkenberg"),
  as.character("Halmstad"),
  as.character("Hylte"),
  as.character("Kungsbacka"),
  as.character("Laholm"),
  as.character("Varberg"),
  as.character("Ale"),
  as.character("Alingsas"),
  as.character("Bengtsfors"),
  as.character("Bollebygd"),
  as.character("Boras"),
  as.character("Dals-Ed"),
  as.character("Essunga"),
  as.character("Falkoping"),
  as.character("Fargelanda"),
  as.character("Grastorp"),
  as.character("Gullspang"),
  as.character("Goteborg"),
  as.character("Gotene"),
  as.character("Herrljunga"),
  as.character("Hjo"),
  as.character("Harryda"),
  as.character("Karlsborg"),
  
  as.character("Kungalv"),
  as.character("Lerum"),
  as.character("Lidkoping"),
  as.character("Lilla Edet"),
  as.character("Lysekil"),
  as.character("Mariestad"),
  as.character("Mark"),
  as.character("Mellerud"),
  as.character("Munkedal"),
  as.character("Molndal"),
  as.character("Orust"),
  as.character("Partille"),
  as.character("Skara"),
  as.character("Skovde"),
  as.character("Sotenas"),
  as.character("Stenugnsund"),
  as.character("Stromstad"),
  as.character("Svenljunga"),
  as.character("Tanum"),
  as.character("Tibro"),
  as.character("Tidaholm"),
  as.character("Tjorn"),
  as.character("Tranemo"),
  as.character("Trollhattan"),
  as.character("Toreboda"),
  as.character("Uddevalla"),
  as.character("Ullricehamn"),
  as.character("Vara"),
  as.character("Vargarda"),
  as.character("Vanersborg"),
  as.character("amal"),
  as.character("ockero"),
  as.character("Arvika"),
  as.character("Eda"),
  as.character("Filipstad"),
  as.character("Forshaga"),
  as.character("Grums"),
  as.character("Hagfors"),
  as.character("Hammaro"),
  
  as.character("Karlstad"),
  as.character("Kil"),
  as.character("Kristinehamn"),
  as.character("Munkfors"),
  as.character("Storfors"),
  as.character("Sunne"),
  as.character("Saffle"),
  as.character("Torsby"),
  as.character("arjang"),
  as.character("Askersund"),
  as.character("Degerfors"),
  as.character("Hallsberg"),
  as.character("Hallefors"), 
  as.character("Karlskoga"),
  as.character("Kumla"), 
  as.character("Laxa"),
  as.character("Lekeberg"),
  as.character("Lindesberg"),
  as.character("Ljusnarsberg"),
  as.character("Nora"),
  as.character("Orebro"),
  as.character("Arboga"),
  as.character("Fagersta"),
  as.character("Hallstahammar"),
  as.character("Heby"),
  as.character("Kungsor"),
  as.character("Koping"),
  as.character("Norberg"),
  as.character("Sala"),
  as.character("Skinnskatteberg"),
  as.character("Surahammar"),
  as.character("Vasteras"),
  as.character("Avesta"),
  as.character("Borlange"),
  as.character("Falun"),
  as.character("Gagnef"),
  as.character("Hedemora"),
  as.character("Leksand"),
  as.character("Ludvika"),
  as.character("Malung"),
  as.character("Mora"),
  as.character("Orsa"),
  as.character("Rattsvik"),
  as.character("Smedjebacken"),
  as.character("Sater"),
  as.character("Vansbro"),
  as.character("Alvsdalen"),
  as.character("Bollnas"),
  as.character("Gavle"),
  as.character("Hofors"),
  as.character("Hudiksvall"),
  
  as.character("Ljusdal"),
  as.character("Nordanstig"),
  as.character("Ockelbo"),
  as.character("Ovanaker"),
  as.character("Sandviken"),
  as.character("Soderhamn"),
  as.character("Harnosand"),
  as.character("Kramfors"),
  as.character("Solleftea"),
  as.character("Sundsvall"),
  as.character("Timra"),
  as.character("Ange"),
  as.character("Ornskoldsvik"),
  as.character("Berg"),
  as.character("Bracke"),
  as.character("Harjedalen"),
  as.character("Krokom"),
  as.character("Ragunda"),
  as.character("Stromsund"),
  as.character("Are"),
  as.character("Ostersund"),
  as.character("Bjurholm"),
  as.character("Dorotea"),
  as.character("Lyckesele"),
  as.character("Mala"),
  as.character("Nordmaling"),
  as.character("Norsjo"),
  as.character("Robertsfors"),
  as.character("Skelleftea"),
  as.character("Sorsele"),
  as.character("Storuman"),
  as.character("Umea"),
  as.character("Vilhelmina"),
  as.character("Vindeln"),
  as.character("Vannas"),
  as.character("Asele"),
  as.character("Arjeplog"),
  as.character("Arvidsjaur"),
  as.character("Boden"),
  as.character("Gallivare"),
  as.character("Haparanda"),
  as.character("Jokkmokk"),
  as.character("Kalix"),
  as.character("Kiruna"),
  as.character("Lulea"),
  as.character("Pajala"),
  as.character("Pitea"),
  as.character("Alvsbyn"),
  as.character("Overkalix"),
  as.character("Overtornea")
  
  )

For_M_2002[,1]<-New_v

##


## Manual municipalities. 

yr_2000<-rep(0, 290)


for (i in seq(0, 289)) {
  
yr_2000[i+1]<-as.character(thesis[3+(i*20), 2]) #Extracting data from every 20th year.
               

}

For_M_2002<-For_M_2002[match(yr_2000, For_M_2002[, 1]), ] # Sorting to match thesis


## Adding the year of 2002

for (i in seq(0, 289)) {


thesis[3+(i*20), seq(18, 23)]<-as.numeric(For_M_2002[i+1, seq(3, 8)])

      
}

#############################################################################################################

                                            ## Year 2003

##############################################################################################################


For_2003 <- as.data.frame(read_excel("C:/Users/46766/Dropbox/Thesis_2021/Thesis data/Foreign 2003.xls", skip = 6))
For_2003<-For_2003[-c(1:3),]

Remove<-c(30,38,48,62,seq(67,76),86,95,108,110,116, seq(140,149),160,167,seq(214,223),227,244,257,269,285,seq(287,296),306,314,323,339,354)
Remove<-Remove-3


For_2003<-For_2003[-c(Remove),]


# Col: c(6, 11, 15) = Eu, c(19)=Africa, c(21)=NA, c(23)=SA, c(25)=Asia, c(29)=Oceania

# Remineder: not sorted similiarly


For_M_2003<-matrix(data=0, ncol=6, nrow=290, byrow=TRUE)
colnames(For_M_2003)<-c("EU", "Africa", "NA", "SA", "Asia", "Oce")

for (i in seq(1, 290)) {
  
  For_M_2003[i,1]<-sum(as.numeric(For_2003[i, c(6,11,15,28)]))/as.numeric(For_2003[i, 3])
  
} 

for (j in seq(1, 290)) {
  
  For_M_2003[j,2]<-sum(as.numeric(For_2003[j, 19]))/as.numeric(For_2003[j, 3])
  
}

for (z in seq(1, 290)) {
  
  For_M_2003[z,3]<-sum(as.numeric(For_2003[z, 21]))/as.numeric(For_2003[z, 3])
  
} 

for (v in seq(1, 290)) {
  
  For_M_2003[v,4]<-sum(as.numeric(For_2003[v, 23]))/as.numeric(For_2003[v, 3])
  
} 

for (w in seq(1, 290)) {
  
  For_M_2003[w,5]<-(sum(as.numeric(For_2003[w, 25]))-(as.numeric(For_2003[w, 28]))) /as.numeric(For_2003[w, 3])
  
} 

for (r in seq(1, 290)) {
  
  For_M_2003[r,6]<-sum(as.numeric(For_2003[r, 29]))/as.numeric(For_2003[r, 3])
  
}

For_M_2003<-cbind(For_2003$...2,For_M_2003)
For_M_2003<-cbind(seq(1, 290), For_M_2003)

For_M_2003[,1]<-New_v



## Manual municipalities. 

yr_2003<-rep(0, 290)


for (i in seq(0, 289)) {
  
  yr_2003[i+1]<-as.character(thesis[4+(i*20), 2]) #Extracting data from every 20th year.
  
  
}

For_M_2003<-For_M_2003[match(yr_2003, For_M_2003[, 1]), ] # Sorting to match thesis


## Adding the year of 2002


for (i in seq(0, 289)) {
  
  
  thesis[4+(i*20), seq(18, 23)]<-as.numeric(For_M_2003[i+1, seq(3, 8)])
  
  
}

#############################################################################################################

                                   ## Year 2004

##############################################################################################################


For_2004 <- as.data.frame(read_excel("C:/Users/46766/Dropbox/Thesis_2021/Thesis data/Foreign 2004.xls", skip = 6))
For_2004<-For_2004[-c(1:3),]

Remove<-c(30,38,48,62,seq(67,76),86,95,108,110,116, seq(140,149),160,167,seq(214,223),227,244,257,269,285,seq(287,296),306,314,323,339,354)
Remove<-Remove-3


For_2004<-For_2004[-c(Remove),]
For_2004[For_2004 == "-"]<-0

# Col: c(6, 11, 15) = Eu, c(19)=Africa, c(21)=NA, c(23)=SA, c(25)=Asia, c(29)=Oceania

# Remineder: not sorted similiarly


For_M_2004<-matrix(data=0, ncol=6, nrow=290, byrow=TRUE)
colnames(For_M_2004)<-c("EU", "Africa", "NA", "SA", "Asia", "Oce")

for (i in seq(1, 290)) {
  
  For_M_2004[i,1]<-sum(as.numeric(For_2004[i, c(6,11,16)]))/as.numeric(For_2004[i, 3])
  
} 

for (j in seq(1, 290)) {
  
  For_M_2004[j,2]<-sum(as.numeric(For_2004[j, 20]))/as.numeric(For_2004[j, 3])
  
}

for (z in seq(1, 290)) {
  
  For_M_2004[z,3]<-sum(as.numeric(For_2004[z, 22]))/as.numeric(For_2004[z, 3])
  
} 

for (v in seq(1, 290)) {
  
  For_M_2004[v,4]<-sum(as.numeric(For_2004[v, 24]))/as.numeric(For_2004[v, 3])
  
} 

for (w in seq(1, 290)) {
  
  For_M_2004[w,5]<-sum(as.numeric(For_2004[w, 26]))/as.numeric(For_2004[w, 3])
  
} 

for (r in seq(1, 290)) {
  
  For_M_2004[r,6]<-sum(as.numeric(For_2004[r, 29]))/as.numeric(For_2004[r, 3])
  
}

For_M_2004<-cbind(For_2004$...2,For_M_2004)
For_M_2004<-cbind(seq(1, 290), For_M_2004)

For_M_2004[,1]<-New_v



## Manual municipalities. 

yr_2004<-rep(0, 290)


for (i in seq(0, 289)) {
  
  yr_2004[i+1]<-as.character(thesis[4+(i*20), 2]) #Extracting data from every 20th year.
  
  
}

For_M_2004<-For_M_2004[match(yr_2004, For_M_2004[, 1]), ] # Sorting to match thesis


## Adding the year of 2002


for (i in seq(0, 289)) {
  
  
  thesis[5+(i*20), seq(18, 23)]<-as.numeric(For_M_2004[i+1, seq(3, 8)])
  
  
}


#############################################################################################################

                                  ## Year 2005

##############################################################################################################


For_2005 <- as.data.frame(read_excel("C:/Users/46766/Dropbox/Thesis_2021/Thesis data/Foreign 2005.xls", skip = 6))
For_2005<-For_2005[-c(1:4),]


For_2005<-For_2005[-c(which(is.na(For_2005[, 2]) == TRUE)),]
For_2005<-For_2005[-c(which(For_2005[, 2] == "Kommun")),]

For_2005[For_2005 == "-"]<-0

# Col: c(6, 11, 15) = Eu, c(19)=Africa, c(21)=NA, c(23)=SA, c(25)=Asia, c(29)=Oceania

# Remineder: not sorted similiarly


For_M_2005<-matrix(data=0, ncol=6, nrow=290, byrow=TRUE)
colnames(For_M_2005)<-c("EU", "Africa", "NA", "SA", "Asia", "Oce")

for (i in seq(1, 290)) {
  
  For_M_2005[i,1]<-sum(as.numeric(For_2005[i, c(6,11,15)]))/as.numeric(For_2005[i, 3])
  
} 

for (j in seq(1, 290)) {
  
  For_M_2005[j,2]<-sum(as.numeric(For_2005[j, 19]))/as.numeric(For_2005[j, 3])
  
}

for (z in seq(1, 290)) {
  
  For_M_2005[z,3]<-sum(as.numeric(For_2005[z, 20]))/as.numeric(For_2005[z, 3])
  
} 

for (v in seq(1, 290)) {
  
  For_M_2005[v,4]<-sum(as.numeric(For_2005[v, 21]))/as.numeric(For_2005[v, 3])
  
} 

for (w in seq(1, 290)) {
  
  For_M_2005[w,5]<-sum(as.numeric(For_2005[w, 23]))/as.numeric(For_2005[w, 3])
  
} 

for (r in seq(1, 290)) {
  
  For_M_2005[r,6]<-sum(as.numeric(For_2005[r, 29]))/as.numeric(For_2005[r, 3])
  
}

For_M_2005<-cbind(For_2005$...2,For_M_2005)
For_M_2005<-cbind(seq(1, 290), For_M_2005)

For_M_2005[,1]<-New_v



## Manual municipalities. 

yr_2005<-rep(0, 290)


for (i in seq(0, 289)) {
  
  yr_2005[i+1]<-as.character(thesis[4+(i*20), 2]) #Extracting data from every 20th year.
  
  
}

For_M_2005<-For_M_2005[match(yr_2005, For_M_2005[, 1]), ] # Sorting to match thesis


## Adding the year of 2005


for (i in seq(0, 289)) {
  
  
  thesis[6+(i*20), seq(18, 23)]<-as.numeric(For_M_2005[i+1, seq(3, 8)])
  
  
}


#############################################################################################################

                                        ## Year 2006

##############################################################################################################


For_2006 <- as.data.frame(read_excel("C:/Users/46766/Dropbox/Thesis_2021/Thesis data/Foreign 2006.xls", skip = 6))
For_2006<-For_2006[-c(1:4),]


For_2006<-For_2006[-c(which(is.na(For_2006[, 2]) == TRUE)),]
For_2006<-For_2006[-c(which(For_2006[, 2] == "Kommun")),]

For_2006[For_2006 == "-"]<-0

# Col: c(6, 11, 15) = Eu, c(19)=Africa, c(21)=NA, c(23)=SA, c(25)=Asia, c(29)=Oceania

# Remineder: not sorted similiarly


For_M_2006<-matrix(data=0, ncol=6, nrow=290, byrow=TRUE)
colnames(For_M_2006)<-c("EU", "Africa", "NA", "SA", "Asia", "Oce")

for (i in seq(1, 290)) {
  
  For_M_2006[i,1]<-sum(as.numeric(For_2006[i, c(6,11,15)]))/as.numeric(For_2006[i, 3])
  
} 

for (j in seq(1, 290)) {
  
  For_M_2006[j,2]<-sum(as.numeric(For_2006[j, 19]))/as.numeric(For_2006[j, 3])
  
}

for (z in seq(1, 290)) {
  
  For_M_2006[z,3]<-sum(as.numeric(For_2006[z, 21]))/as.numeric(For_2006[z, 3])
  
} 

for (v in seq(1, 290)) {
  
  For_M_2006[v,4]<-sum(as.numeric(For_2006[v, 22]))/as.numeric(For_2006[v, 3])
  
} 

for (w in seq(1, 290)) {
  
  For_M_2006[w,5]<-sum(as.numeric(For_2006[w, 24]))/as.numeric(For_2006[w, 3])
  
} 

for (r in seq(1, 290)) {
  
  For_M_2006[r,6]<-sum(as.numeric(For_2006[r, 29]))/as.numeric(For_2006[r, 3])
  
}

For_M_2006<-cbind(For_2006$...2,For_M_2006)
For_M_2006<-cbind(seq(1, 290), For_M_2006)

For_M_2006[,1]<-New_v



## Manual municipalities. 

yr_2006<-rep(0, 290)


for (i in seq(0, 289)) {
  
  yr_2006[i+1]<-as.character(thesis[4+(i*20), 2]) #Extracting data from every 20th year.
  
  
}

For_M_2006<-For_M_2006[match(yr_2006, For_M_2006[, 1]), ] # Sorting to match thesis


## Adding the year of 2006


for (i in seq(0, 289)) {
  
  
  thesis[7+(i*20), seq(18, 23)]<-as.numeric(For_M_2006[i+1, seq(3, 8)])
  
  
}


#############################################################################################################

                          ## Year 2007

##############################################################################################################


For_2007 <- as.data.frame(read_excel("C:/Users/46766/Dropbox/Thesis_2021/Thesis data/Foreign 2007.xls", skip = 6))
For_2007<-For_2007[-c(1:4),]


For_2007<-For_2007[-c(which(is.na(For_2007[, 2]) == TRUE)),]
For_2007<-For_2007[-c(which(For_2007[, 2] == "Kommun")),]

For_2007[For_2007 == "-"]<-0

colnames(For_2007) == colnames(For_2006)

# Col: c(6, 11, 15) = Eu, c(19)=Africa, c(21)=NA, c(23)=SA, c(25)=Asia, c(29)=Oceania

# Remineder: not sorted similiarly


For_M_2007<-matrix(data=0, ncol=6, nrow=290, byrow=TRUE)
colnames(For_M_2007)<-c("EU", "Africa", "NA", "SA", "Asia", "Oce")

for (i in seq(1, 290)) {
  
  For_M_2007[i,1]<-sum(as.numeric(For_2007[i, c(6,11,15)]))/as.numeric(For_2007[i, 3])
  
} 

for (j in seq(1, 290)) {
  
  For_M_2007[j,2]<-sum(as.numeric(For_2007[j, 19]))/as.numeric(For_2007[j, 3])
  
}

for (z in seq(1, 290)) {
  
  For_M_2007[z,3]<-sum(as.numeric(For_2007[z, 21]))/as.numeric(For_2007[z, 3])
  
} 

for (v in seq(1, 290)) {
  
  For_M_2007[v,4]<-sum(as.numeric(For_2007[v, 22]))/as.numeric(For_2007[v, 3])
  
} 

for (w in seq(1, 290)) {
  
  For_M_2007[w,5]<-sum(as.numeric(For_2007[w, 24]))/as.numeric(For_2007[w, 3])
  
} 

for (r in seq(1, 290)) {
  
  For_M_2007[r,6]<-sum(as.numeric(For_2007[r, 29]))/as.numeric(For_2007[r, 3])
  
}

For_M_2007<-cbind(For_2007$...2,For_M_2007)
For_M_2007<-cbind(seq(1, 290), For_M_2007)

For_M_2007[,1]<-New_v



## Manual municipalities. 

yr_2007<-rep(0, 290)


for (i in seq(0, 289)) {
  
  yr_2007[i+1]<-as.character(thesis[4+(i*20), 2]) #Extracting data from every 20th year.
  
  
}

For_M_2007<-For_M_2007[match(yr_2007, For_M_2007[, 1]), ] # Sorting to match thesis


## Adding the year of 2007


for (i in seq(0, 289)) {
  
  
  thesis[8+(i*20), seq(18, 23)]<-as.numeric(For_M_2007[i+1, seq(3, 8)])
  
  
}


#############################################################################################################

              ## Year 2008

##############################################################################################################


For_2008 <- as.data.frame(read_excel("C:/Users/46766/Dropbox/Thesis_2021/Thesis data/Foreign 2008.xlsx", skip = 8))

For_2008<-For_2008[-c(which(is.na(For_2008[, 2]) == TRUE)),]

For_2008[For_2008 == "-"]<-0

# Col: c(6, 11, 15) = Eu, c(19)=Africa, c(21)=NA, c(23)=SA, c(25)=Asia, c(29)=Oceania

# Remineder: not sorted similiarly


For_M_2008<-matrix(data=0, ncol=6, nrow=290, byrow=TRUE)
colnames(For_M_2008)<-c("EU", "Africa", "NA", "SA", "Asia", "Oce")

for (i in seq(1, 290)) {
  
  For_M_2008[i,1]<-sum(as.numeric(For_2008[i, c(6,11,15)]))/as.numeric(For_2008[i, 3])
  
} 

for (j in seq(1, 290)) {
  
  For_M_2008[j,2]<-sum(as.numeric(For_2008[j, 19]))/as.numeric(For_2008[j, 3])
  
}

for (z in seq(1, 290)) {
  
  For_M_2008[z,3]<-sum(as.numeric(For_2008[z, 21]))/as.numeric(For_2008[z, 3])
  
} 

for (v in seq(1, 290)) {
  
  For_M_2008[v,4]<-sum(as.numeric(For_2008[v, 22]))/as.numeric(For_2008[v, 3])
  
} 

for (w in seq(1, 290)) {
  
  For_M_2008[w,5]<-sum(as.numeric(For_2008[w, 24]))/as.numeric(For_2008[w, 3])
  
} 

for (r in seq(1, 290)) {
  
  For_M_2008[r,6]<-sum(as.numeric(For_2008[r, 29]))/as.numeric(For_2008[r, 3])
  
}

For_M_2008<-cbind(For_2008$...2,For_M_2008)
For_M_2008<-cbind(seq(1, 290), For_M_2008)

For_M_2008[,1]<-New_v



## Manual municipalities. 

yr_2008<-rep(0, 290)


for (i in seq(0, 289)) {
  
  yr_2008[i+1]<-as.character(thesis[4+(i*20), 2]) #Extracting data from every 20th year.
  
  
}

For_M_2008<-For_M_2008[match(yr_2008, For_M_2008[, 1]), ] # Sorting to match thesis


## Adding the year of 2008


for (i in seq(0, 289)) {
  
  
  thesis[9+(i*20), seq(18, 23)]<-as.numeric(For_M_2008[i+1, seq(3, 8)])
  
  
}


#############################################################################################################

             ## Year 2009

##############################################################################################################


For_2009 <- as.data.frame(read_excel("C:/Users/46766/Dropbox/Thesis_2021/Thesis data/Foreign 2009.xlsx", skip = 6))

For_2009<-For_2009[-c(which(is.na(For_2009[, 2]) == TRUE)),]
For_2009<-For_2009[-c(291),]


For_2009[For_2009 == "-"]<-0

# Col: c(6, 11, 15) = Eu, c(19)=Africa, c(21)=NA, c(23)=SA, c(25)=Asia, c(29)=Oceania

# Reminder: not sorted similarly


For_M_2009<-matrix(data=0, ncol=6, nrow=290, byrow=TRUE)
colnames(For_M_2009)<-c("EU", "Africa", "NA", "SA", "Asia", "Oce")

for (i in seq(1, 290)) {
  
  For_M_2009[i,1]<-sum(as.numeric(For_2009[i, c(6,11,14)]))/as.numeric(For_2009[i, 3])
  
} 

for (j in seq(1, 290)) {
  
  For_M_2009[j,2]<-sum(as.numeric(For_2009[j, 18]))/as.numeric(For_2009[j, 3])
  
}

for (z in seq(1, 290)) {
  
  For_M_2009[z,3]<-sum(as.numeric(For_2009[z, 20]))/as.numeric(For_2009[z, 3])
  
} 

for (v in seq(1, 290)) {
  
  For_M_2009[v,4]<-sum(as.numeric(For_2009[v, 21]))/as.numeric(For_2009[v, 3])
  
} 

for (w in seq(1, 290)) {
  
  For_M_2009[w,5]<-sum(as.numeric(For_2009[w, 23]))/as.numeric(For_2009[w, 3])
  
} 

for (r in seq(1, 290)) {
  
  For_M_2009[r,6]<-sum(as.numeric(For_2009[r, 29]))/as.numeric(For_2009[r, 3])
  
}

For_M_2009<-cbind(For_2009$...2,For_M_2009)
For_M_2009<-cbind(seq(1, 290), For_M_2009)

For_M_2009[,1]<-New_v



## Manual municipalities. 

yr_2009<-rep(0, 290)


for (i in seq(0, 289)) {
  
  yr_2009[i+1]<-as.character(thesis[4+(i*20), 2]) #Extracting data from every 20th year.
  
  
}

For_M_2009<-For_M_2009[match(yr_2009, For_M_2009[, 1]), ] # Sorting to match thesis


## Adding the year of 2009


for (i in seq(0, 289)) {
  
  
  thesis[10+(i*20), seq(18, 23)]<-as.numeric(For_M_2009[i+1, seq(3, 8)])
  
  
}

#############################################################################################################

               ## Year 2010

##############################################################################################################


For_2010 <- as.data.frame(read_excel("C:/Users/46766/Dropbox/Thesis_2021/Thesis data/Foreign 2010.xls", skip = 10))

For_2010<-For_2010[-c(which(is.na(For_2010[, 2]) == TRUE)),]
For_2010<-For_2010[-c(291),]


For_2010[For_2010 == "-"]<-0

# Col: c(6, 11, 15) = Eu, c(19)=Africa, c(21)=NA, c(23)=SA, c(25)=Asia, c(29)=Oceania

# Reminder: not sorted similarly


For_M_2010<-matrix(data=0, ncol=6, nrow=290, byrow=TRUE)
colnames(For_M_2010)<-c("EU", "Africa", "NA", "SA", "Asia", "Oce")

for (i in seq(1, 290)) {
  
  For_M_2010[i,1]<-sum(as.numeric(For_2010[i, c(6,11,14)]))/as.numeric(For_2010[i, 3])
  
} 

for (j in seq(1, 290)) {
  
  For_M_2010[j,2]<-sum(as.numeric(For_2010[j, 18]))/as.numeric(For_2010[j, 3])
  
}

for (z in seq(1, 290)) {
  
  For_M_2010[z,3]<-sum(as.numeric(For_2010[z, 20]))/as.numeric(For_2010[z, 3])
  
} 

for (v in seq(1, 290)) {
  
  For_M_2010[v,4]<-sum(as.numeric(For_2010[v, 21]))/as.numeric(For_2010[v, 3])
  
} 

for (w in seq(1, 290)) {
  
  For_M_2010[w,5]<-sum(as.numeric(For_2010[w, 23]))/as.numeric(For_2010[w, 3])
  
} 

for (r in seq(1, 290)) {
  
  For_M_2010[r,6]<-sum(as.numeric(For_2010[r, 29]))/as.numeric(For_2010[r, 3])
  
}

For_M_2010<-cbind(For_2010$...2,For_M_2010)
For_M_2010<-cbind(seq(1, 290), For_M_2010)

For_M_2010[,1]<-New_v



## Manual municipalities. 

yr_2010<-rep(0, 290)


for (i in seq(0, 289)) {
  
  yr_2010[i+1]<-as.character(thesis[4+(i*20), 2]) #Extracting data from every 20th year.
  
  
}

For_M_2010<-For_M_2010[match(yr_2010, For_M_2010[, 1]), ] # Sorting to match thesis


## Adding the year of 2010


for (i in seq(0, 289)) {
  
  
  thesis[11+(i*20), seq(18, 23)]<-as.numeric(For_M_2010[i+1, seq(3, 8)])
  
  
}

#############################################################################################################

       ## Year 2011

##############################################################################################################

For_2011 <- as.data.frame(read_excel("C:/Users/46766/Dropbox/Thesis_2021/Thesis data/Foreign 2011.xlsx", skip = 10))

For_2011<-For_2011[-c(which(is.na(For_2011[, 2]) == TRUE)),]
For_2011<-For_2011[-c(291),]

For_2011[For_2011 == "-"]<-0

# Col: c(6, 11, 15) = Eu, c(19)=Africa, c(21)=NA, c(23)=SA, c(25)=Asia, c(29)=Oceania

# Reminder: not sorted similarly


For_M_2011<-matrix(data=0, ncol=6, nrow=290, byrow=TRUE)
colnames(For_M_2011)<-c("EU", "Africa", "NA", "SA", "Asia", "Oce")

for (i in seq(1, 290)) {
  
  For_M_2011[i,1]<-sum(as.numeric(For_2011[i, c(6,11,14)]))/as.numeric(For_2011[i, 3])
  
} 

for (j in seq(1, 290)) {
  
  For_M_2011[j,2]<-sum(as.numeric(For_2011[j, 18]))/as.numeric(For_2011[j, 3])
  
}

for (z in seq(1, 290)) {
  
  For_M_2011[z,3]<-sum(as.numeric(For_2011[z, 20]))/as.numeric(For_2011[z, 3])
  
} 

for (v in seq(1, 290)) {
  
  For_M_2011[v,4]<-sum(as.numeric(For_2011[v, 21]))/as.numeric(For_2011[v, 3])
  
} 

for (w in seq(1, 290)) {
  
  For_M_2011[w,5]<-sum(as.numeric(For_2011[w, 23]))/as.numeric(For_2011[w, 3])
  
} 

for (r in seq(1, 290)) {
  
  For_M_2011[r,6]<-sum(as.numeric(For_2011[r, 29]))/as.numeric(For_2011[r, 3])
  
}

For_M_2011<-cbind(For_2011$...2,For_M_2011)
For_M_2011<-cbind(seq(1, 290), For_M_2011)

For_M_2011[,1]<-New_v



## Manual municipalities. 

yr_2011<-rep(0, 290)


for (i in seq(0, 289)) {
  
  yr_2011[i+1]<-as.character(thesis[4+(i*20), 2]) #Extracting data from every 20th year.
  
  
}

For_M_2011<-For_M_2011[match(yr_2011, For_M_2011[, 1]), ] # Sorting to match thesis


## Adding the year of 2011


for (i in seq(0, 289)) {
  
  
  thesis[12+(i*20), seq(18, 23)]<-as.numeric(For_M_2011[i+1, seq(3, 8)])
  
  
}

#############################################################################################################

           ## Year 2012

##############################################################################################################

For_2012 <- as.data.frame(read_excel("C:/Users/46766/Dropbox/Thesis_2021/Thesis data/Foreign 2012.xlsx", skip = 10))

For_2012<-For_2012[-c(which(is.na(For_2012[, 2]) == TRUE)),]
For_2012<-For_2012[-c(291),]

For_2012[For_2012 == "-"]<-0

# Col: c(6, 11, 15) = Eu, c(19)=Africa, c(21)=NA, c(23)=SA, c(25)=Asia, c(29)=Oceania

# Reminder: not sorted similarly


For_M_2012<-matrix(data=0, ncol=6, nrow=290, byrow=TRUE)
colnames(For_M_2012)<-c("EU", "Africa", "NA", "SA", "Asia", "Oce")

for (i in seq(1, 290)) {
  
  For_M_2012[i,1]<-sum(as.numeric(For_2012[i, c(6,11,14)]))/as.numeric(For_2012[i, 3])
  
} 

for (j in seq(1, 290)) {
  
  For_M_2012[j,2]<-sum(as.numeric(For_2012[j, 18]))/as.numeric(For_2012[j, 3])
  
}

for (z in seq(1, 290)) {
  
  For_M_2012[z,3]<-sum(as.numeric(For_2012[z, 20]))/as.numeric(For_2012[z, 3])
  
} 

for (v in seq(1, 290)) {
  
  For_M_2012[v,4]<-sum(as.numeric(For_2012[v, 21]))/as.numeric(For_2012[v, 3])
  
} 

for (w in seq(1, 290)) {
  
  For_M_2012[w,5]<-sum(as.numeric(For_2012[w, 23]))/as.numeric(For_2012[w, 3])
  
} 

for (r in seq(1, 290)) {
  
  For_M_2012[r,6]<-sum(as.numeric(For_2012[r, 29]))/as.numeric(For_2012[r, 3])
  
}

For_M_2012<-cbind(For_2012$...2,For_M_2012)
For_M_2012<-cbind(seq(1, 290), For_M_2012)

For_M_2012[,1]<-New_v



## Manual municipalities. 

yr_2012<-rep(0, 290)


for (i in seq(0, 289)) {
  
  yr_2012[i+1]<-as.character(thesis[4+(i*20), 2]) #Extracting data from every 20th year.
  
  
}

For_M_2012<-For_M_2012[match(yr_2012, For_M_2012[, 1]), ] # Sorting to match thesis


## Adding the year of 2012


for (i in seq(0, 289)) {
  
  
  thesis[13+(i*20), seq(18, 23)]<-as.numeric(For_M_2012[i+1, seq(3, 8)])
  
  
}

#############################################################################################################

             ## Year 2013

##############################################################################################################

For_2013 <- as.data.frame(read_excel("C:/Users/46766/Dropbox/Thesis_2021/Thesis data/Foreign 2013.xlsx", skip = 9))

For_2013<-For_2013[-c(which(is.na(For_2013[, 2]) == TRUE)),]

For_2013[For_2013 == "-"]<-0

# Col: c(6, 11, 15) = Eu, c(19)=Africa, c(21)=NA, c(23)=SA, c(25)=Asia, c(29)=Oceania

# Reminder: not sorted similarly


For_M_2013<-matrix(data=0, ncol=6, nrow=290, byrow=TRUE)
colnames(For_M_2013)<-c("EU", "Africa", "NA", "SA", "Asia", "Oce")

for (i in seq(1, 290)) {
  
  For_M_2013[i,1]<-sum(as.numeric(For_2013[i, c(6,11,14)]))/as.numeric(For_2013[i, 3])
  
} 

for (j in seq(1, 290)) {
  
  For_M_2013[j,2]<-sum(as.numeric(For_2013[j, 18]))/as.numeric(For_2013[j, 3])
  
}

for (z in seq(1, 290)) {
  
  For_M_2013[z,3]<-sum(as.numeric(For_2013[z, 20]))/as.numeric(For_2013[z, 3])
  
} 

for (v in seq(1, 290)) {
  
  For_M_2013[v,4]<-sum(as.numeric(For_2013[v, 21]))/as.numeric(For_2013[v, 3])
  
} 

for (w in seq(1, 290)) {
  
  For_M_2013[w,5]<-sum(as.numeric(For_2013[w, 23]))/as.numeric(For_2013[w, 3])
  
} 

for (r in seq(1, 290)) {
  
  For_M_2013[r,6]<-sum(as.numeric(For_2013[r, 29]))/as.numeric(For_2013[r, 3])
  
}

For_M_2013<-cbind(For_2013$...2,For_M_2013)
For_M_2013<-cbind(seq(1, 290), For_M_2013)

For_M_2013[,1]<-New_v



## Manual municipalities. 

yr_2013<-rep(0, 290)


for (i in seq(0, 289)) {
  
  yr_2013[i+1]<-as.character(thesis[4+(i*20), 2]) #Extracting data from every 20th year.
  
  
}

For_M_2013<-For_M_2013[match(yr_2013, For_M_2013[, 1]), ] # Sorting to match thesis


## Adding the year of 2013


for (i in seq(0, 289)) {
  
  
  thesis[14+(i*20), seq(18, 23)]<-as.numeric(For_M_2013[i+1, seq(3, 8)])
  
  
}

#############################################################################################################

       ## Year 2014

##############################################################################################################

For_2014 <- as.data.frame(read_excel("C:/Users/46766/Dropbox/Thesis_2021/Thesis data/Foreign 2014.xlsx", skip = 4))

For_2014<-For_2014[-c(which(is.na(For_2014[, 2]) == TRUE)),]

For_2014[For_2014 == "-"]<-0

# Col: c(6, 11, 15) = Eu, c(19)=Africa, c(21)=NA, c(23)=SA, c(25)=Asia, c(29)=Oceania

# Reminder: not sorted similarly


For_M_2014<-matrix(data=0, ncol=6, nrow=290, byrow=TRUE)
colnames(For_M_2014)<-c("EU", "Africa", "NA", "SA", "Asia", "Oce")

for (i in seq(1, 290)) {
  
  For_M_2014[i,1]<-sum(as.numeric(For_2014[i, c(6,11,14)]))/as.numeric(For_2014[i, 3])
  
} 

for (j in seq(1, 290)) {
  
  For_M_2014[j,2]<-sum(as.numeric(For_2014[j, 18]))/as.numeric(For_2014[j, 3])
  
}

for (z in seq(1, 290)) {
  
  For_M_2014[z,3]<-sum(as.numeric(For_2014[z, 20]))/as.numeric(For_2014[z, 3])
  
} 

for (v in seq(1, 290)) {
  
  For_M_2014[v,4]<-sum(as.numeric(For_2014[v, 21]))/as.numeric(For_2014[v, 3])
  
} 

for (w in seq(1, 290)) {
  
  For_M_2014[w,5]<-sum(as.numeric(For_2014[w, 22]))/as.numeric(For_2014[w, 3])
  
} 

for (r in seq(1, 290)) {
  
  For_M_2014[r,6]<-sum(as.numeric(For_2014[r, 29]))/as.numeric(For_2014[r, 3])
  
}

For_M_2014<-cbind(For_2014$...2,For_M_2014)
For_M_2014<-cbind(seq(1, 290), For_M_2014)

For_M_2014[,1]<-New_v



## Manual municipalities. 

yr_2014<-rep(0, 290)


for (i in seq(0, 289)) {
  
  yr_2014[i+1]<-as.character(thesis[4+(i*20), 2]) #Extracting data from every 20th year.
  
  
}

For_M_2014<-For_M_2014[match(yr_2014, For_M_2014[, 1]), ] # Sorting to match thesis


## Adding the year of 2014


for (i in seq(0, 289)) {
  
  
  thesis[15+(i*20), seq(18, 23)]<-as.numeric(For_M_2014[i+1, seq(3, 8)])
  
  
}

############################################################################################################

            ## Year 2015

##############################################################################################################

For_2015 <- as.data.frame(read_excel("C:/Users/46766/Dropbox/Thesis_2021/Thesis data/Foreign 2015.xlsx", skip = 9))

For_2015<-For_2015[-c(which(is.na(For_2015[, 2]) == TRUE)),]

For_2015[For_2015 == "-"]<-0

# Col: c(6, 11, 15) = Eu, c(19)=Africa, c(21)=NA, c(23)=SA, c(25)=Asia, c(29)=Oceania

# Reminder: not sorted similarly


For_M_2015<-matrix(data=0, ncol=6, nrow=290, byrow=TRUE)
colnames(For_M_2015)<-c("EU", "Africa", "NA", "SA", "Asia", "Oce")

for (i in seq(1, 290)) {
  
  For_M_2015[i,1]<-sum(as.numeric(For_2015[i, c(6,11,14)]))/as.numeric(For_2015[i, 3])
  
} 

for (j in seq(1, 290)) {
  
  For_M_2015[j,2]<-sum(as.numeric(For_2015[j, 18]))/as.numeric(For_2015[j, 3])
  
}

for (z in seq(1, 290)) {
  
  For_M_2015[z,3]<-sum(as.numeric(For_2015[z, 21]))/as.numeric(For_2015[z, 3])
  
} 

for (v in seq(1, 290)) {
  
  For_M_2015[v,4]<-sum(as.numeric(For_2015[v, 20]))/as.numeric(For_2015[v, 3])
  
} 

for (w in seq(1, 290)) {
  
  For_M_2015[w,5]<-sum(as.numeric(For_2015[w, 23]))/as.numeric(For_2015[w, 3])
  
} 

for (r in seq(1, 290)) {
  
  For_M_2015[r,6]<-sum(as.numeric(For_2015[r, 29]))/as.numeric(For_2015[r, 3])
  
}

For_M_2015<-cbind(For_2015$...2,For_M_2015)
For_M_2015<-cbind(seq(1, 290), For_M_2015)

For_M_2015[,1]<-New_v



## Manual municipalities. 

yr_2015<-rep(0, 290)


for (i in seq(0, 289)) {
  
  yr_2015[i+1]<-as.character(thesis[4+(i*20), 2]) #Extracting data from every 20th year.
  
  
}

For_M_2015<-For_M_2015[match(yr_2015, For_M_2015[, 1]), ] # Sorting to match thesis


## Adding the year of 2015


for (i in seq(0, 289)) {
  
  
  thesis[16+(i*20), seq(18, 23)]<-as.numeric(For_M_2015[i+1, seq(3, 8)])
  
  
}

############################################################################################################
 
       ## Year 2016

##############################################################################################################

For_2016 <- as.data.frame(read_excel("C:/Users/46766/Dropbox/Thesis_2021/Thesis data/Foreign 2016.xlsx", skip = 9))

For_2016<-For_2016[-c(which(is.na(For_2016[, 2]) == TRUE)),]

For_2016[For_2016 == "-"]<-0

# Col: c(6, 11, 15) = Eu, c(19)=Africa, c(21)=NA, c(23)=SA, c(25)=Asia, c(29)=Oceania

# Reminder: not sorted similarly


For_M_2016<-matrix(data=0, ncol=6, nrow=290, byrow=TRUE)
colnames(For_M_2016)<-c("EU", "Africa", "NA", "SA", "Asia", "Oce")

for (i in seq(1, 290)) {
  
  For_M_2016[i,1]<-sum(as.numeric(For_2016[i, c(6,11,14)]))/as.numeric(For_2016[i, 3])
  
} 

for (j in seq(1, 290)) {
  
  For_M_2016[j,2]<-sum(as.numeric(For_2016[j, 18]))/as.numeric(For_2016[j, 3])
  
}

for (z in seq(1, 290)) {
  
  For_M_2016[z,3]<-sum(as.numeric(For_2016[z, 21]))/as.numeric(For_2016[z, 3])
  
} 

for (v in seq(1, 290)) {
  
  For_M_2016[v,4]<-sum(as.numeric(For_2016[v, 20]))/as.numeric(For_2016[v, 3])
  
} 

for (w in seq(1, 290)) {
  
  For_M_2016[w,5]<-sum(as.numeric(For_2016[w, 23]))/as.numeric(For_2016[w, 3])
  
} 

for (r in seq(1, 290)) {
  
  For_M_2016[r,6]<-sum(as.numeric(For_2016[r, 29]))/as.numeric(For_2016[r, 3])
  
}

For_M_2016<-cbind(For_2016$...2,For_M_2016)
For_M_2016<-cbind(seq(1, 290), For_M_2016)

For_M_2016[,1]<-New_v



## Manual municipalities. 

yr_2016<-rep(0, 290)


for (i in seq(0, 289)) {
  
  yr_2016[i+1]<-as.character(thesis[4+(i*20), 2]) #Extracting data from every 20th year.
  
  
}

For_M_2016<-For_M_2016[match(yr_2016, For_M_2016[, 1]), ] # Sorting to match thesis


## Adding the year of 2016


for (i in seq(0, 289)) {
  
  
  thesis[17+(i*20), seq(18, 23)]<-as.numeric(For_M_2016[i+1, seq(3, 8)])
  
  
}

############################################################################################################

           ## Year 2017

##############################################################################################################

For_2017 <- as.data.frame(read_excel("C:/Users/46766/Dropbox/Thesis_2021/Thesis data/Foreign 2017.xlsx", skip = 9))

For_2017<-For_2017[-c(which(is.na(For_2017[, 2]) == TRUE)),]

For_2017[For_2017 == "-"]<-0

# Col: c(6, 11, 15) = Eu, c(19)=Africa, c(21)=NA, c(23)=SA, c(25)=Asia, c(29)=Oceania

# Reminder: not sorted similarly


For_M_2017<-matrix(data=0, ncol=6, nrow=290, byrow=TRUE)
colnames(For_M_2017)<-c("EU", "Africa", "NA", "SA", "Asia", "Oce")

for (i in seq(1, 290)) {
  
  For_M_2017[i,1]<-sum(as.numeric(For_2017[i, c(6,11,14)]))/as.numeric(For_2017[i, 3])
  
} 

for (j in seq(1, 290)) {
  
  For_M_2017[j,2]<-sum(as.numeric(For_2017[j, 18]))/as.numeric(For_2017[j, 3])
  
}

for (z in seq(1, 290)) {
  
  For_M_2017[z,3]<-sum(as.numeric(For_2017[z, 21]))/as.numeric(For_2017[z, 3])
  
} 

for (v in seq(1, 290)) {
  
  For_M_2017[v,4]<-sum(as.numeric(For_2017[v, 20]))/as.numeric(For_2017[v, 3])
  
} 

for (w in seq(1, 290)) {
  
  For_M_2017[w,5]<-sum(as.numeric(For_2017[w, 23]))/as.numeric(For_2017[w, 3])
  
} 

for (r in seq(1, 290)) {
  
  For_M_2017[r,6]<-sum(as.numeric(For_2017[r, 29]))/as.numeric(For_2017[r, 3])
  
}

For_M_2017<-cbind(For_2017$...2,For_M_2017)
For_M_2017<-cbind(seq(1, 290), For_M_2017)

For_M_2017[,1]<-New_v



## Manual municipalities. 

yr_2017<-rep(0, 290)


for (i in seq(0, 289)) {
  
  yr_2017[i+1]<-as.character(thesis[4+(i*20), 2]) #Extracting data from every 20th year.
  
  
}

For_M_2017<-For_M_2017[match(yr_2017, For_M_2017[, 1]), ] # Sorting to match thesis


## Adding the year of 2017


for (i in seq(0, 289)) {
  
  
  thesis[18+(i*20), seq(18, 23)]<-as.numeric(For_M_2017[i+1, seq(3, 8)])
  
  
}

############################################################################################################

   ## Year 2018

##############################################################################################################

For_2018 <- as.data.frame(read_excel("C:/Users/46766/Dropbox/Thesis_2021/Thesis data/Foreign 2018.xlsx", skip = 13))

For_2018<-For_2018[-c(which(is.na(For_2018[, 2]) == TRUE)),]
For_2018<-For_2018[,-c(3)]

For_2018<-For_2018[seq(1, 870, 3),]

For_2018[For_2018 == "-"]<-0

# Col: c(6, 11, 15) = Eu, c(19)=Africa, c(21)=NA, c(23)=SA, c(25)=Asia, c(29)=Oceania

# Reminder: not sorted similarly


For_M_2018<-matrix(data=0, ncol=6, nrow=290, byrow=TRUE)
colnames(For_M_2018)<-c("EU", "Africa", "NA", "SA", "Asia", "Oce")

for (i in seq(1, 290)) {
  
  For_M_2018[i,1]<-sum(as.numeric(For_2018[i, c(6,11,14)]))/as.numeric(For_2018[i, 3])
  
} 

for (j in seq(1, 290)) {
  
  For_M_2018[j,2]<-sum(as.numeric(For_2018[j, 18]))/as.numeric(For_2018[j, 3])
  
}

for (z in seq(1, 290)) {
  
  For_M_2018[z,3]<-sum(as.numeric(For_2018[z, 21]))/as.numeric(For_2018[z, 3])
  
} 

for (v in seq(1, 290)) {
  
  For_M_2018[v,4]<-sum(as.numeric(For_2018[v, 20]))/as.numeric(For_2018[v, 3])
  
} 

for (w in seq(1, 290)) {
  
  For_M_2018[w,5]<-sum(as.numeric(For_2018[w, 23]))/as.numeric(For_2018[w, 3])
  
} 

for (r in seq(1, 290)) {
  
  For_M_2018[r,6]<-sum(as.numeric(For_2018[r, 29]))/as.numeric(For_2018[r, 3])
  
}

For_M_2018<-cbind(For_2018$...2,For_M_2018)
For_M_2018<-cbind(seq(1, 290), For_M_2018)

For_M_2018[,1]<-New_v



## Manual municipalities. 

yr_2018<-rep(0, 290)


for (i in seq(0, 289)) {
  
  yr_2018[i+1]<-as.character(thesis[4+(i*20), 2]) #Extracting data from every 20th year.
  
  
}

For_M_2018<-For_M_2018[match(yr_2018, For_M_2018[, 1]), ] # Sorting to match thesis


## Adding the year of 2018


for (i in seq(0, 289)) {
  
  
  thesis[19+(i*20), seq(18, 23)]<-as.numeric(For_M_2018[i+1, seq(3, 8)])
  
  
}

############################################################################################################

  ## Year 2019

##############################################################################################################

For_2019 <- as.data.frame(read_excel("C:/Users/46766/Dropbox/Thesis_2021/Thesis data/Foreign 2019.xlsx", skip = 13))

For_2019<-For_2019[-c(which(is.na(For_2019[, 2]) == TRUE)),]
For_2019<-For_2019[,-c(3)]

For_2019<-For_2019[seq(1, 870, 3),]

For_2019[For_2019 == "-"]<-0

# Col: c(6, 11, 15) = Eu, c(19)=Africa, c(21)=NA, c(23)=SA, c(25)=Asia, c(29)=Oceania

# Reminder: not sorted similarly


For_M_2019<-matrix(data=0, ncol=6, nrow=290, byrow=TRUE)
colnames(For_M_2019)<-c("EU", "Africa", "NA", "SA", "Asia", "Oce")

for (i in seq(1, 290)) {
  
  For_M_2019[i,1]<-sum(as.numeric(For_2019[i, c(6,11,14)]))/as.numeric(For_2019[i, 3])
  
} 

for (j in seq(1, 290)) {
  
  For_M_2019[j,2]<-sum(as.numeric(For_2019[j, 18]))/as.numeric(For_2019[j, 3])
  
}

for (z in seq(1, 290)) {
  
  For_M_2019[z,3]<-sum(as.numeric(For_2019[z, 21]))/as.numeric(For_2019[z, 3])
  
} 

for (v in seq(1, 290)) {
  
  For_M_2019[v,4]<-sum(as.numeric(For_2019[v, 20]))/as.numeric(For_2019[v, 3])
  
} 

for (w in seq(1, 290)) {
  
  For_M_2019[w,5]<-sum(as.numeric(For_2019[w, 23]))/as.numeric(For_2019[w, 3])
  
} 

for (r in seq(1, 290)) {
  
  For_M_2019[r,6]<-sum(as.numeric(For_2019[r, 29]))/as.numeric(For_2019[r, 3])
  
}

For_M_2019<-cbind(For_2019$...2,For_M_2019)
For_M_2019<-cbind(seq(1, 290), For_M_2019)

For_M_2019[,1]<-New_v



## Manual municipalities. 

yr_2019<-rep(0, 290)


for (i in seq(0, 289)) {
  
  yr_2019[i+1]<-as.character(thesis[4+(i*20), 2]) #Extracting data from every 20th year.
  
  
}

For_M_2019<-For_M_2019[match(yr_2019, For_M_2019[, 1]), ] # Sorting to match thesis


## Adding the year of 2019


for (i in seq(0, 289)) {
  
  
  thesis[20+(i*20), seq(18, 23)]<-as.numeric(For_M_2019[i+1, seq(3, 8)])
  
  
}





############################################################################################################

   ## Year 2000-2001

##############################################################################################################



for (i in seq(0,289)) {
  
  a<-rnorm(50, mean=as.numeric(thesis[seq(3,4)+20*i,18])-0.0001, sd=1/600)
  a_2<-sample(a[a>0], 2)
  
  thesis[seq(1,2)+20*i,18]<-a_2
  
}


for (i in seq(0,289)) {
  
  a<-rnorm(50, mean=as.numeric(thesis[seq(3,4)+20*i,19])-0.0001, sd=1/600)
  a_2<-sample(a[a>0], 2)
  
  thesis[seq(1,2)+20*i,19]<-a_2
  
}

for (i in seq(0,289)) {
  
  a<-rnorm(50, mean=as.numeric(thesis[seq(3,4)+20*i,20])-0.0001, sd=1/600)
  a_2<-sample(a[a>0], 2)
  
  thesis[seq(1,2)+20*i,20]<-a_2
  
}

for (i in seq(0,289)) {
  
  a<-rnorm(50, mean=as.numeric(thesis[seq(3,4)+20*i,21])-0.0001, sd=1/600)
  a_2<-sample(a[a>0], 2)
  
  thesis[seq(1,2)+20*i,21]<-a_2
  
}

for (i in seq(0,289)) {
  
  a<-rnorm(50, mean=as.numeric(thesis[seq(3,4)+20*i,22])-0.0001, sd=1/600)
  a_2<-sample(a[a>0], 2)
  
  thesis[seq(1,2)+20*i,22]<-a_2
  
}

a<-rnorm(50, mean=as.numeric(thesis[seq(3,4)+20*i,23])-0.0001, sd=1/600)
a_2<-sample(a[a>0], 2)

for (i in seq(0,289)) {
  
  thesis[seq(1,2)+20*i,23]<-a_2
  
}


## Setting columnames

colnames(thesis)<-c("Year", "Municipality", "Gini", "Q25","Theil","Atkinson", "Meanage", "PF", "High_Educ", "F_M", "Gini_Sim", "Taxes", "Meanwage", "Population", "EU", "Africa", "N_A", "SA", "Asia","Oceanien", "Unemp")

## Exporting data

For_m<-as.matrix(thesis[ , seq(18, 23)])
colnames(For_m)<-c("EU", "Africa", "NA", "SA", "Asia", "Oce")
write.xlsx(For_m, "Foreign new 2") #Write as a matrix.

      
