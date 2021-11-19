

#############################################################################################################

                                ############## Thesis_2021 - Outline ##############

#############################################################################################################

  
                                                   
# 1) Packages

# 2) Data

## 3) Foreign decomposition

# 4) Gini function & Comparison to SCB

# 5) P90/P10  matrix

# 6) P970/P30 matrix

# 6) Education analysis 

# 7) Female/Male ratio

## 8) Thesis dataset

## 9) Figures

## 10) Regressions

###############################################  Thesis_2021   ##############################################

                                          ###### Packages ########

#############################################################################################################

library(Rfast)
library("ineq")
library(readxl)
library(AER)
library("")
library(emg)
library('gamlss.dist')
library(openxlsx)
library(writexl)
library(Rfast)
library(ggplot2)
library(dplyr)
library(rgdal)
library(cartogram)
library(plotly)
library(sf)
library(cartography)
library(pastecs)
library(GB2)
library(e1071)
library(AER)
library(dynlm)
library(forecast)
library(readxl)
library(stargazer)
library(scales)
library(quantmod)
library(urca)
library(stats)
library(plm)
library('sjmisc')

###############################################  Thesis_2021   ##############################################

                                           ###### Loading data ########

#############################################################################################################

 ## From SCB

Number_of_people <- read_excel("C:/Users/46766/Dropbox/Thesis_2021/Thesis data/Number of people.xlsx")

Mean <- as.data.frame(read_excel("C:/Users/46766/Dropbox/Thesis_2021/Thesis data/Mean.xlsx"))

Age <- as.data.frame(read_excel("C:/Users/46766/Dropbox/Thesis_2021//Thesis data/Ålder.xlsx"))

## Removing SCB author information

Number_of_people<-Number_of_people[1:7540,] 
Numb<-Number_of_people[,c(-2)] 

Mean <- Mean[1:7540,]
Mean <- Mean[,c(-2, -3)]

Age <- Age[1:290,]

## Matrix form; intervals averages

Mean_matrix<-as.data.frame(apply(Mean[, seq(5, 24)], 2, as.numeric))

## Matrix form; number of ppl in every interval 

Ppl_matrix<-as.data.frame(apply(Numb[, seq(3, 22)], 2, as.numeric))
Ppl_matrix[is.na(Ppl_matrix) == TRUE] <-0 # Recoding NA as 0.

## Matrix form; mean age

Age_matrix<-as.data.frame(apply(Age[, seq(3, 22)], 2, as.numeric))

###############################################  Thesis_2021   ##############################################

                                       ###### Wage ########

#############################################################################################################


Wage<-function(y, x, z){
 
  Wages<-NULL  
 
  for (i in seq(1, z)) {
    
    Wages<-c(Wages, rep(y[i],  x[i]))
    
  }
  
  return(Wages)
  }
   
# Returns a vector of wages for all income ranges


###############################################  Thesis_2021   ##############################################

                                          ###### Gini matrix ########
 
#############################################################################################################

Municipalities_matrix<-matrix(data=0, nrow=290, ncol=20, byrow=TRUE) #Storage matrix for municipality Gini

##

for (i in seq(1, 20)) {
  

for (j in seq(0,289)) {
  

  
  Municipalities_matrix[j+1, i]<-ineq(Wage(Mean_matrix[(1+j*(26)):(26+j*(26)),i], Ppl_matrix[(1+j*(26)):(26+j*(26)),i], z=26), type = "Gini")
  
  
}
}

###############################################  Thesis_2021   ##############################################

                                         ###### Theil matrix ########

#############################################################################################################

Theil_matrix<-matrix(data=0, nrow=290, ncol=20, byrow=TRUE) #Storage matrix for municipality Gini

##



for (i in seq(1, 20)) {
  
  
  for (j in seq(0,289)) {
    
    
    
    Theil_matrix[j+1, i]<-ineq(Wage(Mean_matrix[(1+j*(26)):(26+j*(26)),i], Ppl_matrix[(1+j*(26)):(26+j*(26)),i]), type = "Theil")
    
    
  }
}

###############################################  Thesis_2021   ##############################################

                                          ###### 4 Range matrix ########

#############################################################################################################

L4_matrix<-matrix(data=0, nrow=290, 20) #Storage matrix for municipality Gini

for (j in seq(0, 289)) {
  
  for (i in seq(1, 20)) {
    
    
    L4_matrix[j+1, i]<-length(Wage_4(Mean_matrix[(1+j*(26)):(4+j*(26)),i], Ppl_matrix[(1+j*(26)):(4+j*(26)),i]))/length(Wage(Mean_matrix[(1+j*(26)):(26+j*(26)),i], Ppl_matrix[(1+j*(26)):(26+j*(26)),i]))
    
    
    
  }
  
}


###############################################  Thesis_2021   ##############################################

                                       ###### 8 Range matrix ########

#############################################################################################################

L8_matrix<-matrix(data=0, nrow=290, 20) #Storage matrix for municipality Gini

for (j in seq(0, 289)) {
  
  for (i in seq(1, 20)) {
    
    
    L8_matrix[j+1, i]<-length(Wage_2(Mean_matrix[(1+j*(26)):(8+j*(26)),i], Ppl_matrix[(1+j*(26)):(8+j*(26)),i]))/length(Wage(Mean_matrix[(1+j*(26)):(26+j*(26)),i], Ppl_matrix[(1+j*(26)):(26+j*(26)),i]))
    
    
    
  }
  
}

###############################################  Thesis_2021   ##############################################

                                    ###### 13 Range matrix ########

#############################################################################################################

Q50_matrix<-matrix(data=0, nrow=290, 20) #Storage matrix for municipality Gini

for (j in seq(0, 289)) {
  
  for (i in seq(1, 20)) {
    
    
    Q50_matrix[j+1, i]<-length(Wage_3(Mean_matrix[(1+j*(26)):(13+j*(26)),i], Ppl_matrix[(1+j*(26)):(13+j*(26)),i]))/length(Wage(Mean_matrix[(1+j*(26)):(26+j*(26)),i], Ppl_matrix[(1+j*(26)):(26+j*(26)),i]))
   
  
    
  }
  
}

###############################################  Thesis_2021   ##############################################

                                   ###### Top Range matrix ########

#############################################################################################################

TOP_matrix<-matrix(data=0, nrow=290, 20) #Storage matrix for municipality Gini

for (j in seq(0, 289)) {
  
  for (i in seq(1, 20)) {
    
    
    TOP_matrix[j+1, i]<-length(Wage_TOP(Mean_matrix[(20+j*(26)):(26+j*(26)),i], Ppl_matrix[(20+j*(26)):(26+j*(26)),i]))/length(Wage(Mean_matrix[(1+j*(26)):(26+j*(26)),i], Ppl_matrix[(1+j*(26)):(26+j*(26)),i]))
    
    
    
  }
  
}

###############################################  Thesis_2021   ##############################################

                                        ###### Skewness matrix ########

#############################################################################################################

skew_m<-matrix(data=0, nrow=290, 20) #Storage matrix for municipality Gini


for (j in seq(0, 289)) {
  
  for (i in seq(1, 20)) {
    
    
    skew_m[j+1, i]<-skewness(Wage(Mean_matrix[(1+j*(26)):(26+j*(26)),i], Ppl_matrix[(1+j*(26)):(26+j*(26)),i]))
    
    
    
  }
  
}

###############################################  Thesis_2021   ##############################################

                                           ###### Mean wage ########

#############################################################################################################

Meanwage_matrix<-matrix(data=0, nrow=290, 20, byrow=TRUE) #Storage matrix for municipality Gini

for (j in seq(0, 289)) {
  
  for (i in seq(1, 20)) {
    
    
    Meanwage_matrix[j+1, i]<-mean(Wage(Mean_matrix[(1+j*(26)):(26+j*(26)),i], Ppl_matrix[(1+j*(26)):(26+j*(26)),i]))
    
  }
  
}

###############################################  Thesis_2021   ##############################################

                                         ###### Unemployment ########

#############################################################################################################

Unemployment <- as.data.frame(read_excel("C:Users/46766/Dropbox/Thesis_2021/Thesis data/Unemployment.xlsx", col_names = FALSE))

Unemp_matrix<-matrix(data=0, ncol=21, nrow=290, byrow=TRUE)

Unemp_matrix[, 1]<-as.character(Unemployment[seq(294,294+289),6 ])  # Names

Unemp_matrix[, 2]<-Unemployment[seq(2,291), 9]          # 2000

Unemp_matrix[, 3]<-Unemployment[seq(2,291),11]          # 2001

Unemp_matrix[, 4]<-Unemployment[seq(2,291),13]          # 2002

Unemp_matrix[, 5]<-Unemployment[seq(2,291),15]          # 2003

Unemp_matrix[, 6]<-Unemployment[seq(294,294+289),7 ]    # 2004

Unemp_matrix[, 7]<-Unemployment[seq(586,586+289),7 ]    # 2005

Unemp_matrix[, 8]<-Unemployment[seq(878,878+289),7 ]    # 2006

Unemp_matrix[, 9]<-Unemployment[seq(1170,1170+289),7 ]  # 2007

Unemp_matrix[, 10]<-Unemployment[seq(1462,1462+289),7 ]  # 2008

Unemp_matrix[, 11]<-Unemployment[seq(1754,1754+289),7 ] # 2009

Unemp_matrix[, 12]<-Unemployment[seq(2046,2046+289),7 ] # 2010

Unemp_matrix[, 13]<-Unemployment[seq(2338,2338+289),7 ] # 2011

Unemp_matrix[, 14]<-Unemployment[seq(2630,2630+289),7 ] # 2012

Unemp_matrix[, 15]<-Unemployment[seq(2922,2922+289),7 ] # 2013

Unemp_matrix[, 16]<-Unemployment[seq(3214,3214+289),7 ] # 2014

Unemp_matrix[, 17]<-Unemployment[seq(3506,3506+289),7 ] # 2015

Unemp_matrix[, 18]<-Unemployment[seq(3798,3798+289),7 ] # 2016

Unemp_matrix[, 19]<-Unemployment[seq(4090,4090+289),7 ] # 2017

Unemp_matrix[, 20]<-Unemployment[seq(4382,4382+289),7 ] # 2018

Unemp_matrix[, 21]<-Unemployment[seq(4674,4674+289),7 ] # 2019

Unemp_frame<-as.data.frame(Unemp_matrix)

## Loading data


Unemp<-rep(0, 5800)

for (i in seq(0, 289)) {
  
  Unemp[seq(1+(20*i),20+(20*i))]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == as.character(unique(Municipality)[i+1]),])
  
}


## Fixing non-matches

Unemp[1:20]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Upplands Väsby",])

Unemp[41:60]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Österåker",])

Unemp[61:80]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Värmdö",])

Unemp[81:100]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Järfälla",])

Unemp[101:120]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Ekerö",])

Unemp[201:220]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Tyresö",])

Unemp[261:280]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Täby",])

Unemp[341:360]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Södertälje",])

Unemp[421:440]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Lidingö",])

Unemp[461:480]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Norrtälje",])

Unemp[501:520]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Nynäshamn",])

Unemp[521:540]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Håbo",])

Unemp[541:560]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Älvkarleby",])

Unemp[641:660]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Enköping",])

Unemp[661:680]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Östhammar",])

Unemp[681:700]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Vingåker",])

Unemp[721:740]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Nyköping",])

Unemp[741:760]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Oxelösund",])

Unemp[821:840]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Strängnäs",])

Unemp[861:880]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Ödeshög",])

Unemp[941:960]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Åtvidaberg",])

Unemp[961:980]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Finspång",])

Unemp[1001:1020]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Linköping",])

Unemp[1021:1040]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Norrköping",])

Unemp[1041:1060]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Söderköping",])

Unemp[1101:1120]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Mjölby",])

Unemp[1141:1160]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Gnosjö",])

Unemp[1161:1180]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Mullsjö",])

Unemp[1241:1260]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Jönköping",])

Unemp[1261:1280]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Nässjö",])

Unemp[1281:1300]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Värnamo",])

Unemp[1301:1320]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Sävsjö",])

Unemp[1341:1360]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Eksjö",])

Unemp[1361:1380]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Tranås",])

Unemp[1461:1480]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Älmhult",])

Unemp[1481:1500]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Markaryd",])

Unemp[1501:1520]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Växjö",])

Unemp[1541:1560]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Högsby",])

Unemp[1561:1580]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Torsås",])

Unemp[1581:1600]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Mörbylånga",])

Unemp[1621:1640]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Mönsterås",])

Unemp[1721:1740]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Västervik",])

Unemp[1801:1820]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Olofström",])

Unemp[1881:1900]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Sölvesborg",])

Unemp[1901:1920]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Svalöv",])

Unemp[1941:1960]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Burlöv",])

Unemp[1981:2000]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Östra Göinge",])

Unemp[2001:2020]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Örkelljunga",])

Unemp[2041:2060]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Kävlinge",])

Unemp[2121:2140]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Sjöbo",])

Unemp[2141:2160]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Hörby",])

Unemp[2161:2180]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Höör",])

Unemp[2201:2220]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Bromölla",])

Unemp[2281:2300]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Åstorp",])

Unemp[2301:2320]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Båstad",])

Unemp[2321:2340]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Malmö",])

Unemp[2401:2420]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Höganäs",])

Unemp[2421:2460]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Eslöv",])

Unemp[2521:2540]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Ängelholm",])

Unemp[2541:2560]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Hässleholm",])

Unemp[2681:2700]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Härryda",])

Unemp[2721:2740]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Öckerö",])

Unemp[2741:2760]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Stenungsund",])

Unemp[2761:2780]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Tjörn",])

Unemp[2801:2820]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Sotenäs",])

Unemp[2881:2900]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Färgelanda",])

Unemp[2941:2960]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Vårgårda",])

Unemp[2981:3000]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Grästorp",])

Unemp[3041:3060]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Gullspång",])

Unemp[3221:3240]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Götene",])

Unemp[3261:3280]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Töreboda",])

Unemp[3281:3300]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Göteborg",])

Unemp[3301:3320]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Mölndal",])

Unemp[3321:3340]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Kungälv",])

Unemp[3381:3400]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Strömstad",])

Unemp[3401:3420]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Vänersborg",])

Unemp[3421:3440]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Trollhättan",])

Unemp[3441:3460]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Alingsås",])

Unemp[3421:3440]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Borås",])

Unemp[3421:3440]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Trollhättan",])

Unemp[3421:3440]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Trollhättan",])

Unemp[3421:3440]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Åmål",])

Unemp[3541:3560]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Lidköping",])

Unemp[3581:3600]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Skövde",])

Unemp[3641:3660]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Falköping",])

Unemp[3741:3760]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Hammarö",])

Unemp[3821:3840]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Årjäng",])

Unemp[3961:3980]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Säffle",])

Unemp[3021:4020]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Laxå",])

Unemp[4061:4080]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Hällefors",])

Unemp[4101:4120]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Örebro",])

Unemp[4261:4280]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Kungsör",])

Unemp[4321:4340]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Västerås",])

Unemp[4381:4400]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Köping",])

Unemp[4441:4460]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Malung-Sälen",])

Unemp[4501:4520]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Rättvik",])

Unemp[4541:4560]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Älvdalen",])

Unemp[4621:4640]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Borlänge",])

Unemp[4641:4660]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Säter",])

Unemp[4761:4780]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Ovanåker",])

Unemp[4821:4840]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Gävle",])

Unemp[4861:4880]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Söderhamn",])

Unemp[4881:4900]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Bollnäs",])

Unemp[4921:4940]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Ånge",])

Unemp[4941:4960]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Timrå",])

Unemp[4961:4980]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Härnösand",])

Unemp[5021:5040]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Sollefteå",])

Unemp[5041:5060]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Örnsköldsvik",])

Unemp[5081:5100]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Bräcke",])

Unemp[5121:5140]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Strömsund",])

Unemp[5141:5160]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Åre",])

Unemp[5181:5200]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Härjedalen",])

Unemp[5201:5220]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Östersund",])

Unemp[5301:5320]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Norsjö",])

Unemp[5321:5340]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Malå",])

Unemp[5401:5420]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Vännäs",])

Unemp[5441:5460]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Åsele",])

Unemp[5461:5480]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Umeå",])

Unemp[5481:5500]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Lycksele",])

Unemp[5501:5520]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Skellefteå",])

Unemp[5581:5600]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Överkalix",])

Unemp[5621:5640]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Övertorneå",])

Unemp[5661:5680]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Gällivare",])

Unemp[5681:5700]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Älvsbyn",])

Unemp[5701:5720]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Luleå",])

Unemp[5721:5740]<-as.numeric(Unemp_frame[,seq(2, 21)][Unemp_frame$V1 == "Piteå",])

###############################################  Thesis_2021   ##############################################

                                    ###### NUmber of foreign-borns ########

#############################################################################################################

Utrikes <- as.matrix(read_excel("C:Users/46766/Dropbox/Thesis_2021/Thesis data/Utrikes.xlsx", skip = 2))
Utrikes<-Utrikes[1:580,]


Proportion_For<-matrix(data=0, nrow=290, ncol=20, byrow=TRUE)

for (j in seq(2, 580, by=2)) {
  
for (i in seq(1, 20)) {
  
Proportion_For[j/2, i]<-as.numeric(Utrikes[j, i+2])/(as.numeric(Utrikes[j-1, i+2])+as.numeric(Utrikes[j, i+2]))

}}

Proportion_For[29, seq(1,2)]<-0.08
 
###############################################  Thesis_2021   ##############################################

                                     ###### Education Analysis ########

#############################################################################################################


Education <- as.data.frame(read_excel("C:/Users/46766/Dropbox/Thesis_2021/Thesis data/Education.xlsx", skip = 2))

Education<-Education[1:2030, ]


High<-matrix(data=0, nrow=290, ncol=20, byrow = TRUE)

for (j in seq(0, 289)) {
  for (i in seq(3, 22)) {
    
    
    
    High[j+1, i-2]<-sum(Education[(6+7*j):(7+7*j), i])/sum(Education[(1+7*j):(7+7*j), i])
    
  }
}



High[29, 1]<-0.19
High[29, 2]<-0.195

###############################################  Thesis_2021   ##############################################

                                     ###### Gender distribution ########

#############################################################################################################

Genderdistribution <- as.data.frame(read_excel("C:/Users/46766/Dropbox/Thesis_2021/Thesis data/Könfördelning.xlsx", skip = 2))

Genderdistribution<-Genderdistribution[1:580,]

Genderdistribution[57,5]<-6200
Genderdistribution[57,6]<-6280

Genderdistribution[58,5]<-6250
Genderdistribution[58,6]<-6330

#col 5-24

F_to_M<-matrix(data=0, nrow=290, ncol=20, byrow=TRUE) #Storage matrix for female to male ratio

for (j in seq(2, 580, by=2)) {
  
  
  
  for (i in seq(1, 20)) {
    
    
    
    F_to_M[j/2, i]<- as.numeric(Genderdistribution[j, i+4])/as.numeric(Genderdistribution[j-1, i+4])
    
    
  }
  
}

###############################################  Thesis_2021   ##############################################
 
                                         ###### Tax rates ########

#############################################################################################################


Tax_rates <- as.data.frame(read_excel("C:/Users/46766/Dropbox/Thesis_2021/Thesis data/Tax rates.xlsx", skip = 2))
Tax_rates[29, c(2:4)]<-c(30,30,30)
Tax_matrix<-Tax_rates[c(1:290),c(2:21)]

Tax<-as.numeric(as.vector(t(Tax_matrix)))


###############################################  Thesis_2021   ##############################################

                                      ###### Size of population ########

#############################################################################################################

Population_matrix<-matrix(data=0, nrow=290, 20, byrow=TRUE) #Storage matrix for municipality Gini


for (j in seq(0, 289)) {
  
  for (i in seq(1, 20)) {
    
    
    Population_matrix[j+1, i]<-sum(Ppl_matrix[(1+j*(26)):(26+j*(26)),i])
    
  }
  
}


###############################################  Thesis_2021   ##############################################

                                        ###### THESIS DATASET ########

#############################################################################################################


yrs<-as.character(c("2000", "2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014",
               "2015","2016","2017","2018","2019"))

Year<-factor(rep(yrs, 290)) # Repeat year for 287 municipalities.

thesis<-data.frame(Year)# Creating thesis dataset /w year var.

## Adding municipalities 

Municipality<-factor(
  
  c(
    rep(as.character(x="Uppland"), 20),
    rep(as.character(x="Vallentuna"), 20),
    rep(as.character(x="asteraker"), 20),
    rep(as.character(x="Varmda"), 20),
    rep(as.character(x="Jarfalla"), 20),
    rep(as.character(x="Ekera"), 20), 
    rep(as.character(x="Huddinge"), 20),
    rep(as.character(x="Botkyrka"), 20), 
    rep(as.character(x="Salem"), 20),
    rep(as.character(x="Haninge"), 20),
    rep(as.character(x="Tyresa"), 20), 
    rep(as.character(x="Upplands-Bro"), 20), 
    rep(as.character(x="Nykvarn"), 20), 
    rep(as.character(x="Taby"), 20), 
    rep(as.character(x="Danderyd"), 20), 
    rep(as.character(x="Sollentuna"), 20), 
    rep(as.character(x="Stockholm"), 20), 
    rep(as.character(x="Sadertalje"), 20), 
    rep(as.character(x="Nacka"), 20),
    rep(as.character(x="Sundbyberg"), 20),
    rep(as.character(x="Solna"), 20), 
    rep(as.character(x="Lidinga"), 20), 
    rep(as.character(x="Vaxholm"), 20),
    rep(as.character(x="Norrtalje"), 20), 
    rep(as.character(x="Sigtuna"), 20), 
    rep(as.character(x="Nynashamn"), 20), 
    rep(as.character(x="Heabo"), 20), 
    rep(as.character(x="alvkarleby"), 20), 
    rep(as.character(x="Knivsta"), 20), 
    rep(as.character(x="Heby"), 20), 
    rep(as.character(x="Tierp"), 20),
    rep(as.character(x="Uppsala"), 20),
    rep(as.character(x="Enkaping"), 20),
    rep(as.character(x="asthammar"), 20),
    rep(as.character(x="Vingaker"), 20),
    rep(as.character(x="Gnesta"), 20),
    rep(as.character(x="Nykaping"), 20),
    rep(as.character(x="Oxelasund"), 20),
    rep(as.character(x="Flen"), 20),
    rep(as.character(x="Katrineholm"), 20),
    rep(as.character(x="Eskilstuna"), 20),
    rep(as.character(x="Strangnas"), 20),
    rep(as.character(x="Trosa"), 20),
    rep(as.character(x="adeshag"), 20),
    rep(as.character(x="Ydre"), 20),
    rep(as.character(x="Kinda"), 20),
    rep(as.character(x="Boxholm"), 20),
    rep(as.character(x="atvidaberg"), 20),
    rep(as.character(x="Finspang"), 20),
    rep(as.character(x="Valdemarsvik"), 20),
    rep(as.character(x="Linkaping"), 20),
    rep(as.character(x="Norrkaping"), 20),
    rep(as.character(x="Saderkaping"), 20),
    rep(as.character(x="Motala"), 20),
    rep(as.character(x="Vadstena"), 20),
    rep(as.character(x="Mjalby"), 20),
    rep(as.character(x="Aneby"), 20),
    rep(as.character(x="Gnasja"), 20),
    rep(as.character(x="Mullsja"), 20),
    rep(as.character(x="Habo"), 20),
    rep(as.character(x="Gislaved"), 20),
    rep(as.character(x="Vaggeryd"), 20),
    rep(as.character(x="Jankaping"), 20),
    rep(as.character(x="Nassja"), 20),
    rep(as.character(x="Varnamo"), 20),
    rep(as.character(x="Savsja"), 20),
    rep(as.character(x="Vetlanda"), 20),
    rep(as.character(x="Eksja"), 20),
    rep(as.character(x="Tranas"), 20),
    rep(as.character(x="Uppvidinge"), 20),
    rep(as.character(x="Lessebo"), 20),
    rep(as.character(x="Tingsryd"), 20),
    rep(as.character(x="Alvesta"), 20),
    rep(as.character(x="almhult"), 20),
    rep(as.character(x="Makaryd"), 20),
    rep(as.character(x="Vaxja"), 20),
    rep(as.character(x="Ljungby"), 20),
    rep(as.character(x="Hagsby"), 20),
    rep(as.character(x="Torsas"), 20),
    rep(as.character(x="Marbylanga"), 20),
    rep(as.character(x="Hultsfred"), 20),
    rep(as.character(x="Mansteras"), 20),
    rep(as.character(x="Emmaboda"), 20),
    rep(as.character(x="Kalmar"), 20),
    rep(as.character(x="Nybro"), 20),
    rep(as.character(x="Oskarshamn"), 20),
    rep(as.character(x="Vastervik"), 20),
    rep(as.character(x="Vimmerby"), 20),
    rep(as.character(x="Borgholm"), 20),
    rep(as.character(x="Gotland"), 20),
    rep(as.character(x="Olofstram"), 20),
    rep(as.character(x="Karlskrona"), 20),
    rep(as.character(x="Ronneby"), 20),
    rep(as.character(x="Karlshamn"), 20),
    rep(as.character(x="Salvesborg"), 20),
    rep(as.character(x="Svalav"), 20),
    rep(as.character(x="Staffanstorp"), 20),
    rep(as.character(x="Burlav"), 20),
    rep(as.character(x="Vellinge"), 20),
    rep(as.character(x="astra Gainge"), 20),
    rep(as.character(x="arkelljunga"), 20),
    rep(as.character(x="Bjuv"), 20),
    rep(as.character(x="Kavlinge"), 20),
    rep(as.character(x="Lomma"), 20),
    rep(as.character(x="Svedala"), 20),
    rep(as.character(x="Skurup"), 20),
    rep(as.character(x="Sjabo"), 20),
    rep(as.character(x="Harby"), 20),
    rep(as.character(x="Haar"), 20),
    rep(as.character(x="Tomelilla"), 20),
    rep(as.character(x="Bromalla"), 20),
    rep(as.character(x="Osby"), 20),
    rep(as.character(x="Perstorp"), 20),
    rep(as.character(x="Klippan"), 20),
    rep(as.character(x="astorp"), 20),
    rep(as.character(x="Bastad"), 20),
    rep(as.character(x="Malma"), 20),
    rep(as.character(x="Lund"), 20),
    rep(as.character(x="Landskrona"), 20),
    rep(as.character(x="Helsingborg"), 20),
    rep(as.character(x="Haganas"), 20),
    rep(as.character(x="Eslov"), 20),
    rep(as.character(x="Ystad"), 20),
    rep(as.character(x="Trelleborg"), 20),
    rep(as.character(x="Kristianstad"), 20),
    rep(as.character(x="Simrishamn"), 20),
    rep(as.character(x="angelholm"), 20),
    rep(as.character(x="Hassleholm"), 20),
    rep(as.character(x="Hylte"), 20),
    rep(as.character(x="Halmstad"), 20),
    rep(as.character(x="Laholm"), 20),
    rep(as.character(x="Falkenberg"), 20),
    rep(as.character(x="Varberg"), 20),
    rep(as.character(x="Kungsbacka"), 20),
    rep(as.character(x="Harryda"), 20),
    rep(as.character(x="Partille"), 20),
    rep(as.character(x="ockero"), 20),
    rep(as.character(x="Stenugnsund"), 20),
    rep(as.character(x="Tjorn"), 20),
    rep(as.character(x="Orust"), 20),
    rep(as.character(x="Sotenas"), 20),
    rep(as.character(x="Munkedal"), 20),
    rep(as.character(x="Tanum"), 20),
    rep(as.character(x="Dals-Ed"), 20),
    rep(as.character(x="Fargelanda"), 20),
    rep(as.character(x="Ale"), 20),
    rep(as.character(x="Lerum"), 20),
    rep(as.character(x="Vargarda"), 20),
    rep(as.character(x="Bollebygd"), 20),
    rep(as.character(x="Grastorp"), 20),
    rep(as.character(x="Essunga"), 20),
    rep(as.character(x="Karlsborg"), 20),
    rep(as.character(x="Gullspang"), 20),
    rep(as.character(x="Tranemo"), 20),
    rep(as.character(x="Bengtsfors"), 20),
    rep(as.character(x="Mellerud"), 20),
    rep(as.character(x="Lilla Edet"), 20),
    rep(as.character(x="Mark"), 20),
    rep(as.character(x="Svenljunga"), 20),
    rep(as.character(x="Herrljunga"), 20),
    rep(as.character(x="Vara"), 20),
    rep(as.character(x="Gotene"), 20),
    rep(as.character(x="Tibro"), 20),
    rep(as.character(x="Toreboda"), 20),
    rep(as.character(x="Goteborg"), 20),
    rep(as.character(x="Molndal"), 20),
    rep(as.character(x="Kungalv"), 20),
    rep(as.character(x="Lysekil"), 20),
    rep(as.character(x="Uddevalla"), 20),
    rep(as.character(x="Stromstad"), 20),
    rep(as.character(x="Vanersborg"), 20),
    rep(as.character(x="Trollhattan"), 20),
    rep(as.character(x="Alingsas"), 20),
    rep(as.character(x="Boras"), 20),
    rep(as.character(x="Ullricehamn"), 20),
    rep(as.character(x="amal"), 20),
    rep(as.character(x="Mariestad"), 20),
    rep(as.character(x="Lidkoping"), 20),
    rep(as.character(x="Skara"), 20),
    rep(as.character(x="Skovde"), 20),
    rep(as.character(x="Hjo"), 20),
    rep(as.character(x="Tidaholm"), 20),
    rep(as.character(x="Falkoping"), 20),
    rep(as.character(x="Kil"), 20),
    rep(as.character(x="Eda"), 20),
    rep(as.character(x="Torsby"), 20),
    rep(as.character(x="Storfors"), 20),
    rep(as.character(x="Hammaro"), 20),
    rep(as.character(x="Munkfors"), 20),
    rep(as.character(x="Forshaga"), 20),
    rep(as.character(x="Grums"), 20),
    rep(as.character(x="arjang"), 20),
    rep(as.character(x="Sunne"), 20),
    rep(as.character(x="Karlstad"), 20),
    rep(as.character(x="Kristinehamn"), 20),
    rep(as.character(x="Filipstad"), 20),
    rep(as.character(x="Hagfors"), 20),
    rep(as.character(x="Arvika"), 20),
    rep(as.character(x="Saffle"), 20),
    rep(as.character(x="Lekeberg"), 20),
    rep(as.character(x="Laxa"), 20),
    rep(as.character(x="Hallsberg"), 20),
    rep(as.character(x="Degerfors"), 20),
    rep(as.character(x="Hallefors"), 20),
    rep(as.character(x="Ljusnarsberg"), 20),
    rep(as.character(x="Orebro"), 20),
    rep(as.character(x="Kumla"), 20),
    rep(as.character(x="Askersund"), 20),
    rep(as.character(x="Karlskoga"), 20),
    rep(as.character(x="Nora"), 20),
    rep(as.character(x="Lindesberg"), 20),
    rep(as.character(x="Skinnskatteberg"), 20),
    rep(as.character(x="Surahammar"), 20),
    rep(as.character(x="Kungsor"), 20),
    rep(as.character(x="Hallstahammar"), 20),
    rep(as.character(x="Norberg"), 20),
    rep(as.character(x="Vasteras"), 20),
    rep(as.character(x="Sala"), 20),
    rep(as.character(x="Fagersta"), 20),
    rep(as.character(x="Koping"), 20),
    rep(as.character(x="Arboga"), 20),
    rep(as.character(x="Vansbro"), 20),
    rep(as.character(x="Malung"), 20),
    rep(as.character(x="Gagnef"), 20),
    rep(as.character(x="Leksand"), 20),
    rep(as.character(x="Rattsvik"), 20),
    rep(as.character(x="Orsa"), 20),
    rep(as.character(x="Alvsdalen"), 20),
    rep(as.character(x="Smedjebacken"), 20),
    rep(as.character(x="Mora"), 20),
    rep(as.character(x="Falun"), 20),
    rep(as.character(x="Borlange"), 20),
    rep(as.character(x="Sater"), 20),
    rep(as.character(x="Hedemora"), 20),
    rep(as.character(x="Avesta"), 20),
    rep(as.character(x="Ludvika"), 20),
    rep(as.character(x="Ockelbo"), 20),
    rep(as.character(x="Hofors"), 20),
    rep(as.character(x="Ovanaker"), 20),
    rep(as.character(x="Nordanstig"), 20),
    rep(as.character(x="Ljusdal"), 20),
    rep(as.character(x="Gavle"), 20),
    rep(as.character(x="Sandviken"), 20),
    rep(as.character(x="Soderhamn"), 20),
    rep(as.character(x="Bollnas"), 20),
    rep(as.character(x="Hudiksvall"), 20),
    rep(as.character(x="Ange"), 20),
    rep(as.character(x="Timra"), 20), 
    rep(as.character(x="Harnosand"), 20),
    rep(as.character(x="Sundsvall"), 20),
    rep(as.character(x="Kramfors"), 20),
    rep(as.character(x="Solleftea"), 20),
    rep(as.character(x="Ornskoldsvik"), 20),
    rep(as.character(x="Ragunda"), 20),
    rep(as.character(x="Bracke"), 20),
    rep(as.character(x="Krokom"), 20),
    rep(as.character(x="Stromsund"), 20),
    rep(as.character(x="Are"), 20),
    rep(as.character(x="Berg"), 20),
    rep(as.character(x="Harjedalen"), 20),
    rep(as.character(x="Ostersund"), 20),
    rep(as.character(x="Nordmaling"), 20),
    rep(as.character(x="Bjurholm"), 20),
    rep(as.character(x="Vindeln"), 20),
    rep(as.character(x="Robertsfors"), 20),
    rep(as.character(x="Norsjo"), 20),
    rep(as.character(x="Mala"), 20),
    rep(as.character(x="Storuman"), 20),
    rep(as.character(x="Sorsele"), 20),
    rep(as.character(x="Dorotea"), 20),
    rep(as.character(x="Vannas"), 20),
    rep(as.character(x="Vilhelmina"), 20), ## WOrks
    rep(as.character(x="Asele"), 20),
    rep(as.character(x="Umea"), 20),
    rep(as.character(x="Lyckesele"), 20),
    rep(as.character(x="Skelleftea"), 20),
    rep(as.character(x="Arvidsjaur"), 20),
    rep(as.character(x="Arjeplog"), 20),
    rep(as.character(x="Jokkmokk"), 20),
    rep(as.character(x="Overkalix"), 20),
    rep(as.character(x="Kalix"), 20),
    rep(as.character(x="Overtornea"), 20),
    rep(as.character(x="Pajala"), 20),
    rep(as.character(x="Gallivare"), 20),
    rep(as.character(x="Alvsbyn"), 20),
    rep(as.character(x="Lulea"), 20),
    rep(as.character(x="Pitea"), 20),
    rep(as.character(x="Boden"), 20),
    rep(as.character(x="Haparanda"), 20),
    rep(as.character(x="Kiruna"), 20)
    
  ))

thesis$Municipality<-Municipality ##Adding municpalities

## Adding Gini (real)

thesis$Gini<-as.vector(t(Municipalities_matrix))

## Adding Theil 

thesis$Theil<-as.vector(t(Theil_matrix))

## Adding LTOp 

thesis$L4<-as.vector(t(L4_matrix))

## Adding L4 

thesis$L4<-as.vector(t(L4_matrix))

## Adding L8 

thesis$L8<-as.vector(t(L8_matrix))

## Adding L13 

thesis$L13<-as.vector(t(Q50_matrix))

## Adding Skewness 

thesis$Skewness<-as.vector(t(skew_m))

## Adding mean age for every municipality

thesis$Age<-as.vector(t(Age_matrix))

## Adding proportion of foreign borns

thesis$PF<-as.vector(t(Proportion_For)) ##Turn to vector

## Adding proportion of highly educated

thesis$High_Educ<-as.vector(t(High)) ##Turn to vector

## Adding female to male ratio

thesis$F_M<-as.vector(t(F_to_M)) ##Turn to vector

## Adding simulated values

thesis$Sim_Gini<-as.vector(t(read_excel("Non Grouped_GIni")))

Mean_simGini<-rep(0, 20)

for (i in  seq(2000, 2019)) {
  
  Mean_simGini[i-1999]<-mean(thesis$Sim_Gini[thesis$Year ==i])
  
}

## Adding simulated grouped values

thesis$Sim_Gini_Group<-as.vector(t(read_excel("Grouped_Gini")))

mean_2<-rep(0, 20)


for (i in  seq(2000, 2019)) {
  
  mean_2[i-1999]<-mean(thesis$Sim_Gini_Group[thesis$Year ==i])
  
}


plot(Mean_simGini, ylim=c(min(mean_2), max(Mean_simGini)))
lines(Mean_simGini)
points(mean_2)
lines(mean_2)

Stat_f(thesis$Sim_Gini-thesis$Sim_Gini_Group)

mean(((thesis$Sim_Gini_Group-thesis$Sim_Gini)/thesis$Sim_Gini)*100)


## Adding  Taxes

thesis$Taxes<-Tax/100

## Adding mean wage

thesis$Meanwage<-as.vector(t(Meanwage_matrix))

## Adding population size

thesis$Population<-as.vector(t(Population_matrix))


## Adding EU

thesis$EU<-as.data.frame(read_excel("Foreign new 2"))[, 1]

## Adding Africa

thesis$Africa<-as.data.frame(read_excel("Foreign new 2"))[, 2]

## Adding NA

thesis$N_A<-as.data.frame(read_excel("Foreign new 2"))[, 3]

## Adding SA

thesis$S_A<-as.data.frame(read_excel("Foreign new 2"))[, 4]

## Adding Asia

thesis$Asia<-as.data.frame(read_excel("Foreign new 2"))[, 5]

## Adding Oce

thesis$Oce<-as.data.frame(read_excel("Foreign new 2"))[, 6]

## Adding Non-EU

thesis$Non_Eu<-thesis[, 18]+thesis[, 19]+thesis[, 20]+thesis[, 21]

## Adding Unemployment

thesis$Unemp<-Unemp/thesis$Population


###############################################  Thesis_2021   ##############################################

                                ###### Absolute change analysis ########

#############################################################################################################

thesis<-thesis[, seq(1, 24)]

s<-rep(1, 5800)

Change_frame<-data.frame(s,s,s,s,s, s, s, s, s, s,s, s, s)


for (j in seq(0, 289)) {
  
  for (i in seq(1, 20)) {
    
    Change_frame[i+(j*20), seq(1,12)]<-100*(as.numeric((thesis[(i)+(j*20), c(seq(3, 7),9,seq(17,23))]))/(as.numeric(thesis[1+(j*20), c(seq(3, 7),9,seq(17,23))])))
    
  }
}


colnames(Change_frame)<-colnames(thesis[1+(1*20), c(seq(3, 7),9,seq(17,23))])

## Adding changes

thesis$Gini_Change<-Change_frame[, 1]
thesis$Theil_Change<-Change_frame[, 2]
thesis$L8_Change<-Change_frame[, 3]
thesis$L13_Change<-Change_frame[, 4]
thesis$Skewness_Change<-Change_frame[, 5]
thesis$PFB_Change<-Change_frame[, 6]
thesis$EU_Change<-Change_frame[, 7]
thesis$Africa_Change<-Change_frame[, 8]
thesis$Asia_Change<-Change_frame[, 11]
thesis$Oce_Change<-Change_frame[, 12]
thesis$NA_Change<-Change_frame[, 9]
thesis$SA_Change<-Change_frame[, 10]
thesis$Non_Eu_Change<-Change_frame[, 13]

## Saving the data for quicker access.

install.packages("xlsx")
library("xlsx")

write.xlsx(thesis, "FINAL THESIS DATA")
