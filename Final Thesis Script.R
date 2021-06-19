
#############################################################################################################

                       ############## Packages ##############

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
library(reshape2)

#############################################################################################################

                        ############## Data frame ##############

#############################################################################################################

# Inputting the data

Thesis <-as.data.frame(read_excel("C:/Users/46766/Dropbox/R code/FINAL THESIS DATA"))

###############################################  Thesis_2021   ##############################################

                        ###### Figures & Descriptive ########

#############################################################################################################

## Created Figure for Gothenburg@2000

plot(density(Wage(Mean_matrix[(1+164*(26)):(26+164*(26)),1], Ppl_matrix[(1+164*(26)):(26+164*(26)),1], z=26)))

length(Wage(Mean_matrix[(1+164*(26)):(26+164*(26)),1], Ppl_matrix[(1+164*(26)):(26+164*(26)),1], z=26))

ggplot(, aes(Wage_sim(Mean_matrix[(1+164*(26)):(26+164*(26)),1], Ppl_matrix[(1+164*(26)):(26+164*(26)),1])))+geom_density()+
  xlim(-100,1800)+geom_density(size = 0.6)+theme_bw()+xlab("Yearly income")+ylab("Density")+scale_colour_brewer()+scale_linetype_manual(values=c("twodash", "solid"))

## Trends for dependant variables - section 3

Gini_Trend<-rep(0, 20)
Theil_Trend<-rep(0, 20)
L4_Trend<-rep(0, 20)
L8_Trend<-rep(0, 20)
L13_Trend<-rep(0, 20)
Skewness_Trend<-rep(0, 20)

for (i in seq(1, 20)) {
  
  Gini_Trend[i]<-mean(Thesis$Gini[Thesis$Year == i+1999])
  Theil_Trend[i]<-mean(Thesis$Theil[Thesis$Year == i+1999])
  L4_Trend[i]<-mean(Thesis$L4[Thesis$Year == i+1999])
  L8_Trend[i]<-mean(Thesis$L8[Thesis$Year == i+1999])
  L13_Trend[i]<-mean(Thesis$L13[Thesis$Year == i+1999])
  Skewness_Trend[i]<-mean(Thesis$Skewness[Thesis$Year == i+1999])
  
}

Gini_Trend<-Gini_Trend/sum(Gini_Trend)
Theil_Trend<-Theil_Trend/sum(Theil_Trend)
L4_Trend<-L4_Trend/sum(L4_Trend)
L8_Trend<-L8_Trend/sum(L8_Trend)
L13_Trend<-L13_Trend/sum(L13_Trend)
Skewness_Trend<-Skewness_Trend/sum(Skewness_Trend)

#Plot frame
Trend_frame<-data.frame(c(rep(seq(2000, 2019),6)),c(Gini_Trend, Theil_Trend, L4_Trend, L8_Trend, L13_Trend, Skewness_Trend), factor(c(rep("Gini", 20),rep("Theil", 20),rep("L4", 20),rep("L8", 20),rep("L13", 20),rep("Skewness", 20))))
colnames(Trend_frame)<-c("Year","Value", "Index")

#Mapping
ggplot(Trend_frame)+ geom_line(mapping=aes(x=Year, y=Value, color=Index))+theme_bw()

## Trends for independant variables - section 3.2

PFB_Trend<-rep(0, 20)
Educ_Trend<-rep(0, 20)
FM_Trend<-rep(0, 20)
Age_Trend<-rep(0, 20)
Wage_Trend<-rep(0, 20)
Unemp_Trend<-rep(0, 20)

for (i in seq(1, 20)) {
  
  PFB_Trend[i]<-mean(Thesis$PF[Thesis$Year == i+1999])
  Educ_Trend[i]<-mean(Thesis$High_Educ[Thesis$Year == i+1999])
  FM_Trend[i]<-mean(Thesis$F_M[Thesis$Year == i+1999])
  Age_Trend[i]<-mean(Thesis$Age[Thesis$Year == i+1999])
  Wage_Trend[i]<-mean(Thesis$Meanwage[Thesis$Year == i+1999])
  Unemp_Trend[i]<-mean(Thesis$Unemp[Thesis$Year == i+1999])
  
}

PFB_Trend<-PFB_Trend/sum(PFB_Trend)
Educ_Trend<-Educ_Trend/sum(Educ_Trend)
FM_Trend<-FM_Trend/sum(FM_Trend)
Age_Trend<-Age_Trend/sum(Age_Trend)
Wage_Trend<-Wage_Trend/sum(Wage_Trend)
Unemp_Trend<-Unemp_Trend/sum(Unemp_Trend)

#Plot frame
Trend_frame_2<-data.frame(c(rep(seq(2000, 2019),6)),c(PFB_Trend, Educ_Trend, FM_Trend, Age_Trend, Wage_Trend, Unemp_Trend), factor(c(rep("PFB", 20),rep("Educ", 20),rep("FM", 20),rep("Age", 20),rep("Meanwage", 20),rep("Unemp", 20))))
colnames(Trend_frame_2)<-c("Year","Value", "Index")

#Mapping
ggplot(Trend_frame_2)+ geom_line(mapping=aes(x=Year, y=Value, color=Index))+theme_bw()

## Inequality trend for municipalities with more PFB increase than 50th decile.



install.packages('Hmisc')
library(Hmisc)


Thesis$LagPF<-Lag(Thesis$PF, -1)

l<-lm(Gini~PF+LagPF+Municipality+Year-1, data=Thesis)

summary(l)


##### Trend to where people come from.

EU<-rep(0, 20)
Af<-rep(0, 20)
N_A<-rep(0, 20)
S_A<-rep(0, 20)
Asia<-rep(0, 20)
Oce<-rep(0, 20)
PFB<-rep(0, 20)

for (i in seq(1, 20)) {
  
  EU[i]<-mean(Thesis$EU[Thesis$Year == i+1999])
  
}

for (i in seq(1, 20)) {
  
  Af[i]<-mean(Thesis$Africa[Thesis$Year == i+1999])
  
}
for (i in seq(1, 20)) {
  
  N_A[i]<-mean(Thesis$N_A[Thesis$Year == i+1999])
  
}

for (i in seq(1, 20)) {
  
  S_A[i]<-mean(Thesis$S_A[Thesis$Year == i+1999])
  
}

for (i in seq(1, 20)) {
  
  Asia[i]<-mean(Thesis$Asia[Thesis$Year == i+1999])
  
}
for (i in seq(1, 20)) {
  
  Oce[i]<-mean(Thesis$Oce[Thesis$Year == i+1999])
  
}

for (i in seq(1, 20)) {
  
  PFB[i]<-mean(Thesis$PF[Thesis$Year == i+1999])
  
}



#Plot frame

Oce[c(1, 2)]<-Oce[c(1, 2)]/10

Origin_frame<-data.frame(c(rep(seq(2000, 2019),7)),c(PFB, EU, Af, N_A, S_A, Asia, Oce), factor(c(rep("Total PFB", 20),rep("Europe", 20),rep("Africa", 20),rep("North America", 20),rep("South America", 20),rep("Asia", 20),rep("Oceania", 20))))
colnames(Origin_frame)<-c("Year","Value", "Continent")


ggplot(Origin_frame)+ geom_line(mapping=aes(x=Year, y=Value, color=Continent))+theme_bw()+ylab("Proportion")


## Cartogram plot

file.exists('C:/Users/User/OneDrive/Skrivbord/Thesis_2021/Thesis_figures/Kommun_RT90_region.shp') ## Existing file

setwd <-"C:/Users/User/OneDrive/Skrivbord/Thesis_2021/Thesis_figures/" ## set working directory

Map_2<-readOGR(dsn=path.expand("C:/Users/User/OneDrive/Skrivbord/Thesis_2021/Thesis_figures/Kommun_RT90_region.shp"), layer="Kommun_RT90_region") ## Download the spatial object

Map_2@data$Ineq_Change<-Thesis$Gini[Thesis$Year ==2019]-Thesis$Gini[Thesis$Year ==2000]

Map_2@data$PF_Change<-Thesis$PF[Thesis$Year ==2019]

## Mapping inequality changes

choroLayer(spdf = Map_2, df = Map_2@data, var = "Ineq_Change", breaks=c(seq(0.01, 0.1, length.out=5)), legend.values.rnd = 3, legend.title.txt = "Gini Increase", legend.horiz = FALSE, legend.pos = c(595000,6201428))

help("choroLayer")

## Descripitives

Stat_f<-function(x){
  
  s1<-mean(x)
  s2<-sd(x)
  s3<-min(x)
  s4<-max(x)
  s5<-skewness(x)
  
  return(round(c(s1, s2, s3, s4, s5), digits=2))
  
}


Stat_f(Thesis$EU)
Stat_f(Thesis$Asia)
Stat_f(Thesis$Africa)
Stat_f(Thesis$S_A)
Stat_f(Thesis$N_A)
Stat_f(Thesis$Oce)

round(mean(Thesis$PF[Thesis$Year ==2019]-Thesis$PF[Thesis$Year ==2000]), digits=2)
round(mean(Thesis$EU[Thesis$Year ==2019]-Thesis$EU[Thesis$Year ==2000]), digits=2)
round(mean(Thesis$Africa[Thesis$Year ==2019]-Thesis$Africa[Thesis$Year ==2000]), digits=2)
round(mean(Thesis$Asia[Thesis$Year ==2019]-Thesis$Asia[Thesis$Year ==2000]), digits=2)
round(mean(Thesis$S_A[Thesis$Year ==2019]-Thesis$S_A[Thesis$Year ==2000]), digits=2)
round(mean(Thesis$N_A[Thesis$Year ==2019]-Thesis$N_A[Thesis$Year ==2000]), digits=2)
round(mean(Thesis$Oce[Thesis$Year ==2019]-Thesis$Oce[Thesis$Year ==2000]), digits=2)





###############################################  Thesis_2021   ##############################################

###### Causality analysis ########

#############################################################################################################


Spin<-function(x){
  
  
  z<-rep(0, 600)
  
  
  for (j in seq(0, 29)) {
    
    
    for (i in seq(1, 20)) {
      
      z[(20*j) +i ]<-x[(20*j)+i]-x[(20*j)+1 ]
      
      
      
    }}
  
  return(z)
  
}


## Which municipality has most increase in foreign-born?


ForBorn<-function(x){ #Function for extracting which municipalities that have had the highest foreign-born.

return(Thesis$Municipality[which(Thesis$PF[Thesis$Year==2019]-Thesis$PF[Thesis$Year==2000] ==Rfast::nth(Thesis$PF[Thesis$Year==2019]-Thesis$PF[Thesis$Year==2000], x, descending = TRUE))*20])
  
}


Heterogeneity_ForBorn<-function(x, y){ # Same but based on stock level 2000. 
  
  return(Thesis$Municipality[which(Thesis$PF[Thesis$Year==2019]-Thesis$PF[Thesis$Year==2000] ==Rfast::nth(Thesis$PF[Thesis$Year==2019 & Thesis$Meanstock==y]-Thesis$PF[Thesis$Year==2000 & Thesis$Meanstock==y], x, descending = TRUE))*20])
  
}

ForBorn<-Vectorize(ForBorn)
Heterogeneity_ForBorn<-Vectorize(Heterogeneity_ForBorn)

ForBorn(seq(1, 10))
Heterogeneity_ForBorn(seq(1, 10), 1) #Top 10, given high PFB 2000.
Heterogeneity_ForBorn(seq(1, 10), 0) # To 10, given low PFB 2000.

## Have these groups large increase in Gini?

Extract<-function(x){ #Function for extracting foreign-born stats.
  
  a<-Thesis$PF[Thesis$Municipality == x][1]
  a_2<-Thesis$PF[Thesis$Municipality == x][20]
  
  return(round(c(a, a_2, a_2-a,100*(a_2-a)/a), digits=2))
  
}



## Plot gini for outliers

Outlier_frame<-data.frame("Älmhult"=Thesis$Gini[Thesis$Municipality == "almhult"]
                          ,"Lessebo"=Thesis$Gini[Thesis$Municipality == "Lessebo"]
                          ,"Södertälje"=Thesis$Gini[Thesis$Municipality == "Sadertalje"]
                          ,"Högsby"=Thesis$Gini[Thesis$Municipality == "Hagsby"]
                          ,"Sigtuna"=Thesis$Gini[Thesis$Municipality == "Sigtuna"]
                          ,"Hylte"=Thesis$Gini[Thesis$Municipality == "Hylte"]
                          ,"Burlöv"=Thesis$Gini[Thesis$Municipality == "Burlav"]
                          ,"Uppvidinge"=Thesis$Gini[Thesis$Municipality == "Uppvidinge"]
                          ,"Makaryd"=Thesis$Gini[Thesis$Municipality == "Makaryd"]
                          ,"Östra Göinge"=Thesis$Gini[Thesis$Municipality == "astra Gainge"]
                          
                          ,"Lessebo"=Thesis$Gini[Thesis$Municipality == "Lessebo"]
                          ,"Södertälje"=Thesis$Gini[Thesis$Municipality == "Sadertalje"]
                          ,"Sigtuna"=Thesis$Gini[Thesis$Municipality == "Sigtuna"]
                          ,"Hylte"=Thesis$Gini[Thesis$Municipality == "Hylte"]
                          ,"Burlöv"=Thesis$Gini[Thesis$Municipality == "Burlav"]
                          ,"Uppvidinge"=Thesis$Gini[Thesis$Municipality == "Uppvidinge"]
                          ,"Makaryd"=Thesis$Gini[Thesis$Municipality == "Makaryd"]
                          ,"Sundbyberg"=Thesis$Gini[Thesis$Municipality == "Sundbyberg"]
                          ,"Järfälla"=Thesis$Gini[Thesis$Municipality == "Jarfalla"]
                          ,"Åstorp"=Thesis$Gini[Thesis$Municipality == "astorp"]
                          
                          ,"Älmhult"=Thesis$Gini[Thesis$Municipality == "almhult"]
                          ,"Högsby"=Thesis$Gini[Thesis$Municipality == "Hagsby"]
                          ,"Östra Göinge"=Thesis$Gini[Thesis$Municipality == "astra Gainge"]
                          ,"HUltsfred"=Thesis$Gini[Thesis$Municipality == "Hultsfred"]
                          ,"Sävsjö"=Thesis$Gini[Thesis$Municipality == "Savsja"]
                          ,"Bengtsfors"=Thesis$Gini[Thesis$Municipality == "Bengtsfors"]
                          ,"Emmaboda"=Thesis$Gini[Thesis$Municipality == "Nassja"]
                          ,"Enköping"=Thesis$Gini[Thesis$Municipality == "Emmaboda"]
                          ,"Filipstad"=Thesis$Gini[Thesis$Municipality == "Filipstad"]
                          ,"Ronneby"=Thesis$Gini[Thesis$Municipality == "Ronneby"]
                          ,"Period"=seq(2000, 2019))

##

test_data_long <- melt(Outlier_frame, id="Period")  # convert to long format
test_data_long$ID<-rep(seq(1, 3), each=200)

colnames(test_data_long)[c(2, 3)]<-c("Municipality", "Gini", "ID")

test_data_long$Gini<-Spin(test_data_long$Gini)

Plot_Mean_2_Normal<-rep(0, 20)
Plot_Mean_2_StartLow<-rep(0, 20)
Plot_Mean_2_StartHigh<-rep(0, 20)

for (i in seq(1,20)) {
  
  Plot_Mean_2_Normal[i]<-mean(test_data_long$Gini[test_data_long$Period == 1999+i & test_data_long$ID ==1 ])
  Plot_Mean_2_StartHigh[i]<-mean(test_data_long$Gini[test_data_long$Period == 1999+i & test_data_long$ID ==2])
  Plot_Mean_2_StartLow[i]<-mean(test_data_long$Gini[test_data_long$Period == 1999+i & test_data_long$ID ==3])
  
  
}


################################################################################################################

ForBorn_Low<-function(x){ #Function for extracting which municipalities that have had the highest foreign-born.
  
  return(Thesis$Municipality[which(Thesis$PF[Thesis$Year==2019]-Thesis$PF[Thesis$Year==2000] ==Rfast::nth(Thesis$PF[Thesis$Year==2019]-Thesis$PF[Thesis$Year==2000], x, descending = FALSE))*20])
  
}


Heterogeneity_ForBorn_Low<-function(x, y){ # Same but based on stock level 2000. 
  
  return(Thesis$Municipality[which(Thesis$PF[Thesis$Year==2019]-Thesis$PF[Thesis$Year==2000] ==Rfast::nth(Thesis$PF[Thesis$Year==2019 & Thesis$Meanstock==y]-Thesis$PF[Thesis$Year==2000 & Thesis$Meanstock==y], x, descending = FALSE))*20])
  
}

ForBorn_Low<-Vectorize(ForBorn_Low)
Heterogeneity_ForBorn_Low<-Vectorize(Heterogeneity_ForBorn_Low)

ForBorn_Low(seq(1, 10))
Heterogeneity_ForBorn_Low(seq(1, 10), 1) #Bottom 10, given high PFB 2000.
Heterogeneity_ForBorn_Low(seq(1, 10), 0) #Bottom 10, given low PFB 2000.

## Have these groups large increase in Gini?

Outlier_frame_2<-data.frame("Nykvarn"=Thesis$Gini[Thesis$Municipality == "Nykvarn"][1:20]
                            ,"Hammaro"=Thesis$Gini[Thesis$Municipality == "Hammaro"][1:20]
                            ,"Södertälje"=Thesis$Gini[Thesis$Municipality == "Sadertalje"][1:20]
                            ,"Surahammar"=Thesis$Gini[Thesis$Municipality == "Surahammar"][1:20]
                            ,"Vaxholm"=Thesis$Gini[Thesis$Municipality == "Vaxholm"][1:20]
                            ,"Kil"=Thesis$Gini[Thesis$Municipality == "Kil"][1:20]
                            ,"Gagnef"=Thesis$Gini[Thesis$Municipality == "Gagnef"][1:20]
                            ,"Östhammar"=Thesis$Gini[Thesis$Municipality == "asthammar"][1:20]
                            ,"Härryda"=Thesis$Gini[Thesis$Municipality == "Harryda"][1:20]
                            ,"Söderköping"=Thesis$Gini[Thesis$Municipality == "Saderkaping"][1:20]
                            
                            ,"Nykvarn"=Thesis$Gini[Thesis$Municipality == "Nykvarn"]
                            ,"Surahammar"=Thesis$Gini[Thesis$Municipality == "Surahammar"]
                            ,"Vaxholm"=Thesis$Gini[Thesis$Municipality == "Vaxholm"]
                            ,"Harryda"=Thesis$Gini[Thesis$Municipality == "Harryda"]
                            ,"Varmda"=Thesis$Gini[Thesis$Municipality == "Varmda"]
                            ,"Nacka"=Thesis$Gini[Thesis$Municipality == "Nacka"]
                            ,"Haparanda"=Thesis$Gini[Thesis$Municipality == "Haparanda"]
                            ,"Tyresa"=Thesis$Gini[Thesis$Municipality == "Tyresa"]
                            ,"Hallstahammar"=Thesis$Gini[Thesis$Municipality == "Hallstahammar"]
                            ,"Trosa"=Thesis$Gini[Thesis$Municipality == "Trosa"]
                            
                            
                            ,"Hammarö"=Thesis$Gini[Thesis$Municipality == "Hammaro"]
                            ,"Kil"=Thesis$Gini[Thesis$Municipality == "Kil"]
                            ,"Gagnef"=Thesis$Gini[Thesis$Municipality == "Gagnef"]
                            ,"Lerum"=Thesis$Gini[Thesis$Municipality == "Lerum"]
                            ,"Östhammar"=Thesis$Gini[Thesis$Municipality == "asthammar"]
                            ,"Söderköping"=Thesis$Gini[Thesis$Municipality == "Saderkaping"]
                            ,"Öckero"=Thesis$Gini[Thesis$Municipality == "ockero"]
                            ,"Habo"=Thesis$Gini[Thesis$Municipality == "Habo"]
                            ,"Säter"=Thesis$Gini[Thesis$Municipality == "Sater"]
                            ,"Piteå"=Thesis$Gini[Thesis$Municipality == "Pitea"]
                            
                            ,"Period"=seq(2000, 2019))


test_data_long_2 <- melt(Outlier_frame_2, id="Period")# convert to long format
test_data_long_2$ID<-rep(seq(1, 3), each=200)
colnames(test_data_long_2)[c(2, 3)]<-c("Municipality", "Gini", "ID")

test_data_long_2$Gini<-Spin(test_data_long_2$Gini)

#1
Plot_Mean_2_Normal_Low<-rep(0, 20)
Plot_Mean_2_StartLow_Low<-rep(0, 20)
Plot_Mean_2_StartHigh_Low<-rep(0, 20)

for (i in seq(1,20)) {
  
  Plot_Mean_2_Normal_Low[i]<-mean(test_data_long_2$Gini[test_data_long_2$Period == 1999+i & test_data_long_2$ID ==1 ])
  Plot_Mean_2_StartHigh_Low[i]<-mean(test_data_long_2$Gini[test_data_long_2$Period == 1999+i & test_data_long_2$ID ==2])
  Plot_Mean_2_StartLow_Low[i]<-mean(test_data_long_2$Gini[test_data_long_2$Period == 1999+i & test_data_long_2$ID ==3])
  
  
}


for (i in seq(1,20)) {

Plot_Mean[i]<-mean(test_data_long_2$Gini[test_data_long_2$Period == 1999+i])

}




#
Gini_Trend_2<-rep(0, 20)

for (i in seq(1, 20)) {
  
  Gini_Trend_2[i]<-Gini_Trend[i]-Gini_Trend[1]
  
}


# Add overall mean

Mean<-data.frame("Period"=c(rep(seq(2000, 2019), 3)),"Trend"= c(rep("Overall Mean", 20), rep("Lowest PFB", 20), rep("Highest PFB", 20) ) ,"Gini"= c(Gini_Trend_2, Plot_Mean, Plot_Mean_2))

Mean_HighPFB<-data.frame("Period"=c(rep(seq(2000, 2019), 5)),"Trend"= c(rep("Overall Mean", 20), rep("TOP PFB - Low PFB Start", 20), rep("TOP PFB - High PFB Start", 20), rep("Bottom PFB - High PFB Start", 20), rep("Bottom PFB - Low PFB Start", 20) ) ,"Gini"= c(Gini_Trend_2,Plot_Mean_2_StartLow, Plot_Mean_2_StartHigh, Plot_Mean_2_StartHigh_Low, Plot_Mean_2_StartLow_Low))


ggplot(data=Mean_HighPFB,
       aes(x=Period, y=Gini, colour=Trend)) +
  geom_line()+scale_color_discrete()+theme_bw()+ylab("Gini Increase")


#####


### Africa


Extract_Africa<-function(x){
  
  Thesis$Gini[Thesis$Municipality == as.character(Thesis[20*which(Thesis$Africa[Thesis$Year==2019]-Thesis$Africa[Thesis$Year==2000] ==Rfast::nth(Thesis$Africa[Thesis$Year==2019]-Thesis$Africa[Thesis$Year==2000], x, descending = TRUE)),][2])]
  
}


Extract_Africa<-Vectorize(Extract_Africa)

Africa_Mean<-rowmeans(Extract_Africa(seq(1, 10)))


Extract_Africa_2<-function(x){
  
  Thesis$Gini[Thesis$Municipality == as.character(Thesis[20*which(((Thesis$Africa[Thesis$Year==2019]-Thesis$Africa[Thesis$Year==2000])/Thesis$Africa[Thesis$Year==2000]) ==Rfast::nth(((Thesis$Africa[Thesis$Year==2019]-Thesis$Africa[Thesis$Year==2000])/Thesis$Africa[Thesis$Year==2000]), x, descending = TRUE)),][2])]
  
  
}

Extract_Africa_2<-Vectorize(Extract_Africa_2)

Africa_Mean_perc<-rowmeans(Extract_Africa_2(seq(1, 10)))

### Asia

Extract_Asia<-function(x){
  
  Thesis$Gini[Thesis$Municipality == as.character(Thesis[20*which(Thesis$Asia[Thesis$Year==2019]-Thesis$Asia[Thesis$Year==2000] ==Rfast::nth(Thesis$Asia[Thesis$Year==2019]-Thesis$Asia[Thesis$Year==2000], x, descending = TRUE)),][2])]
  
  
}

Extract_Asia<-Vectorize(Extract_Asia)

Asia_Mean<-rowmeans(Extract_Asia(seq(1, 10)))

###Europe

Extract_EU<-function(x){
  
  Thesis$Gini[Thesis$Municipality == as.character(Thesis[20*which(Thesis$EU[Thesis$Year==2019]-Thesis$EU[Thesis$Year==2000] ==Rfast::nth(Thesis$EU[Thesis$Year==2019]-Thesis$EU[Thesis$Year==2000], x, descending = TRUE)),][2])]
  
  
}

Extract_EU<-Vectorize(Extract_EU)

Europe_Mean<-rowmeans(Extract_EU(seq(1, 10)))


Europe_Mean_2<-rep(0, 20)
Africa_Mean_2<-rep(0, 20)
Africa_Mean_perc_2<-rep(0, 20)
Asia_Mean_2<-rep(0, 20)


for (i in seq(1, 20)) {
  
  Europe_Mean_2[i]<- Europe_Mean[i]-Europe_Mean[1]
  Africa_Mean_2[i]<- Africa_Mean[i]-Africa_Mean[1]
  Asia_Mean_2[i]<- Asia_Mean[i]-Asia_Mean[1]
  Africa_Mean_perc_2[i]<- Africa_Mean_perc[i]-Africa_Mean_perc[1]
}

Mean_2<-data.frame("Period"=c(rep(seq(2000, 2019), 4)),"Trend"= c(rep("TOP Europe", 20), rep("TOP Asia", 20), rep("TOP Africa", 20), rep("Overall Mean", 20)) ,"Gini"= c(Europe_Mean_2, Asia_Mean_2, Africa_Mean_2, Gini_Trend_2))

ggplot(data=Mean_2,
       aes(x=Period, y=Gini, colour=Trend)) +
  geom_line()+scale_color_discrete()+theme_bw()+ylab("Gini Increase")


###############################################  Thesis_2021   ##############################################

                                      ###### Regression analysis ########

#############################################################################################################

# Basic regression analysis  - section 3, hetero-scedastic standard errors.


Thesis$Meanstock<- rep(ifelse(Thesis$PF[Thesis$Year ==2000] > mean(Thesis$PF[Thesis$Year ==2000]), Thesis$Meanstock <- 1, Thesis$Meanstock <-0 ), each=20)

m_1<-lm(Gini~PF+Municipality+Year-1, data=Thesis)

m_2<-lm (Gini~PF+Age+I(Age^2)+Municipality+Year-1, data=Thesis)

m_3<-lm (Gini~PF+Age+I(Age^2)+High_Educ+Municipality+Year-1, data=Thesis)

m_4<-lm (Gini~PF+Age+I(Age^2)+High_Educ+F_M+Municipality+Year-1, data=Thesis)

m_5<-lm (Gini~PF+Age+I(Age^2)+High_Educ+F_M+Meanwage+Municipality+Year-1, data=Thesis)

m_6<-lm (Gini~PF+Age+I(Age^2)+High_Educ+F_M+Meanwage+Unemp+Municipality+Year-1, data=Thesis)

m_7_High<-lm (Gini~PF+Age+I(Age^2)+High_Educ+F_M+Meanwage+Unemp+Municipality+Year-1, data=subset(Thesis, Thesis$Meanstock ==1))
m_7_Low<-lm (Gini~PF+Age+I(Age^2)+High_Educ+F_M+Meanwage+Unemp+Municipality+Year-1, data=subset(Thesis, Thesis$Meanstock ==0))

m_7_High$coefficients[1]
m_7_Low$coefficients[1]

         
rob_se <- list(sqrt(diag(vcovHC(m_1, type = "HC1"))),
               sqrt(diag(vcovHC(m_2, type = "HC1"))),
               sqrt(diag(vcovHC(m_3, type = "HC1"))),
               sqrt(diag(vcovHC(m_4, type = "HC1"))),
               sqrt(diag(vcovHC(m_5, type = "HC1"))),
               sqrt(diag(vcovHC(m_6, type = "HC1"))))
               

stargazer(m_1,m_2,m_3,m_4,m_5,m_6,
          digits = 8,
          header = FALSE,
          type = "latex", 
          se = rob_se,
          title = "Linear Panel Regression Models of Inequality",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"))

########################################### Robustness check - Theil, Skewness and Low-group ##########################################

PFPerc<-rep(0, 5800)

for (j in seq(0, 289)) {

  for (i in seq(1, 20)) {

  
  PFPerc[i+ (20*j)]<-(Thesis$PF[i+ (20*j)]-Thesis$PF[1+ (20*j)])/Thesis$PF[1+ (20*j)]
    
}}

Thesis$PFPerc<-PFPerc*100

m_5_Gini<-lm(Gini~PF+PF*Meanwage+Age+I(Age^2)+High_Educ+F_M+Meanwage+Unemp+Municipality+Year-1, data=Thesis)
m_5_Gini_Perc<-lm(Gini~PFPerc+Age+I(Age^2)+High_Educ+F_M+Meanwage+Unemp+Municipality+Year-1, data=Thesis)

summary(m_5_Gini)
summary(m_5_Gini_Perc)


m_5_Theil<-lm(Theil~PF+Age+I(Age^2)+High_Educ+F_M+Meanwage+Unemp+Municipality+Year-1, data=Thesis)

m_5_L4<-lm(L4~PF+Age+I(Age^2)+High_Educ+F_M+Meanwage+Unemp+Municipality+Year-1, data=Thesis)

m_5_L8<-lm(L8~PF+Age+I(Age^2)+High_Educ+F_M+Meanwage+Unemp+Municipality+Year-1, data=Thesis)

m_5_L13<-lm(L13~PF+Age+I(Age^2)+High_Educ+F_M+Meanwage+Unemp+Municipality+Year-1, data=Thesis)

m_5_Skewness<-lm(Skewness~PF+Age+I(Age^2)+High_Educ+F_M+Meanwage+Unemp+Municipality+Year-1, data=Thesis)

rob_se <- list(sqrt(diag(vcovHC(m_5_Gini, type = "HC1"))),
               sqrt(diag(vcovHC(m_5_Theil, type = "HC1"))),
               sqrt(diag(vcovHC(m_5_L4, type = "HC1"))),
               sqrt(diag(vcovHC(m_5_L8, type = "HC1"))),
               sqrt(diag(vcovHC(m_5_L13, type = "HC1"))),
               sqrt(diag(vcovHC(m_5_Skewness, type = "HC1"))))


stargazer(m_5_Gini,m_5_Theil,m_5_L4,m_5_L8, m_5_L13,m_5_Skewness, 
          digits = 3,
          header = FALSE,
          type = "latex", 
          se = rob_se,
          title = "Linear Panel Regression Models of Inequality",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)","(6)" ))


########################################### Squared effect ##########################################

s<-seq(0, 25/100, length.out=21)

Sq_coef_Gini<-data.frame(rep(0, 20),rep(0, 20),rep(0, 20))
colnames(Sq_coef_Gini)<-c("Point Estimate", "Lower", "Upper")

Sq_coef_Theil<-data.frame(rep(0, 20),rep(0, 20),rep(0, 20))
colnames(Sq_coef)<-c("Point Estimate", "Lower", "Upper")

Sq_coef_L13<-data.frame(rep(0, 20),rep(0, 20),rep(0, 20))
colnames(Sq_coef)<-c("Point Estimate", "Lower", "Upper")

for (i in s) {
  
  a<-lm(Gini~PF+I(PF^2)+Age+I(Age^2)+High_Educ+F_M+Meanwage++Unemp+Municipality+Year-1, data=subset(Thesis, Thesis$PF >= i))
  Sq_coef_Gini[which(s == i), 1]<- a$coefficients[2]
  Sq_coef_Gini[which(s == i), c(2, 3)]<- as.numeric(confint(a)[2, ])
  
}


a_2<-lm(Theil~PF+I(PF^2)+Age+High_Educ+F_M+Meanwage+Taxes+Unemp+Municipality+Year-1, data=subset(Thesis, Thesis$PF >= i))
Sq_coef_Theil[which(s == i), 1]<- a_2$coefficients[2]
Sq_coef_Theil[which(s == i), c(2, 3)]<- as.numeric(confint(a_2)[2, ])

a_3<-lm(L13~PF+I(PF^2)+Age+High_Educ+F_M+Meanwage+Taxes+Unemp+Municipality+Year-1, data=subset(Thesis, Thesis$PF >= i))
Sq_coef_L13[which(s == i), 1]<- a_3$coefficients[2]
Sq_coef_L13[which(s == i), c(2, 3)]<- as.numeric(confint(a_3)[2, ])

## Plot squared term with some nice error bars:)

ggplot(, aes(x=s, y=Sq_coef_Gini[, 1]))+geom_point()+geom_line(col="blue")+ylab("Squared PFB term")+xlab("PFB ={0, ..., 0.25}")+theme_bw()+geom_errorbar(aes(ymin = Sq_coef_Gini[, 2], ymax = Sq_coef_Gini[, 3]), alpha = 0.2)+geom_errorbar(aes(ymin = Sq_coef_Gini[, 2], ymax = Sq_coef_Gini[, 3]), alpha = 0.2)

##

m_5_Theil<-lm(Theil~PF+Age+High_Educ+F_M+Meanwage+Taxes+Unemp+Municipality+Year-1, data=Thesis)

m_5_L8<-lm(L8~PF+Age+High_Educ+F_M+Meanwage+Taxes+Unemp+Municipality+Year-1, data=Thesis)

m_5_L13<-lm(L13~PF+Age+High_Educ+F_M+Meanwage+Taxes+Unemp+Municipality+Year-1, data=Thesis)

m_5_Skewness<-lm(Skewness~PF+Age+High_Educ+F_M+Meanwage+Taxes+Unemp+Municipality+Year-1, data=Thesis)

stargazer(m_5_Gini,m_5_Theil,m_5_L8, m_5_L13,m_5_Skewness) #Transferring to latex



########################################### Heterogeneity analysis 1 ##########################################

T5_Gini<-lm(Gini~EU+Non_Eu+Age+I(Age^2)+High_Educ+F_M+Meanwage+Unemp+Municipality+Year-1, data=Thesis)

T5_Theil<-lm(Theil~EU+Non_Eu+Age+I(Age^2)+High_Educ+F_M+Meanwage+Unemp+Municipality+Year-1, data=Thesis)

T5_L4<-lm(L4~EU+Non_Eu+Age+I(Age^2)+High_Educ+F_M+Meanwage+Unemp+Municipality+Year-1, data=Thesis)

T5_L8<-lm(L8~EU+Non_Eu+Age+I(Age^2)+High_Educ+F_M+Meanwage+Unemp+Municipality+Year-1, data=Thesis)

T5_L13<-lm(L13~EU+Non_Eu+Age+I(Age^2)+High_Educ+F_M+Meanwage+Unemp+Municipality+Year-1, data=Thesis)

T5_Skewness<-lm(Skewness~EU+Non_Eu+Age+I(Age^2)+High_Educ+F_M+Meanwage+Unemp+Municipality+Year-1, data=Thesis)

#Heteroscedasticity-robust standard errors.

rob_se <- list(sqrt(diag(vcovHC(T5_Gini, type = "HC1"))),
               sqrt(diag(vcovHC(T5_Theil, type = "HC1"))),
               sqrt(diag(vcovHC(T5_L4, type = "HC1"))),
               sqrt(diag(vcovHC(T5_L8, type = "HC1"))),
               sqrt(diag(vcovHC(T5_L13, type = "HC1"))),
               sqrt(diag(vcovHC(T5_Skewness, type = "HC1"))))

# Exporting to latex.

stargazer(T5_Gini, T5_Theil,T5_L4, T5_L8, T5_L13, T5_Skewness, 
          digits = 3,
          header = FALSE,
          type = "latex", 
          se = rob_se,
          title = "Linear Panel Regression Models of Inequality - Heterogeneity assessment",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"))

########################################### Initial stock analysis ##########################################

## EU, Asia, Africa, Oce

Index<-rep(seq(1, 290), each=20)
Thesis$Index<-Index

Thesis$Prc1<-Prc1
Thesis$Prc2<-Prc2
Thesis$Prc3<-Prc3

Prc1<-match(Thesis$Index, which(Thesis$PF[Thesis$Year ==2000] < quantile(Thesis$PF[Thesis$Year ==2000], 1/3)))
Prc2<-match(Thesis$Index, which(Thesis$PF[Thesis$Year ==2000] < quantile(Thesis$PF[Thesis$Year ==2000], 2/3) & Thesis$PF[Thesis$Year ==2000] > quantile(Thesis$PF[Thesis$Year ==2000], 1/3)))
Prc3<-match(Thesis$Index, which(Thesis$PF[Thesis$Year ==2000] >= quantile(Thesis$PF[Thesis$Year ==2000], 2/3)))

H5_Gini_1<-lm(Gini~PF+Age+I(Age^2)+High_Educ+F_M+Meanwage+Unemp+Municipality+Year-1, data=subset(Thesis, is.na(Thesis$Prc1)== FALSE))
H5_Gini_2<-lm(Gini~PF+Age+I(Age^2)+High_Educ+F_M+Meanwage+Unemp+Municipality+Year-1, data=subset(Thesis, is.na(Thesis$Prc2)== FALSE))
H5_Gini_3<-lm(Gini~PF+Age+I(Age^2)+High_Educ+F_M+Meanwage+Unemp+Municipality+Year-1, data=subset(Thesis, is.na(Thesis$Prc3)== FALSE))

round(as.numeric(coeftest(H5_Gini_1, vcov = vcovHC, type = "HC1")[1:2]), digits=3)
round(as.numeric(coeftest(H5_Gini_2, vcov = vcovHC, type = "HC1")[1:2]), digits=3)
round(as.numeric(coeftest(H5_Gini_3, vcov = vcovHC, type = "HC1")[1:2]), digits=3)


summary(H5_Gini_1)
summary(H5_Gini_2)
summary(H5_Gini_3)


########################################### Heterogeneity analysis 2 ##########################################

H5_Gini<-lm(Gini~EU+Asia+Africa+I(Africa^2)+Age+I(Age^2)+High_Educ+F_M+Meanwage+Unemp+Municipality+Year-1, data=Thesis)

H5_Theil<-lm(Theil~EU+Asia+Africa+Age+I(Age^2)+High_Educ+F_M+Meanwage+Unemp+Municipality+Year-1, data=Thesis)

H5_L4<-lm(L4~EU+Asia+Africa+Age+I(Age^2)+High_Educ+F_M+Meanwage+Unemp+Municipality+Year-1, data=Thesis)

H5_L8<-lm(L8~EU+Asia+Africa+Age+I(Age^2)+High_Educ+F_M+Meanwage+Unemp+Municipality+Year-1, data=Thesis)

H5_L13<-lm(L13~EU+Asia+Africa+Age+I(Age^2)+High_Educ+F_M+Meanwage+Unemp+Municipality+Year-1, data=Thesis)

H5_Skewness<-lm(Skewness~EU+Asia+Africa+Age+I(Age^2)+High_Educ+F_M+Meanwage+Unemp+Municipality+Year-1, data=Thesis)

#Heteroscedasticity-robust standard errors.

rob_se <- list(sqrt(diag(vcovHC(H5_Gini, type = "HC1"))),
               sqrt(diag(vcovHC(H5_Theil, type = "HC1"))),
               sqrt(diag(vcovHC(H5_L4, type = "HC1"))),
               sqrt(diag(vcovHC(H5_L8, type = "HC1"))),
               sqrt(diag(vcovHC(H5_L13, type = "HC1"))),
               sqrt(diag(vcovHC(H5_Skewness, type = "HC1"))))

# Exporting to latex.

stargazer(H5_Gini, H5_Theil,H5_L4, H5_L8, H5_L13, H5_Skewness, 
          digits = 3,
          header = FALSE,
          type = "latex", 
          se = rob_se,
          title = "Linear Panel Regression Models of Inequality - Heterogeneity assessment",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)","(6)" ))
