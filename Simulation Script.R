

###############################################  Thesis 2020   ##############################################

                                           ###### Packages ########

#############################################################################################################


library("ineq")
library(readxl)
library(AER)
data("Fatalities")
library("plm")
library(emg)
library('gamlss.dist')
library(openxlsx)
library(GB2)

###############################################  Thesis 2020   ##############################################

                                    ###### Wage simulation function ########

#############################################################################################################


Wage_sim<-function(y, x, z){
  
  Wages<-NULL  
  
  for (i in seq(1, z)) {
    
    Wages<-c(Wages, rep(y[i],  x[i]))
    
  }
  
  return(Wages)
}

Wage_sim_2<-function(y, x, z){
  
  Wages<-NULL  
  
  for (i in seq(1, z)) {
    
    Wages<-c(Wages, rep(y[i],  x[i]))
    
  }
  
  return(Wages[Wages>0])
}

###############################################  Thesis 2020   ##############################################

 
                                         ###### MLE SIM ########


                #                ** WARNING: RUNNING TIME: 40 Hours. **                #
                                       

#############################################################################################################

## MLE

MLE_GB2<-function(d){
  
  m<-mlfit.gb2(d)[[2]]
  print(m$par)
  return(m$par)
  
}

###  Estimated parameters

GB2_Parameters<-as.matrix(read_excel("GB2-Parameters"))

##

sim<-rep(0, 100000)
Diff<-rep(0, 5800)
Grouped_Gini<-rep(0, 5800)
Non_Grouped_Gini<-rep(0, 5800)

for (z in seq(1, 5800)) {
  
for(i in 1:100000){
  
  if(runif(1)<zero_m[z]){
    
    sim[i] = runif(1,0,0)
    
  }else if(runif(1)>= zero_m[1]){
    
    sim[i] = rgb2(1,GB2_Parameters[1, z], GB2_Parameters[2, z],   GB2_Parameters[3, z],   GB2_Parameters[4, z])
    
  }}

  Grouped_Gini[z]<-ineq(split(sim[sim<Inf]))
  Non_Grouped_Gini[z]<-ineq(sim[sim<Inf])
  

}

ineq(rgb2())


###

write.xlsx(as.matrix(Non_Grouped_Gini), "Non Grouped_Gini")
write.xlsx(as.matrix(Grouped_Gini), "Grouped_Gini")
cor(Non_Grouped_Gini, Grouped_Gini)

####

Comparison<-as.matrix(read_excel("sofar"))

Check<-as.matrix(Non_Grouped_Gini)

write.xlsx(Check, "sofar")

###############################################  Thesis 2020   ##############################################

                                        ###### Exporting ########

#############################################################################################################

write.xlsx(Sim_matrix, "GB2-Parameters")


write.xlsx(Sim_matrix, "Simulated Gini") #Simulated Ginis



###############################################  Thesis 2020   ##############################################

                                  ###### Uncertainty within simulation ########

#############################################################################################################



Variance_F<-function(x){ 
  
sim<-rep(0, x)

  for(i in 1:x){
    
    if(runif(1)<0.0652){
      
      sim[i] = runif(1,0,0)
      
    }else if(runif(1)>=0.0652 ){
      
      sim[i] = rgb2(1,9.2496466, 240.6631982,   0.1265724,   0.3384886)
      
    }}
  
return(list(
  ineq(split(sim[sim<Inf]))[1],
  ineq(sim[sim<Inf])))

}


SAMPLE<-matrix(data=0, nrow=400, ncol=3, byrow=TRUE)
SAMPLE[,3]<-factor(c(rep(100, 100), rep(1000, 100), rep(10000, 100), rep(100000, 100)))

for (i in seq(1, 100)) {

  SAMPLE[i, c(1, 2)]<-unlist(Variance_F(100))
  SAMPLE[i+100,c(1, 2)]<-unlist(Variance_F(1000))
  SAMPLE[i+200,c(1, 2)]<-unlist(Variance_F(10000))
  SAMPLE[i+300,c(1, 2) ]<-unlist(Variance_F(100000))
  
   
}

SAMPLE<-as.data.frame(SAMPLE)

colnames(SAMPLE)[3]="Sample Size"

ggplot(data=SAMPLE)+geom_density(aes(V1, color=`Sample Size`))+theme_bw()+ylab("Density")+xlab("Gini point estimate")

ggplot(data=SAMPLE)+geom_density(aes(V2, color=`Sample Size`))+theme_bw()+ylab("Density")+xlab("Gini point estimate")


#############################################################################################################

## Assessment of the implications of group data; pic 1 - Gothenburg@2000,  pic 2 - Year mean scores

plot(density(Wage_sim(Mean_matrix[(1+164*(26)):(26+164*(26)),1], Ppl_matrix[(1+164*(26)):(26+164*(26)),1])))

## Density plot


Gbg_2000<-Wage_sim(Mean_matrix[(1+164*(26)):(26+164*(26)),1], Ppl_matrix[(1+164*(26)):(26+164*(26)),1])
sim_Gbg1<-sample(rand.samples[rand.samples<max(Gbg_2000)], size =length(Gbg_2000), replace = TRUE)
sim_Gbg1<-sim_Gbg1[order(sim_Gbg1)]

df_dens<-data.frame("samples"=c(Gbg_2000, sim_Gbg1), "Group"=factor(c(rep("Original", length(Gbg_2000)),rep("Simulated", length(Gbg_2000)))))


# Plotting simulation/orginal comparison.
ggplot(df_dens, aes(x=samples, colour=Group))+geom_density(size=0.55)+theme_bw()+xlim(-100,1500)+xlab("Yearly income")+ylab("Density")+scale_color_manual(values = c("black", "deepskyblue3"))


# Algorithm 1

#The number of samples from the mixture distribution
N = 377765                

#Sample N random uniforms U
U =runif(N)

#Variable to store the samples from the mixture distribution                                             
rand.samples = rep(NA,N)

#Sampling from the mixture
for(i in 1:N){
  if(U[i]<0.0652){
    rand.samples[i] = runif(1,0,0)
  }else if(U[i]>= 0.05){
    rand.samples[i] = rgb2(1,9.2496466, 240.6631982,   0.1265724,   0.3384886)
    
  }
}


# Proportion of zeros

zero_m<-matrix(data=0, nrow=290, ncol=20, byrow=TRUE)

for (i in seq(1, 20)) {
  
  for (j in seq(0, 289)) {
    
    zero_m[j+1, i]<-Ppl_matrix[(1+j*(26)):(1+j*(26)),i]/sum(Ppl_matrix[(1+j*(26)):(26+j*(26)),i])
    
  }}


# 


pop_m<-matrix(data=0, nrow=290, ncol=20, byrow=TRUE)

for (i in seq(1, 20)) {
  
  for (j in seq(0, 289)) {
    
    pop_m[j+1, i]<-sum(Ppl_matrix[(1+j*(26)):(26+j*(26)),i])
    
  }}

# Split function

split<-function(x){
  
  d<-c(
    rep(mean(x[x ==0]), length(x[x ==0])),
    rep(mean(x[x >0 & x <190]),length(x[x >0 & x <190])),
    rep(mean(x[x >190 & x <390]),length(x[x >190 & x <390])),
    rep(mean(x[x >390 & x <590]),length(x[x >390 & x <590])),
    rep(mean(x[x >590 & x <790]),length(x[x >590 & x <790])),
    rep(mean(x[x >790 & x <990]),length(x[x >790 & x <990])),
    rep(mean(x[x >990 & x <1190]),length(x[x >990 & x <1190])),
    rep(mean(x[x >1190 & x <1390]),length(x[x >1190 & x <1390])),
    rep(mean(x[x >1390 & x <1590]),length(x[x >1390 & x <1590])),
    rep(mean(x[x >1590 & x <1790]),length(x[x >1590 & x <1790])),
    rep(mean(x[x >1790 & x <1990]),length(x[x >1790 & x <1990])),
    rep(mean(x[x >1990 & x <2190]),length(x[x >1990 & x <2190])),
    rep(mean(x[x >2190 & x <2390]),length(x[x >2190 & x <2390])),
    rep(mean(x[x >2390 & x <2590]),length(x[x >2390 & x <2590])),
    rep(mean(x[x >2590 & x <2790]),length(x[x >2590 & x <2790])),
    rep(mean(x[x >2790 & x <2990]),length(x[x >2790 & x <2990])),
    rep(mean(x[x >2990 & x <3190]),length(x[x >2990 & x <3190])),
    rep(mean(x[x >3190 & x <3390]),length(x[x >3190 & x <3390])),
    rep(mean(x[x >3390 & x <3590]),length(x[x >3390 & x <3590])),
    rep(mean(x[x >3590 & x <3790]),length(x[x >3590 & x <3790])),
    rep(mean(x[x >3790 & x <3990]),length(x[x >3790 & x <3990])),
    rep(mean(x[x >3990 & x <4990]),length(x[x >3990 & x <4990])),
    rep(mean(x[x >4990 & x <5990]),length(x[x >4990 & x <5990])),
    rep(mean(x[x >5990 & x <7990]),length(x[x >5990 & x <6990])),
    rep(mean(x[x >7990 & x <9990]),length(x[x >7990 & x <10000])),
    rep(mean(x[x > 10000]), length(x[x > 10000])))
  return(d)
  
}

