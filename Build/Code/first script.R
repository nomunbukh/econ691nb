#First script to manipulate data
#Economics 691
#Nomun Bukh
#08/30/2021


x<-"Hello, my name is Nomun"
x

y<- -3
y

#This is a script to introduce the idea of scripts 
#created by J. R. Groves on August 30, 2021

rm(list=ls()) #clearing the global environment
install.packages("tidyverse")
library(tidyverse) #tells R to load the package tidyverse

delta <- function(x){
  temp<-(x-lag(x))/(lag(x))
  return(temp)
}

covidIL <- read.csv("./Data/ILCovid19.csv")
head(covidIL)
covidIL <- covidIL %>%
  mutate(pc_test=delta(Tests),
         pc_cases=delta(Cases),
         pc_deaths=delta(Deaths))
covidIL$pc_test<-delta(covidIL$Tests)
covidIL$pc_cases<-delta(covidIL$Cases)
summary(covidIL)


#summ<-function(x){
#        return(summary(x))
#}
#object<-summ(x)

#xd<-runif(10,0,1) #random variable under uniform distribution
#summ(xd)

covidIL$pc_deaths[is.infinite(covidIL$pc_deaths)] <- NA
covidIL$pc_deaths[is.nan(covidIL$pc_deaths)] <- NA #converted NaN to NA
covidIL$Date1 = as.Date(covidIL$Date, "%m/%d/%Y") #created new column because it keeps saying <NA> when I try to overwrite Date column
plot(covidIL$Date1, covidIL$pc_deaths)
plot(covidIL$Date1, covidIL$pc_cases,
     main="Percentage change of COVID cases",
     xlab="Date",
     ylab="",
     type="l",
     col="blue")




head(covidIL)

#Homework assignment
  #creating function DIF

DIF <- function(x){
  temp<-(x-lag(x))
  return(temp)
}
covidIL <- covidIL %>%
  mutate(dif_test=DIF(Tests),
         dif_cases=DIF(Cases),
         dif_deaths=DIF(Deaths))
head(covidIL)

covidIL <- covidIL %>%
  mutate(pc_dif_test=delta(dif_test),
         pc_dif_cases=delta(dif_cases),
         pc_dif_deaths=delta(dif_deaths))
covidIL$pc_dif_deaths[is.nan(covidIL$pc_dif_deaths)] <- NA #converted NaN to NA
head(covidIL)

plot(covidIL$Date1,covidIL$pc_dif_cases,
     main="Percentage change of COVID number of daily cases",
     xlab="Date",
     ylab="",
     type="l",
     col="blue")
plot(covidIL$Date1,covidIL$pc_dif_test,
     main="Percentage change of COVID number of daily tests",
     xlab="Date",
     ylab="",
     type="l",
     col="blue")
plot(covidIL$Date1,covidIL$pc_dif_deaths,
     main="Percentage change of COVID number of daily deaths",
     xlab="Date",
     ylab="",
     type="l",
     col="blue")
