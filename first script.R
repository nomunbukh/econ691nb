x<-"Hello, my name is Nomun"
x

y<- -3
y

#This is a script to introduce the idea of scripts 
#created by J. R. Groves on August 30, 2021

rm(list=ls()) #clearing the global environment
install.packages("tidyverse")
library(tidyverse) #tells R to load the package tidyverse

summ<-function(x){
        return(summary(x))
}
object<-summ(x)
