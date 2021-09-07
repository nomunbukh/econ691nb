#This is a script to download the election results by county for the 
#2016 US Presidential Election, download Census data via API, and use 
#ggplot to create state maps

#Created by N. Bukh on September 9, 2021 

rm(list = ls())

library(rvest)  #rvest is used to scrape the New York Times website for 
                #the needed data
library(tidyverse)

