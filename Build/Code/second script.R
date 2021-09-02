#This is script for class two
#Created by Nomun
#Created on September 1, 2021
#web scripping

rm(list = ls())

library(tidyverse)
install.packages("rvest")
library(rvest)

#doing a loop to pull out 5 other states data. we don't have to do it 5 times
states<-c("illinois","indiana","kentucky","missouri","wisconsin","iowa")

for(i in states){
  
#paste is concentinate in excel

url<-paste0("https://www.nytimes.com/elections/2016/results/",i)

webpage<-read_html(url)

tables<-webpage %>%
  html_nodes("table")

results<-as.data.frame(html_table(tables[2],header = TRUE,fill = TRUE))

#results2<-tables[2] %>%
#  html_table(fill = TRUE, header = TRUE) %>%
#  as.data.frame()

temp<-results %>% #it was illinois
  rename("County" = "Vote.by.county") %>%
  mutate("Clinton" = as.numeric(gsub(",","",Clinton)),
         "Trump" = as.numeric(gsub(",","",Trump)),
         "pctClinton" = (Clinton)/(Clinton+Trump),
         "pctTrump" = (Trump)/(Clinton+Trump),
         "State" = i)      #converting to number and adding new column called State using a loop

#results$Clinton <- we don't have do it when we use pipe

assign(i,temp) #using assign instead of just i
}
