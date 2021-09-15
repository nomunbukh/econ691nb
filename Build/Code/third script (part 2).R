rm(list=ls())

library(tidyverse)

load("./Build/Output/census.RData")
load("./Build/Output/votes.RData")

census <- census %>%
  mutate(county = as.data.frame(str_split_fixed(NAME, ",", 2))[,1],
         county = trimws(gsub(" County", "", county)))


votes$County[which(votes$County=="DeWitt")]<-"De Witt"
votes$County[which(votes$County=="JoDaviess")]<-"Jo Daviess"
votes$County[which(votes$County=="LaClede")]<-"Laclede"
votes$County[which(votes$County=="LaRue")]<-"Larue"
votes$County[which(votes$County=="St. Louis City")]<"St. Louis city"
census$county[which(census$county=="St. Louis")]<-"St. Louis County"

core <- merge(census, votes, by.x=c("state", "county"), by.y=c("State", "County"), all = TRUE)

#Regression

mod1 <- lm(pctClinton ~ perWhite, data = core)
mod2 <- lm(pctClinton ~ perWhite - 1, data = core)

mod3 <- lm(pctClinton ~ perWhite + perMale + perSameCounty +
             perSameSt + perOthState + perAbroad -1, data = core)
mod4 <- lm(pctClinton ~ perWhite + perMale + perSameCounty +
             perSameSt + perOthState + perAbroad + factor(state) 
           -1, data = core)
mod5 <- lm(pctClinton ~ perWhite + perMale + perSameCounty +
             perSameSt + perOthState + perAbroad + factor(state) + perWhite*perMale 
             -1, data = core)

install.packages("stargazer")
library(stargazer)

stargazer(mod1, mod2, mod3, mod4, mod5, type = "latex",
          out = "./Build/Output/table1")