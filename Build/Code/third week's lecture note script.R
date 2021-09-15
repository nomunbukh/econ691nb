#This is a script for third week
#Created by N. Bukh-Ochir on September 2021

rm(list=ls())

save(df, file = "C:/Users/nomun/OneDrive/Documents/Git test/econ691nb/econ691nb/Build/Output/dataforthirdlecturenote.RData")
save.image(file = "C:/Users/nomun/OneDrive/Documents/Git test/econ691nb/econ691nb/Build/Output/fileforthirdlecturenote.RData")
load("C:/Users/nomun/OneDrive/Documents/Git test/econ691nb/econ691nb/Build/Output/votes.RData")

#This script creates the data needed for the regression lecture.
#Jeremy R. Groves
#September 5, 2020
rm(list=ls())
#Generate data from Census API
#Pre-defining variables to be used in loop
vars<-c("B01001_001","B01001_002","B02001_001","B02001_002", "B02001_003","B05001_001","B05001_006","B07001_001", "B07001_017","B07001_033","B07001_049","B07001_065","B07001_081" )
states<-c("nevada","arizona","oregon","idaho","utah")
fips<-c(32,04,41,16,49)
#API Command
library(tidycensus)
library(sf)
k<-1
for(i in fips){
  acs<-get_acs(geography="county",
               variables = vars,
               state = i,
               year = 2016,
               geometry = TRUE)
  temp<-acs %>%
    mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                                 variable=="B01001_002" ~ "Male",
                                 variable=="B02001_001" ~ "TotRace",
                                 variable=="B02001_002" ~ "White",
                                 variable=="B02001_003" ~ "Black",
                                 variable=="B05001_001" ~ "TotCit",
                                 variable=="B05001_006" ~ "NonCit",
                                 variable=="B07001_001" ~ "TotMob",
                                 variable=="B07001_017" ~ "Stay",
                                 variable=="B07001_033" ~ "SameCounty",
                                 variable=="B07001_049" ~ "SameSt",
                                 variable=="B07001_065" ~ "OthState",
                                 variable=="B07001_081" ~ "Abroad",
                                 TRUE ~ "other")) %>%
    select(!c(moe,variable)) %>%
    spread(key=variable2, value=estimate) %>%
    mutate(perMale = Male/TotPop,
           perWhite = White/TotPop,
           perBlack = Black/TotPop,
           perCit = 1-(NonCit/TotCit),
           perStay = Stay/TotMob,
           perSameCounty = SameCounty/TotMob,
           perSameSt = SameSt/TotMob,
           perOthState = OthState/TotMob,
           perAbroad = Abroad/TotMob) %>%
    select("GEOID","NAME",starts_with("per"),"geometry") %>%
    mutate(state = states[k])
  assign(paste0(states[k],"census"),temp)
  temp$area<-st_area(temp)
  map <- temp %>%
    summarise(area = sum(area)) %>%
    mutate(state = states[k])
  assign(paste0(states[k],"map"),map)
  k<-k+1
  rm(temp, map)
}

arizonacensus$County<-trimws(gsub(" County, Arizona","",arizonacensus$NAME))
idahocensus$County<-trimws(gsub(" County, Idaho","",idahocensus$NAME))
nevadacensus$County<-trimws(gsub(" County, Nevada","",nevadacensus$NAME))
oregoncensus$County<-trimws(gsub(" County, Oregon","",oregoncensus$NAME))
utahcensus$County<-trimws(gsub(" County, Utah","",utahcensus$NAME))

census<-rbind(arizonacensus,idahocensus,nevadacensus,oregoncensus,
              utahcensus)
states<-rbind(arizonamap,idahomap,nevadamap,oregonmap,utahmap)

census$NAME<-as.data.frame(str_split_fixed(census$NAME, ",", 2))[,1]
census$NAME<-trimws(gsub(" County","",census$NAME))

#Load Vote Data
load("./Build/Output/votes.RData")
votes<-votes[order(votes$County),]
census<-census[order(census$NAME),]
votes$County==census$NAME

votes$County[63]
census$NAME[63]
census$NAME[63]<-votes$County[63]

#votes$County[which(votes$County=="DeWitt")]<-"De Witt"
#votes$County[which(votes$County=="JoDaviess")]<-"Jo Daviess"
#votes$County[which(votes$County=="LaClede")]<-"Laclede"
#votes$County[which(votes$County=="LaRue")]<-"Larue"
#votes$County[which(votes$County=="St. Louis City")]<-"St. Louis city"
#census$NAME[which(census$NAME=="St. Louis")]<-"St. Louis County"
#Merge vote and census data
core<-merge(census,votes,by.x=c("NAME","state"), by.y=c("County","state"),all=TRUE)
core$area<-st_area(core) #Command for maps in ggplot later

ggplot(core)+
  geom_sf(aes(fill = perWhite))+
  scale_fill_gradient(low="white",high="blue",limits=c(0,1),
                      aes(name="Percent White"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())+
  geom_sf(
    data = states,
    fill=NA, colour="black",
    size=1,
    inherit.aes=FALSE
  )

ggplot(core)+
  geom_sf(aes(fill = pctClinton))+
  scale_fill_gradient(low="red",high="blue",limits=c(0,1),
                      aes(name="Percent Clinton"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank()) +
  geom_sf(
    data = states,
    fill=NA, colour="black",
    size=1,
    inherit.aes=FALSE
  )

mod1<-lm(pctClinton~perWhite, data=core)
mod2<-lm(pctClinton~perWhite-1, data=core)

print(mod1)
summary(mod1)
print(mod2)
summary(mod2)

mod3<-lm(formula = pctClinton ~ perWhite + perMale + perCit + perSameCounty +
           perSameSt + perOthState + perAbroad, data = core)
summary(mod3)

mod4<-lm(formula = pctClinton ~ perWhite + perMale + perCit + perSameCounty +
           perSameSt + perOthState + perAbroad + factor(state), data = core)
summary(mod4)
mod5<-lm(formula = pctClinton ~ perWhite + perMale + perCit + perSameCounty +
           perSameSt + perOthState + perAbroad + factor(state)-1, data = core)
summary(mod5)


#Plotting other elements of the mod List
p1<-ggplot(core, aes(x=perWhite))+
  geom_point(aes(y=pctClinton))+
  xlim(0,1)+
  xlab("Percent Population White")+
  ylim(0,1)+
  ylab("Percent Clinton")+
  geom_line(aes(y=mod1$fitted.values), color="red")+
  theme_bw()
p1

residual<-seq(141)

install.packages("jtools")
install.packages("stargazer")
install.packages("finalfit")
library(stargazer)

core.2<-core %>%
  select(-c(GEOID, area))
core.2$geometry<-NULL
stargazer(core.2, type="html", out="./Build/Output/SumStat.html")
stargazer(mod1, mod2, mod3, mod4, mod5, type="html", out="./Build/Output/regress.html")
