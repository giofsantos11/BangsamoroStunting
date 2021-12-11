#################################################
##Create plots of stunting rates around the world
##BY: ANGELO SANTOS
#################################################

# Load the packages
library(tidyverse)
library(WDI)
library(ggrepel)
WDIsearch("stunting")

# Set working directory
setwd("C:/Users/Angelo Santos/OneDrive - George Mason University - O365 Production/PUBP 793/R PROJECT")

# Set the format for the charts
source("formattable.R")

##Collect stunting data from WDI
StuntingRaw = WDI(indicator = "SH.STA.STNT.ZS", extra = "TRUE", country = "all", start = 2014, end = 2019)
StuntingRaw<-na.omit(StuntingRaw)

stunting <- StuntingRaw %>%
  group_by(country) %>%
  mutate(maxyear = max(year)) %>%
  filter(year==maxyear)

##Collect GDP per capita from WDI

WDIsearch("GDP per capita")
GDPRaw = WDI(indicator = "NY.GDP.PCAP.PP.KD", country = "all", start = 2014, end = 2019)
GDPRaw<-na.omit(GDPRaw)

GDPGrowthRaw = WDI(indicator = "NY.GDP.PCAP.KD.ZG", country = "all", start = 2014, end = 2019)
GDPGrowthRaw<-na.omit(GDPGrowthRaw)

##Merge GDP data
GDPfull <- merge(GDPRaw, GDPGrowthRaw, by=c("iso2c", "year"))

##Merge with stunting data
Merged <- merge(GDPfull, stunting, by=c("iso2c", "year"))
colnames(Merged)[c(4, 6, 8)] <- c("GDP_pc", "GDP_gr", "stunting")
Merged$Region <- as.factor(Merged$region)

##Create scatterplot
Merged$logGDP <- log(Merged$GDP_pc)

Merged$Region <- factor(Merged$Region, levels = c("Sub-Saharan Africa", "South Asia", "East Asia & Pacific", 
      "Middle East & North Africa", "Latin America & Caribbean", "Europe & Central Asia", "North America"))

#+ fig.width = 7, fig.height = 5
ggplot(Merged, aes(x=logGDP, y=stunting)) +geom_smooth(method=lm, color = "lightsalmon", fill = "#ffe6dc")+
  geom_point(aes(color = Region))+geom_text_repel(data = 
  subset(Merged, country == "Philippines" | country =="Guatemala"| country == "Libya" |
    country == "Burundi" | country == "Vietnam" | country == "Ghana" | stunting >42 | country == "Pakistan" |
    country == "Gambia, The"| country == "Iraq" | country == "Togo" | country == "Tonga" |
    country == "Thailand" |country =="Mexico"|  country == "Bolivia"| country =="China" |
    logGDP>10.9), aes(logGDP, stunting, label =country), size = 3)+ scale_color_brewer(palette = "Dark2")+
  geom_point(data = subset(Merged, country =="Philippines"), shape = 4, size= 2)+ 
  labs(x = "Log GDP per capita", y = "Stunting rate", title = 
  "Figure1: Stunting rate and log GDP per capita", caption = "Source: World Development Indicators")+theme(plot.caption = element_text(hjust=0))

