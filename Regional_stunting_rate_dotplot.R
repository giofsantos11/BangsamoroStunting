######################################################################
##Regional stunting rates
##BY: ANGELO SANTOS
# This creates a dotplot of the regional stuting rates within the country
######################################################################

library(readxl)
library(tidyverse)

# Set working directory
setwd("C:/Users/Angelo Santos/OneDrive - George Mason University - O365 Production/PUBP 793/R PROJECT")

# Set the format for the charts
source("formattable.R")

# Soure the data from an Excel sheet
Stunting <- read_excel("Stunting.xlsx")

##Create panels
Stunting <- Stunting[order(-Stunting$StuntingRate),]

panel_num <- as.integer(c(rep(1:3, each=5), rep(4, each = 2)))
panel_num

Stunting$panel_num <- panel_num

##Plot
ggplot(Stunting, aes(x = StuntingRate, y = reorder(REGION, StuntingRate)))+ 
  geom_point(shape = 21, size= 4, color = "black", fill = "blue")+
  labs(x= "Stunting rate (%)", y = "Administrative region", 
       title = "Figure 2: Stunting rates by region",
       caption = "Source: Food and Nutrition Research Institute of the Philippines")+
  hw+facet_grid(vars(panel_num),  space = "free", scales = "free")+
  theme(strip.text=element_blank())+theme(plot.caption = element_text(hjust=0))




