#---------------------------------------------------------------------------
##Create the dot plots
##By: Angelo Santos
##Dependencies:
  # 1. BARMM_HFSA.dta -- the individual level dataset in Stata
  # 2. formattable.R -- the code to automatically standardize chart format
#-----------------------------------------------------------------------------

# First set the working directory that contains the dependencies
setwd("C:/Users/Angelo Santos/OneDrive - George Mason University - O365 Production/PUBP 793/R PROJECT")

#------------------------------------------------------------------------------

## Load packages
packages <- c(
  "haven",
  "tidyverse",
  "doBy",
  "DT",
  "collapse",
  "broom",
  "tidyselect",
  "MASS",
  "ISLR",
  "rpart",
  "rpart.plot",
  "DMwR2",
  "ggplot2",
  "GGally"
)

# If not yet installed, change install = TRUE
# install.packages("pacman")
pacman::p_load(packages, character.only= TRUE, install = FALSE)

#------------------------------------------------------------------------------
# Load the dependencies
BARMM_HFSA <- read_dta("BARMM_HFSA.dta")

BARMM_HFSA$age_created_months <- BARMM_HFSA$age_created_years*12

source("formattable.R")

#--------------------------------------------------------------------------
##Data check 1: Histogram of children's age (unweighted)
histo1 <- ggplot(BARMM_HFSA)+geom_histogram(aes(x=age_created_months), color="black", 
binwidth=1)+labs(x="Age of children (in months)", y="# of children (unweighted)", 
title = "Figure 4: Histogram for children's age (unweighted)")+geom_vline(xintercept=24,  
color="red", size = 0.5)+geom_text(aes(x=30, y = 200,
label="24 months", color = "red"))+formattable+theme(legend.position="none")

#+ fig.width = 7, fig.height = 4.5
histo1

#--------------------------------------------------------------------------
# Histogram of children's age (weighted)
histo2 <- ggplot(BARMM_HFSA, aes(weight = weight))+geom_histogram(aes(x=age_created_months), color="black", 
binwidth=1)+labs(x="Age of children (in months)", y="# of children (weighted)", 
title = "Figure 5: Histogram of children's age (weighted)")+geom_vline(xintercept=24,  
color="red", size = 0.5)+geom_text(aes(x=30, y = 20000,
label="24 months", color = "red"))+formattable+theme(legend.position="none")

#+ fig.width = 7, fig.height = 4.5
histo2


#--------------------------------------------------------------------------
# Histogram to show height-for-age distribution

histo3 <- ggplot(BARMM_HFSA)+geom_histogram(aes(x=haz06), color="black", 
binwidth=0.5)+labs(x="Height-for-age z scores", y="# of children", 
title = "Figure 3: Histogram for height-for-age z-scores", caption =
"Height z-scores outside the -6 and 6 range are considered biologically impossible (WHO, 2005)")+
geom_vline(xintercept=-6, color="red", size = 0.5)+
scale_x_continuous(breaks = c(-10, -6, 0, 6, 10))+
geom_vline(xintercept=6,color="red", size = 0.5)+formattable+theme(legend.position="none")+
theme(plot.caption = element_text(hjust=0))

#+ fig.width = 7, fig.height = 4.5

histo3

##Exclude observations with biologically impossible height
BARMM_HFSA <- subset(BARMM_HFSA, haz06>=-6 & BARMM_HFSA$haz06<=6)

#--------------------------------------------------------------------------
## Some data tidying: convert all to factor variables if character length is 13
## Why 13? Because that is the length of the (true) string ID variables

## Use this shortcut from: https://www.listendata.com/2015/05/converting-multiple-numeric-variables.html#:~:text=In%20R%2C%20you%20can%20convert,be%20set%20as%20factor%20variables.
col_names <- sapply(BARMM_HFSA, function(col) length(unique(col)) < 13)
BARMM_HFSA[ , col_names] <- lapply(BARMM_HFSA[ , col_names] , factor)

sapply(BARMM_HFSA, class)

##Create labels for the provinces
BARMM_HFSA$province <- factor(BARMM_HFSA$ID01, labels = c("North Cotabato", "Cotabato City",
    "Basilan","Lanao del Sur", "Maguindanao", "Sulu", "Tawi-Tawi"))
BARMM_HFSA$stunted <- as.numeric(BARMM_HFSA$stunted)
BARMM_HFSA$stunted_f <- as.factor(BARMM_HFSA$stunted)

#--------------------------------------------------------------------------
##Create visuals

##Visual 1: Dot plots showing stunting rates by province and average income
province_u <- summaryBy(stunted + hh_income ~ province, FUN = mean, data = BARMM_HFSA)
province_u$value1 <- 1
province_u$hh_income.mean <- province_u$hh_income.mean*12
province_u$stunted.mean <- (province_u$stunted.mean - province_u$value1)*100

provinceplot_u <- ggplot(province_u, aes(x = stunted.mean, y = reorder(province, stunted.mean), fill = hh_income.mean))+
  geom_point(shape = 21, size= 3)+scale_fill_gradient(low = "red", high = "green",
  labels = c(15, 20, 25, 30, 35), limits = c(17, 35))

provinceplot_u

province_w <- collap(BARMM_HFSA, stunted + hh_income ~ province, w = ~weight, wFUN = fsum)
                   
province_w$value1 <- 1
province_w$hh_income <- province_w$hh_income*12
province_w$stunted <- (province_w$stunted - province_w$value1)*100

provinceplot_w <- ggplot(province_w, aes(x = stunted, y = reorder(province, stunted), fill = hh_income))+
  geom_point(shape = 21, size= 3)+scale_fill_gradient(low = "red", high = "green",
  labels = c(15, 20, 25, 30, 35), limits = c(15, 35), name = "Mean income ('000 Pesos)")+labs(x= "Stunting rate (%)",
  y = "Province", title = "Provincial stunting rates in Bangsamoro Region", 
  caption = "Sampling weights are applied.", 
  "Cotabato City is not a province, but does not belong to any province as an
  independent component city")+ guides(fill = guide_colorbar(reverse = TRUE))+formattable

provinceplot_w

#--------------------------------------------------------------------------
##Visual 2: Dot plots showing stunting rates by age group
BARMM_HFSA$agegroup= ifelse(BARMM_HFSA$age_created_months<6, 0, 
    ifelse(BARMM_HFSA$age_created_months>=19, 2, 1))
BARMM_HFSA$agegroup <- factor(BARMM_HFSA$agegroup, labels = c("0 - 5 months", 
        "6 - 18 months", "19 - 59 months"))

#Create dataframe
agegroup <- collap(BARMM_HFSA, stunted ~ agegroup, w = ~weight, wFUN = fsum)
agegroup$value1 <- 1
agegroup$stunted <- (agegroup$stunted -agegroup$value1)*100

agegroupplot <- ggplot(agegroup, aes(x = stunted, y = reorder(agegroup, stunted)))+
  geom_point(shape = 21, size= 4, color = "black", fill = "blue")+labs(x= "Stunting rate (%)",
  y = "Age group", caption = "Sampling weights are applied.")+formattable

agegroupplot 

#--------------------------------------------------------------------------
##Visual 3: Dot plots showing stunting rates by education level
BARMM_HFSA$hhhead_educ <- factor(BARMM_HFSA$hhhead_educ, labels = c("No education", 
  "Kindergarten", "Some elementary", "Elementary graduate", "Some high school", 
  "High school graduate", "Some college", "College graduate"))

education <- collap(BARMM_HFSA, stunted ~ hhhead_educ, w = ~weight, wFUN = fsum)
education$value1 <- 1
education$stunted <- (education$stunted -education$value1)*100

educationplot <- ggplot(education, aes(x = stunted, y = reorder(hhhead_educ, stunted)))+
  geom_point(shape = 21, size= 4, color = "black", fill = "blue")+labs(x= "Stunting rate (%)",
  y = "Education" , caption = "Sampling weights are applied.")+formattable

educationplot

#--------------------------------------------------------------------------
#Visual 4: Income levels
BARMM_HFSA$Household_income_grouped <- factor(BARMM_HFSA$Household_income_grouped, 
  labels = c("Less than $100", "$100 - $300", "$300 - $800", "Over $800"))

incomegroup <- collap(BARMM_HFSA, stunted ~ Household_income_grouped, w = ~weight, wFUN = fsum)
incomegroup$value1 <- 1
incomegroup$stunted <- (incomegroup$stunted -incomegroup$value1)*100

incomeplot <- ggplot(incomegroup, aes(x = stunted, y = reorder(Household_income_grouped, 
  stunted)))+ geom_point(shape = 21, size= 4, color = "black", fill = "blue")+
  labs(x= "Stunting rate (%)", y = "Monthly income" , caption = "Sampling weights are applied.")+formattable

incomeplot


BARMM_HFSA$log_age <- log(BARMM_HFSA$age_created_years)

ggplot(BARMM_HFSA, aes(x = age_created_years, y = haz06, color = F1, 
                       weight = weight))+
  geom_point(shape = 21)+geom_smooth(se=TRUE, method="loess",
  method.args=list(degree=1, family="symmetric"), color = "black")+
  scale_color_gradient(low = "red", high = "black")


ggplot(BARMM_HFSA, aes(x = agegroup, y = haz06, color = hh_income, 
    weight = weight))+ geom_boxplot()

ggplot(BARMM_HFSA, aes(x = F1, y = haz06, color = F1, 
                       weight = weight))+
  geom_point(shape = 21)+geom_smooth(se=TRUE, method="loess",
                                     method.args=list(degree=1, family="symmetric"), color = "black")+
  scale_color_gradient(low = "red", high = "black")

##Do R-bind for all datasets

agegroup <- agegroup %>% rename(indicator=agegroup)
agegroup$group <- "Age"

education <- education %>% rename(indicator=hhhead_educ)
education$group <- "Education"

incomegroup <- incomegroup %>% rename(indicator=Household_income_grouped)
incomegroup$group <- "Income"

province_w <- province_w %>% rename(indicator=province)
province_w <- province_w %>% select(!(hh_income))
province_w$group <- "Province" 

combine <- rbind(education, agegroup, incomegroup, province_w)
combine$group <- as.factor(combine$group)

combine$group <- factor(combine$group, levels = c("Education", "Province", 
                        "Income", "Age"))

##Prepare panel dot plot
#+ fig.width = 6.5, fig.height = 5

ggplot(combine, aes(x = stunted, y = reorder(indicator, 
  stunted), fill = group))+ geom_point(shape = 21, size= 3.5, color = "black")+
  labs(x= "Stunting rate (%)", y = "Household and child characteristics", 
  title = "Figure 6: Stunting rates by household and child characteristics",
  caption = paste("Notes:", "1. Sampling weights are applied.", "2. Monthly incomes converted to US$ are reported.", 
  "3. As an independent component city, Cotabato City does not have a province;", "hence, it is reported separately.", 
  sep = "\n"))+scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3")) + 
  hw+facet_grid(group ~., scales = "free", space = "free")+
  theme(legend.position="none")+theme(plot.caption = element_text(hjust=0))

#--------------------------------------------------------------------------

## Tidying for logistic regression
## R quirk: not specifying dplyr below would sometimes work but not last time I ran

analysis <- BARMM_HFSA %>% 
  dplyr::select(!(starts_with("ID")))
                                  
analysis <- analysis %>% 
  dplyr::select(!(c(weight, haz06, stunted, otherstunted,
      HOUSEHOLD_ROSTER_CONTROL_NUMBER)))

##Rename variables
analysis <- analysis %>% 
  rename(sought_help = HS29, village_stunted_sh = 
    stunted_brgy_sh, age_years = age_created_years, perception_score = F1)

analysis$stunted_f <- factor(analysis$stunted_f, labels = c("Not stunted", "Stunted"))

analysis$sought_help <- factor(analysis$sought_help, labels = c("Traditional Medicine",
    "Midwife", "Nurse", "Doctor"))

analysis$watersource <- factor(analysis$watersource, labels = c("Own use faucet",
    "Shared faucet", "Own use piped well", "Shared piped well", "Tubed piped shallow well",
    "Dug well", "Protected spring", "Unprotected spring", "Surface water", 
    "Rainwater collection", "Bought bottled water", "Bought water from tanker"))

#ggpairs(select(analysis, stunted, everything()),
 #   lower=list(continuous='blank', combo='blank', discrete='blank'),
  #  upper=list(continuous="points", combo="facethist", discrete="facetbar"),
   # switch="y", mapping=aes(color=stunted))

#--------------------------------------------------------------------------
##Run the model
model.glm <- glm(stunted_f ~ hhhead_educ + perception_score + hh_income + province + agegroup+ 
                   village_stunted_sh + watersource, family = "binomial",data = analysis)
summary(model.glm)

gg<- ggcoef(model.glm, exponentiate=TRUE, sort = "ascending", color = "#8B0000") ##as odds rather than log odds

#+ fig.width = 7.5, fig.height = 6
gg +labs(title="Figure 7: Odds ratios with error bars", y= "Explanatory variables")+formattable

##Train the data
SEED <- 5974
set.seed(SEED)
train = sample(1:nrow(analysis), size = nrow(analysis)/2)

model.glm.tr <- glm(stunted_f ~ hhhead_educ + perception_score + hh_income + province + agegroup+ 
                   village_stunted_sh, family = "binomial",data = analysis[train,])
summary(model.glm.tr)

##Get training set error for logit
analysis.test = analysis[-train, "stunted_f"]
yhat.stunted = predict(model.glm.tr,data=analysis.test, type = "response")
yhat.predicted = rep("Not stunted", 3099)

yhat.predicted[yhat.stunted>0.5] <- "Stunted"

table(yhat.predicted, analysis.test$stunted_f)

mean(yhat.predicted == analysis.test$stunted_f)

##MSE
mean((yhat.predicted!=analysis.test$stunted_f)^2)

##RMSE
sqrt(mean((yhat.predicted!=analysis.test$stunted_f)^2))

#------------------------------------------------------------------------
# Estimate LDA model 
SEED <- 5974
set.seed(SEED) # for replicability

train = sample(1:nrow(analysis), size = nrow(analysis)/2)

lda.model <- lda(stunted_f ~ hhhead_educ + perception_score + hh_income + province + agegroup+ 
   village_stunted_sh+ watersource, data = analysis[train,])

# Generate predicted probabilities using estimated model

lda.pred=predict(lda.model, data=analysis.test)

lda.class<- lda.pred$class # Save in a vector to be used for the confusion matrix
table(lda.class,analysis.test$stunted_f)
mean(lda.class==analysis.test$stunted_f)

##MSE
mean((lda.class!=analysis.test$stunted_f)^2)

##RMSE
sqrt(mean((lda.class!=analysis.test$stunted_f)^2))

#----------------------------------------------------------------------------
##Classification tree

set.seed(SEED)
rpart.stunted = rpart(stunted_f~., analysis, cp = 0.0000000000001)

plotcp(rpart.stunted)
printcp(rpart.stunted) ##levels off at cp = 4.0126e-03


##Assign labels

rpart.stunted.min<- prune(rpart.stunted, cp= 4.0126e-03)
rpart.plot(rpart.stunted.min)

rpart.stunted.1se <- rt.prune(rpart.stunted, se = 1) ##prune to satisfy 1 - se error
rpart.plot(rpart.stunted.1se, main = "Figure 8: Stunting 1-se classification tree") 

##Training error

analysis.test2 = analysis[-train, ]

rpart.stunted.train = rpart(stunted_f~., analysis, subset = train, cp = 0.02)
rpart.pred = predict(rpart.stunted.train, analysis.test2, type = "class")

mean((rpart.pred==analysis.test2$stunted_f))
table(rpart.pred, analysis.test2$stunted_f)
mean((rpart.pred!=analysis.test2$stunted_f)^2)
sqrt(mean((rpart.pred!=analysis.test2$stunted_f)^2))

#------------------------------------------------------------------------------


