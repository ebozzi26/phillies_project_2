#Load Packages
library(tidyverse)
library(dplyr)
library(caret)
library(corrplot)
library(ggcorrplot)
library(caTools)
library(reshape2)
###Pre Processing###

#Load files
batting <- read.csv("C:/Users/Admin/Downloads/batting.csv")
exitvelo <- read.csv("C:/Users/Admin/Downloads/FanGraphs Leaderboard (5).csv")
colnames(exitvelo)[1] <- "Events"
#Merge Files
df <- merge(batting, exitvelo, by = "playerid")

#Change % fields to numeric decimals, merge them back into data frame
df$MarApr_BB <- as.numeric(sub("%", "", df$MarApr_BB))/100
df$MarApr_K<- as.numeric(sub("%", "", df$MarApr_K))/100
df$MarApr_LD <- as.numeric(sub("%", "", df$MarApr_LD))/100
df$MarApr_GB<- as.numeric(sub("%", "", df$MarApr_GB))/100
df$MarApr_FB <- as.numeric(sub("%", "", df$MarApr_FB))/100
df$MarApr_IFFB <- as.numeric(sub("%", "", df$MarApr_IFFB))/100
df$MarApr_HR.FB <- as.numeric(sub("%", "", df$MarApr_HR.FB))/100
df$MarApr_O.Swing <- as.numeric(sub("%", "", df$MarApr_O.Swing))/100
df$MarApr_Z.Swing <- as.numeric(sub("%", "", df$MarApr_Z.Swing))/100
df$MarApr_Swing <- as.numeric(sub("%", "", df$MarApr_Swing))/100
df$MarApr_O.Contact <- as.numeric(sub("%", "", df$MarApr_O.Contact))/100
df$MarApr_Z.Contact <- as.numeric(sub("%", "", df$MarApr_Z.Contact))/100
df$MarApr_Contact <- as.numeric(sub("%", "", df$MarApr_Contact))/100

df <- df[-c(11,12,18:28)]

#Only Numeric Columns in df2, also remove playerid
batters <- df %>% dplyr::select(where(is.numeric))

#Remove ID Column
batters <- batters[-c(1)]

#Scale all but full season OBP
batters[, -c(13)] <- scale(batters[, -c(13)])

###Building The Model

#Model 1 to compare Dependent Variable FullSeason_OBP to all variables
model1 <- lm(FullSeason_OBP ~ ., data = batters)
summary(model1)

#Use the Step function to improve model - https://towardsdatascience.com/modelling-multiple-linear-regression-using-r-research-oriented-modelling-and-interpretation-206bea8e43c1
model2 <- step(model1, direction = "backward")

model3 <- lm(FullSeason_OBP ~ MarApr_BABIP + MarApr_AVG + MarApr_SLG +  MarApr_O.Swing + MarApr_O.Contact + EV +
               Events + MarApr_Swing + MarApr_K + MarApr_Z.Swing + MarApr_Z.Contact + MarApr_Contact + MarApr_AB +
               MarApr_H + MarApr_PA,
             data = batters)

summary(model3)

#Predictions
final <- df
final$PredictedOBP <- predict(model3, batters)
final$OBPdiff <- final$PredictedOBP - final$FullSeason_OBP


# Team Data
wins <- read.csv("C:/Users/Admin/Downloads/team records.csv")
wins <- wins[,colSums(is.na(wins))<nrow(wins)]
wins <- wins[complete.cases(wins), ]
wins <- t(wins)
wins <- data.frame(r1= row.names(wins), wins, row.names=NULL) 
colnames(wins)[1] <- "Tm"
colnames(wins)[2] <- "2021"
colnames(wins)[3] <- "2020"
colnames(wins)[4] <- "2019"
wins <- wins[-c(1), ]
wins$percent <- rowSums(wins[c(2,3,4)])/ 384
wins <- wins[c(1,5)]
bottomorder <- read.csv("C:/Users/Admin/Downloads/Splits Leaderboard Data Batting 7th Batting 8th Batting 6th.csv")
bottomorder <- bottomorder[c(2, 8)]
teams <- merge(wins, bottomorder, by = "Tm")

ggplot(teams, aes(x = OBP, y = percent)) +  geom_point() +
  geom_smooth(method = lm, se = FALSE)
