# --------------------------------------------------------------------------------------------------
# Relational Tracking Experiment 1 Analysis
# --------------------------------------------------------------------------------------------------
# Subject pool: Amazon Mechanical Turk
# Date(s):      April 2024
# Design:       Branden Bio & Sunny Khemlani
# Code:         Branden Bio
# Experimenter:	Knexus Research Corporation
# --------------------------------------------------------------------------------------------------
# Contents:
# 0. Preprocessing; filter subjs by attention check; reformatting responses
# 1. Demographics summary
# 2. Coding for accuracy + tables
# 3. Wilcoxon comparing proportion of accuracy for ambiguous vs. unambiguous problems
# 4. Wilcoxon comparing proportion of response counts for ambiguous vs. unambiguous problems
# 5. Additional tables
# 7. Figures
# --------------------------------------------------------------------------------------------------

library(tidyr)
library(stringr)
library(coin)
library(reshape2)
library(ggplot2)
library(dplyr)
library(lme4)
library(effsize)
library(rstudioapi)

# --------------------------------------------------------------------------------------------------
# 0. Preprocessing; filter subjs by attention check; reformatting responses
# --------------------------------------------------------------------------------------------------

##########
# Setup script
rm(list = ls())
wd <- getwd()
if (wd != rstudioapi::getActiveDocumentContext()$path) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  wd <- getwd()
}

project <- "RT1"
##########

rawData <- read.csv(paste("./", project, "-Aggregate.csv", sep=""), header=T)
data <- rawData
data$ParticipantID <- as.factor(data$ParticipantID)
fullN <- length(unique(data$ParticipantID))

# Subject exclusion based on AC performance
ac.data <- data
ac.data$AC1 <- ifelse(ac.data$Direction == "AC1" & ac.data$Answer == "Yes", 1, 0)
ac.data$AC2 <- ifelse(ac.data$Direction == "AC2" & ac.data$Answer == "No", 1, 0)
ac.data <- subset(ac.data, ProblemType == "AC") %>% select(ParticipantID, AC1, AC2)
ac.data.cor <- melt(ac.data, id = "ParticipantID") %>% group_by(ParticipantID) %>% 
  summarise(Combined = sum(value))
ac.data.cor$AC50 <- ifelse(ac.data.cor$Combined >= 1, 1, 0)
ac.data.cor$AC100 <- ifelse(ac.data.cor$Combined == 2, 1, 0)

#failedAC <- c(as.character(subset(ac.data.cor, Combined == 0)$ParticipantID))
AC50 <- c(as.character(subset(ac.data.cor, Combined == 1)$ParticipantID))
AC100 <- c(as.character(subset(ac.data.cor, Combined == 2)$ParticipantID))

#data$FailedAC <- ifelse(data$ParticipantID %in% failedAC, 1, 0)
data$AC50 <- ifelse(data$ParticipantID %in% AC50, 1, 0)
data$AC100 <- ifelse(data$ParticipantID %in% AC100, 1, 0)

#Percentage corect needed on AC trials to be included in analysis
# ctrl.threshold <- 100

# Subject exclusion based on CTRL performance
# ctrl.data <- subset(data, Condition == "CTRL")
# ctrl.data$missedCTRL <- ifelse(ctrl.data$AnswerContent != "nothing", 1, 0)
# missedCTRL <- unique(c(as.character(subset(ctrl.data, missedCTRL == 1)$ParticipantID)))
# data$missedCTRL <- ifelse(data$ParticipantID %in% missedCTRL, 1, 0)

#Percentage corect needed on AC trials to be included in analysis
ac.threshold <- 100

if (ac.threshold == 50) {
  # This option includes P's who missed one of the two AC trials
  excl.data <- subset(data, AC50!=1)
  # excl.data <- rbind(excl.data, subset(data, missedCTRL==1))
  data <- subset(data, AC50==1) #& missedCTRL==0)
  missedAC <- c(as.character(subset(ac.data.cor, Combined < 1)$ParticipantID))
} else {
  # Default includes only P's who got 100% corect on AC trials
  excl.data <- subset(data, AC100!=1)
  # excl.data <- rbind(excl.data, subset(data, missedCTRL==1))
  data <- subset(data, AC100==1) #& missedCTRL==0)
  missedAC <- c(as.character(subset(ac.data.cor, Combined < 2)$ParticipantID))
}

useableSubs <- unique(data$ParticipantID)
excl.subs <- setdiff(rawData$ParticipantID, useableSubs)
excludedN <- length(unique(excl.data$ParticipantID))
finalN <- length(unique(data$ParticipantID))
subjLoss <- round(1 - (finalN / fullN),2)
excl.data <- subset(excl.data, ProblemType != "AC")

#data$Direction <- factor(data$Direction, levels = c("Above", "Right", "Below", "Left"))

data <- subset(data, ProblemType == "EXP") #& Condition != "CTRL")

# # --------------------------------------------------------------------------------------------------
# # 1. Demographics summary
# # --------------------------------------------------------------------------------------------------

demoSummary <- function(subs) {
  # Import & subset data
  demoData <- read.csv(paste("./", project, "-Demographics.csv", sep=""), header=T)

  # Age info
  meanAge <- round(mean(demoData$Age), 2)
  minAge  <- min(demoData$Age)
  maxAge  <- max(demoData$Age)

  # Sex info
  maleCount      <- sum(str_count(demoData$Sex, "Male"))
  femaleCount    <- sum(str_count(demoData$Sex, "Female"))
  otherCount     <- sum(str_count(demoData$Sex, "Other"))
  preferNotCount <- sum(str_count(demoData$Sex, "Prefernot"))

  # Output df
  demoSum <- data.frame(fullN, finalN, excludedN, meanAge, minAge, maxAge,
                        femaleCount, maleCount, otherCount, preferNotCount)
  return(demoSum)
}

demoSummary(rawData$ParticipantID)
# fullN finalN excludedN meanAge minAge maxAge femaleCount maleCount otherCount preferNotCount
#   102    100         2   45.13     18     78          46        56          0              0

# --------------------------------------------------------------------------------------------------
# 2. Translating coordinates for analysis
# --------------------------------------------------------------------------------------------------

data$translated_X_Coord_Obj1 <- data$X_Coord_Obj1 - data$X_Coord_Obj2
data$translated_Y_Coord_Obj1 <- data$Y_Coord_Obj1 - data$Y_Coord_Obj2
data$translated_X_Coord_Obj2 <- data$X_Coord_Obj2 - data$X_Coord_Obj2
data$translated_Y_Coord_Obj2 <- data$Y_Coord_Obj2 - data$Y_Coord_Obj2

data$translated_Answer <- ifelse(data$Answer=="Yes",1,0)

data$Obj1_Leftof_Obj2  <- ifelse(data$translated_X_Coord_Obj1<0,1,0)
data$Obj1_Rightof_Obj2 <- ifelse(data$translated_X_Coord_Obj1>0,1,0)
data$Obj1_Above_Obj2   <- ifelse(data$translated_Y_Coord_Obj1<0,1,0)
data$Obj1_Below_Obj2   <- ifelse(data$translated_Y_Coord_Obj1>0,1,0)

data$Acc <- 0

data$Acc[data$Direction=="left" & data$Obj1_Leftof_Obj2==1 & data$translated_Answer==1] <- 1
data$Acc[data$Direction=="right" & data$Obj1_Rightof_Obj2==1 & data$translated_Answer==1] <- 1
data$Acc[data$Direction=="above" & data$Obj1_Above_Obj2==1 & data$translated_Answer==1] <- 1
data$Acc[data$Direction=="below" & data$Obj1_Below_Obj2==1 & data$translated_Answer==1] <- 1

# write.csv(data, "/Users/bbio/Repository/Projects/Perception/Relational Tracking/modeling/RT1/RT1-humanData.csv", row.names=FALSE)

# # --------------------------------------------------------------------------------------------------
# # 3. Wilcoxon comparing proportion of accuracy for ambiguous vs. unambiguous problems
# # --------------------------------------------------------------------------------------------------
# 
# data %>% group_by(Condition) %>% summarise(Accuracy = mean(Accuracy))
# #   Condition     Acc
# # 1 Ambiguous   0.975
# # 2 Unambiguous 0.662
# 
# test <- data %>% group_by(ParticipantID, Condition) %>% summarise(Accuracy = mean(Accuracy))
# 
# wilcoxsign_test(subset(test, Condition=="Ambiguous")$Accuracy ~ subset(test, Condition=="Unambiguous")$Accuracy)
# 
# # Asymptotic Wilcoxon-Pratt Signed-Rank Test
# # 
# # data:  y by x (pos, neg) 
# # stratified by block
# # Z = 4.6383, p-value = 3.513e-06
# # alternative hypothesis: true mu is not equal to 0
# 
# cliff.delta(subset(test, Condition=="Ambiguous")$Accuracy ~ subset(test, Condition=="Unambiguous")$Accuracy)
# 
# #   Cliff's Delta
# # 
# # delta estimate: -0.1111111 (negligible)
# # 95 percent confidence interval:
# #      lower      upper 
# # -0.3423214  0.1327874 
# 
# # --------------------------------------------------------------------------------------------------
# # 4. Wilcoxon comparing proportion of response counts for ambiguous vs. unambiguous problems
# # --------------------------------------------------------------------------------------------------
# 
# data %>% group_by(Condition) %>% summarise(ResponseCount = mean(ResponseCount))
# #   Condition   RespCount
# # 1 Ambiguous        1.95
# # 2 Unambiguous      1.33
# 
# test <- data %>% group_by(ParticipantID, Condition) %>% summarise(RespCount = mean(ResponseCount))
# 
# wilcoxsign_test(subset(test, Condition=="Ambiguous")$RespCount ~ subset(test, Condition=="Unambiguous")$RespCount)
# 
# # Asymptotic Wilcoxon-Pratt Signed-Rank Test
# # 
# # data:  y by x (pos, neg) 
# # stratified by block
# # Z = 5.8952, p-value = 3.742e-09
# # alternative hypothesis: true mu is not equal to 0
# 
# cliff.delta(subset(test, Condition=="Ambiguous")$RespCount ~ subset(test, Condition=="Unambiguous")$RespCount)
# 
# #   Cliff's Delta
# # 
# # delta estimate: -0.06666667 (negligible)
# # 95 percent confidence interval:
# #      lower      upper 
# # -0.4532696  0.3410465 
# 
# # --------------------------------------------------------------------------------------------------
# # 5. Additional tables
# # --------------------------------------------------------------------------------------------------
# 
# data %>% group_by(Direction) %>% summarise(Acc = mean(Accuracy))
# #   Direction   Acc
# # 1 Above     0.765
# # 2 Right     0.853
# # 3 Below     0.775
# # 4 Left      0.882
# 
# data %>% group_by(Direction) %>% summarise(RespCount = mean(ResponseCount))
# #   Direction RespCount
# # 1 Above          1.73
# # 2 Right          1.57
# # 3 Below          1.69
# # 4 Left           1.59

# --------------------------------------------------------------------------------------------------
# 7. Figures
# --------------------------------------------------------------------------------------------------

ggplot(data, aes(x = translated_X_Coord_Obj1, y = translated_Y_Coord_Obj1, color = translated_Answer)) +
    geom_point() +
    geom_text(data = NULL, aes(x = 0, y = 0, label = "X"), color = "black") +
    xlim(-400, 400) +
    ylim(400, -400) +
    scale_color_gradient(low = "blue", high = "red", breaks = c(0.01, 0.99), labels = c("No", "Yes")) +
    theme_minimal() +
    labs(title = "Heatmap of proximity judgments",
         x = "Object 1 X Coord",
         y = "Object 1 Y Coord",
         color = "Answer")

# Figures by problem direction
left_probs <- filter(data,Direction=="left")

ggplot(left_probs, aes(x = translated_X_Coord_Obj1, y = translated_Y_Coord_Obj1, color = translated_Answer)) +
    geom_point() +
    geom_text(data = NULL, aes(x = 0, y = 0, label = "X"), color = "black") +
    xlim(-400, 400) +
    ylim(400, -400) +
    scale_color_gradient(low = "blue", high = "red", breaks = c(0.01, 0.99), labels = c("No", "Yes")) +
    theme_minimal() +
    labs(title = "Ball on left?",
         x = "Object 1 X Coord",
         y = "Object 1 Y Coord",
         color = "Answer")

right_probs <- filter(data,Direction=="right")

ggplot(right_probs, aes(x = translated_X_Coord_Obj1, y = translated_Y_Coord_Obj1, color = translated_Answer)) +
    geom_point() +
    geom_text(data = NULL, aes(x = 0, y = 0, label = "X"), color = "black") +
    xlim(-400, 400) +
    ylim(400, -400) +
    scale_color_gradient(low = "blue", high = "red", breaks = c(0.01, 0.99), labels = c("No", "Yes")) +
    theme_minimal() +
    labs(title = "Ball on right?",
         x = "Object 1 X Coord",
         y = "Object 1 Y Coord",
         color = "Answer")

above_probs <- filter(data,Direction=="above")

ggplot(above_probs, aes(x = translated_X_Coord_Obj1, y = translated_Y_Coord_Obj1, color = translated_Answer)) +
    geom_point() +
    geom_text(data = NULL, aes(x = 0, y = 0, label = "X"), color = "black") +
    xlim(-400, 400) +
    ylim(400, -400) +
    scale_color_gradient(low = "blue", high = "red", breaks = c(0.01, 0.99), labels = c("No", "Yes")) +
    theme_minimal() +
    labs(title = "Ball above?",
         x = "Object 1 X Coord",
         y = "Object 1 Y Coord",
         color = "Answer")

below_probs <- filter(data,Direction=="below")

ggplot(below_probs, aes(x = translated_X_Coord_Obj1, y = translated_Y_Coord_Obj1, color = translated_Answer)) +
    geom_point() +
    geom_text(data = NULL, aes(x = 0, y = 0, label = "X"), color = "black") +
    xlim(-400, 400) +
    ylim(400, -400) +
    scale_color_gradient(low = "blue", high = "red", breaks = c(0.01, 0.99), labels = c("No", "Yes")) +
    theme_minimal() +
    labs(title = "Ball below?",
         x = "Object 1 X Coord",
         y = "Object 1 Y Coord",
         color = "Answer")

# Problems by ball location
left_config <- filter(data,Obj1_Leftof_Obj2==1 & Direction=="left")

ggplot(left_config, aes(x = translated_X_Coord_Obj1, y = translated_Y_Coord_Obj1, color = Acc)) +
    geom_point() +
    geom_text(data = NULL, aes(x = 0, y = 0, label = "X"), color = "black") +
    xlim(-400, 400) +
    ylim(400, -400) +
    scale_color_gradient(low = "blue", high = "red", breaks = c(0.01, 0.99), labels = c("Inc", "Cor")) +
    theme_minimal() +
    labs(title = "Located left",
         x = "Object 1 X Coord",
         y = "Object 1 Y Coord",
         color = "Answer")

right_config <- filter(data,Obj1_Rightof_Obj2==1 & Direction=="right")

ggplot(right_config, aes(x = translated_X_Coord_Obj1, y = translated_Y_Coord_Obj1, color = Acc)) +
    geom_point() +
    geom_text(data = NULL, aes(x = 0, y = 0, label = "X"), color = "black") +
    xlim(-400, 400) +
    ylim(400, -400) +
    scale_color_gradient(low = "blue", high = "red", breaks = c(0.01, 0.99), labels = c("Inc", "Cor")) +
    theme_minimal() +
    labs(title = "Located right",
         x = "Object 1 X Coord",
         y = "Object 1 Y Coord",
         color = "Answer")

above_config <- filter(data,Obj1_Above_Obj2==1 & Direction=="above")

ggplot(above_config, aes(x = translated_X_Coord_Obj1, y = translated_Y_Coord_Obj1, color = Acc)) +
    geom_point() +
    geom_text(data = NULL, aes(x = 0, y = 0, label = "X"), color = "black") +
    xlim(-400, 400) +
    ylim(400, -400) +
    scale_color_gradient(low = "blue", high = "red", breaks = c(0.01, 0.99), labels = c("Inc", "Cor")) +
    theme_minimal() +
    labs(title = "Located above",
         x = "Object 1 X Coord",
         y = "Object 1 Y Coord",
         color = "Answer")

below_config <- filter(data,Obj1_Below_Obj2==1 & Direction=="below")

ggplot(below_config, aes(x = translated_X_Coord_Obj1, y = translated_Y_Coord_Obj1, color = Acc)) +
    geom_point() +
    geom_text(data = NULL, aes(x = 0, y = 0, label = "X"), color = "black") +
    xlim(-400, 400) +
    ylim(400, -400) +
    scale_color_gradient(low = "blue", high = "red", breaks = c(0.01, 0.99), labels = c("Inc", "Cor")) +
    theme_minimal() +
    labs(title = "Located below",
         x = "Object 1 X Coord",
         y = "Object 1 Y Coord",
         color = "Answer")
