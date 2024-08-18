# --------------------------------------------------------------------------------------------------
# Pragmatic Spatial Reasoning Experiment 3 Analysis
# --------------------------------------------------------------------------------------------------
# Subject pool: Amazon Mechanical Turk
# Date(s):      August 2023
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
# 6. Multivariate regression on responses as a function of ambiguity + directionality
# 6. (Alternative) Multivariate covariance generalized linear model on responses as a function of
#    ambiguity + directionality, controlling for noise from participants + problems
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
library(diptest)
library(car)
library(mcglm)
library(Matrix)
library(htmcglm)

# --------------------------------------------------------------------------------------------------
# 0. Preprocessing; filter subjs by attention check; reformatting responses
# --------------------------------------------------------------------------------------------------

##########
# Setup script

wd <- getwd()
if (wd != rstudioapi::getActiveDocumentContext()$path) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  wd <- getwd()
}

project <- "PSR3"
##########

rawData <- read.csv(paste("./", project, "-Aggregate.csv", sep=""), header=T)
data <- rawData
data$ParticipantID <- as.factor(data$ParticipantID)
data$AnswerContent <- gsub(";$", "", data$AnswerContent)
data$AnswerAbstract <- gsub("NA", "Nothing", data$AnswerAbstract)
fullN <- length(unique(data$ParticipantID))

# Subject exclusion based on AC performance
ac.data <- data
ac.data$AC1 <- ifelse(ac.data$Condition == "AC1" & lengths(regmatches(ac.data$AnswerAbstract, 
                                                                      gregexpr("A", ac.data$AnswerAbstract))) == 2, 1, 0)
ac.data$AC2 <- ifelse(ac.data$Condition == "AC2" & lengths(regmatches(ac.data$AnswerAbstract, 
                                                                      gregexpr("A", ac.data$AnswerAbstract))) == 3, 1, 0)
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
ctrl.threshold <- 100

# Subject exclusion based on CTRL performance
ctrl.data <- subset(data, Condition == "CTRL")
ctrl.data$missedCTRL <- ifelse(ctrl.data$AnswerContent != "nothing", 1, 0)
missedCTRL <- unique(c(as.character(subset(ctrl.data, missedCTRL == 1)$ParticipantID)))
data$missedCTRL <- ifelse(data$ParticipantID %in% missedCTRL, 1, 0)

#Percentage corect needed on AC trials to be included in analysis
ac.threshold <- 100

if (ac.threshold == 50) {
  # This option includes P's who missed one of the two AC trials
  excl.data <- subset(data, AC50!=1)
  excl.data <- rbind(excl.data, subset(data, missedCTRL==1))
  data <- subset(data, AC50==1 & missedCTRL==0)
  missedAC <- c(as.character(subset(ac.data.cor, Combined < 1)$ParticipantID))
} else {
  # Default includes only P's who got 100% corect on AC trials
  excl.data <- subset(data, AC100!=1)
  excl.data <- rbind(excl.data, subset(data, missedCTRL==1))
  data <- subset(data, AC100==1 & missedCTRL==0)
  missedAC <- c(as.character(subset(ac.data.cor, Combined < 2)$ParticipantID))
}

useableSubs <- unique(data$ParticipantID)
excl.subs <- setdiff(rawData$ParticipantID, useableSubs)
excludedN <- length(unique(excl.data$ParticipantID))
finalN <- length(unique(data$ParticipantID))
subjLoss <- round(1 - (finalN / fullN),2)
excl.data <- subset(excl.data, ProblemType != "AC")

data$Condition <- factor(data$Condition, levels = c("Ambiguous", "Unambiguous"))
data$Direction <- factor(data$Direction, levels = c("Above", "Right", "Below", "Left"))

data <- subset(data, ProblemType == "EXP" & Condition != "CTRL")

# --------------------------------------------------------------------------------------------------
# 1. Demographics summary
# --------------------------------------------------------------------------------------------------

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
#  fullN finalN excludedN meanAge minAge maxAge femaleCount maleCount otherCount preferNotCount
#     98     93         5   42.91     24     79          42        55          1              0

# --------------------------------------------------------------------------------------------------
# 2. Coding for accuracy + # of responses
# --------------------------------------------------------------------------------------------------

# Accuracy Rubric
data$Accuracy <- 0
data$Accuracy[data$Condition == "Ambiguous" & (data$AnswerAbstract == "A;" | data$AnswerAbstract == "C;" | 
                                            data$AnswerAbstract == "A;C;" | data$AnswerAbstract == "C;A;")] <- 1

data$Accuracy[data$Condition == "Unambiguous" & data$Direction == "Left" & data$AnswerAbstract == "C;"] <- 1

data$Accuracy[data$Condition == "Unambiguous" & data$Direction == "Right" & data$AnswerAbstract == "C;"] <- 1

data$Accuracy[data$Condition == "Unambiguous" & data$Direction == "Above" & data$AnswerAbstract == "A;"] <- 1

data$Accuracy[data$Condition == "Unambiguous" & data$Direction == "Below" & data$AnswerAbstract == "A;"] <- 1

table(data$Condition, data$Accuracy)
#               0   1
# Ambiguous     8 364
# Unambiguous 125 247

# Count for number of responses
data$ResponseCount <- 0
data$ResponseCount <- str_count(data$AnswerAbstract, ";")

table(data$Condition, data$ResponseCount)
#               1   2   3
# Ambiguous    19 351   2
# Unambiguous 260 112   0

data %>% group_by(Accuracy, Condition) %>% summarise(RespCount = mean(ResponseCount))
#   Accuracy Condition   RespCount
# 1        0 Ambiguous        1.5 
# 2        0 Unambiguous      1.90
# 3        1 Ambiguous        1.96
# 4        1 Unambiguous      1   

# Dummy coding responses
data$SelectedA <- ifelse(str_detect(data$AnswerAbstract, "A"), 1, 0)
data$SelectedB <- ifelse(str_detect(data$AnswerAbstract, "B"), 1, 0)
data$SelectedC <- ifelse(str_detect(data$AnswerAbstract, "C"), 1, 0)
data$SelectedNothing <- ifelse(str_detect(data$AnswerAbstract, "Noth"), 1, 0)

data$SelectedPos1.1 <- ifelse(str_detect(data$AnswerPosition, "1_1"), 1, 0)
data$SelectedPos1.2 <- ifelse(str_detect(data$AnswerPosition, "1_2"), 1, 0)
data$SelectedPos4.1 <- ifelse(str_detect(data$AnswerPosition, "4_1"), 1, 0)
data$SelectedPos4.2 <- ifelse(str_detect(data$AnswerPosition, "4_2"), 1, 0)

dummy.data <- data %>% select(ProblemNumber, ParticipantID, SelectedA, SelectedB, SelectedC, SelectedNothing) %>% 
  melt(value.name = "Response", id = c("ProblemNumber", "ParticipantID")) %>% arrange(ProblemNumber, ParticipantID)

#dip.test(dummy.data$Resp[dummy.data$ProblemNumber==8], B=2000)

# --------------------------------------------------------------------------------------------------
# 3. Wilcoxon comparing proportion of accuracy for ambiguous vs. unambiguous problems
# --------------------------------------------------------------------------------------------------

data %>% group_by(Condition) %>% summarise(Accuracy = mean(Accuracy))
#   Condition     Acc
# 1 Ambiguous      0.978
# 2 Unambiguous    0.664

test <- data %>% group_by(ParticipantID, Condition) %>% summarise(Accuracy = mean(Accuracy))

wilcoxsign_test(subset(test, Condition=="Ambiguous")$Accuracy ~ subset(test, Condition=="Unambiguous")$Accuracy)

# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# 
# data:  y by x (pos, neg) 
# stratified by block
# Z = 6.9102, p-value = 4.839e-12
# alternative hypothesis: true mu is not equal to 0

cliff.delta(subset(test, Condition=="Ambiguous")$Accuracy ~ subset(test, Condition=="Unambiguous")$Accuracy)

#   Cliff's Delta
# 
# delta estimate: -0.07142857 (negligible)
# 95 percent confidence interval:
#   lower       upper 
# -0.21771767  0.07799936 

# --------------------------------------------------------------------------------------------------
# 4. Wilcoxon comparing proportion of response counts for ambiguous vs. unambiguous problems
# --------------------------------------------------------------------------------------------------

data %>% group_by(Condition) %>% summarise(ResponseCount = mean(ResponseCount))
#   Condition   RespCount
# 1 Ambiguous            1.95
# 2 Unambiguous          1.30

test <- data %>% group_by(ParticipantID, Condition) %>% summarise(RespCount = mean(ResponseCount))

wilcoxsign_test(subset(test, Condition=="Ambiguous")$RespCount ~ subset(test, Condition=="Unambiguous")$RespCount)

# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# 
# data:  y by x (pos, neg) 
# stratified by block
# Z = 8.2845, p-value = 2.22e-16
# alternative hypothesis: true mu is not equal to 0

cliff.delta(subset(test, Condition=="Ambiguous")$RespCount ~ subset(test, Condition=="Unambiguous")$RespCount)

#   Cliff's Delta
# 
# delta estimate: 0.1458967 (negligible)
# 95 percent confidence interval:
#   lower      upper 
# -0.1188429  0.3912691 

# --------------------------------------------------------------------------------------------------
# 5. Additional tables
# --------------------------------------------------------------------------------------------------

data %>% group_by(Direction) %>% summarise(Acc = mean(Accuracy))
#   Direction   Acc
# 1 Above     0.780
# 2 Right     0.882
# 3 Below     0.753
# 4 Left      0.871

data %>% group_by(Direction) %>% summarise(RespCount = mean(ResponseCount))
#   Direction RespCount
# 1 Above          1.66
# 2 Right          1.58
# 3 Below          1.69
# 4 Left           1.58

# Nothing resps
# All nothing resps
filter(data, grepl("nothing",AnswerContent)) %>% group_by(Condition) %>% summarise(Count = n())
#   Condition   Count
# 1 Ambiguous       8
# 2 Unambiguous    94

# Nothing only resps
filter(data,AnswerContent=="nothing") %>% group_by(Condition) %>% summarise(Count = n())
# Condition   Count
# 1 Ambiguous       6
# 2 Unambiguous    10

# Nothing by direction
filter(data, grepl("nothing",AnswerContent)) %>% group_by(Direction) %>% summarise(Count = n())
#   Direction Count
# 1 Above        31
# 2 Right        19
# 3 Below        30
# 4 Left         22

# --------------------------------------------------------------------------------------------------
# 6. Multivariate regression on responses as a function of ambiguity + directionality
# --------------------------------------------------------------------------------------------------

mlm <- lm(cbind(SelectedA, SelectedB, SelectedC) ~ Condition + Direction, data=data)
summary(mlm)

# Response SelectedA :
#   
#   Call:
#   lm(formula = SelectedA ~ Condition + Direction, data = data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.2070 -0.2446 -0.1962  0.2661  0.7554 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           1.19624    0.02408  49.679   <2e-16 ***
#   ConditionUnambiguous -0.48925    0.02154 -22.716   <2e-16 ***
#   DirectionRight       -0.46237    0.03046 -15.180   <2e-16 ***
#   DirectionBelow        0.01075    0.03046   0.353    0.724    
# DirectionLeft        -0.45161    0.03046 -14.827   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2937 on 739 degrees of freedom
# Multiple R-squared:  0.5694,	Adjusted R-squared:  0.5671 
# F-statistic: 244.3 on 4 and 739 DF,  p-value: < 2.2e-16
# 
# 
# Response SelectedB :
#   
#   Call:
#   lm(formula = SelectedB ~ Condition + Direction, data = data)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.11828 -0.04839 -0.04301  0.02688  0.95699 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           0.01613    0.01513   1.066   0.2868    
# ConditionUnambiguous  0.07527    0.01353   5.562 3.73e-08 ***
#   DirectionRight       -0.04301    0.01914  -2.247   0.0249 *  
#   DirectionBelow        0.02688    0.01914   1.405   0.1606    
# DirectionLeft        -0.04839    0.01914  -2.528   0.0117 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1846 on 739 degrees of freedom
# Multiple R-squared:  0.06584,	Adjusted R-squared:  0.06079 
# F-statistic: 13.02 on 4 and 739 DF,  p-value: 2.978e-10
# 
# 
# Response SelectedC :
#   
#   Call:
#   lm(formula = SelectedC ~ Condition + Direction, data = data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.2137 -0.2487 -0.1922  0.2755  0.7460 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           0.719086   0.024000  29.962   <2e-16 ***
#   ConditionUnambiguous -0.470430   0.021466 -21.915   <2e-16 ***
#   DirectionRight        0.494624   0.030358  16.293   <2e-16 ***
#   DirectionBelow        0.005376   0.030358   0.177    0.859    
# DirectionLeft         0.473118   0.030358  15.585   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2928 on 739 degrees of freedom
# Multiple R-squared:  0.5709,	Adjusted R-squared:  0.5686 
# F-statistic: 245.8 on 4 and 739 DF,  p-value: < 2.2e-16

Manova(mlm)

# Type II MANOVA Tests: Pillai test statistic
# Df test stat approx F num Df den Df    Pr(>F)    
# Condition  1   0.79509   953.21      3    737 < 2.2e-16 ***
#   Direction  3   0.44612    43.03      9   2217 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# --------------------------------------------------------------------------------------------------
# 6. (Alternative) Multivariate covariance generalized linear model on responses as a function of
#    ambiguity + directionality, controlling for noise from participants + problems
# --------------------------------------------------------------------------------------------------

form.A <- data$SelectedA ~ data$Condition + data$Direction
form.B <- data$SelectedB ~ data$Condition + data$Direction
form.C <- data$SelectedC ~ data$Condition + data$Direction

Z0 <- mc_id(data)
Z1 <- mc_mixed(~0 + factor(data$ParticipantID), data = data)

fit_ABC <- mcglm(linear_pred = c(form.A, form.B, form.C),
                 matrix_pred = list(c(Z0,Z1), c(Z0,Z1), c(Z0,Z1)),
			     link=c("identity", "identity", "identity"),
				 variance = c("constant","constant","constant"),
				 Ntrial = list(NULL, NULL, NULL),
			     data=data)

summary(fit_ABC)

# Call: data$SelectedA ~ data$Condition + data$Direction
# 
# Link function: identity
# Variance function: constant
# Covariance function: identity
# Regression:
#   Estimates  Std.error     Z value      Pr(>|z|)
# (Intercept)                1.18124825 0.03701445  31.9131678 1.753382e-223
# data$ConditionUnambiguous -0.48682546 0.02246651 -21.6689411 4.029154e-104
# data$DirectionRight       -0.48973082 0.03187839 -15.3624686  2.922317e-53
# data$DirectionBelow       -0.01141173 0.03171747  -0.3597931  7.190019e-01
# data$DirectionLeft        -0.45901801 0.03186289 -14.4060404  4.741239e-47
# 
# Dispersion:
#   Estimates Std.error Z value Pr(>|z|)
# 1 0.10038474       NaN     NaN      NaN
# 2 0.09076109       NaN     NaN      NaN
# 
# Call: data$SelectedB ~ data$Condition + data$Direction
# 
# Link function: identity
# Variance function: constant
# Covariance function: identity
# Regression:
#   Estimates  Std.error    Z value     Pr(>|z|)
# (Intercept)                0.01517242 0.01567453  0.9679661 3.330613e-01
# data$ConditionUnambiguous  0.07559564 0.01264908  5.9763736 2.281594e-09
# data$DirectionRight       -0.04665345 0.01789152 -2.6075727 9.118671e-03
# data$DirectionBelow        0.02432275 0.01788674  1.3598199 1.738869e-01
# data$DirectionLeft        -0.04932817 0.01789101 -2.7571487 5.830783e-03
# 
# Dispersion:
#   Estimates   Std.error  Z value     Pr(>|z|)
# 1 0.029868046 0.005572063 5.360321 8.307405e-08
# 2 0.004331266 0.001055339 4.104146 4.058111e-05
# 
# Call: data$SelectedC ~ data$Condition + data$Direction
# 
# Link function: identity
# Variance function: constant
# Covariance function: identity
# Regression:
#   Estimates  Std.error     Z value      Pr(>|z|)
# (Intercept)                0.72380408 0.02582091  28.0317000 6.677360e-173
# data$ConditionUnambiguous -0.47255557 0.02249572 -21.0064623  5.724389e-98
# data$DirectionRight        0.52178248 0.03191606  16.3485874  4.451811e-60
# data$DirectionBelow        0.02332883 0.03175713   0.7346012  4.625824e-01
# data$DirectionLeft         0.47999224 0.03189939  15.0470653  3.608923e-51
# 
# Dispersion:
#   Estimates   Std.error  Z value      Pr(>|z|)
# 1 0.100598978 0.004471991 22.49534 4.610028e-112
# 2 0.006128642         NaN      NaN           NaN
# 
# Correlation matrix:
#   Parameters  Estimates  Std.error   Z value     Pr(>|z|)
# 1      rho12  0.1943360 0.03780095  5.141035 2.732294e-07
# 2      rho13 -0.7641305        NaN       NaN          NaN
# 3      rho23 -0.1685292 0.03794890 -4.440951 8.956206e-06
# 
# Algorithm: chaser
# Correction: TRUE
# Number iterations: 20

mc_manova_II(fit_ABC)

# MANOVA type II using Wald statistic for fixed effects
# 
# Call: ~ data$Condition+data$Direction
# Covariate Df       Chi Pr(>Chi)
# 1      Intercept  3 5181.9809        0
# 2 data$Condition  3 3640.7723        0
# 3 data$Direction  9  525.8278        0

# --------------------------------------------------------------------------------------------------
# 7. Figures
# --------------------------------------------------------------------------------------------------

respA <- filter(data, SelectedA==1) %>% group_by(ProblemNumber) %>% summarise(Prop = n()/finalN)
#   ProblemNumber   Prop
# 1             1 0.989 
# 2             2 0.0108
# 3             3 0.968 
# 4             4 0.0108
# 5             5 0.957 
# 6             6 0.946 
# 7             7 0.968 
# 8             8 0.957 

respB <- filter(data, SelectedB==1) %>% group_by(ProblemNumber) %>% summarise(Prop = n()/finalN)
#   ProblemNumber   Prop
# 1             2 0.0108
# 2             4 0.0215
# 3             6 0.108 
# 4             8 0.161 

respC <- filter(data, SelectedC==1) %>% group_by(ProblemNumber) %>% summarise(Prop = n()/finalN)
#   ProblemNumber  Prop
# 1             1 0.946 
# 2             2 0.968 
# 3             3 0.968 
# 4             4 0.989 
# 5             5 0.968 
# 6             7 0.968 
# 7             8 0.0108

respDF <- data.frame(ProblemNumber = seq(1,8,1))
respDF$Prop0 <- rep(0,8)
respDF <- merge(respDF,respA,all.x = TRUE)
respDF <- rename(respDF, "A" = "Prop")
respDF <- merge(respDF,respB,all.x = TRUE)
respDF <- rename(respDF, "B" = "Prop")
respDF <- merge(respDF,respC,all.x = TRUE)
respDF <- rename(respDF, "C" = "Prop")
respDF <- respDF %>% select(ProblemNumber, A, B, C)
respDF[is.na(respDF)] <- 0

pos1.1 <- filter(data, SelectedPos1.1==1) %>% group_by(ProblemNumber) %>% summarise(Prop = n()/finalN)
pos1.2 <- filter(data, SelectedPos1.2==1) %>% group_by(ProblemNumber) %>% summarise(Prop = n()/finalN)
pos4.1 <- filter(data, SelectedPos4.1==1) %>% group_by(ProblemNumber) %>% summarise(Prop = n()/finalN)
pos4.2 <- filter(data, SelectedPos4.2==1) %>% group_by(ProblemNumber) %>% summarise(Prop = n()/finalN)

posDF <- data.frame(ProblemNumber = seq(1,8,1))
posDF$Prop0 <- rep(0,8)
posDF <- merge(posDF,pos1.1,all.x = TRUE)
posDF <- rename(posDF, "Position_1_1" = "Prop")
posDF <- merge(posDF,pos1.2,all.x = TRUE)
posDF <- rename(posDF, "Position_1_2" = "Prop")
posDF <- merge(posDF,pos4.1,all.x = TRUE)
posDF <- rename(posDF, "Position_4_1" = "Prop")
posDF <- merge(posDF,pos4.2,all.x = TRUE)
posDF <- rename(posDF, "Position_4_2" = "Prop")
posDF <- posDF %>% select(ProblemNumber, Position_1_1, Position_1_2, Position_4_1, Position_4_2)
posDF[is.na(posDF)] <- 0

figMaker <- function(probNum) {
  figDF <- data.frame(x = c(1,2,1,2,1,2,1,2))
  figDF$y <- rev(c(1,1,2,2,3,3,4,4))
  figDF$labelName <- c(data$Position_1_1[data$ProblemNumber==probNum][1], 
                       data$Position_1_2[data$ProblemNumber==probNum][1], 
                       "", "", "", "", 
                       data$Position_4_1[data$ProblemNumber==probNum][1], 
                       data$Position_4_2[data$ProblemNumber==probNum][1])

  referent <- list(c(2,8), 8, c(1,7), 1, c(7,8), 7, c(1,2), 1)
  figDF$alpha <- c(posDF[probNum,"Position_1_1"], posDF[probNum,"Position_1_2"], -1, -1, 
                   -1, -1, posDF[probNum,"Position_4_1"], posDF[probNum,"Position_4_2"])
  figDF$label <- ifelse(figDF$alpha==-1, "", 
                        paste(figDF$labelName, " (", round(figDF$alpha, digits=2)*100, "%)", sep=""))
  figDF$color <- ifelse(figDF$alpha>0.5, "white", "black")
  figDF$border <- ifelse(figDF$labelName=="", "white", "black")
  for (r in referent[probNum]) {
    figDF$border[r] <- "red"
  }
  figDF$fontface <- ifelse(figDF$alpha>0, "bold", "plain")
  figDirection <- tolower(data$Direction[data$ProblemNumber==probNum][1])
  if (figDirection == "left") {
    figDirection <- "left of"
  } else if (figDirection == "right") {
    figDirection <- "right of"
  }
  
  figName <- paste("Problem ", probNum, ": What is ", figDirection, " B?", sep="")
  
  fig <- ggplot(data = figDF, aes(x,y)) +
    geom_tile(alpha=figDF$alpha, colour = figDF$border, width=0.95, height=0.95, size=1.0) +
    scale_size(range = c(2, 20)) +
    geom_text(size=3, nudge_y = 0, label = figDF$label, colour=figDF$color, fontface=figDF$fontface) +
    scale_y_continuous(name="", limits=c(0, 4.5), breaks=c(0, 4), labels=rep("", 2)) +
    scale_x_continuous(name=figName, limits=c(0, 3), breaks=c(0, 1, 2, 3), labels=rep("", 4), position="top") +
    theme_minimal(10) +
    theme(panel.grid = element_blank(),
          axis.line.x = element_blank())
  
  return(fig)
}

# figMaker
# 1: Ambiguous   - left
# 2: Unambiguous - left
# 3: Ambiguous   - right
# 4: Unambiguous - right
# 5: Ambiguous   - above
# 6: Unambiguous - above
# 7: Ambiguous   - below
# 8: Unambiguous - below
