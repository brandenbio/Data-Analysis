# --------------------------------------------------------------------------------------------------
# Pragmatic Spatial Reasoning Experiment 2 Analysis
# --------------------------------------------------------------------------------------------------
# Subject pool: Amazon Mechanical Turk
# Date(s):      May 2023
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

project <- "PSR2"
##########

rawData <- read.csv(paste("./", project, "-Aggregate.csv", sep=""), header=T)
data <- rawData
data$ParticipantID <- as.factor(data$ParticipantID)
data$AnswerContent <- gsub(";$", "", data$AnswerContent)
data$AnswerAbstract <- gsub("NA", "Nothing", data$AnswerAbstract)
fullN <- length(unique(data$ParticipantID))

# Subject exclusion based on AC performance
ac.data <- data
ac.data$AC1 <- ifelse(ac.data$Condition == "AC1" & ac.data$AnswerContent == "two", 1, 0)
ac.data$AC2 <- ifelse(ac.data$Condition == "AC2" & ac.data$AnswerContent == "three", 1, 0)
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
#fullN finalN excludedN meanAge minAge maxAge femaleCount maleCount otherCount preferNotCount
#   60     51         9      37     22     75          29        31          0              0

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
# Ambiguous     5 199
# Unambiguous  69 135

# Count for number of responses
data$ResponseCount <- 0
data$ResponseCount <- str_count(data$AnswerAbstract, ";")

table(data$Condition, data$ResponseCount)
#               1   2   3
# Ambiguous    12 190   2
# Unambiguous 139  62   3

data %>% group_by(Accuracy, Condition) %>% summarise(RespCount = mean(ResponseCount))
#   Accuracy Condition   RespCount
#      <dbl> <fct>           <dbl>
# 1        0 Ambiguous        1.8
# 2        0 Unambiguous      1.99
# 3        1 Ambiguous        1.95
# 4        1 Unambiguous      1

# Dummy coding responses
data$SelectedA <- ifelse(str_detect(data$AnswerAbstract, "A"), 1, 0)
data$SelectedB <- ifelse(str_detect(data$AnswerAbstract, "B"), 1, 0)
data$SelectedC <- ifelse(str_detect(data$AnswerAbstract, "C"), 1, 0)
data$SelectedNothing <- ifelse(str_detect(data$AnswerAbstract, "Noth"), 1, 0)

dummy.data <- data %>% select(ProblemNumber, ParticipantID, SelectedA, SelectedB, SelectedC, SelectedNothing) %>% 
  melt(value.name = "Response", id = c("ProblemNumber", "ParticipantID")) %>% arrange(ProblemNumber, ParticipantID)

#dip.test(dummy.data$Resp[dummy.data$ProblemNumber==8], B=2000)

# --------------------------------------------------------------------------------------------------
# 3. Wilcoxon comparing proportion of accuracy for ambiguous vs. unambiguous problems
# --------------------------------------------------------------------------------------------------

data %>% group_by(Condition) %>% summarise(Accuracy = mean(Accuracy))
#   Condition     Acc
# 1 Ambiguous   0.975
# 2 Unambiguous 0.662

test <- data %>% group_by(ParticipantID, Condition) %>% summarise(Accuracy = mean(Accuracy))

wilcoxsign_test(subset(test, Condition=="Ambiguous")$Accuracy ~ subset(test, Condition=="Unambiguous")$Accuracy)

# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# 
# data:  y by x (pos, neg) 
# stratified by block
# Z = 4.6383, p-value = 3.513e-06
# alternative hypothesis: true mu is not equal to 0

cliff.delta(subset(test, Condition=="Ambiguous")$Accuracy ~ subset(test, Condition=="Unambiguous")$Accuracy)

#   Cliff's Delta
# 
# delta estimate: -0.1111111 (negligible)
# 95 percent confidence interval:
#      lower      upper 
# -0.3423214  0.1327874 

# --------------------------------------------------------------------------------------------------
# 4. Wilcoxon comparing proportion of response counts for ambiguous vs. unambiguous problems
# --------------------------------------------------------------------------------------------------

data %>% group_by(Condition) %>% summarise(ResponseCount = mean(ResponseCount))
#   Condition   RespCount
# 1 Ambiguous        1.95
# 2 Unambiguous      1.33

test <- data %>% group_by(ParticipantID, Condition) %>% summarise(RespCount = mean(ResponseCount))

wilcoxsign_test(subset(test, Condition=="Ambiguous")$RespCount ~ subset(test, Condition=="Unambiguous")$RespCount)

# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# 
# data:  y by x (pos, neg) 
# stratified by block
# Z = 5.8952, p-value = 3.742e-09
# alternative hypothesis: true mu is not equal to 0

cliff.delta(subset(test, Condition=="Ambiguous")$RespCount ~ subset(test, Condition=="Unambiguous")$RespCount)

#   Cliff's Delta
# 
# delta estimate: -0.06666667 (negligible)
# 95 percent confidence interval:
#      lower      upper 
# -0.4532696  0.3410465 

# --------------------------------------------------------------------------------------------------
# 5. Additional tables
# --------------------------------------------------------------------------------------------------

data %>% group_by(Direction) %>% summarise(Acc = mean(Accuracy))
#   Direction   Acc
# 1 Above     0.765
# 2 Right     0.853
# 3 Below     0.775
# 4 Left      0.882

data %>% group_by(Direction) %>% summarise(RespCount = mean(ResponseCount))
#   Direction RespCount
# 1 Above          1.73
# 2 Right          1.57
# 3 Below          1.69
# 4 Left           1.59

# Nothing resps
# All nothing resps
filter(data, grepl("nothing",AnswerContent)) %>% group_by(Condition) %>% summarise(Count = n())
#   Condition   Count
# 1 Ambiguous       5
# 2 Unambiguous    44

# Nothing only resps
filter(data,AnswerContent=="nothing") %>% group_by(Condition) %>% summarise(Count = n())
# Condition   Count
# 1 Ambiguous       3
# 2 Unambiguous     3

# Nothing by direction
filter(data, grepl("nothing",AnswerContent)) %>% group_by(Direction) %>% summarise(Count = n())
#   Direction Count
# 1 Above        13
# 2 Right        13
# 3 Below        12
# 4 Left         11

# --------------------------------------------------------------------------------------------------
# 6. Multivariate regression on responses as a function of ambiguity + directionality
# --------------------------------------------------------------------------------------------------

mlm <- lm(cbind(SelectedA, SelectedB, SelectedC) ~ Condition + Direction, data=data)
summary(mlm)

# Response SelectedA :
#
# Call:
# lm(formula = SelectedA ~ Condition + Direction, data = data)
#
# Residuals:
#     Min      1Q  Median      3Q     Max
# -1.2255 -0.2451 -0.2059  0.2647  0.7549
#
# Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)
# (Intercept)           1.20588    0.03131  38.516   <2e-16 ***
# ConditionUnambiguous -0.47059    0.02800 -16.805   <2e-16 ***
# DirectionRight       -0.49020    0.03960 -12.378   <2e-16 ***
# DirectionBelow        0.01961    0.03960   0.495    0.621
# DirectionLeft        -0.49020    0.03960 -12.378   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.2828 on 403 degrees of freedom
# Multiple R-squared:  0.5988,	Adjusted R-squared:  0.5948
# F-statistic: 150.4 on 4 and 403 DF,  p-value: < 2.2e-16
#
#
# Response SelectedB :
#
# Call:
# lm(formula = SelectedB ~ Condition + Direction, data = data)
#
# Residuals:
#      Min       1Q   Median       3Q      Max
# -0.20588 -0.08824 -0.06863  0.04902  0.92157
#
# Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)
# (Intercept)           0.06863    0.02640   2.599 0.009682 **
# ConditionUnambiguous  0.13725    0.02361   5.812 1.25e-08 ***
# DirectionRight       -0.12745    0.03340  -3.816 0.000157 ***
# DirectionBelow       -0.02941    0.03340  -0.881 0.378996
# DirectionLeft        -0.11765    0.03340  -3.523 0.000476 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.2385 on 403 degrees of freedom
# Multiple R-squared:  0.1211,	Adjusted R-squared:  0.1123
# F-statistic: 13.88 on 4 and 403 DF,  p-value: 1.298e-10
#
#
# Response SelectedC :
#
# Call:
# lm(formula = SelectedC ~ Condition + Direction, data = data)
#
# Residuals:
#     Min      1Q  Median      3Q     Max
# -1.2181 -0.2328 -0.1887  0.2721  0.2917
#
# Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)
# (Intercept)           0.72794    0.03242  22.456   <2e-16 ***
# ConditionUnambiguous -0.47549    0.02899 -16.399   <2e-16 ***
# DirectionRight        0.46078    0.04100  11.238   <2e-16 ***
# DirectionBelow       -0.01961    0.04100  -0.478    0.633
# DirectionLeft         0.49020    0.04100  11.955   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.2928 on 403 degrees of freedom
# Multiple R-squared:  0.5771,	Adjusted R-squared:  0.5729
# F-statistic: 137.5 on 4 and 403 DF,  p-value: < 2.2e-16

Manova(mlm)

# Type II MANOVA Tests: Pillai test statistic
#           Df test stat approx F num Df den Df    Pr(>F)
# Condition  1   0.79678   524.08      3    401 < 2.2e-16 ***
# Direction  3   0.48089    25.64      9   1209 < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

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
#                             Estimates  Std.error     Z value     Pr(>|z|)
# (Intercept)                1.20644759 0.03069159  39.3087350 0.000000e+00
# data$ConditionUnambiguous -0.47159431 0.02820123 -16.7224710 8.991261e-63
# data$DirectionRight       -0.49090993 0.03988744 -12.3073800 8.266328e-35
# data$DirectionBelow        0.02121887 0.03989021   0.5319318 5.947732e-01
# data$DirectionLeft        -0.48918823 0.03989069 -12.2632166 1.427199e-34
#
# Dispersion:
#      Estimates   Std.error   Z value     Pr(>|z|)
# 1  0.081267918 0.006304122 12.891234 5.043028e-38
# 2 -0.002635182 0.001361124 -1.936033 5.286360e-02
#
# Call: data$SelectedB ~ data$Condition + data$Direction
#
# Link function: identity
# Variance function: constant
# Covariance function: identity
# Regression:
#                             Estimates  Std.error    Z value     Pr(>|z|)
# (Intercept)                0.06989133 0.02750405  2.5411290 1.104951e-02
# data$ConditionUnambiguous  0.13560440 0.02194489  6.1793164 6.437974e-10
# data$DirectionRight       -0.12857817 0.03103853 -4.1425343 3.434889e-05
# data$DirectionBelow       -0.02814919 0.03104022 -0.9068615 3.644800e-01
# data$DirectionLeft        -0.11783150 0.03104107 -3.7959876 1.470569e-04
#
# Dispersion:
#     Estimates   Std.error  Z value     Pr(>|z|)
# 1 0.049200901 0.008118254 6.060527 1.356761e-09
# 2 0.007949218 0.002734485 2.907026 3.648831e-03
#
# Call: data$SelectedC ~ data$Condition + data$Direction
#
# Link function: identity
# Variance function: constant
# Covariance function: identity
# Regression:
#                            Estimates  Std.error    Z value      Pr(>|z|)
# (Intercept)                0.7267017 0.03210910  22.632267 2.085991e-113
# data$ConditionUnambiguous -0.4735017 0.02898551 -16.335807  5.490121e-60
# data$DirectionRight        0.4623476 0.04099727  11.277521  1.694538e-29
# data$DirectionBelow       -0.0220607 0.04100030  -0.538062  5.905342e-01
# data$DirectionLeft         0.4895052 0.04100099  11.938862  7.423211e-33
#
# Dispersion:
#       Estimates   Std.error    Z value     Pr(>|z|)
# 1  0.0858649278 0.007508977 11.4349703 2.796323e-30
# 2 -0.0009372257 0.001748461 -0.5360288 5.919386e-01
#
# Correlation matrix:
#   Parameters  Estimates  Std.error    Z value     Pr(>|z|)
# 1      rho12  0.2243446 0.04930765   4.549894 5.367283e-06
# 2      rho13 -0.6331783 0.03456561 -18.318155 5.928861e-75
# 3      rho23 -0.2497934 0.04960915  -5.035229 4.772784e-07
#
# Algorithm: chaser
# Correction: TRUE
# Number iterations: 9

mc_manova_II(fit_ABC)

# MANOVA type II using Wald statistic for fixed effects
#
# Call: ~ data$Condition+data$Direction
#        Covariate Df       Chi Pr(>Chi)
# 1      Intercept  3 5301.1110        0
# 2 data$Condition  3 1514.4053        0
# 3 data$Direction  9  369.4201        0

# --------------------------------------------------------------------------------------------------
# 7. Figures
# --------------------------------------------------------------------------------------------------

respA <- filter(data, SelectedA==1) %>% group_by(ProblemNumber) %>% summarise(Prop = n()/finalN)
#   ProblemNumber   Prop
# 1             1  0.961
# 2             2      0
# 3             3  0.941 
# 4             4  0.0196
# 5             5  0.980 
# 6             6  0.961 
# 7             7  0.980 
# 8             8  1

respB <- filter(data, SelectedB==1) %>% group_by(ProblemNumber) %>% summarise(Prop = n()/finalN)
#   ProblemNumber   Prop
# 1             1      0
# 2             2 0.0392
# 3             3      0
# 4             4 0.0196
# 5             5      0
# 6             6  0.275
# 7             7      0
# 8             8  0.216

respC <- filter(data, SelectedC==1) %>% group_by(ProblemNumber) %>% summarise(Prop = n()/finalN)
#   ProblemNumber  Prop
# 1             1 0.980
# 2             2 0.980
# 3             3 0.941
# 4             4 0.961
# 5             5 0.980
# 6             6     0
# 7             7 0.941
# 8             8     0

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

figMaker <- function(probNum) {
  figDF <- data.frame(x = c(1,2,1,2,1,2,1,2))
  figDF$y <- rev(c(1,1,2,2,3,3,4,4))
  figDF$labelName <- c(data$Position_1_1[data$ProblemNumber==probNum][1], 
                       data$Position_1_2[data$ProblemNumber==probNum][1], 
                       "", "", "", "", 
                       data$Position_4_1[data$ProblemNumber==probNum][1], 
                       data$Position_4_2[data$ProblemNumber==probNum][1])
  colList <- c()
  for (l in figDF$labelName) {
    m <- 1
    if (l == "A") {
      colList <- c(colList, 2)
    } else if (l == "B") {
      colList <- c(colList, 3)
    } else if (l == "C") {
      colList <- c(colList, 4)
    }
    m <- m + 1
  }
  referent <- list(c(2,8), 8, c(1,7), 1, c(7,8), 7, c(1,2), 1)
  figDF$alpha <- c(respDF[probNum,colList[1]], respDF[probNum,colList[2]], -1, -1, 
                   -1, -1, respDF[probNum,colList[3]], respDF[probNum,colList[4]])
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
    geom_tile(alpha=figDF$alpha, colour = figDF$border, width=0.95, height=0.95, linewidth=1.0) +
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
