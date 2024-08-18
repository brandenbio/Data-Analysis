# --------------------------------------------------------------------------------------------------
# Knowing Thinking Experiment 15 Analysis
# --------------------------------------------------------------------------------------------------
# Subject pool: Amazon Mechanical Turk
# Date(s):      April 2022
# Design:       Branden Bio & Sunny Khemlani
# Code:         Branden Bio
# Experimenter:	Knexus Research Corporation
# --------------------------------------------------------------------------------------------------
# Contents:
# 0. Preprocessing, filtering, and demographics
# 1. Wilcoxon test on "yes" responses for modulation
# 2. Wilcoxon test on "yes" responses for conditional order
# 3. Wilcoxon test on "yes" responses for problem type
# 4. Wilcoxon test on "yes" responses for interaction between modulation and conditional order
# 5. Wilcoxon test on "yes" responses for interaction between modulation and problem type
# 6. Wilcoxon test on "yes" responses for interaction between conditional order and problem type
# 7. Wilcoxon test on "yes" responses for three-way interaction
# 8. Binomial test on whether effect holds across Ps
# --------------------------------------------------------------------------------------------------

library(tidyr)
library(stringr)
library(coin)
library(reshape2)
library(ggplot2)
library(dplyr)
library(lme4)
library(effsize)

# --------------------------------------------------------------------------------------------------
# 0. Preprocessing, filtering, and demographics
# --------------------------------------------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rawData                  <- read.csv("./KT15-Aggregate.csv", header=T)
rawData$ParticipantID    <- as.factor(rawData$ParticipantID)
rawData$Cond             <- factor(rawData$Cond,levels = c("AffirmConsequent", "DenyAntecedent", "AC"))
rawData$Verb             <- factor(rawData$Verb,levels = c("Believes", "Knows"))
rawData$ConditionalOrder <- factor(rawData$ConditionalOrder,levels = c("Conditional First", "Conditional Second"))
rawData$Modulation       <- factor(rawData$Modulation,levels = c("Modulated", "Unmodulated"))
rawData$AnswerY          <- ifelse(rawData$Answer=="Yes", 1, 0)
fullN                    <- length(unique(rawData$ParticipantID))

rawData.AC               <- subset(rawData, Cond == "AC")
rawData.AC$PassedAC      <- ifelse(rawData.AC$Cond == rawData.AC$Answer, 1, 0)
missedAC                 <- c(as.character(subset(rawData.AC, PassedAC==0)$ParticipantID))
failedAC                 <- missedAC[duplicated(missedAC)]
rawData$FailedAC         <- ifelse(rawData$ParticipantID %in% failedAC, 1, 0)
rawData$missedAC         <- ifelse(rawData$ParticipantID %in% missedAC, 1, 0)

data1                    <- subset(rawData, FailedAC==0)
data1                    <- subset(data1, Cond != "AC")
useableN1                <- length(unique(data1$ParticipantID))
subjLoss1                <- 1 - (useableN1 / fullN)

# Setup dataframes
data                     <- data1
finalN                   <- length(unique(data$ParticipantID))
excludedN                <- fullN - finalN
useableSubs              <- unique(data$ParticipantID)

# Demographics summary:

demoData <- read.csv("./KT15-Demographics.csv", header=T)
demoData <- subset(demoData, ParticipantID %in% keepers)

# Age info
meanAge <- round(mean(demoData$Age), 2)
minAge  <- min(demoData$Age)
maxAge  <- max(demoData$Age)

# Sex breakdown
femaleCount    <- sum(str_count(demoData$Sex, "Female"))
maleCount      <- sum(str_count(demoData$Sex, "Male"))
otherCount     <- sum(str_count(demoData$Sex, "Other"))
preferNotCount <- sum(str_count(demoData$Sex, "Prefernot"))

# Experience
nologicCount <- sum(str_count(demoData$Coursework, "None"))
logicCount   <- length(keepers) - nologicCount

# Output df
data.frame(fullN, finalN, excludedN, meanAge, minAge, maxAge, femaleCount, 
         maleCount, otherCount, preferNotCount, nologicCount, logicCount)

#   fullN finalN excludedN meanAge minAge maxAge femaleCount maleCount otherCount preferNotCount nologicCount logicCount
# 1    60     58         5   39.34     24     71          24        33          0              1           40         18

# --------------------------------------------------------------------------------------------------
# 1. Wilcoxon test on "yes" responses for modulation
# --------------------------------------------------------------------------------------------------

data %>% group_by(Modulation) %>% summarise(AnswerY = mean(AnswerY))

#   Modulation  AnswerY
#   <fct>         <dbl>
# 1 Modulated     0.764
# 2 Unmodulated   0.568

test <- data %>% group_by(ParticipantID, Modulation) %>% summarise(AnswerY = mean(AnswerY))

wilcoxsign_test(subset(test, Modulation=="Modulated")$AnswerY ~
                subset(test, Modulation=="Unmodulated")$AnswerY)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test
#
# data:  y by x (pos, neg)
# 	 stratified by block
# Z = 3.2954, p-value = 0.000983
# alternative hypothesis: true mu is not equal to 0

cliff.delta(subset(test, Modulation=="Modulated")$AnswerY,
            subset(test, Modulation=="Unmodulated")$AnswerY)

# Cliff's Delta
#
# delta estimate: 0.2661157 (small)
# 95 percent confidence interval:
#      lower      upper
# 0.05375723 0.45544460

# --------------------------------------------------------------------------------------------------
# 2. Wilcoxon test on "yes" responses for conditional order
# --------------------------------------------------------------------------------------------------

data %>% group_by(ConditionalOrder) %>% summarise(AnswerY = mean(AnswerY))

#   ConditionalOrder   AnswerY
#   <fct>                <dbl>
# 1 Conditional First    0.705
# 2 Conditional Second   0.627

test <- data %>% group_by(ParticipantID, ConditionalOrder) %>% summarise(AnswerY = mean(AnswerY))

wilcoxsign_test(subset(test, ConditionalOrder=="Conditional First")$AnswerY ~
                subset(test, ConditionalOrder=="Conditional Second")$AnswerY)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test
#
# data:  y by x (pos, neg)
# 	 stratified by block
# Z = 1.8529, p-value = 0.0639
# alternative hypothesis: true mu is not equal to 0

cliff.delta(subset(test, ConditionalOrder=="Conditional First")$AnswerY,
            subset(test, ConditionalOrder=="Conditional Second")$AnswerY)

# Cliff's Delta
#
# delta estimate: 0.1557025 (small)
# 95 percent confidence interval:
#      lower      upper
# -0.0564737  0.3544225

# --------------------------------------------------------------------------------------------------
# 3. Wilcoxon test on "yes" responses for problem type
# --------------------------------------------------------------------------------------------------

data %>% group_by(Cond) %>% summarise(AnswerY = mean(AnswerY))

#   Cond             AnswerY
#   <fct>              <dbl>
# 1 AffirmConsequent   0.686
# 2 DenyAntecedent     0.645

test <- data %>% group_by(ParticipantID, Cond) %>% summarise(AnswerY = mean(AnswerY))

wilcoxsign_test(subset(test, Cond=="AffirmConsequent")$AnswerY ~
                subset(test, Cond=="DenyAntecedent")$AnswerY)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test
#
# data:  y by x (pos, neg)
# 	 stratified by block
# Z = 0.74104, p-value = 0.4587
# alternative hypothesis: true mu is not equal to 0

cliff.delta(subset(test, Cond=="AffirmConsequent")$AnswerY,
            subset(test, Cond=="DenyAntecedent")$AnswerY)

# Cliff's Delta
#
# delta estimate: 0.07966942 (negligible)
# 95 percent confidence interval:
#      lower      upper
# -0.1312705  0.2837076

# --------------------------------------------------------------------------------------------------
# 4. Wilcoxon test on "yes" responses for interaction between modulation and conditional order
# --------------------------------------------------------------------------------------------------

data %>% group_by(Modulation, ConditionalOrder) %>% summarise(AnswerY = mean(AnswerY))

#   Modulation  ConditionalOrder   AnswerY
#   <fct>       <fct>                <dbl>
# 1 Modulated   Conditional First    0.8
# 2 Modulated   Conditional Second   0.727
# 3 Unmodulated Conditional First    0.609
# 4 Unmodulated Conditional Second   0.527

test <- data %>% group_by(ParticipantID, Modulation, ConditionalOrder) %>% summarise(AnswerY = mean(AnswerY))

A <- subset(test, Modulation=="Modulated" & ConditionalOrder=="Conditional First")$AnswerY -
     subset(test, Modulation=="Unmodulated" & ConditionalOrder=="Conditional First")$AnswerY
B <- subset(test, Modulation=="Modulated" & ConditionalOrder=="Conditional Second")$AnswerY -
     subset(test, Modulation=="Unmodulated" & ConditionalOrder=="Conditional Second")$AnswerY

wilcoxsign_test(A ~ B)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test
#
# data:  y by x (pos, neg)
# 	 stratified by block
# Z = 0.26555, p-value = 0.7906
# alternative hypothesis: true mu is not equal to 0

cliff.delta(A, B)

# Cliff's Delta
#
# delta estimate: 0.01355372 (negligible)
# 95 percent confidence interval:
#      lower      upper
# -0.1876044  0.2136207

# --------------------------------------------------------------------------------------------------
# 5. Wilcoxon test on "yes" responses for interaction between modulation and problem type
# --------------------------------------------------------------------------------------------------

data %>% group_by(Modulation, Cond) %>% summarise(AnswerY = mean(AnswerY))

#   Modulation  Cond             AnswerY
#   <fct>       <fct>              <dbl>
# 1 Modulated   AffirmConsequent   0.791
# 2 Modulated   DenyAntecedent     0.736
# 3 Unmodulated AffirmConsequent   0.582
# 4 Unmodulated DenyAntecedent     0.555

test <- data %>% group_by(ParticipantID, Modulation, Cond) %>% summarise(AnswerY = mean(AnswerY))

A <- subset(test, Modulation=="Modulated" & Cond=="AffirmConsequent")$AnswerY -
     subset(test, Modulation=="Unmodulated" & Cond=="AffirmConsequent")$AnswerY
B <- subset(test, Modulation=="Modulated" & Cond=="DenyAntecedent")$AnswerY -
     subset(test, Modulation=="Unmodulated" & Cond=="DenyAntecedent")$AnswerY

wilcoxsign_test(A ~ B)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test
#
# data:  y by x (pos, neg)
# 	 stratified by block
# Z = 0.29526, p-value = 0.7678
# alternative hypothesis: true mu is not equal to 0

cliff.delta(A, B)

# Cliff's Delta
#
# delta estimate: 0.03371901 (negligible)
# 95 percent confidence interval:
#      lower      upper
# -0.1685696  0.2332821

# --------------------------------------------------------------------------------------------------
# 6. Wilcoxon test on "yes" responses for interaction between conditional order and problem type
# --------------------------------------------------------------------------------------------------

data %>% group_by(ConditionalOrder, Cond) %>% summarise(AnswerY = mean(AnswerY))

#   ConditionalOrder   Cond             AnswerY
#   <fct>              <fct>              <dbl>
# 1 Conditional First  AffirmConsequent   0.727
# 2 Conditional First  DenyAntecedent     0.682
# 3 Conditional Second AffirmConsequent   0.645
# 4 Conditional Second DenyAntecedent     0.609

test <- data %>% group_by(ParticipantID, ConditionalOrder, Cond) %>% summarise(AnswerY = mean(AnswerY))

A <- subset(test, ConditionalOrder=="Conditional First" & Cond=="AffirmConsequent")$AnswerY -
     subset(test, ConditionalOrder=="Conditional Second" & Cond=="AffirmConsequent")$AnswerY
B <- subset(test, ConditionalOrder=="Conditional First" & Cond=="DenyAntecedent")$AnswerY -
     subset(test, ConditionalOrder=="Conditional Second" & Cond=="DenyAntecedent")$AnswerY

wilcoxsign_test(A ~ B)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test
#
# data:  y by x (pos, neg)
# 	 stratified by block
# Z = -0.27073, p-value = 0.7866
# alternative hypothesis: true mu is not equal to 0

cliff.delta(A, B)

# Cliff's Delta
#
# delta estimate: -0.004958678 (negligible)
# 95 percent confidence interval:
#      lower      upper
# -0.1967941  0.1872424

# --------------------------------------------------------------------------------------------------
# 7. Wilcoxon test on "yes" responses for three-way interaction
# --------------------------------------------------------------------------------------------------

data %>% group_by(Modulation, ConditionalOrder, Cond) %>% summarise(AnswerY = mean(AnswerY))

#   Modulation  ConditionalOrder   Cond             AnswerY
#   <fct>       <fct>              <fct>              <dbl>
# 1 Modulated   Conditional First  AffirmConsequent   0.8
# 2 Modulated   Conditional First  DenyAntecedent     0.8
# 3 Modulated   Conditional Second AffirmConsequent   0.782
# 4 Modulated   Conditional Second DenyAntecedent     0.673
# 5 Unmodulated Conditional First  AffirmConsequent   0.655
# 6 Unmodulated Conditional First  DenyAntecedent     0.564
# 7 Unmodulated Conditional Second AffirmConsequent   0.509
# 8 Unmodulated Conditional Second DenyAntecedent     0.545

test <- data %>% group_by(ParticipantID, Modulation, ConditionalOrder, Cond) %>% summarise(AnswerY = mean(AnswerY))

A1 <- subset(test, Modulation == "Modulated" & ConditionalOrder=="Conditional First" & Cond=="AffirmConsequent")$AnswerY -
      subset(test, Modulation == "Modulated" & ConditionalOrder=="Conditional Second" & Cond=="AffirmConsequent")$AnswerY
A2 <- subset(test, Modulation == "Modulated" & ConditionalOrder=="Conditional First" & Cond=="DenyAntecedent")$AnswerY -
      subset(test, Modulation == "Modulated" & ConditionalOrder=="Conditional Second" & Cond=="DenyAntecedent")$AnswerY

B1 <- subset(test, Modulation == "Unmodulated" & ConditionalOrder=="Conditional First" & Cond=="AffirmConsequent")$AnswerY -
      subset(test, Modulation == "Unmodulated" & ConditionalOrder=="Conditional Second" & Cond=="AffirmConsequent")$AnswerY
B2 <- subset(test, Modulation == "Unmodulated" & ConditionalOrder=="Conditional First" & Cond=="DenyAntecedent")$AnswerY -
      subset(test, Modulation == "Unmodulated" & ConditionalOrder=="Conditional Second" & Cond=="DenyAntecedent")$AnswerY

A <- A1 - A2
B <- B1 - B2

wilcoxsign_test(A ~ B)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test
#
# data:  y by x (pos, neg)
# 	 stratified by block
# Z = -1.6772, p-value = 0.0935
# alternative hypothesis: true mu is not equal to 0

cliff.delta(A, B)

# Cliff's Delta
#
# delta estimate: -0.1771901 (small)
# 95 percent confidence interval:
#        lower        upper
# -0.352004040  0.009568719

# --------------------------------------------------------------------------------------------------
# 8. Binomial test on whether effect holds across Ps
# --------------------------------------------------------------------------------------------------

test <- data %>% group_by(Modulation, ParticipantID) %>% summarise(AnswerY = mean(AnswerY)) %>% print(n=Inf)

#   Modulation ParticipantID AnswerY
#    <fct>      <fct>           <dbl>
#  1 Modulated  P10              0.75
#  2 Modulated  P12              1
#  3 Modulated  P13              0.75
#  4 Modulated  P14              1
#  5 Modulated  P16              0.75
#  6 Modulated  P17              1
#  7 Modulated  P18              0.75
#  8 Modulated  P2               0.75
#  9 Modulated  P20              1
# 10 Modulated  P21              1
# … with 100 more rows

A <- subset(test, Modulation == "Modulated")$AnswerY
B <- subset(test, Modulation == "Unmodulated")$AnswerY
C <- ifelse(A >= B, 1, 0)
sum(C) # 46
length(C) # 55

binom.test(46, 55, p=.5)

# 	Exact binomial test
#
# data:  46 and 55
# number of successes = 46, number of trials = 55, p-value = 4.336e-07
# alternative hypothesis: true probability of success is not equal to 0.5
# 95 percent confidence interval:
#  0.7119700 0.9223412
# sample estimates:
# probability of success
#              0.8363636