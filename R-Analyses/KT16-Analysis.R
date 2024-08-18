# --------------------------------------------------------------------------------------------------
# Knowing Thinking Experiment 16 Analysis
# --------------------------------------------------------------------------------------------------
# Subject pool: Amazon Mechanical Turk
# Date(s):      May 2022
# Design:       Branden Bio & Sunny Khemlani
# Code:         Branden Bio
# Experimenter:	Knexus Research Corporation
# --------------------------------------------------------------------------------------------------
# Contents:
#  X. Preprocessing; filter subjs by attention check; reformatting responses
#  1. Wilcoxon tests comparing proportion of yes responses in condition by modulation
#  2. Wilcoxon test comparing proportion of yes responses in conditional first vs second
#  3. Wilcoxon test comparing proportion of yes responses in modulated vs unmodulated
#  4. Wilcoxon test comparing reaction times in modulated vs unmodulated
#  5. Figures
#     5.1) Proportion of yes responses for condition by modulation
#     5.2) Proportion of yes responses for conditional first vs second
#     5.3) Proportion of yes responses for modulated vs unmodulated
#     5.4) Average reaction time for modulated vs unmodulated
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
# X. Preprocessing; filter subjs by attention check; reformatting responses
# --------------------------------------------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rawData <- read.csv("./KT16-Aggregate.csv", header=T)
rawData$ParticipantID <- as.factor(rawData$ParticipantID)
rawData$Cond <- factor(rawData$Cond,levels = c("AffirmConsequent", "DenyAntecedent", "AC"))
rawData$Verb <- factor(rawData$Verb,levels = c("Believes", "Knows"))
rawData$ConditionalOrder <- factor(rawData$ConditionalOrder,levels = c("Conditional First", "Conditional Second"))
rawData$Modulation <- factor(rawData$Modulation,levels = c("Modulated", "Unmodulated"))
rawData$AnswerY <- ifelse(rawData$Answer=="Yes", 1, 0)
fullN <- length(unique(rawData$ParticipantID))

rawData.AC <- subset(rawData, Cond == "AC")
rawData.AC$PassedAC <- ifelse(rawData.AC$Cond == rawData.AC$Answer, 1, 0)
missedAC <- c(as.character(subset(rawData.AC, PassedAC==0)$ParticipantID))
failedAC <- missedAC[duplicated(missedAC)]
rawData$FailedAC <- ifelse(rawData$ParticipantID %in% failedAC, 1, 0)
rawData$missedAC <- ifelse(rawData$ParticipantID %in% missedAC, 1, 0)

data <- subset(rawData, FailedAC==0)
data <- subset(data, Cond != "AC")
finalN <- length(unique(data$ParticipantID))
subjLoss <- 1 - (useableN / fullN)

demoData       <- read.csv("./KT16-Demographics.csv", header=T)
demoData       <- subset(demoData, ParticipantID %in% unique(data$ParticipantID))
meanAge        <- round(mean(demoData$Age), 2)
minAge         <- min(demoData$Age)
maxAge         <- max(demoData$Age)
femaleCount    <- sum(str_count(demoData$Sex, "Female"))
maleCount      <- sum(str_count(demoData$Sex, "Male"))
otherCount     <- sum(str_count(demoData$Sex, "Other"))
preferNotCount <- sum(str_count(demoData$Sex, "Prefernot"))
nologicCount   <- sum(str_count(demoData$Coursework, "None"))
logicCount     <- finalN - nologicCount

data.frame(fullN, finalN, excludedN, meanAge, minAge, maxAge, femaleCount, 
           maleCount, otherCount, preferNotCount, nologicCount, logicCount)

#   fullN finalN excludedN meanAge minAge maxAge femaleCount maleCount otherCount preferNotCount nologicCount logicCount
# 1     60     P1         5   39.34     24     71          24        33          0              1           40         18

# --------------------------------------------------------------------------------------------------
# 1. Wilcoxon test on "yes" responses for modulation
# --------------------------------------------------------------------------------------------------

data %>% group_by(Modulation) %>% summarise(AnswerY = mean(AnswerY))

#   Modulation  AnswerY
#   <fct>         <dbl>
# 1 Modulated     0.582
# 2 Unmodulated   0.5

test <- data %>% group_by(ParticipantID, Modulation) %>% summarise(AnswerY = mean(AnswerY))

wilcoxsign_test(subset(test, Modulation=="Modulated")$AnswerY ~
                subset(test, Modulation=="Unmodulated")$AnswerY)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test
#
# data:  y by x (pos, neg)
# 	 stratified by block
# Z = 1.6971, p-value = 0.08968
# alternative hypothesis: true mu is not equal to 0

cliff.delta(subset(test, Modulation=="Modulated")$AnswerY,
            subset(test, Modulation=="Unmodulated")$AnswerY)

# Cliff's Delta
#
# delta estimate: 0.1352556 (negligible)
# 95 percent confidence interval:
#       lower       upper
# -0.07299436  0.33220402

# --------------------------------------------------------------------------------------------------
# 2. Wilcoxon test on "yes" responses for conditional order
# --------------------------------------------------------------------------------------------------

data %>% group_by(ConditionalOrder) %>% summarise(AnswerY = mean(AnswerY))

#   ConditionalOrder   AnswerY
#   <fct>                <dbl>
# 1 Conditional First    0.573
# 2 Conditional Second   0.509

test <- data %>% group_by(ParticipantID, ConditionalOrder) %>% summarise(AnswerY = mean(AnswerY))

wilcoxsign_test(subset(test, ConditionalOrder=="Conditional First")$AnswerY ~
                subset(test, ConditionalOrder=="Conditional Second")$AnswerY)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test
#
# data:  y by x (pos, neg)
# 	 stratified by block
# Z = 1.5616, p-value = 0.1184
# alternative hypothesis: true mu is not equal to 0

cliff.delta(subset(test, ConditionalOrder=="Conditional First")$AnswerY,
            subset(test, ConditionalOrder=="Conditional Second")$AnswerY)

# Cliff's Delta
#
# delta estimate: 0.1186088 (negligible)
# 95 percent confidence interval:
#       lower       upper
# -0.08909468  0.31642654

# --------------------------------------------------------------------------------------------------
# 3. Wilcoxon test on "yes" responses for problem type
# --------------------------------------------------------------------------------------------------

data %>% group_by(Cond) %>% summarise(AnswerY = mean(AnswerY))

#   Cond             AnswerY
#   <fct>              <dbl>
# 1 AffirmConsequent   0.591
# 2 DenyAntecedent     0.491

test <- data %>% group_by(ParticipantID, Cond) %>% summarise(AnswerY = mean(AnswerY))

wilcoxsign_test(subset(test, Cond=="AffirmConsequent")$AnswerY ~
                subset(test, Cond=="DenyAntecedent")$AnswerY)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test
#
# data:  y by x (pos, neg)
# 	 stratified by block
# Z = 1.9513, p-value = 0.05102
# alternative hypothesis: true mu is not equal to 0

cliff.delta(subset(test, Cond=="AffirmConsequent")$AnswerY,
            subset(test, Cond=="DenyAntecedent")$AnswerY)

# Cliff's Delta
#
# delta estimate: 0.1539834 (small)
# 95 percent confidence interval:
#       lower       upper
# -0.05594872  0.35087670

# --------------------------------------------------------------------------------------------------
# 4. Wilcoxon test on "yes" responses for interaction between modulation and conditional order
# --------------------------------------------------------------------------------------------------

data %>% group_by(Modulation, ConditionalOrder) %>% summarise(AnswerY = mean(AnswerY))

#   Modulation  ConditionalOrder   AnswerY
#   <fct>       <fct>                <dbl>
# 1 Modulated   Conditional First    0.629
# 2 Modulated   Conditional Second   0.534
# 3 Unmodulated Conditional First    0.517
# 4 Unmodulated Conditional Second   0.483

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
# Z = 0.65904, p-value = 0.5099
# alternative hypothesis: true mu is not equal to 0

cliff.delta(A, B)

# Cliff's Delta
#
# delta estimate: 0.07639715 (negligible)
# 95 percent confidence interval:
#      lower      upper
# -0.1203496  0.2673693

# --------------------------------------------------------------------------------------------------
# 5. Wilcoxon test on "yes" responses for interaction between modulation and problem type
# --------------------------------------------------------------------------------------------------

data %>% group_by(Modulation, Cond) %>% summarise(AnswerY = mean(AnswerY))

#   Modulation  Cond             AnswerY
#   <fct>       <fct>              <dbl>
# 1 Modulated   AffirmConsequent   0.664
# 2 Modulated   DenyAntecedent     0.5
# 3 Unmodulated AffirmConsequent   0.517
# 4 Unmodulated DenyAntecedent     0.483

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
# Z = 1.8411, p-value = 0.06561
# alternative hypothesis: true mu is not equal to 0

cliff.delta(A, B)

# Cliff's Delta
#
# delta estimate: 0.1840071 (small)
# 95 percent confidence interval:
#       lower       upper
# -0.01462604  0.36866853

# --------------------------------------------------------------------------------------------------
# 6. Wilcoxon test on "yes" responses for interaction between conditional order and problem type
# --------------------------------------------------------------------------------------------------

data %>% group_by(ConditionalOrder, Cond) %>% summarise(AnswerY = mean(AnswerY))

#   ConditionalOrder   Cond             AnswerY
#   <fct>              <fct>              <dbl>
# 1 Conditional First  AffirmConsequent   0.647
# 2 Conditional First  DenyAntecedent     0.5
# 3 Conditional Second AffirmConsequent   0.534
# 4 Conditional Second DenyAntecedent     0.483

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
# Z = 1.1396, p-value = 0.2545
# alternative hypothesis: true mu is not equal to 0

cliff.delta(A, B)

# Cliff's Delta
#
# delta estimate: 0.1310939 (negligible)
# 95 percent confidence interval:
#       lower       upper
# -0.06284196  0.31548979

# --------------------------------------------------------------------------------------------------
# 7. Wilcoxon test on "yes" responses for three-way interaction
# --------------------------------------------------------------------------------------------------

data %>% group_by(Modulation, ConditionalOrder, Cond) %>% summarise(AnswerY = mean(AnswerY))

#   Modulation  ConditionalOrder   Cond             AnswerY
#   <fct>       <fct>              <fct>              <dbl>
# 1 Modulated   Conditional First  AffirmConsequent   0.724
# 2 Modulated   Conditional First  DenyAntecedent     0.534
# 3 Modulated   Conditional Second AffirmConsequent   0.603
# 4 Modulated   Conditional Second DenyAntecedent     0.466
# 5 Unmodulated Conditional First  AffirmConsequent   0.569
# 6 Unmodulated Conditional First  DenyAntecedent     0.466
# 7 Unmodulated Conditional Second AffirmConsequent   0.466
# 8 Unmodulated Conditional Second DenyAntecedent     0.5

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
# Z = -0.74515, p-value = 0.4562
# alternative hypothesis: true mu is not equal to 0

cliff.delta(A, B)

# Cliff's Delta
#
# delta estimate: -0.05558859 (negligible)
# 95 percent confidence interval:
#      lower      upper
# -0.2428939  0.1357128

# --------------------------------------------------------------------------------------------------
# 8. Binomial test on whether effect holds across Ps
# --------------------------------------------------------------------------------------------------

test <- data %>% group_by(Modulation, ParticipantID) %>% summarise(AnswerY = mean(AnswerY)) %>% print(n=Inf)

#    Modulation ParticipantID AnswerY
#    <fct>      <fct>           <dbl>
#  1 Modulated  P1               0.25
#  2 Modulated  P10              0.75
#  3 Modulated  P11              0.5
#  4 Modulated  P12              1
#  5 Modulated  P13              0
#  6 Modulated  P14              0
#  7 Modulated  P16              0.75
#  8 Modulated  P17              0
#  9 Modulated  P18              1
# 10 Modulated  P19              0.75
# # … with 106 more rows

A <- subset(test, Modulation == "Modulated")$AnswerY
B <- subset(test, Modulation == "Unmodulated")$AnswerY
C <- ifelse(A > B, 1, 0)
sum(C) # 46
length(C) # 55

binom.test(44, 55, p=.5)

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

