# --------------------------------------------------------------------------------------------------
# Epistemic Modulation Experiment 2 Analysis
# --------------------------------------------------------------------------------------------------
# Subject pool: Amazon Mechanical Turk
# Date(s):      Apr 2023
# Design:       Branden Bio & Sunny Khemlani
# Code:         Branden Bio
# Experimenter:	Knexus Research Corporation
# --------------------------------------------------------------------------------------------------
# Contents:
#  X. Preprocessing; filter subjs by attention check; reformatting responses
#  1. Analyses
#     1.1) Wilcoxon test on response pattern for modulation                                      ***
#     1.2) Wilcoxon test on response pattern for problem type                                    n.s.
#     1.3) Wilcoxon test on response pattern for epistemic VerbType                                *
#     1.4) Wilcoxon test on response pattern for interaction between modulation and problem type n.s.
#     1.5) Wilcoxon test on response pattern for interaction between modulation and VerbType     n.s.
#     1.6) Wilcoxon test on response pattern for interaction between VerbType and problem type   n.s.
#     1.7) Wilcoxon test on three-way interaction                                                n.s.
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
# X. Preprocessing; filter subjs by attention check; reformatting responses
# --------------------------------------------------------------------------------------------------

##########
# Setup script
wd <- getwd()
if (wd != rstudioapi::getActiveDocumentContext()$path) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  wd <- getwd()
}
project <- "EM2"
##########

rawData <- read.csv(paste("./", project, "-Aggregate.csv", sep=""), header=T)
rawData$ParticipantID <- as.factor(rawData$ParticipantID)
rawData$Cond <- factor(rawData$Cond,levels = c("AffirmConsequent", "DenyAntecedent", "Attn-Chk", "Interpretation"))
#rawData$Verb <- str_to_sentence(rawData$Verb)
rawData$VerbType <- factor(rawData$VerbType,levels = c("Factive", "nonFactive"))
rawData$Modulation <- factor(rawData$Modulation,levels = c("Modulated", "Unmodulated"))
rawData$AnswerY <- ifelse(rawData$Answer=="Yes", 1, 0)
rawData$AnswerN <- ifelse(rawData$Answer=="No", 1, 0)
fullN <- length(unique(rawData$ParticipantID))

rawData.AC <- subset(rawData, ProblemType == "AC")
rawData.AC$PassedAC <- ifelse(rawData.AC$ProblemType == rawData.AC$Answer, 1, 0)
missedAC <- c(as.character(subset(rawData.AC, PassedAC==0)$ParticipantID))
failedAC <- missedAC[duplicated(missedAC)]
rawData$FailedAC <- ifelse(rawData$ParticipantID %in% failedAC, 1, 0)
rawData$missedAC <- ifelse(rawData$ParticipantID %in% missedAC, 1, 0)

data <- subset(rawData, FailedAC==0)
data <- subset(data, ProblemType != "AC")
allSubs  <- unique(rawData$ParticipantID)
useableSubs  <- unique(data$ParticipantID)
finalN       <- length(unique(data$ParticipantID))

excl.data    <- subset(rawData, FailedAC==1)
excl.data    <- subset(excl.data, ProblemType != "AC")
excludedN    <- length(unique(excl.data$ParticipantID))
subjLoss     <- round(1 - (finalN / fullN),2)

# Subsets
int.data <- subset(data, Cond=="Interpretation") %>% 
  select(ParticipantID, Cond, VerbType, IndividX, PropP, PropQ, ConditionalContent, 
         CategoricalContent, ConclusionContent, Answer)

table(int.data$VerbType,int.data$Answer)
#              I'm not sure No Yes
#   Factive              24 25   5
#   nonFactive           36  8  10

# Interpretation check subset
int.excl.subs <- c(as.character(unique(subset(int.data, VerbType=="Factive" & Answer=="Yes")$ParticipantID)))
int.excl.subs <- c(int.excl.subs, as.character(unique(subset(int.data, VerbType=="nonFactive" & (Answer=="No" | Answer=="Yes"))$ParticipantID)))
excluded.interp.N <- length(unique(int.excl.subs))
int.data$failedINT <- ifelse(int.data$ParticipantID %in% int.excl.subs, 1, 0)
int.data.excl <- subset(int.data, failedINT==1)
data$failedINT <- ifelse(data$ParticipantID %in% int.excl.subs, 1, 0)
data <- subset(data, failedINT==0)

data <- subset(data, Cond != "Interpretation")
finalFinalN <- length(unique(data$ParticipantID))

# Response pattern
data$RespPattern <- 0

data$RespPattern[data$Cond=="AffirmConsequent" & data$Answer=="Yes"] <- 1

data$RespPattern[data$Cond=="DenyAntecedent" & data$Answer=="No"] <- 1

# --------------------------------------------------------------------------------------------------
# X. Demographics summary
# --------------------------------------------------------------------------------------------------

demoSummary <- function(keepers) {
  # Import & subset data
  demoData <- read.csv(paste("./", project, "-Demographics.csv", sep=""), header=T)
  demoData <- subset(demoData, ParticipantID %in% keepers)
  
  # Age info
  meanAge <- round(mean(demoData$Age), 2)
  minAge  <- min(demoData$Age)
  maxAge  <- max(demoData$Age)
  
  # Sex breakdown
  maleCount      <- sum(str_count(demoData$Sex, "Male"))
  femaleCount    <- sum(str_count(demoData$Sex, "Female"))
  otherCount     <- sum(str_count(demoData$Sex, "Other"))
  preferNotCount <- sum(str_count(demoData$Sex, "Prefernot"))
  
  # Output df
  demoSum <- data.frame(fullN, finalN, excludedN, meanAge, minAge, maxAge, 
                        femaleCount, maleCount, otherCount, preferNotCount)
  return(demoSum)
}

demoSummary(useableSubs)
#  fullN finalN excludedN meanAge minAge maxAge femaleCount maleCount otherCount preferNotCount
#     60     54         6   41.33     23     75          22        32          0              0

demoSummary(allSubs)
#  fullN finalN excludedN meanAge minAge maxAge femaleCount maleCount otherCount preferNotCount
#     60     54         6   40.88     23     75          25        35          0              0

# --------------------------------------------------------------------------------------------------
# 1. Analyses
# --------------------------------------------------------------------------------------------------

# 1.1) Wilcoxon test on response pattern for modulation

data %>% group_by(Modulation) %>% summarise(RespPattern = mean(RespPattern))

# Modulation  ACorDAPattern
# 1 Modulated           0.774
# 2 Unmodulated         0.433

test <- data %>% group_by(ParticipantID, Modulation) %>% summarise(RespPattern = mean(RespPattern))

wilcoxsign_test(subset(test, Modulation=="Modulated")$RespPattern ~
                  subset(test, Modulation=="Unmodulated")$RespPattern)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test

# data:  y by x (pos, neg) 
# stratified by block
# Z = 4.0204, p-value = 5.81e-05
# alternative hypothesis: true mu is not equal to 0

cliff.delta(subset(test, Modulation=="Modulated")$RespPattern ~
              subset(test, Modulation=="Unmodulated")$RespPattern)

# Cliff's Delta

# delta estimate: -0.08823529 (negligible)
# 95 percent confidence interval:
#   lower      upper 
# -0.5708407  0.4396834 

# 1.2) Wilcoxon test on response pattern for problem type

data %>% group_by(Cond) %>% summarise(RespPattern = mean(RespPattern))

# Cond               RespPattern
# 1 AffirmConsequent 0.604
# 2 DenyAntecedent   0.604

test <- data %>% group_by(ParticipantID, Cond) %>% summarise(RespPattern = mean(RespPattern))

wilcoxsign_test(subset(test, Cond=="AffirmConsequent")$RespPattern ~
                  subset(test, Cond=="DenyAntecedent")$RespPattern)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test

# data:  y by x (pos, neg) 
# stratified by block
# Z = -0.34889, p-value = 0.7272
# alternative hypothesis: true mu is not equal to 0

cliff.delta(subset(test, Cond=="AffirmConsequent")$RespPattern,
            subset(test, Cond=="DenyAntecedent")$RespPattern)

# Cliff's Delta

# delta estimate: -0.003569304 (negligible)
# 95 percent confidence interval:
#   lower      upper 
# -0.2441058  0.2373809 

# 1.3) Wilcoxon test on response pattern for epistemic VerbType

data %>% group_by(VerbType) %>% summarise(RespPattern = mean(RespPattern))

# VerbType   RespPattern
# Factive          0.640
# nonFactive       0.567

test <- data %>% group_by(ParticipantID, VerbType) %>% summarise(RespPattern = mean(RespPattern))

wilcoxsign_test(subset(test, VerbType=="nonFactive")$RespPattern ~
                  subset(test, VerbType=="Factive")$RespPattern)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test

# data:  y by x (pos, neg)
# 	 stratified by block
# Z = -2.1082, p-value = 0.03502
# alternative hypothesis: true mu is not equal to 0

cliff.delta(subset(test, VerbType=="nonFactive")$RespPattern,
            subset(test, VerbType=="Factive")$RespPattern)

# Cliff's Delta

# delta estimate: -0.1261154 (negligible)
# 95 percent confidence interval:
#   lower      upper 
# -0.3566881  0.1189394  

# 1.4) Wilcoxon test on response pattern for interaction between modulation and problem type

data %>% group_by(Modulation, Cond) %>% summarise(RespPattern = mean(RespPattern))

# Modulation  Cond             RespPattern
# 1 Modulated   AffirmConsequent       0.756
# 2 Modulated   DenyAntecedent         0.793
# 3 Unmodulated AffirmConsequent       0.451
# 4 Unmodulated DenyAntecedent         0.415

test <- data %>% group_by(ParticipantID, Modulation, Cond) %>% summarise(RespPattern = mean(RespPattern))

A <- subset(test, Modulation=="Modulated" & Cond=="AffirmConsequent")$RespPattern -
  subset(test, Modulation=="Unmodulated" & Cond=="AffirmConsequent")$RespPattern
B <- subset(test, Modulation=="Modulated" & Cond=="DenyAntecedent")$RespPattern -
  subset(test, Modulation=="Unmodulated" & Cond=="DenyAntecedent")$RespPattern

wilcoxsign_test(A ~ B)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test

# data:  y by x (pos, neg) 
# stratified by block
# Z = -0.41604, p-value = 0.6774
# alternative hypothesis: true mu is not equal to 0

cliff.delta(A, B)

# Cliff's Delta

# delta estimate: -0.0594884 (negligible)
# 95 percent confidence interval:
#   lower      upper 
# -0.2897427  0.1772750 

# 1.5) Wilcoxon test on response pattern for interaction between modulation and VerbType

data %>% group_by(Modulation, VerbType) %>% summarise(RespPattern = mean(RespPattern))

# Modulation  VerbType   RespPattern
# 1 Modulated   Factive          0.805
# 2 Modulated   nonFactive       0.744
# 3 Unmodulated Factive          0.476
# 4 Unmodulated nonFactive       0.390

test <- data %>% group_by(ParticipantID, Modulation, VerbType) %>% summarise(RespPattern = mean(RespPattern))

A <- subset(test, Modulation=="Modulated" & VerbType=="nonFactive")$RespPattern -
  subset(test, Modulation=="Unmodulated" & VerbType=="nonFactive")$RespPattern
B <- subset(test, Modulation=="Modulated" & VerbType=="Factive")$RespPattern -
  subset(test, Modulation=="Unmodulated" & VerbType=="Factive")$RespPattern

wilcoxsign_test(A ~ B)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test

# data:  y by x (pos, neg) 
# stratified by block
# Z = 0.4709, p-value = 0.6377
# alternative hypothesis: true mu is not equal to 0

cliff.delta(A, B)

# Cliff's Delta

# delta estimate: 0.03985723 (negligible)
# 95 percent confidence interval:
#   lower      upper 
# -0.1952489  0.2706314

# 1.6) Wilcoxon test on response pattern for interaction between VerbType and problem type

data %>% group_by(VerbType, Cond) %>% summarise(RespPattern = mean(RespPattern))

# VerbType   Cond             RespPattern
# 1 Factive    AffirmConsequent       0.659
# 2 Factive    DenyAntecedent         0.622
# 3 nonFactive AffirmConsequent       0.549
# 4 nonFactive DenyAntecedent         0.585

test <- data %>% group_by(ParticipantID, VerbType, Cond) %>% summarise(RespPattern = mean(RespPattern))

A <- subset(test, VerbType=="nonFactive" & Cond=="AffirmConsequent")$RespPattern -
  subset(test, VerbType=="Factive" & Cond=="AffirmConsequent")$RespPattern
B <- subset(test, VerbType=="nonFactive" & Cond=="DenyAntecedent")$RespPattern -
  subset(test, VerbType=="Factive" & Cond=="DenyAntecedent")$RespPattern

wilcoxsign_test(A ~ B)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test
#
# data:  y by x (pos, neg) 
# stratified by block
# Z = -1.0541, p-value = 0.2918
# alternative hypothesis: true mu is not equal to 0

cliff.delta(A, B)

# Cliff's Delta

# delta estimate: -0.1320642 (negligible)
# 95 percent confidence interval:
#   lower       upper 
# -0.32108954  0.06708021 

# 1.7) Wilcoxon test on three-way interaction

data %>% group_by(Cond, Modulation, VerbType) %>% summarise(RespPattern = mean(RespPattern))
# Cond             Modulation  VerbType   RespPattern
# 1 AffirmConsequent Modulated   Factive          0.805
# 2 AffirmConsequent Modulated   nonFactive       0.707
# 3 AffirmConsequent Unmodulated Factive          0.512
# 4 AffirmConsequent Unmodulated nonFactive       0.390
# 5 DenyAntecedent   Modulated   Factive          0.805
# 6 DenyAntecedent   Modulated   nonFactive       0.780
# 7 DenyAntecedent   Unmodulated Factive          0.439
# 8 DenyAntecedent   Unmodulated nonFactive       0.390

test <- data %>% group_by(ParticipantID, Cond, Modulation, VerbType) %>% summarise(RespPattern = mean(RespPattern))

A1 <- subset(test, Cond=="AffirmConsequent" & Modulation=="Modulated" & VerbType=="nonFactive")$RespPattern -
  subset(test, Cond=="AffirmConsequent" & Modulation=="Unmodulated" & VerbType=="nonFactive")$RespPattern
A2 <- subset(test, Cond=="AffirmConsequent" & Modulation=="Modulated" & VerbType=="Factive")$RespPattern -
  subset(test, Cond=="AffirmConsequent" & Modulation=="Unmodulated" & VerbType=="Factive")$RespPattern

B1 <- subset(test, Cond=="DenyAntecedent" & Modulation=="Modulated" & VerbType=="nonFactive")$RespPattern -
  subset(test, Cond=="DenyAntecedent" & Modulation=="Unmodulated" & VerbType=="nonFactive")$RespPattern
B2 <- subset(test, Cond=="DenyAntecedent" & Modulation=="Modulated" & VerbType=="Factive")$RespPattern -
  subset(test, Cond=="DenyAntecedent" & Modulation=="Unmodulated" & VerbType=="Factive")$RespPattern

A <- A1 - A2
B <- B1 - B2

wilcoxsign_test(A ~ B)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test

# data:  y by x (pos, neg) 
# stratified by block
# Z = 0.038598, p-value = 0.9692
# alternative hypothesis: true mu is not equal to 0

cliff.delta(A, B)
# Cliff's Delta

# delta estimate: 0.03390839 (negligible)
# 95 percent confidence interval:
#   lower      upper 
# -0.1509821  0.2165067 

