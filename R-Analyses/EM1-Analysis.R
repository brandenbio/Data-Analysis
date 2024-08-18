# --------------------------------------------------------------------------------------------------
# Epistemic Modulation Experiment 1 Analysis
# --------------------------------------------------------------------------------------------------
# Subject pool: Amazon Mechanical Turk
# Date(s):      Dec 2022
# Design:       Branden Bio & Sunny Khemlani
# Code:         Branden Bio
# Experimenter:	Knexus Research Corporation
# --------------------------------------------------------------------------------------------------
# Contents:
#  X. Preprocessing; filter subjs by attention check; reformatting responses
#  1. Analyses
#     1.1) Wilcoxon test on response pattern for modulation                                      ***
#     1.2) Wilcoxon test on response pattern for problem type                                    n.s.
#     1.3) Wilcoxon test on response pattern for epistemic verb                                   **
#     1.4) Wilcoxon test on response pattern for interaction between modulation and problem type n.s.
#     1.5) Wilcoxon test on response pattern for interaction between modulation and verb         n.s.
#     1.6) Wilcoxon test on response pattern for interaction between verb and problem type       n.s.
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
project <- "EM1"
##########

rawData <- read.csv(paste("./", project, "-Aggregate.csv", sep=""), header=T)
rawData$ParticipantID <- as.factor(rawData$ParticipantID)
rawData$Cond <- factor(rawData$Cond,levels = c("AffirmConsequent", "DenyAntecedent", "Attn-Chk", "Interpretation"))
rawData$Verb <- str_to_sentence(rawData$Verb)
rawData$Verb <- factor(rawData$Verb,levels = c("Believes", "Knows"))
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
useableSubs  <- unique(data$ParticipantID)
finalN       <- length(unique(data$ParticipantID))

excl.data    <- subset(rawData, FailedAC==1)
excl.data    <- subset(excl.data, ProblemType != "AC")
excludedN    <- length(unique(excl.data$ParticipantID))
subjLoss     <- round(1 - (finalN / fullN),2)

# Subsets
int.data <- subset(data, Cond=="Interpretation") %>% 
  select(ParticipantID, Cond, Verb, IndividX, PropP, PropQ, ConditionalContent, 
         CategoricalContent, ConclusionContent, Answer, AnswerY)

table(int.data$Verb,int.data$Answer)
#            I'm not sure No Yes
# Believes             38  7  14
# Knows                27 28   4

# Interpretation check subset
int.excl.subs <- c(as.character(unique(subset(data, Cond=="Interpretation" & Answer=="Yes")$ParticipantID)))
data$failedINT <- ifelse(data$ParticipantID %in% int.excl.subs, 1, 0)
data <- subset(data, failedINT==0)

data <- subset(data, Cond != "Interpretation")

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
#     60     59         1   40.42     19     67          22        36          0              1

# --------------------------------------------------------------------------------------------------
# 1. Analyses
# --------------------------------------------------------------------------------------------------

# 1.1) Wilcoxon test on response pattern for modulation

data %>% group_by(Modulation) %>% summarise(RespPattern = mean(RespPattern))

# Modulation  ACorDAPattern
# 1 Modulated           0.758
# 2 Unmodulated         0.483

test <- data %>% group_by(ParticipantID, Modulation) %>% summarise(RespPattern = mean(RespPattern))

wilcoxsign_test(subset(test, Modulation=="Modulated")$RespPattern ~
                  subset(test, Modulation=="Unmodulated")$RespPattern)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test

# data:  y by x (pos, neg) 
# stratified by block
# Z = 4.2745, p-value = 1.915e-05
# alternative hypothesis: true mu is not equal to 0

cliff.delta(subset(test, Modulation=="Modulated")$RespPattern ~
              subset(test, Modulation=="Unmodulated")$RespPattern)

# Cliff's Delta

# delta estimate: 0.01680672 (negligible)
# 95 percent confidence interval:
#   lower      upper 
# -0.4554500  0.4816819 

# 1.2) Wilcoxon test on response pattern for problem type

data %>% group_by(Cond) %>% summarise(RespPattern = mean(RespPattern))

# Cond               RespPattern
# 1 AffirmConsequent 0.627
# 2 DenyAntecedent   0.614

test <- data %>% group_by(ParticipantID, Cond) %>% summarise(RespPattern = mean(RespPattern))

wilcoxsign_test(subset(test, Cond=="AffirmConsequent")$RespPattern ~
                  subset(test, Cond=="DenyAntecedent")$RespPattern)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test

# data:  y by x (pos, neg) 
# stratified by block
# Z = 0.19861, p-value = 0.8426
# alternative hypothesis: true mu is not equal to 0

cliff.delta(subset(test, Cond=="AffirmConsequent")$RespPattern,
            subset(test, Cond=="DenyAntecedent")$RespPattern)

# Cliff's Delta

# delta estimate: 0.02384372 (negligible)
# 95 percent confidence interval:
#   lower      upper 
# -0.1791706  0.2249103 

# 1.3) Wilcoxon test on response pattern for epistemic verb

data %>% group_by(Verb) %>% summarise(RespPattern = mean(RespPattern))

#   Verb       RespPattern
# 1 Believes 0.572
# 2 Knows    0.669

test <- data %>% group_by(ParticipantID, Verb) %>% summarise(RespPattern = mean(RespPattern))

wilcoxsign_test(subset(test, Verb=="Believes")$RespPattern ~
                  subset(test, Verb=="Knows")$RespPattern)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test

# data:  y by x (pos, neg)
# 	 stratified by block
# Z = -3.0656, p-value = 0.002173
# alternative hypothesis: true mu is not equal to 0

cliff.delta(subset(test, Verb=="Believes")$RespPattern,
            subset(test, Verb=="Knows")$RespPattern)

# Cliff's Delta

# delta estimate: -0.1738006 (small)
# 95 percent confidence interval:
#   lower       upper 
# -0.36366261  0.02992527  

# 1.4) Wilcoxon test on response pattern for interaction between modulation and problem type

data %>% group_by(Modulation, Cond) %>% summarise(RespPattern = mean(RespPattern))

#   Modulation  Cond             RespPattern
# 1 Modulated   AffirmConsequent 0.763
# 2 Modulated   DenyAntecedent   0.754    
# 3 Unmodulated AffirmConsequent 0.492
# 4 Unmodulated DenyAntecedent   0.475

test <- data %>% group_by(ParticipantID, Modulation, Cond) %>% summarise(RespPattern = mean(RespPattern))

A <- subset(test, Modulation=="Modulated" & Cond=="AffirmConsequent")$RespPattern -
  subset(test, Modulation=="Unmodulated" & Cond=="AffirmConsequent")$RespPattern
B <- subset(test, Modulation=="Modulated" & Cond=="DenyAntecedent")$RespPattern -
  subset(test, Modulation=="Unmodulated" & Cond=="DenyAntecedent")$RespPattern

wilcoxsign_test(A ~ B)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test

# data:  y by x (pos, neg) 
# stratified by block
# Z = -0.32526, p-value = 0.745
# alternative hypothesis: true mu is not equal to 0

cliff.delta(A, B)

# Cliff's Delta

# delta estimate: -0.01350187 (negligible)
# 95 percent confidence interval:
#   lower      upper 
# -0.2106154  0.1846667 

# 1.5) Wilcoxon test on response pattern for interaction between modulation and verb

data %>% group_by(Modulation, Verb) %>% summarise(RespPattern = mean(RespPattern))

#   Modulation  Verb     RespPattern
# 1 Modulated   Believes 0.712
# 2 Modulated   Knows    0.805
# 3 Unmodulated Believes 0.432
# 4 Unmodulated Knows    0.534  

test <- data %>% group_by(ParticipantID, Modulation, Verb) %>% summarise(RespPattern = mean(RespPattern))

A <- subset(test, Modulation=="Modulated" & Verb=="Believes")$RespPattern -
  subset(test, Modulation=="Unmodulated" & Verb=="Believes")$RespPattern
B <- subset(test, Modulation=="Modulated" & Verb=="Knows")$RespPattern -
  subset(test, Modulation=="Unmodulated" & Verb=="Knows")$RespPattern

wilcoxsign_test(A ~ B)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test

# data:  y by x (pos, neg) 
# stratified by block
# Z = 0.33555, p-value = 0.7372
# alternative hypothesis: true mu is not equal to 0

cliff.delta(A, B)

# Cliff's Delta

# delta estimate: 0.0120655 (negligible)
# 95 percent confidence interval:
#   lower      upper 
# -0.1835838  0.2067953 

# 1.6) Wilcoxon test on response pattern for interaction between verb and problem type

data %>% group_by(Verb, Cond) %>% summarise(RespPattern = mean(RespPattern))

#   Verb     Cond             RespPattern
# 1 Believes AffirmConsequent 0.576
# 2 Believes DenyAntecedent   0.568
# 3 Knows    AffirmConsequent 0.678
# 4 Knows    DenyAntecedent   0.661

test <- data %>% group_by(ParticipantID, Verb, Cond) %>% summarise(RespPattern = mean(RespPattern))

A <- subset(test, Verb=="Believes" & Cond=="AffirmConsequent")$RespPattern -
  subset(test, Verb=="Knows" & Cond=="AffirmConsequent")$RespPattern
B <- subset(test, Verb=="Believes" & Cond=="DenyAntecedent")$RespPattern -
  subset(test, Verb=="Knows" & Cond=="DenyAntecedent")$RespPattern

wilcoxsign_test(A ~ B)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test
#
# data:  y by x (pos, neg) 
# stratified by block
# Z = 0.38349, p-value = 0.7014
# alternative hypothesis: true mu is not equal to 0

cliff.delta(A, B)

# Cliff's Delta

# delta estimate: 0.00459638 (negligible)
# 95 percent confidence interval:
#   lower      upper 
# -0.1794598  0.1883417 

# 1.7) Wilcoxon test on three-way interaction

data %>% group_by(Cond, Modulation, Verb) %>% summarise(RespPattern = mean(RespPattern))
# Cond             Modulation  Verb       RespPattern
# 1 AffirmConsequent Modulated   Believes 0.729
# 2 AffirmConsequent Modulated   Knows    0.797
# 3 AffirmConsequent Unmodulated Believes 0.424
# 4 AffirmConsequent Unmodulated Knows    0.559
# 5 DenyAntecedent   Modulated   Believes 0.695    
# 6 DenyAntecedent   Modulated   Knows    0.814    
# 7 DenyAntecedent   Unmodulated Believes 0.441
# 8 DenyAntecedent   Unmodulated Knows    0.508

test <- data %>% group_by(ParticipantID, Cond, Modulation, Verb) %>% summarise(RespPattern = mean(RespPattern))

A1 <- subset(test, Cond=="AffirmConsequent" & Modulation=="Modulated" & Verb=="Believes")$RespPattern -
  subset(test, Cond=="AffirmConsequent" & Modulation=="Unmodulated" & Verb=="Believes")$RespPattern
A2 <- subset(test, Cond=="AffirmConsequent" & Modulation=="Modulated" & Verb=="Knows")$RespPattern -
  subset(test, Cond=="AffirmConsequent" & Modulation=="Unmodulated" & Verb=="Knows")$RespPattern

B1 <- subset(test, Cond=="DenyAntecedent" & Modulation=="Modulated" & Verb=="Believes")$RespPattern -
  subset(test, Cond=="DenyAntecedent" & Modulation=="Unmodulated" & Verb=="Believes")$RespPattern
B2 <- subset(test, Cond=="DenyAntecedent" & Modulation=="Modulated" & Verb=="Knows")$RespPattern -
  subset(test, Cond=="DenyAntecedent" & Modulation=="Unmodulated" & Verb=="Knows")$RespPattern

A <- A1 - A2
B <- B1 - B2

wilcoxsign_test(A ~ B)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test

# data:  y by x (pos, neg) 
# stratified by block
# Z = 1.1495, p-value = 0.2504
# alternative hypothesis: true mu is not equal to 0

cliff.delta(A, B)
# Cliff's Delta

# delta estimate: 0.09480034 (negligible)
# 95 percent confidence interval:
#   lower       upper 
# -0.08987788  0.27317608 
