# --------------------------------------------------------------------------------------------------
# Knowing Thinking Experiment 22 Analysis
# --------------------------------------------------------------------------------------------------
# Subject pool: Amazon Mechanical Turk
# Date(s):      Oct 2022
# Design:       Branden Bio & Sunny Khemlani
# Code:         Branden Bio
# Experimenter:	Knexus Research Corporation
# --------------------------------------------------------------------------------------------------
# Contents:
#  X. Preprocessing; filter subjs by attention check; reformatting responses
#  1. Analyses for 'Believes'
#     1.1) Wilcoxon test on "yes" responses for modulation
#     1.2) Wilcoxon test on "yes" responses for problem type
#     1.3) Wilcoxon test on "yes" responses for interaction between modulation and problem type
#  2. Analyses for 'Knows'
#     2.1) Wilcoxon test on "yes" responses for modulation
#     2.2) Wilcoxon test on "yes" responses for problem type
#     2.3) Wilcoxon test on "yes" responses for interaction between modulation and problem type 
#  3. Between group comparisons
#     3.1) Wilcoxon test on "yes" responses for epistemic verb
#     3.2) Wilcoxon test on "yes" responses for interaction between modulation and verb
#     3.3) Wilcoxon test on "yes" responses for interaction between problem type and verb
#     3.4) Wilcoxon test on "yes" responses for three-way interaction

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
project <- "KT22"
##########

rawData <- read.csv(paste("./", project, "-Aggregate.csv", sep=""), header=T)
rawData$ParticipantID <- as.factor(rawData$ParticipantID)
rawData$Cond <- factor(rawData$Cond,levels = c("AffirmConsequent", "DenyAntecedent", "AC"))
rawData$Verb <- str_to_sentence(rawData$Verb)
rawData$Verb <- factor(rawData$Verb,levels = c("Believes", "Knows"))
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
useableSubs  <- unique(data$ParticipantID)
finalN       <- length(unique(data$ParticipantID))

excl.data    <- subset(rawData, FailedAC==1)
excl.data    <- subset(excl.data, Cond != "AC")
excludedN    <- length(unique(excl.data$ParticipantID))
subjLoss     <- round(1 - (finalN / fullN),2)

# Condition dataframes
b.data <- subset(data, Verb=="Believes") %>% 
  select(ParticipantID, Cond, Verb, Modulation, ProblemType, IndividX, PropP, PropQ, 
         ConditionalContent, CategoricalContent, ConclusionContent, Answer, AnswerY)
k.data <- subset(data, Verb=="Knows") %>% 
  select(ParticipantID, Cond, Verb, Modulation, ProblemType, IndividX, PropP, PropQ, 
         ConditionalContent, CategoricalContent, ConclusionContent, Answer, AnswerY)

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

# fullN finalN excludedN meanAge minAge maxAge femaleCount maleCount otherCount preferNotCount
#  121    108        13   39.52     20     80          46        61          1              0

# --------------------------------------------------------------------------------------------------
# 1. Analyses for 'Believes'
# --------------------------------------------------------------------------------------------------

# 1.1) Wilcoxon test on "yes" responses for modulation

b.data %>% group_by(Modulation) %>% summarise(AnswerY = mean(AnswerY))

# # A tibble: 2 × 2
#   Modulation  AnswerY
# 1 Modulated     0.671
# 2 Unmodulated   0.347

test <- b.data %>% group_by(ParticipantID, Modulation) %>% summarise(AnswerY = mean(AnswerY))

wilcoxsign_test(subset(test, Modulation=="Modulated")$AnswerY ~
                  subset(test, Modulation=="Unmodulated")$AnswerY)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test
#
# b.data:  y by x (pos, neg)
# 	 stratified by block
# Z = 3.9715, p-value = 7.141e-05
# alternative hypothesis: true mu is not equal to 0

cliff.delta(subset(test, Modulation=="Modulated")$AnswerY,
            subset(test, Modulation=="Unmodulated")$AnswerY)

# Cliff's Delta
#
# delta estimate: 0.4410151 (medium)
# 95 percent confidence interval:
#     lower     upper
# 0.2340589 0.6097362 

# 1.2) Wilcoxon test on "yes" responses for problem type

b.data %>% group_by(Cond) %>% summarise(AnswerY = mean(AnswerY))

# # A tibble: 2 × 2
#   Cond             AnswerY
# 1 AffirmConsequent   0.542
# 2 DenyAntecedent     0.477

test <- b.data %>% group_by(ParticipantID, Cond) %>% summarise(AnswerY = mean(AnswerY))

wilcoxsign_test(subset(test, Cond=="AffirmConsequent")$AnswerY ~
                  subset(test, Cond=="DenyAntecedent")$AnswerY)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test
#
# b.data:  y by x (pos, neg)
# 	 stratified by block
# Z = 1.9694, p-value = 0.04891
# alternative hypothesis: true mu is not equal to 0

cliff.delta(subset(test, Cond=="AffirmConsequent")$AnswerY,
            subset(test, Cond=="DenyAntecedent")$AnswerY)

# Cliff's Delta
#
# delta estimate: 0.1100823 (negligible)
# 95 percent confidence interval:
#   lower      upper 
# -0.1023659  0.3129264 

# 1.3) Wilcoxon test on "yes" responses for interaction between modulation and problem type

b.data %>% group_by(Modulation, Cond) %>% summarise(AnswerY = mean(AnswerY))

#   Modulation  Cond             AnswerY
# 1 Modulated   AffirmConsequent   0.713
# 2 Modulated   DenyAntecedent     0.630
# 3 Unmodulated AffirmConsequent   0.370
# 4 Unmodulated DenyAntecedent     0.324

test <- b.data %>% group_by(ParticipantID, Modulation, Cond) %>% summarise(AnswerY = mean(AnswerY))

A <- subset(test, Modulation=="Modulated" & Cond=="AffirmConsequent")$AnswerY -
  subset(test, Modulation=="Unmodulated" & Cond=="AffirmConsequent")$AnswerY
B <- subset(test, Modulation=="Modulated" & Cond=="DenyAntecedent")$AnswerY -
  subset(test, Modulation=="Unmodulated" & Cond=="DenyAntecedent")$AnswerY

wilcoxsign_test(A ~ B)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test
#
# b.data:  y by x (pos, neg)
# 	 stratified by block
# Z = 0.89519, p-value = 0.3707
# alternative hypothesis: true mu is not equal to 0

cliff.delta(A, B)

# Cliff's Delta
#
# delta estimate: 0.02229081 (negligible)
# 95 percent confidence interval:
#   lower       upper 
# -0.1870643  0.2297090 

# --------------------------------------------------------------------------------------------------
# 2. Analyses for 'Knows'
# --------------------------------------------------------------------------------------------------

# 2.1) Wilcoxon test on "yes" responses for modulation

k.data %>% group_by(Modulation) %>% summarise(AnswerY = mean(AnswerY))

# # A tibble: 2 × 2
#   Modulation  AnswerY
# 1 Modulated     0.75
# 2 Unmodulated   0.329

test <- k.data %>% group_by(ParticipantID, Modulation) %>% summarise(AnswerY = mean(AnswerY))

wilcoxsign_test(subset(test, Modulation=="Modulated")$AnswerY ~
                  subset(test, Modulation=="Unmodulated")$AnswerY)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test
#
# k.data:  y by x (pos, neg)
# 	 stratified by block
# Z = 5.0614, p-value = 4.162e-07
# alternative hypothesis: true mu is not equal to 0

cliff.delta(subset(test, Modulation=="Modulated")$AnswerY,
            subset(test, Modulation=="Unmodulated")$AnswerY)

# Cliff's Delta
#
# delta estimate: 0.6066529 (large)
# 95 percent confidence interval:
#   lower     upper 
# 0.4177711 0.7452687 

# 2.2) Wilcoxon test on "yes" responses for problem type

k.data %>% group_by(Cond) %>% summarise(AnswerY = mean(AnswerY))

# # A tibble: 2 × 2
#   Cond             AnswerY
# 1 AffirmConsequent   0.602
# 2 DenyAntecedent     0.477

test <- k.data %>% group_by(ParticipantID, Cond) %>% summarise(AnswerY = mean(AnswerY))

wilcoxsign_test(subset(test, Cond=="AffirmConsequent")$AnswerY ~
                  subset(test, Cond=="DenyAntecedent")$AnswerY)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test
#
# k.data:  y by x (pos, neg)
# 	 stratified by block
# Z = 2.6841, p-value = 0.007272
# alternative hypothesis: true mu is not equal to 0

cliff.delta(subset(test, Cond=="AffirmConsequent")$AnswerY,
            subset(test, Cond=="DenyAntecedent")$AnswerY)

# Cliff's Delta
#
# delta estimate: 0.2376543 (small)
# 95 percent confidence interval:
#        lower        upper
#   0.02823188 0.42709138 

# 2.3) Wilcoxon test on "yes" responses for interaction between modulation and problem type

k.data %>% group_by(Modulation, Cond) %>% summarise(AnswerY = mean(AnswerY))

#   Modulation  Cond             AnswerY
# 1 Modulated   AffirmConsequent   0.824
# 2 Modulated   DenyAntecedent     0.676
# 3 Unmodulated AffirmConsequent   0.380
# 4 Unmodulated DenyAntecedent     0.278

test <- k.data %>% group_by(ParticipantID, Modulation, Cond) %>% summarise(AnswerY = mean(AnswerY))

A <- subset(test, Modulation=="Modulated" & Cond=="AffirmConsequent")$AnswerY -
  subset(test, Modulation=="Unmodulated" & Cond=="AffirmConsequent")$AnswerY
B <- subset(test, Modulation=="Modulated" & Cond=="DenyAntecedent")$AnswerY -
  subset(test, Modulation=="Unmodulated" & Cond=="DenyAntecedent")$AnswerY

wilcoxsign_test(A ~ B)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test
#
# k.data:  y by x (pos, neg)
# 	 stratified by block
# Z = 0.67135, p-value = 0.502
# alternative hypothesis: true mu is not equal to 0

cliff.delta(A, B)

# Cliff's Delta
#
# delta estimate: 0.05555556 (negligible)
# 95 percent confidence interval:
#       lower       upper
# -0.1521512  0.2585627 

# --------------------------------------------------------------------------------------------------
# 3. Between group comparisons
# --------------------------------------------------------------------------------------------------

mann_whitney <- function(x, y)
{
  A <- x
  B <- y
  wilcox_test(response~cond, 
              data=data.frame(response = c(A,B), cond=factor(c(rep("A", length(A)), rep("B", length(B))))))
}

# 3.1) Wilcoxon test on "yes" responses for epistemic verb

data %>% group_by(Verb) %>% summarise(AnswerY = mean(AnswerY))

# # A tibble: 2 × 2
#   Verb     AnswerY
# 1 Believes   0.509
# 2 Knows      0.539

test <- data %>% group_by(ParticipantID, Verb) %>% summarise(AnswerY = mean(AnswerY))

mann_whitney(subset(test, Verb=="Believes")$AnswerY, subset(test, Verb=="Knows")$AnswerY)

# Asymptotic Wilcoxon-Mann-Whitney Test
# 
# data:  response by cond (A, B)
# Z = -0.24391, p-value = 0.8073
# alternative hypothesis: true mu is not equal to 0

cliff.delta(subset(test, Verb=="Believes")$AnswerY,
            subset(test, Verb=="Knows")$AnswerY)

# Cliff's Delta
# 
# delta estimate: -0.02674897 (negligible)
# 95 percent confidence interval:
#      lower      upper 
# -0.2396883  0.1886458 

# 3.2) Wilcoxon test on "yes" responses for interaction between modulation and verb

data %>% group_by(Modulation, Verb) %>% summarise(AnswerY = mean(AnswerY))

# # A tibble: 4 × 3
# # Groups:   Modulation [2]
#   Modulation  Verb     AnswerY
# 1 Modulated   Believes   0.671
# 2 Modulated   Knows      0.75 
# 3 Unmodulated Believes   0.347
# 4 Unmodulated Knows      0.329

test <- data %>% group_by(ParticipantID, Modulation, Verb) %>% summarise(AnswerY = mean(AnswerY))

A <- subset(test, Modulation=="Modulated" & Verb=="Believes")$AnswerY -
  subset(test, Modulation=="Unmodulated" & Verb=="Believes")$AnswerY
B <- subset(test, Modulation=="Modulated" & Verb=="Knows")$AnswerY -
  subset(test, Modulation=="Unmodulated" & Verb=="Knows")$AnswerY

mann_whitney(A, B)

# Asymptotic Wilcoxon-Mann-Whitney Test
# 
# data:  response by cond (A, B)
# Z = -0.96224, p-value = 0.3359
# alternative hypothesis: true mu is not equal to 0

cliff.delta(A, B)

# Cliff's Delta
#
# delta estimate: -0.1056241 (negligible)
# 95 percent confidence interval:
#   lower      upper 
# -0.3128168  0.1111642 


# 3.3) Wilcoxon test on "yes" responses for interaction between problem type and verb

data %>% group_by(Cond, Verb) %>% summarise(AnswerY = mean(AnswerY))

# # A tibble: 4 × 3
# # Groups:   Modulation [2]
# Cond             Verb     AnswerY
# 1 AffirmConsequent Believes   0.542
# 2 AffirmConsequent Knows      0.602
# 3 DenyAntecedent   Believes   0.477
# 4 DenyAntecedent   Knows      0.477

test <- data %>% group_by(ParticipantID, Cond, Verb) %>% summarise(AnswerY = mean(AnswerY))

A <- subset(test, Cond=="AffirmConsequent" & Verb=="Believes")$AnswerY -
  subset(test, Cond=="DenyAntecedent" & Verb=="Believes")$AnswerY
B <- subset(test, Cond=="AffirmConsequent" & Verb=="Knows")$AnswerY -
  subset(test, Cond=="DenyAntecedent" & Verb=="Knows")$AnswerY

mann_whitney(A, B)

# Asymptotic Wilcoxon-Mann-Whitney Test
# 
# data:  response by cond (A, B)
# Z = -1.118, p-value = 0.2636
# alternative hypothesis: true mu is not equal to 0

cliff.delta(A, B)

# Cliff's Delta
#
# delta estimate: -0.117284 (negligible)
# 95 percent confidence interval:
#   lower       upper 
# -0.31683346  0.09220822 

# 3.4) Wilcoxon test on "yes" responses for three-way interaction

data %>% group_by(Modulation, Verb, Cond) %>% summarise(AnswerY = mean(AnswerY))

#   Modulation  Verb     Cond             AnswerY
# 1 Modulated   Believes AffirmConsequent   0.713
# 2 Modulated   Believes DenyAntecedent     0.630
# 3 Modulated   Knows    AffirmConsequent   0.824
# 4 Modulated   Knows    DenyAntecedent     0.676
# 5 Unmodulated Believes AffirmConsequent   0.370
# 6 Unmodulated Believes DenyAntecedent     0.324
# 7 Unmodulated Knows    AffirmConsequent   0.380
# 8 Unmodulated Knows    DenyAntecedent     0.278

test <- data %>% group_by(ParticipantID, Modulation, Verb, Cond) %>% summarise(AnswerY = mean(AnswerY))

A1 <- subset(test, Modulation == "Modulated" & Verb=="Believes" & Cond=="AffirmConsequent")$AnswerY -
  subset(test, Modulation == "Modulated" & Verb=="Knows" & Cond=="AffirmConsequent")$AnswerY
A2 <- subset(test, Modulation == "Modulated" & Verb=="Believes" & Cond=="DenyAntecedent")$AnswerY -
  subset(test, Modulation == "Modulated" & Verb=="Knows" & Cond=="DenyAntecedent")$AnswerY

B1 <- subset(test, Modulation == "Unmodulated" & Verb=="Believes" & Cond=="AffirmConsequent")$AnswerY -
  subset(test, Modulation == "Unmodulated" & Verb=="Knows" & Cond=="AffirmConsequent")$AnswerY
B2 <- subset(test, Modulation == "Unmodulated" & Verb=="Believes" & Cond=="DenyAntecedent")$AnswerY -
  subset(test, Modulation == "Unmodulated" & Verb=="Knows" & Cond=="DenyAntecedent")$AnswerY

A <- A1 - A2
B <- B1 - B2

mann_whitney(A, B)

# Asymptotic Wilcoxon-Mann-Whitney Test
# 
# data:  response by cond (A, B)
# Z = -0.14762, p-value = 0.8826
# alternative hypothesis: true mu is not equal to 0

cliff.delta(A, B)

# Cliff's Delta
#
# delta estimate: -0.01577503 (negligible)
# 95 percent confidence interval:
#   lower       upper 
# -0.04256264  0.35587151 

