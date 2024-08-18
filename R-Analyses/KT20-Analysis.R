#!/usr/bin/env Rscript
# --------------------------------------------------------------------------------------------------
# Knowing Thinking Experiment 20 Analysis
# --------------------------------------------------------------------------------------------------
# Subject pool: Amazon Mechanical Turk
# Date(s):      August 2022
# Design:       Branden Bio & Sunny Khemlani
# Code:         Branden Bio
# Experimenter:	Knexus Research Corporation
# --------------------------------------------------------------------------------------------------
# Contents:
#  X. Preprocessing; filter subjs by attention check; reformatting responses
#  X. Demographics summary
#  1. Omnibus tables
#  2. Errors of Omniscience
#     2.1)  Wilcoxon test comparing errors of omniscience in ProblemType
#     2.2)  Wilcoxon test comparing errors of omniscience in Construction
#     2.3)  Wilcoxon test comparing errors of omniscience in EvidenceAccess
#     2.4)  Wilcoxon test on errors of omniscience for interaction between ProblemType and Construction
#     2.5)  Wilcoxon test on errors of omniscience for interaction between Construction and Evidence
#     2.6)  Wilcoxon test on errors of omniscience for interaction between ProblemType and Evidence
#     2.7)  Wilcoxon test on errors of omniscience for three-way interaction
#  3. Accuracy
#     3.1)  Wilcoxon test comparing accuracy in ProblemType
#     3.2)  Wilcoxon test comparing accuracy in Construction
#     3.3)  Wilcoxon test comparing accuracy in EvidenceAccess
#     3.4)  Wilcoxon test on accuracy for interaction between ProblemType and Construction
#     3.5)  Wilcoxon test on accuracy for interaction between Construction and Evidence
#     3.6)  Wilcoxon test on accuracy for interaction between ProblemType and Evidence
#     3.7)  Wilcoxon test on accuracy for three-way interaction
# --------------------------------------------------------------------------------------------------

library(tidyr)
library(coin)
library(reshape2)
library(lattice)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(lme4)
library(stringr)
library(irr)
library(ggridges)
library(rstudioapi)
library(DescTools)
library(effsize)
library(ggsci)

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
project <- "KT20"

# --------------------------------------------------------------------------------------------------
# X. Preprocessing; filter subjs by attention check; reformatting responses
# --------------------------------------------------------------------------------------------------

rawData <- read.csv(paste("./", project, "-Aggregate.csv", sep=""), header=T)
data.Prelim <- rawData
data.Prelim$ParticipantID <- as.factor(data.Prelim$ParticipantID)
data.Prelim$Answer <- gsub("(\\.).*","\\1",data.Prelim$Answer)
data.Prelim$Answer<- str_to_sentence(data.Prelim$Answer)
data.Prelim$Answer<- str_remove(data.Prelim$Answer, "\\.")
fullN <- length(unique(data.Prelim$ParticipantID))

data.Prelim$condC <- ifelse(grepl("then", data.Prelim$Premise1Content, fixed = TRUE), 
                            data.Prelim$Premise1Content, data.Prelim$Premise2Content)
data.Prelim$PropositionP <- gsub(".*then ", "", data.Prelim$condC)
data.Prelim$PropositionP <- gsub("it's", "It is", data.Prelim$PropositionP)
data.Prelim.AC <- subset(data.Prelim, Cond == "AC")
data.Prelim.AC$correctAC <- "Believes that knows that nothing follows"
data.Prelim.AC$PassedAC <- ifelse(data.Prelim.AC$Answer == data.Prelim.AC$correctAC, 1, 0)
missedAC <- c(as.character(subset(data.Prelim.AC, PassedAC==0)$ParticipantID))
failedAC <- missedAC[duplicated(missedAC)]
data.Prelim$FailedAC <- ifelse(data.Prelim$ParticipantID %in% failedAC, 1, 0)

data.debrief <- subset(data.Prelim, Cond == "DB")
data.debrief <- subset(data.debrief, FailedAC==0)
data.probs   <- subset(data.Prelim, Cond != "DB")

data         <- subset(data.probs, FailedAC==0)
data         <- subset(data, Cond != "AC")
useableSubs  <- unique(data$ParticipantID)
finalN       <- length(unique(data$ParticipantID))
excludedN    <- fullN - finalN
subjLoss     <- round(1 - (finalN / fullN),2)
debrief.out  <- data.debrief %>% select(ParticipantID, ProblemNumber, FreeResp, Answer)

debrief.filename <- paste(project, "-debrief-resps.csv", sep="")
if (file.exists(debrief.filename) == FALSE) {
  write.csv(debrief.out, debrief.filename, row.names = FALSE)
}

# Substantive responses from subjects that aren't nonsense
responseOptions <- list(`Q`                = "Q",
                        `not Q`            = "not Q",
                        `X believes Q`     = "X believes that Q",
                        `X believes not Q` = "X believes that not Q",
                        `X knows Q`        = "X knows that Q",
                        `X knows not Q`    = "X knows that not Q",
                        `P`                = "P",
                        `not P`            = "not P",
                        `X believes P`     = "X believes that P",
                        `X believes not P` = "X believes that not P",
                        `X knows P`        = "X knows that P",
                        `X knows not P`    = "X knows that not P",
                        `Nothing follows`  = "Nothing follows")

data$ResponseFormat <- str_replace(data$Answer, data$IndividX, "X")
data$ResponseFormat <- str_replace(data$ResponseFormat, data$PropQ, "Q")
data$ResponseFormat <- str_replace(data$ResponseFormat, data$PropP, "P")
data$ResponseFormat <- str_replace(data$ResponseFormat, regex("it is ", ignore_case = TRUE), "")
data$ResponseFormat <- gsub("\\..*","\\1",data$ResponseFormat)

data$AnswerType <- ifelse(data$ResponseFormat %in% responseOptions,
                          names(responseOptions)[match(data$ResponseFormat, responseOptions)], 
                          "Nonsense")

data$AnswerType[data$Cond == "AC"] <- "AC"

data$Verb <- as.factor(str_remove(data$ProblemType, "-Fact"))

# Error of omniscience rubric
# Error of omniscience occurs whenever an answer contains 1 or more epistemic verbs

# Assign error of omniscience scores
data$OmniscienceError <- ifelse(grepl("believes", data$AnswerType), 1, 
                                    ifelse(grepl("knows", data$AnswerType), 1, 0))
data <- transform(data, OmniscienceError = as.numeric(OmniscienceError))

# Accuracy Rubric
# Cor = 1, Inc = 0
# Modus Ponens                            # Modus Tollens
# ProblemType   | AnswerType        | Acc # ProblemType   | AnswerType        | Acc
# Believes-Fact | Nonsense          | Inc # Believes-Fact | Nonsense          | Inc
# Believes-Fact | Nothing follows   | Cor # Believes-Fact | Nothing follows   | Cor
# Believes-Fact | Q                 | Inc # Believes-Fact | Q                 | Inc
# Believes-Fact | not Q             | Inc # Believes-Fact | not Q             | Cor
# Believes-Fact | X believes Q      | Inc # Believes-Fact | X believes Q      | Inc
# Believes-Fact | X believes not Q  | Inc # Believes-Fact | X believes not Q  | Inc
# Believes-Fact | X knows Q         | Inc # Believes-Fact | X knows Q         | Inc
# Believes-Fact | X knows not Q     | Inc # Believes-Fact | X knows not Q     | Inc
# Believes-Fact | P                 | Cor # Believes-Fact | P                 | Inc
# Believes-Fact | not P             | Inc # Believes-Fact | not P             | Inc
# Believes-Fact | X believes P      | Inc # Believes-Fact | X believes P      | Inc
# Believes-Fact | X believes not P  | Inc # Believes-Fact | X believes not P  | Inc
# Believes-Fact | X knows P         | Inc # Believes-Fact | X knows P         | Inc
# Believes-Fact | X knows not P     | Inc # Believes-Fact | X knows not P     | Inc
# Modus Ponens                            # Modus Tollens
# ProblemType   | AnswerType        | Acc # ProblemType   | AnswerType        | Acc
# Knows-Fact    | Nonsense          | Inc # Knows-Fact    | Nonsense          | Inc
# Knows-Fact    | Nothing follows   | Inc # Knows-Fact    | Nothing follows   | Inc
# Knows-Fact    | Q                 | Cor # Knows-Fact    | Q                 | Inc
# Knows-Fact    | not Q             | Inc # Knows-Fact    | not Q             | Cor
# Knows-Fact    | X believes Q      | Inc # Knows-Fact    | X believes Q      | Inc
# Knows-Fact    | X believes not Q  | Inc # Knows-Fact    | X believes not Q  | Inc
# Knows-Fact    | X knows Q         | Inc # Knows-Fact    | X knows Q         | Inc
# Knows-Fact    | X knows not Q     | Inc # Knows-Fact    | X knows not Q     | Inc
# Knows-Fact    | P                 | Cor # Knows-Fact    | P                 | Inc
# Knows-Fact    | not P             | Inc # Knows-Fact    | not P             | Cor
# Knows-Fact    | X believes P      | Inc # Knows-Fact    | X believes P      | Inc
# Knows-Fact    | X believes not P  | Inc # Knows-Fact    | X believes not P  | Inc
# Knows-Fact    | X knows P         | Inc # Knows-Fact    | X knows P         | Inc
# Knows-Fact    | X knows not P     | Inc # Knows-Fact    | X knows not P     | Inc

# Assign accuracy scores
data$Acc <- 0
# Believes + Modus Ponens
data$Acc[(data$AnswerType == "Nothing follows" | data$AnswerType == "P") &
           data$ProblemType == "Believes-Fact" & data$Construction == "Modus Ponens"] <- 1
# Believes + Modus Tollens
data$Acc[(data$AnswerType == "Nothing follows" | data$AnswerType == "not Q") &
           data$ProblemType == "Believes-Fact" & data$Construction == "Modus Tollens"] <- 1
# Knows + Modus Ponens
data$Acc[data$AnswerType == "P" | data$AnswerType == "Q" &
           data$ProblemType == "Knows-Fact" & data$Construction == "Modus Ponens"] <- 1
# Knows + Modus Tollens
data$Acc[data$AnswerType == "not P" | data$AnswerType == "not Q" &
           data$ProblemType == "Knows-Fact" & data$Construction == "Modus Tollens"] <- 1

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
#    60     51         9   38.22     24     69          22        29          0               0

# --------------------------------------------------------------------------------------------------
# 1. Omnibus tables
# --------------------------------------------------------------------------------------------------
responses.table.omni <- group_by(data, ProblemType, Construction, EvidenceAccess) %>% 
  summarise(n=n(), mean=mean(OmniscienceError), sd=sd(OmniscienceError)) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1)) %>%
  arrange(desc(mean))

# Errors of omniscience
# ProblemType   Construction  EvidenceAccess     n  mean    sd     se     ic
# 1 Believes-Fact Modus Ponens  Evidence         102 0.843 0.365 0.0362 0.0718
# 2 Knows-Fact    Modus Ponens  Evidence         102 0.843 0.365 0.0362 0.0718
# 3 Believes-Fact Modus Tollens Evidence         102 0.539 0.501 0.0496 0.0984
# 4 Knows-Fact    Modus Tollens Evidence         102 0.539 0.501 0.0496 0.0984
# 5 Believes-Fact Modus Ponens  noEvidence       102 0.402 0.493 0.0488 0.0968
# 6 Knows-Fact    Modus Tollens noEvidence       102 0.402 0.493 0.0488 0.0968
# 7 Believes-Fact Modus Tollens noEvidence       102 0.392 0.491 0.0486 0.0964
# 8 Knows-Fact    Modus Ponens  noEvidence       102 0.363 0.483 0.0478 0.0949

responses.table.acc <- group_by(data, ProblemType, Construction, EvidenceAccess) %>% 
  summarise(n=n(), mean=mean(Acc), sd=sd(Acc)) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1)) %>%
  arrange(desc(mean))

# Accuracy
# ProblemType   Construction  EvidenceAccess     n   mean    sd     se     ic
# 1 Knows-Fact    Modus Ponens  noEvidence       102 0.627  0.486 0.0481 0.0954
# 2 Believes-Fact Modus Tollens noEvidence       102 0.588  0.495 0.0490 0.0971
# 3 Knows-Fact    Modus Tollens noEvidence       102 0.588  0.495 0.0490 0.0971
# 4 Believes-Fact Modus Tollens Evidence         102 0.461  0.501 0.0496 0.0984
# 5 Knows-Fact    Modus Tollens Evidence         102 0.441  0.499 0.0494 0.0980
# 6 Believes-Fact Modus Ponens  noEvidence       102 0.373  0.486 0.0481 0.0954
# 7 Knows-Fact    Modus Ponens  Evidence         102 0.147  0.356 0.0352 0.0699
# 8 Believes-Fact Modus Ponens  Evidence         102 0.0196 0.139 0.0138 0.0274

data.dummy <- data %>% mutate(var = 1) %>% spread(AnswerType, var, fill = 0, sep = "_") %>% 
  left_join(data) %>% 
  select(ParticipantID, Cond, ProblemType, Construction, EvidenceAccess, OmniscienceError, AnswerType, 
         `AnswerType_Nonsense`, `AnswerType_not P`, `AnswerType_not Q`, `AnswerType_Nothing follows`, 
         `AnswerType_P`, `AnswerType_Q`, `AnswerType_X believes not P`, `AnswerType_X believes not Q`, 
         `AnswerType_X believes P`, `AnswerType_X believes Q`, `AnswerType_X knows not P`, 
         `AnswerType_X knows not Q`, `AnswerType_X knows P`, `AnswerType_X knows Q`) %>% 
  gather(AnswerType, Response, AnswerType_Nonsense:`AnswerType_X knows Q`)

data.dummy.constr <- data.dummy %>% select(Construction, AnswerType, Response)

responses.table.constr  <- group_by(data.dummy.constr, Construction, AnswerType) %>% 
  summarise(Response = mean(Response)) %>%
  arrange(Construction, desc(Response))

#     Construction                  AnswerType    Response
# 1   Modus Ponens     AnswerType_X believes Q 0.311274510
# 2   Modus Ponens                AnswerType_Q 0.225490196
# 3   Modus Ponens        AnswerType_X knows Q 0.220588235
# 4   Modus Ponens  AnswerType_Nothing follows 0.125000000
# 5   Modus Ponens        AnswerType_X knows P 0.051470588
# 6   Modus Ponens                AnswerType_P 0.029411765
# 7   Modus Ponens     AnswerType_X believes P 0.024509804
# 8   Modus Ponens         AnswerType_Nonsense 0.007352941
# 9   Modus Ponens AnswerType_X believes not Q 0.004901961
# 10  Modus Ponens            AnswerType_not P 0.000000000
# 11  Modus Ponens            AnswerType_not Q 0.000000000
# 12  Modus Ponens AnswerType_X believes not P 0.000000000
# 13  Modus Ponens    AnswerType_X knows not P 0.000000000
# 14  Modus Ponens    AnswerType_X knows not Q 0.000000000
# 15 Modus Tollens            AnswerType_not P 0.232843137
# 16 Modus Tollens  AnswerType_Nothing follows 0.188725490
# 17 Modus Tollens    AnswerType_X knows not P 0.171568627
# 18 Modus Tollens AnswerType_X believes not P 0.115196078
# 19 Modus Tollens            AnswerType_not Q 0.098039216
# 20 Modus Tollens    AnswerType_X knows not Q 0.083333333
# 21 Modus Tollens        AnswerType_X knows P 0.034313725
# 22 Modus Tollens AnswerType_X believes not Q 0.024509804
# 23 Modus Tollens     AnswerType_X believes Q 0.017156863
# 24 Modus Tollens        AnswerType_X knows Q 0.012254902
# 25 Modus Tollens         AnswerType_Nonsense 0.009803922
# 26 Modus Tollens     AnswerType_X believes P 0.009803922
# 27 Modus Tollens                AnswerType_Q 0.002450980
# 28 Modus Tollens                AnswerType_P 0.000000000

# --------------------------------------------------------------------------------------------------
# 2. Errors of Omniscience
# --------------------------------------------------------------------------------------------------

# ProblemType
BF <- data %>% filter(ProblemType == "Believes-Fact")
KF <- data %>% filter(ProblemType == "Knows-Fact")

# Construction
MP <- data %>% filter(Construction == "Modus Ponens")
MT <- data %>% filter(Construction == "Modus Tollens")

# EvidenceAccess
Ev   <- data %>% filter(EvidenceAccess == "Evidence")
noEv <- data %>% filter(EvidenceAccess == "noEvidence")

# 2.1) Wilcoxon test comparing errors of omniscience - ProblemType
data %>% group_by(ProblemType) %>% summarise(OmniscienceError = mean(OmniscienceError))
# ProblemType     OmniscienceError
# 1 Believes-Fact            0.544
# 2 Knows-Fact               0.537

B_omniErr <- aggregate(OmniscienceError ~ ParticipantID, data=BF, FUN=mean)
K_omniErr <- aggregate(OmniscienceError ~ ParticipantID, data=KF, FUN=mean)

wilcoxsign_test(B_omniErr$OmniscienceError ~ K_omniErr$OmniscienceError)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = 0, p-value = 1
# alternative hypothesis: true mu is not equal to 0

cliff.delta(B_omniErr$OmniscienceError,K_omniErr$OmniscienceError,conf.level = 0.95)
# delta estimate: 0.01768551 (negligible)

# 2.2) Wilcoxon test comparing errors of omniscience - Construction
data %>% group_by(Construction) %>% summarise(OmniscienceError = mean(OmniscienceError))
# Construction    OmniscienceError
# 1 Modus Ponens             0.613
# 2 Modus Tollens            0.468

MP_omniErr <- aggregate(OmniscienceError ~ ParticipantID, data=MP, FUN=mean)
MT_omniErr <- aggregate(OmniscienceError ~ ParticipantID, data=MT, FUN=mean)

wilcoxsign_test(MP_omniErr$OmniscienceError ~ MT_omniErr$OmniscienceError)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = 3.6192, p-value = 0.0002955
# alternative hypothesis: true mu is not equal to 0

cliff.delta(MP_omniErr$OmniscienceError,MT_omniErr$OmniscienceError ,conf.level = 0.95)
# delta estimate: 0.2429835 (small)

# 2.3) Wilcoxon test comparing errors of omniscience - EvidenceAccess
data %>% group_by(EvidenceAccess) %>% summarise(OmniscienceError = mean(OmniscienceError))
# EvidenceAccess    OmniscienceError
# 1 Evidence                  0.691
# 2 noEvidence                0.390

Ev_omniErr <- aggregate(OmniscienceError ~ ParticipantID, data=Ev, FUN=mean)
noEv_omniErr <- aggregate(OmniscienceError ~ ParticipantID, data=noEv, FUN=mean)

wilcoxsign_test(Ev_omniErr$OmniscienceError ~ noEv_omniErr$OmniscienceError)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = 5.0779, p-value = 3.816e-07
# alternative hypothesis: true mu is not equal to 0

cliff.delta(Ev_omniErr$OmniscienceError,noEv_omniErr$OmniscienceError ,conf.level = 0.95)
# delta estimate: 0.4290657 (medium)

# 2.4) Wilcoxon test on errors of omniscience for interaction between ProblemType and Construction
data %>% group_by(ProblemType, Construction) %>% summarise(OmniscienceError = mean(OmniscienceError))
#   ProblemType   Construction  OmniscienceError
# 1 Believes-Fact Modus Ponens             0.623
# 2 Believes-Fact Modus Tollens            0.466
# 3 Knows-Fact    Modus Ponens             0.603
# 4 Knows-Fact    Modus Tollens            0.471

ProbxCon_Omni <- data %>% group_by(ParticipantID, ProblemType, Construction) %>% 
  summarise(OmniscienceError = mean(OmniscienceError))

Prob_MP_Omni <- subset(ProbxCon_Omni, ProblemType=="Believes-Fact" & Construction=="Modus Ponens")$OmniscienceError - 
  subset(ProbxCon_Omni, ProblemType=="Knows-Fact" & Construction=="Modus Ponens")$OmniscienceError
Prob_MT_Omni <- subset(ProbxCon_Omni, ProblemType=="Believes-Fact" & Construction=="Modus Tollens")$OmniscienceError - 
  subset(ProbxCon_Omni, ProblemType=="Knows-Fact" & Construction=="Modus Tollens")$OmniscienceError

wilcoxsign_test(Prob_MP_Omni ~ Prob_MT_Omni)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = 0.39567, p-value = 0.6923
# alternative hypothesis: true mu is not equal to 0

cliff.delta(Prob_MP_Omni, Prob_MT_Omni, conf.level = 0.95)
# delta estimate: 0.0165321 (negligible)

# 2.5) Wilcoxon test on errors of omniscience for interaction between Construction and Evidence
data %>% group_by(Construction, EvidenceAccess) %>% summarise(OmniscienceError = mean(OmniscienceError))
# Construction    EvidenceAccess OmniscienceError
# 1 Modus Ponens  Evidence                  0.843
# 2 Modus Ponens  noEvidence                0.382
# 3 Modus Tollens Evidence                  0.539
# 4 Modus Tollens noEvidence                0.397

ConxEv_Omni <- data %>% group_by(ParticipantID, Construction, EvidenceAccess) %>% 
  summarise(OmniscienceError = mean(OmniscienceError))

Con_Ev_Omni <- subset(ConxEv_Omni, Construction=="Modus Ponens" & EvidenceAccess=="Evidence")$OmniscienceError - 
  subset(ConxEv_Omni, Construction=="Modus Tollens" & EvidenceAccess=="Evidence")$OmniscienceError
Con_noEv_Omni <- subset(ConxEv_Omni, Construction=="Modus Ponens" & EvidenceAccess=="noEvidence")$OmniscienceError - 
  subset(ConxEv_Omni, Construction=="Modus Tollens" & EvidenceAccess=="noEvidence")$OmniscienceError

wilcoxsign_test(Con_Ev_Omni ~ Con_noEv_Omni)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = 4.0819, p-value = 4.468e-05
# alternative hypothesis: true mu is not equal to 0

cliff.delta(Con_Ev_Omni, Con_noEv_Omni, conf.level = 0.95)
# delta estimate: 0.4602076 (medium)

# 2.6) Wilcoxon test on errors of omniscience for interaction between ProblemType and Evidence
data %>% group_by(ProblemType, EvidenceAccess) %>% summarise(OmniscienceError = mean(OmniscienceError))
#   ProblemType   EvidenceAccess OmniscienceError
# 1 Believes-Fact Evidence                  0.691
# 2 Believes-Fact noEvidence                0.397
# 3 Knows-Fact    Evidence                  0.691
# 4 Knows-Fact    noEvidence                0.382

ProbxEv_Omni <- data %>% group_by(ParticipantID, ProblemType, EvidenceAccess) %>% 
  summarise(OmniscienceError = mean(OmniscienceError))

Prob_Ev_Omni <- subset(ProbxEv_Omni, ProblemType=="Believes-Fact" & EvidenceAccess=="Evidence")$OmniscienceError - 
  subset(ProbxEv_Omni, ProblemType=="Knows-Fact" & EvidenceAccess=="Evidence")$OmniscienceError
Prob_noEv_Omni <- subset(ProbxEv_Omni, ProblemType=="Believes-Fact" & EvidenceAccess=="noEvidence")$OmniscienceError - 
  subset(ProbxEv_Omni, ProblemType=="Knows-Fact" & EvidenceAccess=="noEvidence")$OmniscienceError

wilcoxsign_test(Prob_Ev_Omni ~ Prob_noEv_Omni)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = -0.49337, p-value = 0.6218
# alternative hypothesis: true mu is not equal to 0

cliff.delta(Prob_Ev_Omni, Prob_noEv_Omni, conf.level = 0.95)
# delta estimate: -0.07612457 (negligible)

# 2.7) Wilcoxon test on errors of omniscience for three-way interaction
data %>% group_by(ProblemType, Construction, EvidenceAccess) %>% summarise(OmniscienceError = mean(OmniscienceError))
#   ProblemType   Construction  EvidenceAccess OmniscienceError
# 1 Believes-Fact Modus Ponens  Evidence                  0.843
# 2 Believes-Fact Modus Ponens  noEvidence                0.402
# 3 Believes-Fact Modus Tollens Evidence                  0.539
# 4 Believes-Fact Modus Tollens noEvidence                0.392
# 5 Knows-Fact    Modus Ponens  Evidence                  0.843
# 6 Knows-Fact    Modus Ponens  noEvidence                0.363
# 7 Knows-Fact    Modus Tollens Evidence                  0.539
# 8 Knows-Fact    Modus Tollens noEvidence                0.402

omni.test <- data %>% group_by(ParticipantID, ProblemType, Construction, EvidenceAccess) %>% 
                            summarise(OmniscienceError = mean(OmniscienceError))

omni.A1 <- subset(omni.test, ProblemType=="Believes-Fact" & Construction=="Modus Ponens" & EvidenceAccess=="Evidence")$OmniscienceError -
  subset(omni.test, ProblemType=="Believes-Fact" & Construction=="Modus Tollens" & EvidenceAccess=="Evidence")$OmniscienceError
omni.A2 <- subset(omni.test, ProblemType=="Believes-Fact" & Construction=="Modus Ponens" & EvidenceAccess=="noEvidence")$OmniscienceError -
  subset(omni.test, ProblemType=="Believes-Fact" & Construction=="Modus Tollens" & EvidenceAccess=="noEvidence")$OmniscienceError

omni.B1 <- subset(omni.test, ProblemType=="Knows-Fact" & Construction=="Modus Ponens" & EvidenceAccess=="Evidence")$OmniscienceError -
  subset(omni.test, ProblemType=="Knows-Fact" & Construction=="Modus Tollens" & EvidenceAccess=="Evidence")$OmniscienceError
omni.B2 <- subset(omni.test, ProblemType=="Knows-Fact" & Construction=="Modus Ponens" & EvidenceAccess=="noEvidence")$OmniscienceError -
  subset(omni.test, ProblemType=="Knows-Fact" & Construction=="Modus Tollens" & EvidenceAccess=="noEvidence")$OmniscienceError

omni.A <- omni.A1 - omni.A2
omni.B <- omni.B1 - omni.B2

wilcoxsign_test(omni.A ~ omni.B)
# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg)
# 	 stratified by block
# Z = -0.75457, p-value = 0.4505
# alternative hypothesis: true mu is not equal to 0

cliff.delta(omni.A, omni.B)
# delta estimate: -0.05574779 (negligible)

# --------------------------------------------------------------------------------------------------
# 3. Accuracy
# --------------------------------------------------------------------------------------------------

# 3.1) Wilcoxon test comparing accuracy in ProblemType
data %>% group_by(ProblemType) %>% summarise(Acc = mean(Acc))
# ProblemType     Acc
# 1 Believes-Fact 0.360
# 2 Knows-Fact    0.451

B_acc <- aggregate(Acc ~ ParticipantID, data=BF, FUN=mean)
K_acc <- aggregate(Acc ~ ParticipantID, data=KF, FUN=mean)

wilcoxsign_test(B_acc$Acc ~ K_acc$Acc)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = -3.0327, p-value = 0.002424
# alternative hypothesis: true mu is not equal to 0

cliff.delta(B_acc$Acc,K_acc$Acc,conf.level = 0.95)
# delta estimate: -0.1760861 (small)

# 3.2) Wilcoxon test comparing accuracy in Construction
data %>% group_by(Construction) %>% summarise(Acc = mean(Acc))
# Construction    Acc
# 1 Modus Ponens  0.292
# 2 Modus Tollens 0.520

MP_acc <- aggregate(Acc ~ ParticipantID, data=MP, FUN=mean)
MT_acc <- aggregate(Acc ~ ParticipantID, data=MT, FUN=mean)

wilcoxsign_test(MP_acc$Acc ~ MT_acc$Acc)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = -4.6658, p-value = 3.074e-06
# alternative hypothesis: true mu is not equal to 0

cliff.delta(MP_acc$Acc,MT_acc$Acc,conf.level = 0.95)
# delta estimate: -0.3598616 (medium)

# 3.3) Wilcoxon test comparing accuracy in EvidenceAccess
data %>% group_by(EvidenceAccess) %>% summarise(Acc = mean(Acc))
# EvidenceAccess   Acc
# 1 Evidence       0.267
# 2 noEvidence     0.544

Ev_acc   <- aggregate(Acc ~ ParticipantID, data=Ev, FUN=mean)
noEv_acc <- aggregate(Acc ~ ParticipantID, data=noEv, FUN=mean)

wilcoxsign_test(Ev_acc$Acc ~ noEv_acc$Acc)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = -5.0978, p-value = 3.437e-07
# alternative hypothesis: true mu is not equal to 0

cliff.delta(Ev_acc$Acc,noEv_acc$Acc,conf.level = 0.95)
# delta estimate: -0.4548251 (medium)

# 3.4) Wilcoxon test on accuracy for interaction between ProblemType and Construction
data %>% group_by(ProblemType, Construction) %>% summarise(Acc = mean(Acc))
# ProblemType     Construction    Acc
# 1 Believes-Fact Modus Ponens  0.196
# 2 Believes-Fact Modus Tollens 0.525
# 3 Knows-Fact    Modus Ponens  0.387
# 4 Knows-Fact    Modus Tollens 0.515

ProbxCon_Acc <- data %>% group_by(ParticipantID, ProblemType, Construction) %>% summarise(Acc = mean(Acc))

Prob_MP_Acc <- subset(ProbxCon_Acc, ProblemType=="Believes-Fact" & Construction=="Modus Ponens")$Acc - 
  subset(ProbxCon_Acc, ProblemType=="Knows-Fact" & Construction=="Modus Ponens")$Acc
Prob_MT_Acc <- subset(ProbxCon_Acc, ProblemType=="Believes-Fact" & Construction=="Modus Tollens")$Acc - 
  subset(ProbxCon_Acc, ProblemType=="Knows-Fact" & Construction=="Modus Tollens")$Acc

wilcoxsign_test(Prob_MP_Acc ~ Prob_MT_Acc)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = -3.4408, p-value = 0.0005801
# alternative hypothesis: true mu is not equal to 0

cliff.delta(Prob_MP_Acc, Prob_MT_Acc, conf.level = 0.95)
# delta estimate: -0.3763937 (medium)

# 3.5) Wilcoxon test on accuracy for interaction between Construction and Evidence
data %>% group_by(Construction, EvidenceAccess) %>% summarise(Acc = mean(Acc))
# Construction    EvidenceAccess    Acc
# 1 Modus Ponens  Evidence       0.0833
# 2 Modus Ponens  noEvidence     0.5   
# 3 Modus Tollens Evidence       0.451 
# 4 Modus Tollens noEvidence     0.588 

ConxEv_Acc <- data %>% group_by(ParticipantID, Construction, EvidenceAccess) %>% summarise(Acc = mean(Acc))

Con_Ev_Acc <- subset(ConxEv_Acc, Construction=="Modus Ponens" & EvidenceAccess=="Evidence")$Acc - 
  subset(ConxEv_Acc, Construction=="Modus Tollens" & EvidenceAccess=="Evidence")$Acc
Con_noEv_Acc <- subset(ConxEv_Acc, Construction=="Modus Ponens" & EvidenceAccess=="noEvidence")$Acc - 
  subset(ConxEv_Acc, Construction=="Modus Tollens" & EvidenceAccess=="noEvidence")$Acc

wilcoxsign_test(Con_Ev_Acc ~ Con_noEv_Acc)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = -3.5276, p-value = 0.0004194
# alternative hypothesis: true mu is not equal to 0

cliff.delta(Con_Ev_Acc, Con_noEv_Acc, conf.level = 0.95)
# delta estimate: -0.3775471 (medium)

# 3.6) Wilcoxon test on accuracy for interaction between ProblemType and Evidence
data %>% group_by(ProblemType, EvidenceAccess) %>% summarise(Acc = mean(Acc))
# ProblemType   EvidenceAccess   Acc
# 1 Believes-Fact Evidence       0.240
# 2 Believes-Fact noEvidence     0.480
# 3 Knows-Fact    Evidence       0.294
# 4 Knows-Fact    noEvidence     0.608

ProbxEv_Acc <- data %>% group_by(ParticipantID, ProblemType, EvidenceAccess) %>% summarise(Acc = mean(Acc))

Prob_Ev_Acc <- subset(ProbxEv_Acc, ProblemType=="Believes-Fact" & EvidenceAccess=="Evidence")$Acc - 
  subset(ProbxEv_Acc, ProblemType=="Knows-Fact" & EvidenceAccess=="Evidence")$Acc
Prob_noEv_Acc <- subset(ProbxEv_Acc, ProblemType=="Believes-Fact" & EvidenceAccess=="noEvidence")$Acc - 
  subset(ProbxEv_Acc, ProblemType=="Knows-Fact" & EvidenceAccess=="noEvidence")$Acc

wilcoxsign_test(Prob_Ev_Acc ~ Prob_noEv_Acc)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = 1.4899, p-value = 0.1363
# alternative hypothesis: true mu is not equal to 0

cliff.delta(Prob_Ev_Acc, Prob_noEv_Acc, conf.level = 0.95)
# delta estimate: 0.195694 (small)

# 3.7) Wilcoxon test on accuracy for three-way interaction
data %>% group_by(ProblemType, Construction, EvidenceAccess) %>% summarise(Acc = mean(Acc))
#   ProblemType   Construction  EvidenceAccess    Acc
# 1 Believes-Fact Modus Ponens  Evidence       0.0196
# 2 Believes-Fact Modus Ponens  noEvidence     0.373 
# 3 Believes-Fact Modus Tollens Evidence       0.461 
# 4 Believes-Fact Modus Tollens noEvidence     0.588 
# 5 Knows-Fact    Modus Ponens  Evidence       0.147 
# 6 Knows-Fact    Modus Ponens  noEvidence     0.627 
# 7 Knows-Fact    Modus Tollens Evidence       0.441 
# 8 Knows-Fact    Modus Tollens noEvidence     0.588 

acc.test <- data %>% group_by(ParticipantID, ProblemType, Construction, EvidenceAccess) %>% 
  summarise(Acc = mean(Acc))

acc.A1 <- subset(acc.test, ProblemType=="Believes-Fact" & Construction=="Modus Ponens" & EvidenceAccess=="Evidence")$Acc -
  subset(acc.test, ProblemType=="Believes-Fact" & Construction=="Modus Tollens" & EvidenceAccess=="Evidence")$Acc
acc.A2 <- subset(acc.test, ProblemType=="Believes-Fact" & Construction=="Modus Ponens" & EvidenceAccess=="noEvidence")$Acc -
  subset(acc.test, ProblemType=="Believes-Fact" & Construction=="Modus Tollens" & EvidenceAccess=="noEvidence")$Acc

acc.B1 <- subset(acc.test, ProblemType=="Knows-Fact" & Construction=="Modus Ponens" & EvidenceAccess=="Evidence")$Acc -
  subset(acc.test, ProblemType=="Knows-Fact" & Construction=="Modus Tollens" & EvidenceAccess=="Evidence")$Acc
acc.B2 <- subset(acc.test, ProblemType=="Knows-Fact" & Construction=="Modus Ponens" & EvidenceAccess=="noEvidence")$Acc -
  subset(acc.test, ProblemType=="Knows-Fact" & Construction=="Modus Tollens" & EvidenceAccess=="noEvidence")$Acc

acc.A <- acc.A1 - acc.A2
acc.B <- acc.B1 - acc.B2

wilcoxsign_test(acc.A ~ acc.B)
# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg)
# 	 stratified by block
# Z = 1.4496, p-value = 0.1472
# alternative hypothesis: true mu is not equal to 0

cliff.delta(acc.A, acc.B)
# delta estimate: 0.1226451 (negligible)

###########
# Figures #
###########
fig1 <- data.frame()
fig1 <- aggregate(OmniscienceError ~ ParticipantID * EvidenceAccess * Verb, data=data,
                  FUN=function(x){ round(mean(x), digits=2) })
# fig1$VerbLabel <- ifelse(fig1$Verb=="Knows", "\"knows\"", "\"believes\"")
# fig1$VerbLabel <- factor(fig1$VerbLabel, levels= c("\"knows\"", "\"believes\""))
# fig1$VerbLabel <- relevel(fig1$VerbLabel, "\"knows\"")
#fig1$Color <- ifelse(fig1$Verb=="Knows" & fig1$EvidenceAccess=="Evidence", "highlight", "no-highlight")

fig1.plot <- ggplot(fig1, aes(x=EvidenceAccess, y=OmniscienceError)) +
  geom_violin(trim=TRUE, fill='white', adjust=1.8, size=.4) +
  geom_jitter(shape=16, alpha=.3, size=1.2, position=position_jitter(width=0.04, height=0.13)) +
  theme_bw(12) +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width=.3) +
  stat_summary(fun = "mean", geom = "point", size=2) + 
  #scale_colour_manual(values=c("black", "black", "black", "black")) +
  scale_x_discrete(name="", labels=c("Access", "No Access")) +
  scale_y_continuous(name="Omniscience Errors", limits=c(-.3,1.3),
                     breaks=c(0.0, .50, 1.0)) +
  facet_wrap(~ Verb, nrow=1) +
  theme(panel.background=element_rect(fill="transparent",colour=NA),
        plot.background=element_rect(fill="transparent",colour=NA),
        plot.title = element_text(hjust = 0.15, vjust=.4, size=25),
        axis.line = element_line(colour = "black", size= 0.4),
        axis.line.x = element_line(color="black", size = 0.4),
        axis.line.y = element_line(color="black", size = 0.4),
        axis.text.y = element_text(color="black"),
        axis.text.x = element_text(color="black", angle = 0, size=12, hjust = 0.5),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(color="black", size=15, hjust = 0.5),
        panel.border = element_blank())

fig1.plot
