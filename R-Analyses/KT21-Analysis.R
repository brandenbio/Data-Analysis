# --------------------------------------------------------------------------------------------------
# Knowing Thinking Experiment 21 Analysis
# --------------------------------------------------------------------------------------------------
# Subject pool: Amazon Mechanical Turk
# Date(s):      September 2022
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
#  4. Figures
#     4.1)  Average accuracy of each condition
#     4.2)  Average accuracy of problem construction
#     4.3)  Average accuracy of evidence access
#     4.4)  Average accuracy of verb
#     4.5)  Omniscience Errors for each condition
#     4.6)  Omniscience Errors for problem construction
#     4.7)  Omniscience Errors for evidence access
#     4.8)  Omniscience Errors for verb
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

##########
# Setup script
wd <- getwd()
if (wd != rstudioapi::getActiveDocumentContext()$path) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  wd <- getwd()
}
project <- "KT21"
##########

# --------------------------------------------------------------------------------------------------
# X. Preprocessing; filter subjs by attention check; reformatting responses
# --------------------------------------------------------------------------------------------------

rawData <- read.csv(paste("./", project, "-Aggregate.csv", sep=""), header=T)
data.Prelim <- rawData
data.Prelim$ParticipantID <- as.factor(data.Prelim$ParticipantID)
data.Prelim$Answer <- gsub("(\\.).*","\\1",data.Prelim$Answer)
data.Prelim$Answer <- str_to_sentence(data.Prelim$Answer)
data.Prelim$Answer <- str_remove(data.Prelim$Answer, "\\.")
data.Prelim$PropQ  <- tolower(data.Prelim$PropQ)
#data.Prelim$PropQ  <- str_replace(data.Prelim$PropQ, data.Prelim$PropQ, paste("the letter", data.Prelim$PropQ))
fullN <- length(unique(data.Prelim$ParticipantID))

data.Prelim$condC <- ifelse(grepl("then", data.Prelim$Premise1Content, fixed = TRUE), 
                            data.Prelim$Premise1Content, data.Prelim$Premise2Content)
#data.Prelim$locationL <- strsplit(data.Prelim$Premise1Content, "the ")[[1]][2]
#data.Prelim$locationL  <- strsplit(data.Prelim$locationL, " then")[[1]][1]
data.Prelim$locationL <- sapply(strsplit(data.Prelim$Premise1Content, "the "), `[`, 2)
data.Prelim$locationL <- sapply(strsplit(data.Prelim$locationL, " then"), `[`, 1)
#data.Prelim$PropositionP <- gsub(".*then ", "", data.Prelim$condC)
#data.Prelim$PropositionP <- gsub("it's", "It is", data.Prelim$PropositionP)
data.Prelim.AC <- subset(data.Prelim, Cond == "AC")
data.Prelim.AC$correctAC <- "Believes that knows that nothing follows"
data.Prelim.AC$PassedAC <- ifelse(data.Prelim.AC$Answer == data.Prelim.AC$correctAC, 1, 0)
missedAC <- c(as.character(subset(data.Prelim.AC, PassedAC==0)$ParticipantID))
failedAC <- missedAC[duplicated(missedAC)]
data.Prelim$FailedAC <- ifelse(data.Prelim$ParticipantID %in% failedAC, 1, 0)

data.debrief <- subset(data.Prelim, Cond == "DB")

##########
# AC-DB Comparison
ACDB  <-  subset(data.debrief, TrialNumber == 21) %>% select(ParticipantID, FreeResp, Answer, FailedAC)
ACDB2 <-  subset(data.debrief, Cond == "DB") %>% select(ParticipantID, FreeResp, Answer, FailedAC)
##########

data.debrief <- subset(data.debrief, FailedAC==0)
debrief.out  <- data.debrief %>% select(ParticipantID, ProblemNumber, FreeResp, Answer)
debrief.filename <- paste(project, "-debrief-resps.csv", sep="")
if (file.exists(debrief.filename) == FALSE) {
  write.csv(debrief.out, debrief.filename, row.names = FALSE)
}

# Substantive responses from subjects that aren't nonsense
responseOptions <- list(`Q`                          = "The password is Q",
                        `not Q`                      = "The password is not Q",
                        `X believes Q`               = "X believes that the password is Q",
                        `X believes not Q`           = "X believes that the password is not Q",
                        `X knows Q`                  = "X knows that the password is Q",
                        `X knows not Q`              = "X knows that the password is not Q",
                        `P`                          = "P is in the L",
                        `not P`                      = "P is not in the L",
                        `X believes P`               = "X believes that P is in the L",
                        `X believes not P`           = "X believes that P is not in the L",
                        `X knows P`                  = "X knows that P is in the L",
                        `X knows not P`              = "X knows that P is not in the L",
                        `X knows nothing follows`    = "X knows that nothing follows",
                        `X believes nothing follows` = "X believes that nothing follows",
                        `Nothing follows`            = "Nothing follows")

data.Prelim$ResponseFormat <- str_replace(data.Prelim$Answer, regex(data.Prelim$IndividX, ignore_case = TRUE), "X")
data.Prelim$ResponseFormat <- str_replace(data.Prelim$ResponseFormat, regex(data.Prelim$PropQ, ignore_case = TRUE), "Q")
data.Prelim$ResponseFormat <- str_replace(data.Prelim$ResponseFormat, regex(data.Prelim$PropP, ignore_case = TRUE), "P")
data.Prelim$ResponseFormat <- str_replace(data.Prelim$ResponseFormat, regex(data.Prelim$locationL, ignore_case = TRUE), "L")

data.Prelim$AnswerType <- ifelse(data.Prelim$ResponseFormat %in% responseOptions,
                                 names(responseOptions)[match(data.Prelim$ResponseFormat, responseOptions)], 
                                 "Nonsense")

data.probs   <- subset(data.Prelim, Cond != "DB")
data         <- subset(data.probs, FailedAC==0)
data         <- subset(data, Cond != "AC")
useableSubs  <- unique(data$ParticipantID)
finalN       <- length(unique(data$ParticipantID))

excl.data    <- subset(data.probs, FailedAC==1)
excl.data    <- subset(excl.data, Cond != "AC")
excludedN    <- length(unique(excl.data$ParticipantID))
subjLoss     <- round(1 - (finalN / fullN),2)

ansResps.data <- data %>% select(ParticipantID, Answer, ResponseFormat, AnswerType)
ansResps.data.nonsense <- subset(ansResps.data, AnswerType == "Nonsense")

ansResps.excl.data <- excl.data %>% select(ParticipantID, Answer, ResponseFormat, AnswerType)
ansResps.excl.data.nonsense <- subset(ansResps.excl.data, AnswerType == "Nonsense")

data$Verb <- as.factor(str_remove(data$ProblemType, "-Fact"))

# Error of omniscience rubric
# Error of omniscience occurs whenever an answer contains 1 or more epistemic verbs

# Assign error of omniscience scores
data$OmniscienceError <- ifelse(grepl("believes", data$AnswerType), 1, 
                                    ifelse(grepl("knows", data$AnswerType), 1, 0))
data$OmniscienceError <- ifelse(grepl("that nothing follows", data$AnswerType), 0, data$OmniscienceError)
data <- transform(data, OmniscienceError = as.numeric(OmniscienceError))


# Accuracy Rubric
# Cor = 1, Inc = 0
# Modus Ponens                                       # Modus Tollens
# ProblemType   | AnswerType                 | Acc   # ProblemType   | AnswerType                 | Acc
# Believes-Fact | Nonsense                   | Inc   # Believes-Fact | Nonsense                   | Inc
# Believes-Fact | Nothing follows            | Cor   # Believes-Fact | Nothing follows            | Cor
# Believes-Fact | Q                          | Inc   # Believes-Fact | Q                          | Inc
# Believes-Fact | not Q                      | Inc   # Believes-Fact | not Q                      | Cor
# Believes-Fact | X believes Q               | Inc   # Believes-Fact | X believes Q               | Inc
# Believes-Fact | X believes not Q           | Inc   # Believes-Fact | X believes not Q           | Inc
# Believes-Fact | X knows Q                  | Inc   # Believes-Fact | X knows Q                  | Inc
# Believes-Fact | X knows not Q              | Inc   # Believes-Fact | X knows not Q              | Inc
# Believes-Fact | P                          | Cor   # Believes-Fact | P                          | Inc
# Believes-Fact | not P                      | Inc   # Believes-Fact | not P                      | Inc
# Believes-Fact | X believes P               | Inc   # Believes-Fact | X believes P               | Inc
# Believes-Fact | X believes not P           | Inc   # Believes-Fact | X believes not P           | Inc
# Believes-Fact | X knows P                  | Inc   # Believes-Fact | X knows P                  | Inc
# Believes-Fact | X knows not P              | Inc   # Believes-Fact | X knows not P              | Inc
# Believes-Fact | X believes nothing follows | Cor   # Believes-Fact | X believes nothing follows | Cor
# Believes-Fact | X knows nothing follows    | Cor   # Believes-Fact | X knows nothing follows    | Cor
# Modus Ponens                                       # Modus Tollens
# ProblemType   | AnswerType                 | Acc   # ProblemType   | AnswerType                 | Acc
# Knows-Fact    | Nonsense                   | Inc   # Knows-Fact    | Nonsense                   | Inc
# Knows-Fact    | Nothing follows            | Inc   # Knows-Fact    | Nothing follows            | Inc
# Knows-Fact    | Q                          | Cor   # Knows-Fact    | Q                          | Inc
# Knows-Fact    | not Q                      | Inc   # Knows-Fact    | not Q                      | Cor
# Knows-Fact    | X believes Q               | Inc   # Knows-Fact    | X believes Q               | Inc
# Knows-Fact    | X believes not Q           | Inc   # Knows-Fact    | X believes not Q           | Inc
# Knows-Fact    | X knows Q                  | Inc   # Knows-Fact    | X knows Q                  | Inc
# Knows-Fact    | X knows not Q              | Inc   # Knows-Fact    | X knows not Q              | Inc
# Knows-Fact    | P                          | Cor   # Knows-Fact    | P                          | Inc
# Knows-Fact    | not P                      | Inc   # Knows-Fact    | not P                      | Cor
# Knows-Fact    | X believes P               | Inc   # Knows-Fact    | X believes P               | Inc
# Knows-Fact    | X believes not P           | Inc   # Knows-Fact    | X believes not P           | Inc
# Knows-Fact    | X knows P                  | Inc   # Knows-Fact    | X knows P                  | Inc
# Knows-Fact    | X knows not P              | Inc   # Knows-Fact    | X knows not P              | Inc
# Knows-Fact    | X believes nothing follows | Inc   # Knows-Fact    | X believes nothing follows | Inc
# Knows-Fact    | X knows nothing follows    | Inc   # Knows-Fact    | X knows nothing follows    | Inc

# Assign accuracy scores
data$Acc <- 0
# Believes
data$Acc[data$ProblemType == "Believes-Fact" & 
           (data$AnswerType == "Nothing follows" | grepl("nothing follows", data$AnswerType))] <- 1
# Believes + Modus Ponens
data$Acc[data$ProblemType == "Believes-Fact" & data$Construction == "Modus Ponens" & 
           data$AnswerType == "P"] <- 1
# Believes + Modus Tollens
data$Acc[data$ProblemType == "Believes-Fact" & data$Construction == "Modus Tollens" & 
           data$AnswerType == "not Q"] <- 1
# Knows + Modus Ponens
data$Acc[data$ProblemType == "Knows-Fact" & data$Construction == "Modus Ponens" & 
           (data$AnswerType == "P" | data$AnswerType == "Q")] <- 1
# Knows + Modus Tollens
data$Acc[data$ProblemType == "Knows-Fact" & data$Construction == "Modus Tollens" & 
           (data$AnswerType == "not P" | data$AnswerType == "not Q")] <- 1

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
#     63     54         9   36.74     22     71          21        30          0              3

# --------------------------------------------------------------------------------------------------
# 1. Omnibus tables
# --------------------------------------------------------------------------------------------------
responses.table.omni <- group_by(data, ProblemType, Construction, EvidenceAccess) %>% 
  summarise(n=n(), mean=mean(OmniscienceError), sd=sd(OmniscienceError)) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1)) %>%
  arrange(desc(mean))

# Errors of omniscience
#   ProblemType   Construction  EvidenceAccess     n  mean    sd     se     ic
# 1 Believes-Fact Modus Ponens  Evidence         108 0.630 0.485 0.0467 0.0925
# 2 Knows-Fact    Modus Ponens  Evidence         108 0.556 0.499 0.0480 0.0952
# 3 Knows-Fact    Modus Tollens Evidence         108 0.509 0.502 0.0483 0.0958
# 4 Believes-Fact Modus Tollens Evidence         108 0.463 0.501 0.0482 0.0956
# 5 Believes-Fact Modus Ponens  noEvidence       108 0.407 0.494 0.0475 0.0942
# 6 Believes-Fact Modus Tollens noEvidence       108 0.361 0.483 0.0464 0.0921
# 7 Knows-Fact    Modus Tollens noEvidence       108 0.315 0.467 0.0449 0.0890
# 8 Knows-Fact    Modus Ponens  noEvidence       108 0.241 0.430 0.0413 0.0819

responses.table.acc <- group_by(data, ProblemType, Construction, EvidenceAccess) %>% 
  summarise(n=n(), mean=mean(Acc), sd=sd(Acc)) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1)) %>%
  arrange(desc(mean))

# Accuracy
#   ProblemType   Construction  EvidenceAccess     n   mean    sd     se     ic
# 1 Knows-Fact    Modus Ponens  noEvidence       108 0.491  0.502 0.0483 0.0958
# 2 Knows-Fact    Modus Tollens noEvidence       108 0.435  0.498 0.0479 0.0950
# 3 Knows-Fact    Modus Tollens Evidence         108 0.380  0.488 0.0469 0.0930
# 4 Believes-Fact Modus Tollens noEvidence       108 0.352  0.480 0.0462 0.0915
# 5 Knows-Fact    Modus Ponens  Evidence         108 0.352  0.480 0.0462 0.0915
# 6 Believes-Fact Modus Ponens  noEvidence       108 0.278  0.450 0.0433 0.0858
# 7 Believes-Fact Modus Tollens Evidence         108 0.213  0.411 0.0396 0.0785
# 8 Believes-Fact Modus Ponens  Evidence         108 0.0463 0.211 0.0203 0.0403

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
# 1   Modus Ponens                AnswerType_Q 0.312500000
# 2   Modus Ponens        AnswerType_X knows Q 0.224537037
# 3   Modus Ponens     AnswerType_X believes Q 0.206018519
# 4   Modus Ponens  AnswerType_Nothing follows 0.106481481
# 5   Modus Ponens         AnswerType_Nonsense 0.085648148
# 6   Modus Ponens                AnswerType_P 0.032407407
# 7   Modus Ponens        AnswerType_X knows P 0.023148148
# 8   Modus Ponens            AnswerType_not Q 0.004629630
# 9   Modus Ponens     AnswerType_X believes P 0.002314815
# 10  Modus Ponens    AnswerType_X knows not Q 0.002314815
# 11  Modus Ponens            AnswerType_not P 0.000000000
# 12  Modus Ponens AnswerType_X believes not P 0.000000000
# 13  Modus Ponens AnswerType_X believes not Q 0.000000000
# 14  Modus Ponens    AnswerType_X knows not P 0.000000000
# 15 Modus Tollens            AnswerType_not P 0.291666667
# 16 Modus Tollens    AnswerType_X knows not P 0.180555556
# 17 Modus Tollens  AnswerType_Nothing follows 0.171296296
# 18 Modus Tollens AnswerType_X believes not P 0.106481481
# 19 Modus Tollens         AnswerType_Nonsense 0.067129630
# 20 Modus Tollens            AnswerType_not Q 0.041666667
# 21 Modus Tollens     AnswerType_X believes Q 0.039351852
# 22 Modus Tollens    AnswerType_X knows not Q 0.030092593
# 23 Modus Tollens AnswerType_X believes not Q 0.025462963
# 24 Modus Tollens                AnswerType_P 0.016203704
# 25 Modus Tollens     AnswerType_X believes P 0.013888889
# 26 Modus Tollens        AnswerType_X knows Q 0.011574074
# 27 Modus Tollens        AnswerType_X knows P 0.002314815
# 28 Modus Tollens                AnswerType_Q 0.000000000

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
# 1 Believes-Fact            0.465
# 2 Knows-Fact               0.405

B_omniErr <- aggregate(OmniscienceError ~ ParticipantID, data=BF, FUN=mean)
K_omniErr <- aggregate(OmniscienceError ~ ParticipantID, data=KF, FUN=mean)

wilcoxsign_test(B_omniErr$OmniscienceError ~ K_omniErr$OmniscienceError)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = 2.5491, p-value = 0.0108
# alternative hypothesis: true mu is not equal to 0

cliff.delta(B_omniErr$OmniscienceError,K_omniErr$OmniscienceError,conf.level = 0.95)
# delta estimate: 0.09910837 (negligible)

# 2.2) Wilcoxon test comparing errors of omniscience - Construction
data %>% group_by(Construction) %>% summarise(OmniscienceError = mean(OmniscienceError))
# Construction    OmniscienceError
# 1 Modus Ponens             0.458
# 2 Modus Tollens            0.412

MP_omniErr <- aggregate(OmniscienceError ~ ParticipantID, data=MP, FUN=mean)
MT_omniErr <- aggregate(OmniscienceError ~ ParticipantID, data=MT, FUN=mean)

wilcoxsign_test(MP_omniErr$OmniscienceError ~ MT_omniErr$OmniscienceError)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = 0.96017, p-value = 0.337
# alternative hypothesis: true mu is not equal to 0

cliff.delta(MP_omniErr$OmniscienceError,MT_omniErr$OmniscienceError ,conf.level = 0.95)
# delta estimate: 0.07133059 (negligible)

# 2.3) Wilcoxon test comparing errors of omniscience - EvidenceAccess
data %>% group_by(EvidenceAccess) %>% summarise(OmniscienceError = mean(OmniscienceError))
# EvidenceAccess    OmniscienceError
# 1 Evidence                  0.539
# 2 noEvidence                0.331

Ev_omniErr <- aggregate(OmniscienceError ~ ParticipantID, data=Ev, FUN=mean)
noEv_omniErr <- aggregate(OmniscienceError ~ ParticipantID, data=noEv, FUN=mean)

wilcoxsign_test(Ev_omniErr$OmniscienceError ~ noEv_omniErr$OmniscienceError)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = 3.9444, p-value = 8.001e-05
# alternative hypothesis: true mu is not equal to 0

cliff.delta(Ev_omniErr$OmniscienceError,noEv_omniErr$OmniscienceError ,conf.level = 0.95)
# delta estimate: 0.2880658 (small)

# 2.4) Wilcoxon test on errors of omniscience for interaction between ProblemType and Construction
data %>% group_by(ProblemType, Construction) %>% summarise(OmniscienceError = mean(OmniscienceError))
#   ProblemType   Construction  OmniscienceError
# 1 Believes-Fact Modus Ponens             0.519
# 2 Believes-Fact Modus Tollens            0.412
# 3 Knows-Fact    Modus Ponens             0.398
# 4 Knows-Fact    Modus Tollens            0.412

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
# Z = 2.4874, p-value = 0.01287
# alternative hypothesis: true mu is not equal to 0

cliff.delta(Prob_MP_Omni, Prob_MT_Omni, conf.level = 0.95)
# delta estimate: 0.2969822 (small)

# 2.5) Wilcoxon test on errors of omniscience for interaction between Construction and Evidence
data %>% group_by(Construction, EvidenceAccess) %>% summarise(OmniscienceError = mean(OmniscienceError))
# Construction    EvidenceAccess OmniscienceError
# 1 Modus Ponens  Evidence                  0.593
# 2 Modus Ponens  noEvidence                0.324
# 3 Modus Tollens Evidence                  0.486
# 4 Modus Tollens noEvidence                0.338

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
# Z = 1.8939, p-value = 0.05824
# alternative hypothesis: true mu is not equal to 0

cliff.delta(Con_Ev_Omni, Con_noEv_Omni, conf.level = 0.95)
# delta estimate: 0.2047325 (small)

# 2.6) Wilcoxon test on errors of omniscience for interaction between ProblemType and Evidence
data %>% group_by(ProblemType, EvidenceAccess) %>% summarise(OmniscienceError = mean(OmniscienceError))
#   ProblemType   EvidenceAccess OmniscienceError
# 1 Believes-Fact Evidence                  0.546
# 2 Believes-Fact noEvidence                0.384
# 3 Knows-Fact    Evidence                  0.532
# 4 Knows-Fact    noEvidence                0.278

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
# Z = -2.2472, p-value = 0.02463
# alternative hypothesis: true mu is not equal to 0

cliff.delta(Prob_Ev_Omni, Prob_noEv_Omni, conf.level = 0.95)
# delta estimate: -0.2692044 (small)

# 2.7) Wilcoxon test on errors of omniscience for three-way interaction
data %>% group_by(ProblemType, Construction, EvidenceAccess) %>% summarise(OmniscienceError = mean(OmniscienceError))
#   ProblemType   Construction  EvidenceAccess OmniscienceError
# 1 Believes-Fact Modus Ponens  Evidence                  0.630
# 2 Believes-Fact Modus Ponens  noEvidence                0.407
# 3 Believes-Fact Modus Tollens Evidence                  0.463
# 4 Believes-Fact Modus Tollens noEvidence                0.361
# 5 Knows-Fact    Modus Ponens  Evidence                  0.556
# 6 Knows-Fact    Modus Ponens  noEvidence                0.241
# 7 Knows-Fact    Modus Tollens Evidence                  0.509
# 8 Knows-Fact    Modus Tollens noEvidence                0.315

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
# Z = 0.36498, p-value = 0.7151
# alternative hypothesis: true mu is not equal to 0

cliff.delta(omni.A, omni.B)
# delta estimate: 0.0003429355 (negligible)

# 2.8) Wilcoxon test comparing omniscience errors against 0
Z_omniErr <- Ev_omniErr
Z_omniErr$OmniscienceError <- Z_omniErr$OmniscienceError * 0

# For access
wilcoxsign_test(Ev_omniErr$OmniscienceError ~ Z_omniErr$OmniscienceError)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = 6.0579, p-value = 1.379e-09
# alternative hypothesis: true mu is not equal to 0

cliff.delta(Ev_omniErr$OmniscienceError, Z_omniErr$OmniscienceError, conf.level = 0.95)
# Cliff's Delta
# delta estimate: 0.7592593 (large)
# 95 percent confidence interval:
#     lower     upper 
# 0.6187061 0.8527157

# For no access
wilcoxsign_test(noEv_omniErr$OmniscienceError ~ Z_omniErr$OmniscienceError)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = 5.5757, p-value = 2.465e-08
# alternative hypothesis: true mu is not equal to 0

cliff.delta(noEv_omniErr$OmniscienceError, Z_omniErr$OmniscienceError, conf.level = 0.95)
# Cliff's Delta
# delta estimate: 0.6111111 (large)
# 95 percent confidence interval:
#   lower     upper 
# 0.4623950 0.7263854 

# --------------------------------------------------------------------------------------------------
# 3. Accuracy
# --------------------------------------------------------------------------------------------------

# 3.1) Wilcoxon test comparing accuracy in ProblemType
data %>% group_by(ProblemType) %>% summarise(Acc = mean(Acc))
# ProblemType     Acc
# 1 Believes-Fact 0.222
# 2 Knows-Fact    0.414

B_acc <- aggregate(Acc ~ ParticipantID, data=BF, FUN=mean)
K_acc <- aggregate(Acc ~ ParticipantID, data=KF, FUN=mean)

wilcoxsign_test(B_acc$Acc ~ K_acc$Acc)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = -2.5116, p-value = 0.01202
# alternative hypothesis: true mu is not equal to 0

cliff.delta(B_acc$Acc,K_acc$Acc,conf.level = 0.95)
# delta estimate: -0.2270233 (small)

# 3.2) Wilcoxon test comparing accuracy in Construction
data %>% group_by(Construction) %>% summarise(Acc = mean(Acc))
# Construction    Acc
# 1 Modus Ponens  0.292
# 2 Modus Tollens 0.345

MP_acc <- aggregate(Acc ~ ParticipantID, data=MP, FUN=mean)
MT_acc <- aggregate(Acc ~ ParticipantID, data=MT, FUN=mean)

wilcoxsign_test(MP_acc$Acc ~ MT_acc$Acc)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = -1.5951, p-value = 0.1107
# alternative hypothesis: true mu is not equal to 0

cliff.delta(MP_acc$Acc,MT_acc$Acc,conf.level = 0.95)
# delta estimate: -0.09499314 (negligible)

# 3.3) Wilcoxon test comparing accuracy in EvidenceAccess
data %>% group_by(EvidenceAccess) %>% summarise(Acc = mean(Acc))
# EvidenceAccess   Acc
# 1 Evidence       0.248
# 2 noEvidence     0.389

Ev_acc   <- aggregate(Acc ~ ParticipantID, data=Ev, FUN=mean)
noEv_acc <- aggregate(Acc ~ ParticipantID, data=noEv, FUN=mean)

wilcoxsign_test(Ev_acc$Acc ~ noEv_acc$Acc)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = -4.0209, p-value = 5.797e-05
# alternative hypothesis: true mu is not equal to 0

cliff.delta(Ev_acc$Acc,noEv_acc$Acc,conf.level = 0.95)
# delta estimate: -0.2486283 (small)

# 3.4) Wilcoxon test on accuracy for interaction between ProblemType and Construction
data %>% group_by(ProblemType, Construction) %>% summarise(Acc = mean(Acc))
# ProblemType     Construction    Acc
# 1 Believes-Fact Modus Ponens  0.162
# 2 Believes-Fact Modus Tollens 0.282
# 3 Knows-Fact    Modus Ponens  0.421
# 4 Knows-Fact    Modus Tollens 0.407

ProbxCon_Acc <- data %>% group_by(ParticipantID, ProblemType, Construction) %>% summarise(Acc = mean(Acc))

Prob_MP_Acc <- subset(ProbxCon_Acc, ProblemType=="Believes-Fact" & Construction=="Modus Ponens")$Acc - 
  subset(ProbxCon_Acc, ProblemType=="Knows-Fact" & Construction=="Modus Ponens")$Acc
Prob_MT_Acc <- subset(ProbxCon_Acc, ProblemType=="Believes-Fact" & Construction=="Modus Tollens")$Acc - 
  subset(ProbxCon_Acc, ProblemType=="Knows-Fact" & Construction=="Modus Tollens")$Acc

wilcoxsign_test(Prob_MP_Acc ~ Prob_MT_Acc)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = -2.6683, p-value = 0.007622
# alternative hypothesis: true mu is not equal to 0

cliff.delta(Prob_MP_Acc, Prob_MT_Acc, conf.level = 0.95)
# delta estimate: -0.138546 (negligible)

# 3.5) Wilcoxon test on accuracy for interaction between Construction and Evidence
data %>% group_by(Construction, EvidenceAccess) %>% summarise(Acc = mean(Acc))
# Construction    EvidenceAccess    Acc
# 1 Modus Ponens  Evidence       0.199
# 2 Modus Ponens  noEvidence     0.384
# 3 Modus Tollens Evidence       0.296
# 4 Modus Tollens noEvidence     0.394

ConxEv_Acc <- data %>% group_by(ParticipantID, Construction, EvidenceAccess) %>% summarise(Acc = mean(Acc))

Con_Ev_Acc <- subset(ConxEv_Acc, Construction=="Modus Ponens" & EvidenceAccess=="Evidence")$Acc - 
  subset(ConxEv_Acc, Construction=="Modus Tollens" & EvidenceAccess=="Evidence")$Acc
Con_noEv_Acc <- subset(ConxEv_Acc, Construction=="Modus Ponens" & EvidenceAccess=="noEvidence")$Acc - 
  subset(ConxEv_Acc, Construction=="Modus Tollens" & EvidenceAccess=="noEvidence")$Acc

wilcoxsign_test(Con_Ev_Acc ~ Con_noEv_Acc)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = -1.6024, p-value = 0.1091
# alternative hypothesis: true mu is not equal to 0

cliff.delta(Con_Ev_Acc, Con_noEv_Acc, conf.level = 0.95)
# delta estimate: -0.1611797 (small)

# 3.6) Wilcoxon test on accuracy for interaction between ProblemType and Evidence
data %>% group_by(ProblemType, EvidenceAccess) %>% summarise(Acc = mean(Acc))
# ProblemType   EvidenceAccess   Acc
# 1 Believes-Fact Evidence       0.130
# 2 Believes-Fact noEvidence     0.315
# 3 Knows-Fact    Evidence       0.366
# 4 Knows-Fact    noEvidence     0.463

ProbxEv_Acc <- data %>% group_by(ParticipantID, ProblemType, EvidenceAccess) %>% summarise(Acc = mean(Acc))

Prob_Ev_Acc <- subset(ProbxEv_Acc, ProblemType=="Believes-Fact" & EvidenceAccess=="Evidence")$Acc - 
  subset(ProbxEv_Acc, ProblemType=="Knows-Fact" & EvidenceAccess=="Evidence")$Acc
Prob_noEv_Acc <- subset(ProbxEv_Acc, ProblemType=="Believes-Fact" & EvidenceAccess=="noEvidence")$Acc - 
  subset(ProbxEv_Acc, ProblemType=="Knows-Fact" & EvidenceAccess=="noEvidence")$Acc

wilcoxsign_test(Prob_Ev_Acc ~ Prob_noEv_Acc)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = -1.9775, p-value = 0.04799
# alternative hypothesis: true mu is not equal to 0

cliff.delta(Prob_Ev_Acc, Prob_noEv_Acc, conf.level = 0.95)
# delta estimate: -0.1059671 (negligible)

# 3.7) Wilcoxon test on accuracy for three-way interaction
data %>% group_by(ProblemType, Construction, EvidenceAccess) %>% summarise(Acc = mean(Acc))
#   ProblemType   Construction  EvidenceAccess    Acc
# 1 Believes-Fact Modus Ponens  Evidence       0.0463
# 2 Believes-Fact Modus Ponens  noEvidence     0.278 
# 3 Believes-Fact Modus Tollens Evidence       0.213 
# 4 Believes-Fact Modus Tollens noEvidence     0.352 
# 5 Knows-Fact    Modus Ponens  Evidence       0.352 
# 6 Knows-Fact    Modus Ponens  noEvidence     0.491 
# 7 Knows-Fact    Modus Tollens Evidence       0.380 
# 8 Knows-Fact    Modus Tollens noEvidence     0.435 

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
# Z = 0.18228, p-value = 0.8554
# alternative hypothesis: true mu is not equal to 0

cliff.delta(acc.A, acc.B)
# delta estimate: -0.01097394 (negligible)

# --------------------------------------------------------------------------------------------------
# 4. Figures
# --------------------------------------------------------------------------------------------------

# Acc
responses.Acc <- group_by(data, Cond) %>% 
  summarise( 
    n=n(),
    mean=mean(Acc),
    sd=sd(Acc)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

resp.Acc <- responses.Acc[,3:6]*100

responses.Acc <- cbind(responses.Acc[1:2], resp.Acc)

responses.Acc2 <- group_by(data, Construction) %>% 
  summarise( 
    n=n(),
    mean=mean(Acc),
    sd=sd(Acc)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

resp.Acc2 <- responses.Acc2[,3:6]*100

responses.Acc2 <- cbind(responses.Acc2[1:2], resp.Acc2)

responses.Acc3 <- group_by(data, EvidenceAccess) %>% 
  summarise( 
    n=n(),
    mean=mean(Acc),
    sd=sd(Acc)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

resp.Acc3 <- responses.Acc3[,3:6]*100

responses.Acc3 <- cbind(responses.Acc3[1:2], resp.Acc3)

responses.Acc4 <- group_by(data, ProblemType) %>% 
  summarise( 
    n=n(),
    mean=mean(Acc),
    sd=sd(Acc)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

resp.Acc4 <- responses.Acc4[,3:6]*100

responses.Acc4 <- cbind(responses.Acc4[1:2], resp.Acc4)

# Omni
responses.Omni <- group_by(data, Cond) %>% 
  summarise( 
    n=n(),
    mean=mean(OmniscienceError),
    sd=sd(OmniscienceError)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

resp.Omni <- responses.Omni[,3:6]*100

responses.Omni <- cbind(responses.Omni[1:2], resp.Omni)

responses.Omni2 <- group_by(data, Construction) %>% 
  summarise( 
    n=n(),
    mean=mean(OmniscienceError),
    sd=sd(OmniscienceError)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

resp.Omni2 <- responses.Omni2[,3:6]*100

responses.Omni2 <- cbind(responses.Omni2[1:2], resp.Omni2)

responses.Omni3 <- group_by(data, EvidenceAccess) %>% 
  summarise( 
    n=n(),
    mean=mean(OmniscienceError),
    sd=sd(OmniscienceError)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

resp.Omni3 <- responses.Omni3[,3:6]*100

responses.Omni3 <- cbind(responses.Omni3[1:2], resp.Omni3)

responses.Omni4 <- group_by(data, ProblemType) %>% 
  summarise( 
    n=n(),
    mean=mean(OmniscienceError),
    sd=sd(OmniscienceError)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

resp.Omni4 <- responses.Omni4[,3:6]*100

responses.Omni4 <- cbind(responses.Omni4[1:2], resp.Omni4)

responses.Omni5 <- group_by(data, EvidenceAccess, ProblemType) %>% 
  summarise( 
    n=n(),
    mean=mean(OmniscienceError),
    sd=sd(OmniscienceError)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

resp.Omni5 <- responses.Omni5[,4:7]*100

responses.Omni5 <- cbind(responses.Omni5[1:3], resp.Omni5)

# 4.1)  Average accuracy of each condition
cond.barplot <- ggplot(responses.Acc, aes(y=mean, x=Cond)) +
  geom_bar(stat="identity", position=position_dodge(), fill="darkorange2", size=.5) +
  geom_errorbar(aes(x=Cond, ymin=mean-ic, ymax=mean+ic), 
                position=position_dodge(width=0.9), width=0.4, alpha=0.9, size=.5) +
  scale_y_continuous(name = "Accuracy", limits = c(0, 105), breaks = c(0, 50, 100)) +
  scale_x_discrete(name = "", labels=c("BF_MP_Ev", "BF_MP_NoEv", "BF_MT_Ev", "BF_MT_NoEv", "KF_MP_Ev", "KF_MP_NoEv", "KF_MT_Ev", 
  "KF_MT_NoEv")) +
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position=c(0.26, 0.72),
        legend.key.size = unit(.5, 'cm'),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(size = 20))

cond.barplot

# 4.2)  Average accuracy of problem construction
constr.barplot <- ggplot(responses.Acc2, aes(y=mean, x=Construction)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue4", size=.5) +
  geom_errorbar(aes(x=Construction, ymin=mean-ic, ymax=mean+ic), 
                position=position_dodge(width=0.9), width=0.4, alpha=0.9, size=.5) +
  scale_y_continuous(name = "Accuracy", limits = c(0, 105), breaks = c(0, 50, 100)) +
  scale_x_discrete(name = "", labels=c("Modus Ponens", "Modus Tollens")) +
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position=c(0.26, 0.72),
        legend.key.size = unit(.5, 'cm'),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(size = 20))

constr.barplot

# 4.3)  Average accuracy of evidence access
ev.barplot <- ggplot(responses.Acc3, aes(y=mean, x=EvidenceAccess)) +
  geom_bar(stat="identity", position=position_dodge(), fill="darkseagreen4", size=.5) +
  geom_errorbar(aes(x=EvidenceAccess, ymin=mean-ic, ymax=mean+ic), 
                position=position_dodge(width=0.9), width=0.4, alpha=0.9, size=.5) +
  scale_y_continuous(name = "Accuracy", limits = c(0, 105), breaks = c(0, 50, 100)) +
  scale_x_discrete(name = "", labels=c("Evidence", "No Evidence")) +
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position=c(0.26, 0.72),
        legend.key.size = unit(.5, 'cm'),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(size = 20))

ev.barplot

# 4.4)  Average accuracy of verb
verb.barplot <- ggplot(responses.Acc4, aes(y=mean, x=ProblemType)) +
  geom_bar(stat="identity", position=position_dodge(), fill="darkred", size=.5) +
  geom_errorbar(aes(x=ProblemType, ymin=mean-ic, ymax=mean+ic), 
                position=position_dodge(width=0.9), width=0.4, alpha=0.9, size=.5) +
  scale_y_continuous(name = "Accuracy", limits = c(0, 105), breaks = c(0, 50, 100)) +
  scale_x_discrete(name = "", labels=c("Believes", "Knows")) +
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position=c(0.26, 0.72),
        legend.key.size = unit(.5, 'cm'),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(size = 20))

verb.barplot

# 4.5)  Omniscience Errors for each condition
cond.omni.barplot <- ggplot(responses.Omni, aes(y=mean, x=Cond)) +
  geom_bar(stat="identity", position=position_dodge(), fill="darkorange2", size=.5) +
  geom_errorbar(aes(x=Cond, ymin=mean-ic, ymax=mean+ic), 
                position=position_dodge(width=0.9), width=0.4, alpha=0.9, size=.5) +
  scale_y_continuous(name = "Omniscience Errors", limits = c(0, 105), breaks = c(0, 50, 100)) +
  scale_x_discrete(name = "", labels=c("BF_MP_Ev", "BF_MP_NoEv", "BF_MT_Ev", "BF_MT_NoEv", "KF_MP_Ev", "KF_MP_NoEv", "KF_MT_Ev", 
                                       "KF_MT_NoEv")) +
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position=c(0.26, 0.72),
        legend.key.size = unit(.5, 'cm'),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(size = 20))

cond.omni.barplot

# 4.6)  Omniscience Errors for problem construction
constr.omni.barplot <- ggplot(responses.Omni2, aes(y=mean, x=Construction)) +
  geom_bar(stat="identity", position=position_dodge(), fill="darkolivegreen3", size=.5, width=.75) +
  geom_errorbar(aes(x=Construction, ymin=mean-ic, ymax=mean+ic), 
                position=position_dodge(width=0.9), width=0.2, alpha=0.9, size=.5) +
  scale_y_continuous(name = "Omniscience Errors", limits = c(0, 105), breaks = c(0, 50, 100)) +
  scale_x_discrete(name = "", labels=c("Modus Ponens", "Modus Tollens")) +
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position=c(0.26, 0.72),
        legend.key.size = unit(.5, 'cm'),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(size = 20))

constr.omni.barplot

# 4.7)  Omniscience Errors for evidence access
ev.omni.barplot <- ggplot(responses.Omni3, aes(y=mean, x=EvidenceAccess)) +
  geom_bar(stat="identity", position=position_dodge(), fill="darkolivegreen3", size=.5, width=.75) +
  geom_errorbar(aes(x=EvidenceAccess, ymin=mean-ic, ymax=mean+ic), 
                position=position_dodge(width=0.9), width=0.2, alpha=0.9, size=.5) +
  scale_y_continuous(name = "Omniscience Errors", limits = c(0, 105), breaks = c(0, 50, 100)) +
  scale_x_discrete(name = "", labels=c("Evidence", "No Evidence")) +
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position=c(0.26, 0.72),
        legend.key.size = unit(.5, 'cm'),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(size = 20))

ev.omni.barplot

# 4.8)  Omniscience Errors for verb
verb.omni.barplot <- ggplot(responses.Omni4, aes(y=mean, x=ProblemType)) +
  geom_bar(stat="identity", position=position_dodge(), fill="darkolivegreen3", size=.5, width=.75) +
  geom_errorbar(aes(x=ProblemType, ymin=mean-ic, ymax=mean+ic), 
                position=position_dodge(width=0.9), width=0.2, alpha=0.9, size=.5) +
  scale_y_continuous(name = "Omniscience Errors", limits = c(0, 105), breaks = c(0, 50, 100)) +
  scale_x_discrete(name = "", labels=c("Believes", "Knows")) +
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position=c(0.26, 0.72),
        legend.key.size = unit(.5, 'cm'),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(size = 20))

verb.omni.barplot

# 4.9)  Omniscience Errors for evidence access by verb
EvxVerb.omni.barplot <- ggplot(responses.Omni5, aes(y=mean, x=EvidenceAccess)) +
  geom_bar(stat="identity", position=position_dodge(), fill="dodgerblue4", size=.5, width=0.5) +
  geom_errorbar(aes(x=EvidenceAccess, ymin=mean-ic, ymax=mean+ic), 
                position=position_dodge(width=0.9), width=0.1, alpha=0.9, size=.5) +
  scale_y_continuous(name = "Omniscience Errors", limits = c(0, 105), breaks = c(0, 50, 100)) +
  scale_x_discrete(name = "", labels=c("Evidence", "No Evidence")) +
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position=c(0.26, 0.72),
        legend.key.size = unit(.5, 'cm'),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(size = 20)) + 
  facet_wrap(~ ProblemType)

EvxVerb.omni.barplot

# Response type barplot
sums <- colSums(data[29:30])
sums <- round((sums/length(data$OmniscienceError))*100)
resp_sum_table <- data.frame(Column = names(sums), N = sums)
rownames(resp_sum_table) <- NULL

resps.barplot <- ggplot(resp_sum_table, aes(y=N, x=reorder(Column, -N))) +
  geom_bar(stat="identity", position=position_dodge(), fill="darkolivegreen3", size=.5, width=.75) +
  scale_y_continuous(name = "Response Proportion", limits = c(0, 105), breaks = c(0, 50, 100)) +
  scale_x_discrete(name = "", labels=c("Omniscience Errors", "Correct Responses")) +
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position=c(0.26, 0.72),
        legend.key.size = unit(.5, 'cm'),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(size = 20))

resps.barplot

# Violin plot
fig3 <- aggregate(OmniscienceError ~ ParticipantID * EvidenceAccess * Verb, data=data,
                  FUN=function(x){ round(mean(x), digits=2) })

fig3.vPlot <- ggplot(fig3, aes(x=EvidenceAccess, y=OmniscienceError)) +
  geom_violin(trim=TRUE, fill='white', adjust=1.8, size=.4) +
  geom_jitter(shape=16, alpha=.3, size=1.2, position=position_jitter(width=0.04, height=0.13)) +
  theme_bw(12) +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width=.1) +
  stat_summary(fun = "mean", geom = "point", size=2) +
  scale_x_discrete(name="", labels=c("Evidence", "No Evidence")) +
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
        axis.text.x = element_text(color="black", angle = 0, size=18, hjust = 0.5),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(color="black", size=20, hjust = 0.5),
        panel.border = element_blank(),
        text = element_text(size = 20))

fig3.vPlot
