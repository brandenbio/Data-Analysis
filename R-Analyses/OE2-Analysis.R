# --------------------------------------------------------------------------------------------------
# Omniscience Errors Experiment 2 Analysis
# --------------------------------------------------------------------------------------------------
# Subject pool: Amazon Mechanical Turk
# Date(s):      September 2023
# Design:       Branden Bio & Sunny Khemlani
# Code:         Branden Bio
# Experimenter:	Knexus Research Corporation
# --------------------------------------------------------------------------------------------------
# Contents:
#  X. Preprocessing; filter subjs by attention check; reformatting responses
#  X. Demographics summary
#  1. Omnibus table
#  2. Errors of Omniscience
#     2.1)  Wilcoxon test on errors of omniscience in ProblemType
#     2.2)  Wilcoxon test on errors of omniscience in Construction
#     2.3)  Wilcoxon test on errors of omniscience in EvidenceAccess
#     2.4)  Wilcoxon test on errors of omniscience for interaction between ProblemType and Construction
#     2.5)  Wilcoxon test on errors of omniscience for interaction between Construction and Evidence
#     2.6)  Wilcoxon test on errors of omniscience for interaction between ProblemType and Evidence
#     2.7)  Wilcoxon test on errors of omniscience for three-way interaction
#     2.8)  Wilcoxon test on errors of omniscience in EvidenceAccess against 0
#  3. Figures
#     3.1)  Bar graph of omniscience errors for construction
#     3.2)  Bar graph of omniscience errors for evidence access
#     3.3)  Bar graph of omniscience errors for ProblemType
#     3.4)  Violin plot of omniscience errors for interaction between ProblemType and Evidence
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
project <- "OE2"
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
fullN <- length(unique(data.Prelim$ParticipantID))

data.Prelim$condC <- ifelse(grepl("then", data.Prelim$Premise1Content, fixed = TRUE), 
                            data.Prelim$Premise1Content, data.Prelim$Premise2Content)
data.Prelim$locationL <- sapply(strsplit(data.Prelim$Premise1Content, "the "), `[`, 2)
data.Prelim$locationL <- sapply(strsplit(data.Prelim$locationL, " then"), `[`, 1)
data.Prelim.AC <- subset(data.Prelim, Cond == "AC")
data.Prelim.AC$correctAC <- "Believes that knows that nothing follows"
data.Prelim.AC$PassedAC <- ifelse(data.Prelim.AC$Answer == data.Prelim.AC$correctAC, 1, 0)
missedAC <- c(as.character(subset(data.Prelim.AC, PassedAC==0)$ParticipantID))
failedAC <- missedAC[duplicated(missedAC)]
data.Prelim$FailedAC <- ifelse(data.Prelim$ParticipantID %in% failedAC, 1, 0)

##########
# AC-DB Comparison
data.debrief <- subset(data.Prelim, Cond == "DB")
AC.DB.Comparison1  <-  subset(data.debrief, TrialNumber == 21) %>% select(ParticipantID, FreeResp, Answer, FailedAC)
AC.DB.Comparison2 <-  subset(data.debrief, Cond == "DB") %>% select(ParticipantID, FreeResp, Answer, FailedAC)
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

data         <- subset(data.Prelim, Cond != "AC" & Cond != "DB" & FailedAC==0)
useableSubs  <- unique(data$ParticipantID)
finalN       <- length(unique(data$ParticipantID))

excluded.data    <- subset(data.Prelim, Cond != "DB" & FailedAC==1)
excluded.data    <- subset(excluded.data, Cond != "AC")
excludedN    <- length(unique(excluded.data$ParticipantID))
subjLoss     <- round(1 - (finalN / fullN),2)

data$Verb <- as.factor(data$ProblemType)

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
# ProblemType| AnswerType                 | Acc   # ProblemType| AnswerType                 | Acc
# nonFactive | Nonsense                   | Inc   # nonFactive | Nonsense                   | Inc
# nonFactive | Nothing follows            | Cor   # nonFactive | Nothing follows            | Cor
# nonFactive | Q                          | Inc   # nonFactive | Q                          | Inc
# nonFactive | not Q                      | Inc   # nonFactive | not Q                      | Cor
# nonFactive | X believes Q               | Inc   # nonFactive | X believes Q               | Inc
# nonFactive | X believes not Q           | Inc   # nonFactive | X believes not Q           | Inc
# nonFactive | X knows Q                  | Inc   # nonFactive | X knows Q                  | Inc
# nonFactive | X knows not Q              | Inc   # nonFactive | X knows not Q              | Inc
# nonFactive | P                          | Cor   # nonFactive | P                          | Inc
# nonFactive | not P                      | Inc   # nonFactive | not P                      | Inc
# nonFactive | X believes P               | Inc   # nonFactive | X believes P               | Inc
# nonFactive | X believes not P           | Inc   # nonFactive | X believes not P           | Inc
# nonFactive | X knows P                  | Inc   # nonFactive | X knows P                  | Inc
# nonFactive | X knows not P              | Inc   # nonFactive | X knows not P              | Inc
# nonFactive | X believes nothing follows | Cor   # nonFactive | X believes nothing follows | Cor
# nonFactive | X knows nothing follows    | Cor   # nonFactive | X knows nothing follows    | Cor
# Modus Ponens                                       # Modus Tollens
# ProblemType| AnswerType                 | Acc   # ProblemType| AnswerType                 | Acc
# Factive    | Nonsense                   | Inc   # Factive    | Nonsense                   | Inc
# Factive    | Nothing follows            | Inc   # Factive    | Nothing follows            | Inc
# Factive    | Q                          | Cor   # Factive    | Q                          | Inc
# Factive    | not Q                      | Inc   # Factive    | not Q                      | Cor
# Factive    | X believes Q               | Inc   # Factive    | X believes Q               | Inc
# Factive    | X believes not Q           | Inc   # Factive    | X believes not Q           | Inc
# Factive    | X knows Q                  | Inc   # Factive    | X knows Q                  | Inc
# Factive    | X knows not Q              | Inc   # Factive    | X knows not Q              | Inc
# Factive    | P                          | Cor   # Factive    | P                          | Inc
# Factive    | not P                      | Inc   # Factive    | not P                      | Cor
# Factive    | X believes P               | Inc   # Factive    | X believes P               | Inc
# Factive    | X believes not P           | Inc   # Factive    | X believes not P           | Inc
# Factive    | X knows P                  | Inc   # Factive    | X knows P                  | Inc
# Factive    | X knows not P              | Inc   # Factive    | X knows not P              | Inc
# Factive    | X believes nothing follows | Inc   # Factive    | X believes nothing follows | Inc
# Factive    | X knows nothing follows    | Inc   # Factive    | X knows nothing follows    | Inc

# Assign accuracy scores
data$Acc <- 0
# Believes
data$Acc[data$ProblemType == "nonFactive" & 
           (data$AnswerType == "Nothing follows" | grepl("nothing follows", data$AnswerType))] <- 1
# Believes + Modus Ponens
data$Acc[data$ProblemType == "nonFactive" & data$Construction == "Modus Ponens" & 
           data$AnswerType == "P"] <- 1
# Believes + Modus Tollens
data$Acc[data$ProblemType == "nonFactive" & data$Construction == "Modus Tollens" & 
           data$AnswerType == "not Q"] <- 1
# Knows + Modus Ponens
data$Acc[data$ProblemType == "Factive" & data$Construction == "Modus Ponens" & 
           (data$AnswerType == "P" | data$AnswerType == "Q")] <- 1
# Knows + Modus Tollens
data$Acc[data$ProblemType == "Factive" & data$Construction == "Modus Tollens" & 
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

demoSummary(rawData$ParticipantID)

#   fullN finalN excludedN meanAge minAge maxAge femaleCount maleCount otherCount preferNotCount
# 1    58     43        15   39.05     20     67          26        31          0              1

# --------------------------------------------------------------------------------------------------
# 1. Omnibus table
# --------------------------------------------------------------------------------------------------
omnibus.table <- group_by(data, ProblemType, Construction, EvidenceAccess) %>% 
  summarise(n=n(), mean=mean(OmniscienceError), sd=sd(OmniscienceError)) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1)) %>%
  arrange(desc(mean))

# Errors of omniscience
#   ProblemType Construction EvidenceAccess     n  mean    sd     se     ic
# 1 nonFactive Modus Ponens  Evidence          86 0.698 0.462 0.0498 0.0990
# 2 Factive    Modus Ponens  Evidence          86 0.581 0.496 0.0535 0.106 
# 3 nonFactive Modus Ponens  noEvidence        86 0.488 0.503 0.0542 0.108 
# 4 Factive    Modus Ponens  noEvidence        86 0.488 0.503 0.0542 0.108 
# 5 nonFactive Modus Tollens noEvidence        86 0.465 0.502 0.0541 0.108 
# 6 Factive    Modus Tollens Evidence          86 0.465 0.502 0.0541 0.108 
# 7 nonFactive Modus Tollens Evidence          86 0.419 0.496 0.0535 0.106 
# 8 Factive    Modus Tollens noEvidence        86 0.384 0.489 0.0527 0.105 

# --------------------------------------------------------------------------------------------------
# 2. Errors of Omniscience (OE)
# --------------------------------------------------------------------------------------------------

# 2.1) Wilcoxon test comparing errors of omniscience - ProblemType
data %>% group_by(ProblemType) %>% summarise(OmniscienceError = mean(OmniscienceError))
# ProblemType     OmniscienceError
# 1 Factive                0.328
# 2 nonFactive             0.453

nonFactive.OE <- aggregate(OmniscienceError ~ ParticipantID, data=data[data$ProblemType=="nonFactive",], FUN=mean)
factive.OE <- aggregate(OmniscienceError ~ ParticipantID, data=data[data$ProblemType=="Factive",], FUN=mean)

wilcoxsign_test(nonFactive.OE$OmniscienceError ~ factive.OE$OmniscienceError)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = 3.1246, p-value = 0.001781
# alternative hypothesis: true mu is not equal to 0

cliff.delta(nonFactive.OE$OmniscienceError,factive.OE$OmniscienceError,conf.level = 0.95)
# Cliff's Delta
# 
# delta estimate: 0.2433748 (small)
# 95 percent confidence interval:
#   lower        upper 
# -0.005762951  0.464064969

# 2.2) Wilcoxon test comparing errors of omniscience - Construction
data %>% group_by(Construction) %>% summarise(OmniscienceError = mean(OmniscienceError))
# Construction    OmniscienceError
# 1 Modus Ponens             0.451
# 2 Modus Tollens            0.331

MP.OE <- aggregate(OmniscienceError ~ ParticipantID, data=data[data$Construction=="Modus Ponens",], FUN=mean)
MT.OE <- aggregate(OmniscienceError ~ ParticipantID, data=data[data$Construction=="Modus Tollens",], FUN=mean)

wilcoxsign_test(MP.OE$OmniscienceError ~ MT.OE$OmniscienceError)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = 2.926, p-value = 0.003434
# alternative hypothesis: true mu is not equal to 0

cliff.delta(MP.OE$OmniscienceError,MT.OE$OmniscienceError,conf.level = 0.95)
# Cliff's Delta
# 
# delta estimate: 0.1979448 (small)
# 95 percent confidence interval:
#   lower       upper 
# -0.04901515  0.42209610 

# 2.3) Wilcoxon test comparing errors of omniscience - EvidenceAccess
data %>% group_by(EvidenceAccess) %>% summarise(OmniscienceError = mean(OmniscienceError))
# EvidenceAccess    OmniscienceError
# 1 Evidence                  0.439
# 2 noEvidence                0.343

Evidence.OE <- aggregate(OmniscienceError ~ ParticipantID, data=data[data$EvidenceAccess=="Evidence",], FUN=mean)
noEvidence.OE <- aggregate(OmniscienceError ~ ParticipantID, data=data[data$EvidenceAccess=="noEvidence",], FUN=mean)

wilcoxsign_test(Evidence.OE$OmniscienceError ~ noEvidence.OE$OmniscienceError)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = 1.8823, p-value = 0.0598
# alternative hypothesis: true mu is not equal to 0

cliff.delta(Evidence.OE$OmniscienceError,noEvidence.OE$OmniscienceError,conf.level = 0.95)
# Cliff's Delta
# 
# delta estimate: 0.1838832 (small)
# 95 percent confidence interval:
#   lower       upper 
# -0.06459155  0.41088809

# 2.4) Wilcoxon test on errors of omniscience for interaction between ProblemType and Construction
data %>% group_by(ProblemType, Construction) %>% summarise(OmniscienceError = mean(OmniscienceError))
#   ProblemType   Construction  OmniscienceError
# 1 Factive     Modus Ponens             0.372
# 2 Factive     Modus Tollens            0.285
# 3 nonFactive  Modus Ponens             0.529
# 4 nonFactive  Modus Tollens            0.378

VerbxConstruction.OE <- data %>% group_by(ParticipantID, ProblemType, Construction) %>% 
  summarise(OmniscienceError = mean(OmniscienceError))

VerbxMP.OE <- subset(VerbxConstruction.OE, ProblemType=="nonFactive" & Construction=="Modus Ponens")$OmniscienceError - 
  subset(VerbxConstruction.OE, ProblemType=="Factive" & Construction=="Modus Ponens")$OmniscienceError
VerbxMT.OE <- subset(VerbxConstruction.OE, ProblemType=="nonFactive" & Construction=="Modus Tollens")$OmniscienceError - 
  subset(VerbxConstruction.OE, ProblemType=="Factive" & Construction=="Modus Tollens")$OmniscienceError

wilcoxsign_test(VerbxMP.OE ~ VerbxMT.OE)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = 1.3737, p-value = 0.1695
# alternative hypothesis: true mu is not equal to 0

cliff.delta(VerbxMP.OE, VerbxMT.OE, conf.level = 0.95)
# Cliff's Delta
# 
# delta estimate: 0.09951325 (negligible)
# 95 percent confidence interval:
#   lower      upper 
# -0.1333577  0.3219707 

# 2.5) Wilcoxon test on errors of omniscience for interaction between Construction and Evidence
data %>% group_by(Construction, EvidenceAccess) %>% summarise(OmniscienceError = mean(OmniscienceError))
# Construction    EvidenceAccess OmniscienceError
# 1 Modus Ponens  Evidence                  0.5  
# 2 Modus Ponens  noEvidence                0.401
# 3 Modus Tollens Evidence                  0.378
# 4 Modus Tollens noEvidence                0.285

ConstructionxEvidenceAccess.OE <- data %>% group_by(ParticipantID, Construction, EvidenceAccess) %>% 
  summarise(OmniscienceError = mean(OmniscienceError))

ConstructionxEvidence.OE <- subset(ConstructionxEvidenceAccess.OE, Construction=="Modus Ponens" & EvidenceAccess=="Evidence")$OmniscienceError - 
  subset(ConstructionxEvidenceAccess.OE, Construction=="Modus Tollens" & EvidenceAccess=="Evidence")$OmniscienceError
ConstructionxnoEvidence.OE <- subset(ConstructionxEvidenceAccess.OE, Construction=="Modus Ponens" & EvidenceAccess=="noEvidence")$OmniscienceError - 
  subset(ConstructionxEvidenceAccess.OE, Construction=="Modus Tollens" & EvidenceAccess=="noEvidence")$OmniscienceError

wilcoxsign_test(ConstructionxEvidence.OE ~ ConstructionxnoEvidence.OE)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = 0.59659, p-value = 0.5508
# alternative hypothesis: true mu is not equal to 0

cliff.delta(ConstructionxEvidence.OE, ConstructionxnoEvidence.OE, conf.level = 0.95)
# Cliff's Delta
# 
# delta estimate: 0.05300162 (negligible)
# 95 percent confidence interval:
#   lower      upper 
# -0.1807203  0.2810575 

# 2.6) Wilcoxon test on errors of omniscience for interaction between ProblemType and Evidence
data %>% group_by(ProblemType, EvidenceAccess) %>% summarise(OmniscienceError = mean(OmniscienceError))
#   ProblemType   EvidenceAccess OmniscienceError
# 1 Factive     Evidence                  0.390
# 2 Factive     noEvidence                0.267
# 3 nonFactive  Evidence                  0.488
# 4 nonFactive  noEvidence                0.419

VerbxEvidenceAccess.OE <- data %>% group_by(ParticipantID, ProblemType, EvidenceAccess) %>% 
  summarise(OmniscienceError = mean(OmniscienceError))

VerbxEvidence.OE <- subset(VerbxEvidenceAccess.OE, ProblemType=="nonFactive" & EvidenceAccess=="Evidence")$OmniscienceError - 
  subset(VerbxEvidenceAccess.OE, ProblemType=="Factive" & EvidenceAccess=="Evidence")$OmniscienceError
VerbxnoEvidence.OE <- subset(VerbxEvidenceAccess.OE, ProblemType=="nonFactive" & EvidenceAccess=="noEvidence")$OmniscienceError - 
  subset(VerbxEvidenceAccess.OE, ProblemType=="Factive" & EvidenceAccess=="noEvidence")$OmniscienceError

wilcoxsign_test(VerbxEvidence.OE ~ VerbxnoEvidence.OE)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = -1.2205, p-value = 0.2223
# alternative hypothesis: true mu is not equal to 0

cliff.delta(VerbxEvidence.OE, VerbxnoEvidence.OE, conf.level = 0.95)
# Cliff's Delta
# 
# delta estimate: -0.1438615 (negligible)
# 95 percent confidence interval:
#   lower       upper 
# -0.35628817  0.08270552 

# 2.7) Wilcoxon test on errors of omniscience for three-way interaction
data %>% group_by(ProblemType, Construction, EvidenceAccess) %>% summarise(OmniscienceError = mean(OmniscienceError))
#   ProblemType   Construction  EvidenceAccess OmniscienceError
# 1 Factive     Modus Ponens  Evidence                  0.407
# 2 Factive     Modus Ponens  noEvidence                0.337
# 3 Factive     Modus Tollens Evidence                  0.372
# 4 Factive     Modus Tollens noEvidence                0.198
# 5 nonFactive  Modus Ponens  Evidence                  0.593
# 6 nonFactive  Modus Ponens  noEvidence                0.465
# 7 nonFactive  Modus Tollens Evidence                  0.384
# 8 nonFactive  Modus Tollens noEvidence                0.372

omni.test <- data %>% group_by(ParticipantID, ProblemType, Construction, EvidenceAccess) %>% 
                            summarise(OmniscienceError = mean(OmniscienceError))

omni.A1 <- subset(omni.test, ProblemType=="nonFactive" & Construction=="Modus Ponens" & EvidenceAccess=="Evidence")$OmniscienceError -
  subset(omni.test, ProblemType=="nonFactive" & Construction=="Modus Tollens" & EvidenceAccess=="Evidence")$OmniscienceError
omni.A2 <- subset(omni.test, ProblemType=="nonFactive" & Construction=="Modus Ponens" & EvidenceAccess=="noEvidence")$OmniscienceError -
  subset(omni.test, ProblemType=="nonFactive" & Construction=="Modus Tollens" & EvidenceAccess=="noEvidence")$OmniscienceError

omni.B1 <- subset(omni.test, ProblemType=="Factive" & Construction=="Modus Ponens" & EvidenceAccess=="Evidence")$OmniscienceError -
  subset(omni.test, ProblemType=="Factive" & Construction=="Modus Tollens" & EvidenceAccess=="Evidence")$OmniscienceError
omni.B2 <- subset(omni.test, ProblemType=="Factive" & Construction=="Modus Ponens" & EvidenceAccess=="noEvidence")$OmniscienceError -
  subset(omni.test, ProblemType=="Factive" & Construction=="Modus Tollens" & EvidenceAccess=="noEvidence")$OmniscienceError

omni.A <- omni.A1 - omni.A2
omni.B <- omni.B1 - omni.B2

wilcoxsign_test(omni.A ~ omni.B)
# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg)
# 	 stratified by block
# Z = 2.2133, p-value = 0.02688
# alternative hypothesis: true mu is not equal to 0

cliff.delta(omni.A, omni.B)
# Cliff's Delta
# 
# delta estimate: 0.2623039 (small)
# 95 percent confidence interval:
#   lower      upper 
# 0.04151152 0.45867075 

# 2.8) Wilcoxon test comparing omniscience errors against 0
zeroes.OE <- Evidence.OE
zeroes.OE$OmniscienceError <- zeroes.OE$OmniscienceError * 0

# For access
wilcoxsign_test(Evidence.OE$OmniscienceError ~ zeroes.OE$OmniscienceError)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = 5.5243, p-value = 3.308e-08
# alternative hypothesis: true mu is not equal to 0

cliff.delta(Evidence.OE$OmniscienceError, zeroes.OE$OmniscienceError, conf.level = 0.95)
# Cliff's Delta
# 
# delta estimate: 0.8139535 (large)
# 95 percent confidence interval:
#   lower     upper 
# 0.6594301 0.9024864 

# For no access
wilcoxsign_test(noEvidence.OE$OmniscienceError ~ zeroes.OE$OmniscienceError)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = 5.248, p-value = 1.537e-07
# alternative hypothesis: true mu is not equal to 0

cliff.delta(noEvidence.OE$OmniscienceError, zeroes.OE$OmniscienceError, conf.level = 0.95)
# Cliff's Delta
#
# delta estimate: 0.6976744 (large)
# 95 percent confidence interval:
#   lower     upper 
# 0.5309872 0.8123808 

# --------------------------------------------------------------------------------------------------
# 3. Figures
# --------------------------------------------------------------------------------------------------

# 3.1)  Omniscience Errors for problem construction
fig.construction.df <- group_by(data, Construction) %>% 
  summarise( 
    n=n(),
    mean=mean(OmniscienceError),
    sd=sd(OmniscienceError)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

fig.construction.df.temp <- fig.construction.df[,3:6]*100

fig.construction.df <- cbind(fig.construction.df[1:2], fig.construction.df.temp)

construction.bPlot <- ggplot(fig.construction.df, aes(y=mean, x=Construction)) +
  geom_bar(stat="identity", position=position_dodge(), fill="brown3", size=.5, width=.75) +
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

construction.bPlot

# 3.2)  Omniscience Errors for evidence access
fig.evidence.df <- group_by(data, EvidenceAccess) %>% 
  summarise( 
    n=n(),
    mean=mean(OmniscienceError),
    sd=sd(OmniscienceError)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

fig.evidence.df.temp <- fig.evidence.df[,3:6]*100

fig.evidence.df <- cbind(fig.evidence.df[1:2], fig.evidence.df.temp)

evidence.bPplot <- ggplot(fig.evidence.df, aes(y=mean, x=EvidenceAccess)) +
  geom_bar(stat="identity", position=position_dodge(), fill="brown3", size=.5, width=.75) +
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

evidence.bPplot

# 3.3)  Omniscience Errors for verb
fig.verb.df <- group_by(data, ProblemType) %>% 
  summarise( 
    n=n(),
    mean=mean(OmniscienceError),
    sd=sd(OmniscienceError)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

fig.verb.df.temp <- fig.verb.df[,3:6]*100

fig.verb.df <- cbind(fig.verb.df[1:2], fig.verb.df.temp)

verb.bPlot <- ggplot(fig.verb.df, aes(y=mean, x=reorder(ProblemType, -mean))) +
  geom_bar(stat="identity", position=position_dodge(), fill="brown3", size=.5, width=.75) +
  geom_errorbar(aes(x=ProblemType, ymin=mean-ic, ymax=mean+ic), 
                position=position_dodge(width=0.9), width=0.2, alpha=0.9, size=.5) +
  scale_y_continuous(name = "Omniscience Errors", limits = c(0, 105), breaks = c(0, 50, 100)) +
  scale_x_discrete(name = "", labels=c("Non-factive", "Factive")) +
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position=c(0.26, 0.72),
        legend.key.size = unit(.5, 'cm'),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(size = 20))

verb.bPlot

# Response type barplot
sums <- colSums(data[29:30])
sums <- round((sums/length(data$OmniscienceError))*100)
resp_sum_table <- data.frame(Column = names(sums), N = sums)
rownames(resp_sum_table) <- NULL

resps.barplot <- ggplot(resp_sum_table, aes(y=N, x=reorder(Column, -N))) +
  geom_bar(stat="identity", position=position_dodge(), fill="brown3", size=.5, width=.75) +
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

# 3.4)  Violin plot of omniscience errors for interaction between ProblemType and Evidence
fig.interaction.df <- aggregate(OmniscienceError ~ ParticipantID * EvidenceAccess * Verb, data=data,
                  FUN=function(x){ round(mean(x), digits=2) })

interaction.vPlot <- ggplot(fig.interaction.df, aes(x=EvidenceAccess, y=OmniscienceError)) +
  geom_violin(trim=TRUE, fill='white', adjust=1.8, size=.4) +
  geom_jitter(shape=16, alpha=.3, size=1.2, position=position_jitter(width=0.04, height=0.13)) +
  theme_bw(12) +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width=.1) +
  stat_summary(fun = "mean", geom = "point", size=2) +
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

interaction.vPlot
