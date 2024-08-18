# --------------------------------------------------------------------------------------------------
# Omniscience Errors Experiment 3 Analysis
# --------------------------------------------------------------------------------------------------
# Subject pool: Amazon Mechanical Turk
# Date(s):      October 2023
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
#     2.7)  Wilcoxon test on errors of omniscience in EvidenceAccess against 0
#  3. Figures
#     3.1)  Bar plot of omniscience errors for construction
#     3.2)  Bar plot of omniscience errors for evidence access
#     3.3)  Bar plot of omniscience errors for ProblemType
#     3.4)  Violin plot of omniscience errors for interaction between ProblemType and Evidence
#     3.5)  Bar plot of frequency of each label
#     3.6)  Bar plot of frequency of each label by verb type
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
library(psych)

##########
# Setup script
wd <- getwd()
if (wd != rstudioapi::getActiveDocumentContext()$path) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  wd <- getwd()
}
project <- "OE3"
##########

# --------------------------------------------------------------------------------------------------
# X. Preprocessing; filter subjs by attention check; reformatting responses
# --------------------------------------------------------------------------------------------------

rawData <- readxl::read_xlsx(paste("./", project, "-Aggregate.xlsx", sep=""))
data.Prelim <- rawData
data.Prelim$ParticipantID <- as.factor(data.Prelim$ParticipantID)
data.Prelim$Label <- as.factor(data.Prelim$Label_BB)
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
data.Prelim.AC$correctAC <- "strawberry strawberry strawberry"
data.Prelim.AC$PassedAC <- ifelse(tolower(data.Prelim.AC$Answer) == data.Prelim.AC$correctAC, 1, 0)
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

data         <- subset(data.Prelim, Cond != "AC" & Cond != "DB" & FailedAC==0)
useableSubs  <- unique(data$ParticipantID)
finalN       <- length(unique(data$ParticipantID))

excluded.data    <- subset(data.Prelim, Cond != "DB" & FailedAC==1)
excluded.data    <- subset(excluded.data, Cond != "AC")
excludedN    <- length(unique(excluded.data$ParticipantID))
subjLoss     <- round(1 - (finalN / fullN),2)

# Assign error of omniscience scores
data$OmniscienceError <- ifelse(grepl("omniscienceError", data$Label), 1, 0)
data <- transform(data, OmniscienceError = as.numeric(OmniscienceError))
data$Verb <- as.factor(data$ProblemType)

# Assign accuracy scores
data$Acc <- 0

data$Acc[grepl("validDeduction", data$Label) & !grepl("invalidDeduction", data$Label)] <- 1

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

# fullN finalN excludedN meanAge minAge maxAge femaleCount maleCount otherCount preferNotCount
#    69     55        14   39.26     25     64          35        33          1              0

# --------------------------------------------------------------------------------------------------
# X. Inter-rater relaiability analysis
# --------------------------------------------------------------------------------------------------

valid <- kappa2(na.omit(data[,c("ValidDeduction_BB","ValidDeduction_SK")]))
invalid <- kappa2(na.omit(data[,c("InvalidDeduction_BB","InvalidDeduction_SK")]))
agentic <- kappa2(na.omit(data[,c("Agentic_BB","Agentic_SK")]))
epistemic <- kappa2(na.omit(data[,c("Epistemic_BB","Epistemic_SK")]))
hedging <- kappa2(na.omit(data[,c("Hedging_BB","Hedging_SK")]))
nonsense <- kappa2(na.omit(data[,c("Nonsense_BB","Nonsense_SK")]))
omniscienceError <- kappa2(na.omit(data[,c("OmniscienceError_BB","OmniscienceError_SK")]))

kappas <- c(valid$value,invalid$value,agentic$value,epistemic$value,hedging$value,nonsense$value,omniscienceError$value)
# 0.9063830 0.9452373 1.0000000 1.0000000 0.8474341 0.6625767 1.0000000

valid_mismatches <- which(data$ValidDeduction_BB!=data$ValidDeduction_SK)
invalid_mismatches <- which(data$InvalidDeduction_BB!=data$InvalidDeduction_SK)
agentic_mismatches <- which(data$Agentic_BB!=data$Agentic_SK)
epistemic_mismatches <- which(data$Epistemic_BB!=data$Epistemic_SK)
hedging_mismatches <- which(data$Hedging_BB!=data$Hedging_SK)
nonsense_mismatches <- which(data$Nonsense_BB!=data$Nonsense_SK)
omniscienceError_mismatches <- which(data$OmniscienceError_BB!=data$OmniscienceError_SK)

disagreements <- c(valid_mismatches, invalid_mismatches, agentic_mismatches, epistemic_mismatches, 
                   hedging_mismatches, nonsense_mismatches, omniscienceError_mismatches)

data$Disagree <- 0
data$Disagree[disagreements] <- 1

viewMatchups <- function(category)  {
    switch(category,
           "valid" = View(data[valid_mismatches,c(18:20,23,24,31)]),
           "invalid" = View(data[invalid_mismatches,c(18:20,23,25,32)]),
           "agentic" = View(data[agentic_mismatches,c(18:20,23,26,33)]),
           "epistemic" = View(data[epistemic_mismatches,c(18:20,23,27,34)]),
           "hedging" = View(data[hedging_mismatches,c(18:20,23,28,35)]),
           "nonsense" = View(data[nonsense_mismatches,c(18:20,23,29,36)]),
           "oe" = View(data[omniscienceError_mismatches,c(18:20,23,30,37)]),
           "all" = View(data[valid_mismatches,c(18:20,23,24,31)]) & View(data[invalid_mismatches,c(18:20,23,25,32)]) & 
               View(data[agentic_mismatches,c(18:20,23,26,33)]) & View(data[epistemic_mismatches,c(18:20,23,27,34)]) & 
               View(data[hedging_mismatches,c(18:20,23,28,35)]) & View(data[nonsense_mismatches,c(18:20,23,29,36)]) & 
               View(data[omniscienceError_mismatches,c(18:20,23,30,37)]))
}

exportIRR <- "no"
if (exportIRR=="yes") {write.csv(data,paste(location,"OE3-Aggregate_IRR.csv", sep=""), row.names = FALSE)}

# --------------------------------------------------------------------------------------------------
# 1. Omnibus table
# --------------------------------------------------------------------------------------------------
omnibus.table <- group_by(data, ProblemType, Construction, EvidenceAccess) %>%
  summarise(n=n(), mean=mean(OmniscienceError), sd=sd(OmniscienceError)) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1)) %>%
  arrange(desc(mean))

# Errors of omniscience
#   ProblemType Construction EvidenceAccess    n  mean    sd    se     ic
# 1 nonFactive  Modus Ponens  Evidence         110 0.255  0.438 0.0417 0.0827
# 2 Factive     Modus Ponens  Evidence         110 0.245  0.432 0.0412 0.0817
# 3 nonFactive  Modus Ponens  noEvidence       110 0.227  0.421 0.0401 0.0796
# 4 Factive     Modus Ponens  noEvidence       110 0.164  0.372 0.0354 0.0702
# 5 nonFactive  Modus Tollens Evidence         110 0.0545 0.228 0.0218 0.0431
# 6 Factive     Modus Tollens Evidence         110 0.0455 0.209 0.0200 0.0395
# 7 Factive     Modus Tollens noEvidence       110 0.0455 0.209 0.0200 0.0395
# 8 nonFactive  Modus Tollens noEvidence       110 0.0455 0.209 0.0200 0.0395

# --------------------------------------------------------------------------------------------------
# 2. Errors of Omniscience (OE)
# --------------------------------------------------------------------------------------------------

# 2.1) Wilcoxon test comparing errors of omniscience - ProblemType
data %>% group_by(ProblemType) %>% summarise(OmniscienceError = mean(OmniscienceError))
# ProblemType     OmniscienceError
# 1 Factive                0.125
# 2 nonFactive             0.145

nonFactive.OE <- aggregate(OmniscienceError ~ ParticipantID, data=data[data$ProblemType=="nonFactive",], FUN=mean)
factive.OE <- aggregate(OmniscienceError ~ ParticipantID, data=data[data$ProblemType=="Factive",], FUN=mean)

wilcoxsign_test(nonFactive.OE$OmniscienceError ~ factive.OE$OmniscienceError)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg)
# stratified by block
# Z = 0.72436, p-value = 0.4688
# alternative hypothesis: true mu is not equal to 0

cliff.delta(nonFactive.OE$OmniscienceError,factive.OE$OmniscienceError,conf.level = 0.95)
# Cliff's Delta
# delta estimate: 0.06016529 (negligible)
# 95 percent confidence interval:
#     lower      upper 
# -0.1311657  0.2471751 

# 2.2) Wilcoxon test comparing errors of omniscience - Construction
data %>% group_by(Construction) %>% summarise(OmniscienceError = mean(OmniscienceError))
# Construction    OmniscienceError
# 1 Modus Ponens            0.223 
# 2 Modus Tollens           0.0477

MP.OE <- aggregate(OmniscienceError ~ ParticipantID, data=data[data$Construction=="Modus Ponens",], FUN=mean)
MT.OE <- aggregate(OmniscienceError ~ ParticipantID, data=data[data$Construction=="Modus Tollens",], FUN=mean)

wilcoxsign_test(MP.OE$OmniscienceError ~ MT.OE$OmniscienceError)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg)
# stratified by block
# Z = 4.2502, p-value = 2.135e-05
# alternative hypothesis: true mu is not equal to 0

cliff.delta(MP.OE$OmniscienceError,MT.OE$OmniscienceError,conf.level = 0.95)
# Cliff's Delta
# delta estimate: 0.3087603 (small)
# 95 percent confidence interval:
#     lower     upper 
# 0.1317495 0.4666891 

# 2.3) Wilcoxon test comparing errors of omniscience - EvidenceAccess
data %>% group_by(EvidenceAccess) %>% summarise(OmniscienceError = mean(OmniscienceError))
# EvidenceAccess    OmniscienceError
# 1 Evidence                  0.15 
# 2 noEvidence                0.120

Evidence.OE <- aggregate(OmniscienceError ~ ParticipantID, data=data[data$EvidenceAccess=="Evidence",], FUN=mean)
noEvidence.OE <- aggregate(OmniscienceError ~ ParticipantID, data=data[data$EvidenceAccess=="noEvidence",], FUN=mean)

wilcoxsign_test(Evidence.OE$OmniscienceError ~ noEvidence.OE$OmniscienceError)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg)
# stratified by block
# Z = 0.6403, p-value = 0.522
# alternative hypothesis: true mu is not equal to 0

cliff.delta(Evidence.OE$OmniscienceError,noEvidence.OE$OmniscienceError,conf.level = 0.95)
# Cliff's Delta
# delta estimate: 0.05619835 (negligible)
# 95 percent confidence interval:
#     lower      upper 
# -0.1338882  0.2422963 

# 2.4) Wilcoxon test on errors of omniscience for interaction between ProblemType and Construction
data %>% group_by(ProblemType, Construction) %>% summarise(OmniscienceError = mean(OmniscienceError))
#   ProblemType   Construction  OmniscienceError
# 1 Factive     Modus Ponens            0.205 
# 2 Factive     Modus Tollens           0.0455
# 3 nonFactive  Modus Ponens            0.241 
# 4 nonFactive  Modus Tollens           0.05 

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
# Z = 0.70951, p-value = 0.478
# alternative hypothesis: true mu is not equal to 0

cliff.delta(VerbxMP.OE, VerbxMT.OE, conf.level = 0.95)
# Cliff's Delta
# delta estimate: 0.02181818 (negligible)
# 95 percent confidence interval:
#     lower      upper 
# -0.1358473  0.1784058 

# 2.5) Wilcoxon test on errors of omniscience for interaction between Construction and Evidence
data %>% group_by(Construction, EvidenceAccess) %>% summarise(OmniscienceError = mean(OmniscienceError))
# Construction    EvidenceAccess OmniscienceError
# 1 Modus Ponens  Evidence                 0.25  
# 2 Modus Ponens  noEvidence               0.195 
# 3 Modus Tollens Evidence                 0.05  
# 4 Modus Tollens noEvidence               0.0455

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
# Z = 0.64537, p-value = 0.5187
# alternative hypothesis: true mu is not equal to 0

cliff.delta(ConstructionxEvidence.OE, ConstructionxnoEvidence.OE, conf.level = 0.95)
# Cliff's Delta
# delta estimate: 0.01818182 (negligible)
# 95 percent confidence interval:
#     lower      upper 
# -0.1648193  0.1999728 

# 2.6) Wilcoxon test on errors of omniscience for interaction between ProblemType and Evidence
data %>% group_by(ProblemType, EvidenceAccess) %>% summarise(OmniscienceError = mean(OmniscienceError))
#   ProblemType   EvidenceAccess OmniscienceError
# 1 Factive     Evidence                  0.145
# 2 Factive     noEvidence                0.105
# 3 nonFactive  Evidence                  0.155
# 4 nonFactive  noEvidence                0.136

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
# Z = -1.1577, p-value = 0.247
# alternative hypothesis: true mu is not equal to 0

cliff.delta(VerbxEvidence.OE, VerbxnoEvidence.OE, conf.level = 0.95)
# Cliff's Delta
# delta estimate: -0.08892562 (negligible)
# 95 percent confidence interval:
#     lower       upper 
# -0.25103782  0.07803885 

# 2.7) Wilcoxon test comparing omniscience errors against 0
zeroes.OE <- Evidence.OE
zeroes.OE$OmniscienceError <- zeroes.OE$OmniscienceError * 0

# For access
wilcoxsign_test(Evidence.OE$OmniscienceError ~ zeroes.OE$OmniscienceError)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg)
# stratified by block
# Z = 4.6459, p-value = 3.386e-06
# alternative hypothesis: true mu is not equal to 0

cliff.delta(Evidence.OE$OmniscienceError, zeroes.OE$OmniscienceError, conf.level = 0.95)
# Cliff's Delta
# delta estimate: 0.4 (medium)
# 95 percent confidence interval:
#     lower     upper 
# 0.2608081 0.5228998 

# For no access
wilcoxsign_test(noEvidence.OE$OmniscienceError ~ zeroes.OE$OmniscienceError)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg)
# stratified by block
# Z = 4.4394, p-value = 9.02e-06
# alternative hypothesis: true mu is not equal to 0

cliff.delta(noEvidence.OE$OmniscienceError, zeroes.OE$OmniscienceError, conf.level = 0.95)
# Cliff's Delta
# delta estimate: 0.3636364 (medium)
# 95 percent confidence interval:
#     lower     upper 
# 0.2280126 0.4854177 

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
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue4", size=.5) +
  geom_errorbar(aes(x=Construction, ymin=mean-ic, ymax=mean+ic),
                position=position_dodge(width=0.9), width=0.1, alpha=0.9, size=.5) +
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

verb.bPlot <- ggplot(fig.verb.df, aes(y=mean, x=ProblemType)) +
  geom_bar(stat="identity", position=position_dodge(), fill="darkred", size=.5, width = .5) +
  geom_errorbar(aes(x=ProblemType, ymin=mean-ic, ymax=mean+ic),
                position=position_dodge(width=0.9), width=0.1, alpha=0.9, size=.5) +
  scale_y_continuous(name = "Omniscience Errors", limits = c(0, 105), breaks = c(0, 50, 100)) +
  scale_x_discrete(name = "", labels=c("Factive", "Non-factive")) +
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

# 3.5)  Bar plot of frequency of each label
# Split labels into separate rows
data$Label <- gsub(" ", "", data$Label)
plot_labs <- separate_rows(data, Label, sep = ";")

table(plot_labs$Label) / sum(table(plot_labs$Label))
#           -          agentic        epistemic          hedging invalidDeduction         nonsense omniscienceError   validDeduction 
# 0.005582694      0.241451500      0.122121424      0.042568039      0.278436846      0.005582694      0.083042568      0.221214236 

ggplot(plot_labs, aes(x = Label, fill = Label)) +
  geom_bar() +
  labs(title = "Frequency of Labels", x = "Label", y = "Frequency")

# 3.6)  Bar plot of frequency of each label by verb type
ggplot(plot_labs, aes(x = Label, fill = Label)) +
  geom_bar() +
  facet_wrap(~ProblemType) +
  labs(title = "Frequency of Labels", x = "Label", y = "Frequency")
