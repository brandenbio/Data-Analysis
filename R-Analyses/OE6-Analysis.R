# --------------------------------------------------------------------------------------------------
# Omniscience Errors Experiment 6 Analysis
# --------------------------------------------------------------------------------------------------
# Subject pool: Amazon Mechanical Turk
# Date(s):      July 2024
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
#     2.3)  Wilcoxon test on errors of omniscience in ExplicitDenial
#     2.4)  Wilcoxon test on errors of omniscience for interaction between ProblemType and Construction
#     2.5)  Wilcoxon test on errors of omniscience for interaction between Construction and Denial
#     2.6)  Wilcoxon test on errors of omniscience for interaction between ProblemType and Denial
#     2.7)  Wilcoxon test on errors of omniscience for three-way interaction
#     2.8)  Wilcoxon test on errors of omniscience in ExplicitDenial against 0
#  3. Figures
#     3.1)  Bar graph of omniscience errors for construction
#     3.2)  Bar graph of omniscience errors for Denial access
#     3.3)  Bar graph of omniscience errors for ProblemType
#     3.4)  Violin plot of omniscience errors for interaction between ProblemType and Denial
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
project <- "OE6"
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
data.Prelim$locationL <- sapply(strsplit(data.Prelim$Premise3Content, "the "), `[`, 2)
data.Prelim$locationL <- sapply(strsplit(data.Prelim$locationL, " then"), `[`, 1)
data.Prelim.AC <- subset(data.Prelim, Cond == "AC")
data.Prelim.AC$correctAC <- "Believes that knows that nothing can be concluded"
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
                        `X knows nothing can be concluded`    = "X knows that nothing can be concluded",
                        `X believes nothing can be concluded` = "X believes that nothing can be concluded",
                        `Nothing can be concluded`            = "Nothing can be concluded")

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
data$OmniscienceError <- ifelse(grepl("that nothing can be concluded", data$AnswerType), 0, data$OmniscienceError)
data <- transform(data, OmniscienceError = as.numeric(OmniscienceError))

# Accuracy Rubric
# Cor = 1, Inc = 0
# Modus Ponens                                       # Modus Tollens
# ProblemType| AnswerType                 | Acc   # ProblemType| AnswerType                 | Acc
# nonFactive | Nonsense                   | Inc   # nonFactive | Nonsense                   | Inc
# nonFactive | Nothing can be concluded            | Cor   # nonFactive | Nothing can be concluded            | Cor
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
# nonFactive | X believes nothing can be concluded | Cor   # nonFactive | X believes nothing can be concluded | Cor
# nonFactive | X knows nothing can be concluded    | Cor   # nonFactive | X knows nothing can be concluded    | Cor
# Modus Ponens                                       # Modus Tollens
# ProblemType| AnswerType                 | Acc   # ProblemType| AnswerType                 | Acc
# Factive    | Nonsense                   | Inc   # Factive    | Nonsense                   | Inc
# Factive    | Nothing can be concluded            | Inc   # Factive    | Nothing can be concluded            | Inc
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
# Factive    | X believes nothing can be concluded | Inc   # Factive    | X believes nothing can be concluded | Inc
# Factive    | X knows nothing can be concluded    | Inc   # Factive    | X knows nothing can be concluded    | Inc

# Assign accuracy scores
data$Acc <- 0
# Believes
data$Acc[data$ProblemType == "nonFactive" & 
           (data$AnswerType == "Nothing can be concluded" | grepl("nothing can be concluded", data$AnswerType))] <- 1
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

# fullN finalN excludedN meanAge minAge maxAge femaleCount maleCount otherCount preferNotCount
#    66     55        11   42.98     19     72          31        35          0              0

# --------------------------------------------------------------------------------------------------
# 1. Omnibus table
# --------------------------------------------------------------------------------------------------
omnibus.table <- group_by(data, ProblemType, Construction) %>% 
  summarise(n=n(), mean=mean(OmniscienceError), sd=sd(OmniscienceError)) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1)) %>%
  arrange(desc(mean))

# Errors of omniscience
#   ProblemType Construction      n  mean    sd     se     ic
# 1 Factive     Modus Tollens   220 0.182 0.387 0.0261 0.0514
# 2 Factive     Modus Ponens    220 0.177 0.383 0.0258 0.0509
# 3 nonFactive  Modus Ponens    220 0.168 0.375 0.0253 0.0498
# 4 nonFactive  Modus Tollens   220 0.168 0.375 0.0253 0.0498

# --------------------------------------------------------------------------------------------------
# 2. Errors of Omniscience (OE)
# --------------------------------------------------------------------------------------------------
data %>% filter(OmniscienceError==1) %>% group_by(ProblemType) %>% 
  summarize(OE_N=n(),
            Resp_Believes = round(mean(str_detect(AnswerType, "believes")) * 100,2), 
            Resp_Knows = round(mean(str_detect(AnswerType, "knows")) * 100,2))

#   ProblemType  OE_N Resp_Believes Resp_Knows
# 1 Factive        79          44.3      55.7 
# 2 nonFactive     74          96.0       4.05

# 2.1) Wilcoxon test comparing errors of omniscience - ProblemType
data %>% group_by(ProblemType) %>% summarise(OmniscienceError = mean(OmniscienceError))
# ProblemType     OmniscienceError
# 1 Factive                0.180
# 2 nonFactive             0.168

nonFactive.OE <- aggregate(OmniscienceError ~ ParticipantID, data=data[data$ProblemType=="nonFactive",], FUN=mean)
factive.OE <- aggregate(OmniscienceError ~ ParticipantID, data=data[data$ProblemType=="Factive",], FUN=mean)

wilcoxsign_test(nonFactive.OE$OmniscienceError ~ factive.OE$OmniscienceError)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = -0.32179, p-value = 0.7476
# alternative hypothesis: true mu is not equal to 0

cliff.delta(nonFactive.OE$OmniscienceError,factive.OE$OmniscienceError,conf.level = 0.95)
# Cliff's Delta
# 
# delta estimate: -0.008595041 (negligible)
# 95 percent confidence interval:
#     lower      upper 
# -0.2015035  0.1849553 

# 2.2) Wilcoxon test comparing errors of omniscience - Construction
data %>% group_by(Construction) %>% summarise(OmniscienceError = mean(OmniscienceError))
# Construction    OmniscienceError
# 1 Modus Ponens             0.173
# 2 Modus Tollens            0.175

MP.OE <- aggregate(OmniscienceError ~ ParticipantID, data=data[data$Construction=="Modus Ponens",], FUN=mean)
MT.OE <- aggregate(OmniscienceError ~ ParticipantID, data=data[data$Construction=="Modus Tollens",], FUN=mean)

wilcoxsign_test(MP.OE$OmniscienceError ~ MT.OE$OmniscienceError)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = 0.21366, p-value = 0.8308
# alternative hypothesis: true mu is not equal to 0

cliff.delta(MP.OE$OmniscienceError,MT.OE$OmniscienceError,conf.level = 0.95)
# Cliff's Delta
# 
# delta estimate: 0.05289256 (negligible)
# 95 percent confidence interval:
#     lower      upper 
# -0.1370654  0.2390982 

# 2.3) Wilcoxon test comparing errors of omniscience - Focus
data %>% group_by(Focus) %>% summarise(OmniscienceError = mean(OmniscienceError))
#   Focus       OmniscienceError
# 1 Agent                  0.214
# 2 Participant            0.134

Agent.OE <- aggregate(OmniscienceError ~ ParticipantID, data=data[data$Focus=="Agent",], FUN=mean)
Participant.OE <- aggregate(OmniscienceError ~ ParticipantID, data=data[data$Focus=="Participant",], FUN=mean)

wilcoxsign_test(Agent.OE$OmniscienceError ~ Participant.OE$OmniscienceError)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = 2.6903, p-value = 0.007138
# alternative hypothesis: true mu is not equal to 0

cliff.delta(Agent.OE$OmniscienceError,Participant.OE$OmniscienceError,conf.level = 0.95)
# Cliff's Delta
# 
# delta estimate: 0.1854545 (small)
# 95 percent confidence interval:
#      lower      upper 
# -0.0100883  0.3673359 

# 2.4) Wilcoxon test on errors of omniscience for interaction between ProblemType and Construction
data %>% group_by(ProblemType, Construction) %>% summarise(OmniscienceError = mean(OmniscienceError))
#   ProblemType   Construction  OmniscienceError
# 1 Factive     Modus Ponens             0.177
# 2 Factive     Modus Tollens            0.182
# 3 nonFactive  Modus Ponens             0.168
# 4 nonFactive  Modus Tollens            0.168

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
# Z = 0.055815, p-value = 0.9555
# alternative hypothesis: true mu is not equal to 0

cliff.delta(VerbxMP.OE, VerbxMT.OE, conf.level = 0.95)
# Cliff's Delta
# 
# delta estimate: 0.03768595 (negligible)
# 95 percent confidence interval:
#     lower     upper 
# -0.148377  0.221172

# 2.5) Wilcoxon test on errors of omniscience for interaction between ProblemType and Focus
data %>% group_by(ProblemType, Focus) %>% summarise(OmniscienceError = mean(OmniscienceError))
#   ProblemType Focus       OmniscienceError
# 1 Factive     Agent                  0.227
# 2 Factive     Participant            0.132
# 3 nonFactive  Agent                  0.2  
# 4 nonFactive  Participant            0.136

VerbxFocus.OE <- data %>% group_by(ParticipantID, ProblemType, Focus) %>% 
    summarise(OmniscienceError = mean(OmniscienceError))

VerbxAgent.OE <- subset(VerbxFocus.OE, ProblemType=="nonFactive" & Focus=="Agent")$OmniscienceError - 
    subset(VerbxFocus.OE, ProblemType=="Factive" & Focus=="Agent")$OmniscienceError
VerbxPart.OE <- subset(VerbxFocus.OE, ProblemType=="nonFactive" & Focus=="Participant")$OmniscienceError - 
    subset(VerbxFocus.OE, ProblemType=="Factive" & Focus=="Participant")$OmniscienceError

wilcoxsign_test(VerbxAgent.OE ~ VerbxPart.OE)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = -0.84711, p-value = 0.3969
# alternative hypothesis: true mu is not equal to 0

cliff.delta(VerbxAgent.OE, VerbxPart.OE, conf.level = 0.95)
# Cliff's Delta
# delta estimate: -0.08826446 (negligible)
# 95 percent confidence interval:
#     lower      upper 
# -0.2650804  0.0942945

# 2.6) Wilcoxon test on errors of omniscience for interaction between Construction and Focus
data %>% group_by(Construction, Focus) %>% summarise(OmniscienceError = mean(OmniscienceError))
#   Construction  Focus       OmniscienceError
# 1 Modus Ponens  Agent                  0.209
# 2 Modus Ponens  Participant            0.136
# 3 Modus Tollens Agent                  0.218
# 4 Modus Tollens Participant            0.132

ConstructxFocus.OE <- data %>% group_by(ParticipantID, Construction, Focus) %>% 
    summarise(OmniscienceError = mean(OmniscienceError))

ConsctructxAgent.OE <- subset(ConstructxFocus.OE, Construction=="Modus Ponens" & Focus=="Agent")$OmniscienceError - 
    subset(ConstructxFocus.OE, Construction=="Modus Tollens" & Focus=="Agent")$OmniscienceError
ConstructxPart.OE <- subset(ConstructxFocus.OE, Construction=="Modus Ponens" & Focus=="Participant")$OmniscienceError - 
    subset(ConstructxFocus.OE, Construction=="Modus Tollens" & Focus=="Participant")$OmniscienceError

wilcoxsign_test(ConsctructxAgent.OE ~ ConstructxPart.OE)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = -0.54791, p-value = 0.5838
# alternative hypothesis: true mu is not equal to 0

cliff.delta(ConsctructxAgent.OE, ConstructxPart.OE, conf.level = 0.95)
# Cliff's Delta
# delta estimate: 0.006942149 (negligible)
# 95 percent confidence interval:
#     lower      upper 
# -0.1704945  0.1839427 

# # 2.8) Wilcoxon test comparing omniscience errors against 0
zeroes.OE <- data.frame(OmniscienceError = rep(0, length(data$OmniscienceError)))

wilcoxsign_test(data$OmniscienceError ~ zeroes.OE$OmniscienceError)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg)
# stratified by block
# Z = 12.369, p-value < 2.2e-16
# alternative hypothesis: true mu is not equal to 0

cliff.delta(data$OmniscienceError, zeroes.OE$OmniscienceError, conf.level = 0.95)
# Cliff's Delta
#
# delta estimate: 0.1738636 (small)
# 95 percent confidence interval:
#     lower     upper 
# 0.1486872 0.1988148 

# # 2.8) Wilcoxon test comparing omniscience errors vs accuracy
wilcoxsign_test(data$Acc ~ data$OmniscienceError)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = 13.764, p-value < 2.2e-16
# alternative hypothesis: true mu is not equal to 0

cliff.delta(data$Acc, data$OmniscienceError, conf.level = 0.95)
# Cliff's Delta
# delta estimate: 0.4 (medium)
# 95 percent confidence interval:
#     lower     upper 
# 0.3580101 0.4403753 

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
  geom_bar(stat="identity", position=position_dodge(), fill="darkorange1", size=.5, width=.75) +
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

# 3.2)  Omniscience Errors for agent
fig.agent.df <- group_by(data, Focus) %>% 
    summarise( 
        n=n(),
        mean=mean(OmniscienceError),
        sd=sd(OmniscienceError)
    ) %>%
    mutate( se=sd/sqrt(n))  %>%
    mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

fig.agent.df.temp <- fig.agent.df[,3:6]*100

fig.agent.df <- cbind(fig.agent.df[1:2], fig.agent.df.temp)

agent.bPlot <- ggplot(fig.agent.df, aes(y=mean, x=Focus)) +
    geom_bar(stat="identity", position=position_dodge(), fill="darkorange1", size=.5, width=.75) +
    geom_errorbar(aes(x=Focus, ymin=mean-ic, ymax=mean+ic), 
                  position=position_dodge(width=0.9), width=0.2, alpha=0.9, size=.5) +
    scale_y_continuous(name = "Omniscience Errors", limits = c(0, 105), breaks = c(0, 50, 100)) +
    scale_x_discrete(name = "", labels=c("Agent", "Participant")) +
    theme_minimal() + 
    theme(legend.title = element_blank(),
          legend.position=c(0.26, 0.72),
          legend.key.size = unit(.5, 'cm'),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          text = element_text(size = 20))

agent.bPlot

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
  geom_bar(stat="identity", position=position_dodge(), fill="darkorange1", size=.5, width=.75) +
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
sums <- colSums(data[c("OmniscienceError","Acc")])
sums <- round((sums/length(data$OmniscienceError))*100)
resp_sum_table <- data.frame(Column = names(sums), N = sums)
rownames(resp_sum_table) <- NULL

resps.barplot <- ggplot(resp_sum_table, aes(y=N, x=reorder(Column, N))) +
  geom_bar(stat="identity", position=position_dodge(), fill="darkorange1", size=.5, width=.75) +
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

# 3.4)  Violin plot of omniscience errors for verbs
fig.interaction.df <- aggregate(OmniscienceError ~ ParticipantID * Verb, data=data,
                  FUN=function(x){ round(mean(x), digits=2) })

interaction.vPlot <- ggplot(fig.interaction.df, aes(x=Verb, y=OmniscienceError)) +
  geom_violin(trim=TRUE, fill='white', adjust=1.8, size=.4) +
  geom_jitter(shape=16, alpha=.3, size=1.2, position=position_jitter(width=0.04, height=0.13)) +
  theme_bw(12) +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width=.1) +
  stat_summary(fun = "mean", geom = "point", size=2) +
  scale_x_discrete(name="", labels=c("Factive", "Non-Factive ")) +
  scale_y_continuous(name="Omniscience Errors", limits=c(-.3,1.3),
                     breaks=c(0.0, .50, 1.0)) +
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
