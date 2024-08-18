# --------------------------------------------------------------------------------------------------
# Knowing Thinking Experiment 17 Analysis
# --------------------------------------------------------------------------------------------------
# Subject pool: Amazon Mechanical Turk
# Date(s):      June 2022
# Design:       Branden Bio & Sunny Khemlani
# Code:         Branden Bio
# Experimenter:	Knexus Research Corporation
# --------------------------------------------------------------------------------------------------
# Contents:
#  X. Preprocessing; filter subjs by attention check; reformatting responses
#  1. Omnibus table
#  2. Analyses
#     2.1)  Wilcoxon test comparing accuracy between verbs
#     2.2)  Wilcoxon test comparing accuracy between epistemic vs fact
#     2.3)  Wilcoxon test comparing accuracy between conditional orders
#  3. Figures
#     3.1)  Average accuracy of each problem type
#     3.2)  Average accuracy of epistemic vs fact
#     3.3)  Average accuracy of each conditional order
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

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# --------------------------------------------------------------------------------------------------
# X. Preprocessing; filter subjs by attention check; reformatting responses
# --------------------------------------------------------------------------------------------------

rawData <- read.csv("./KT17-Aggregate.csv", header=T)
data.Prelim <- rawData
data.Prelim$ParticipantID <- as.factor(data.Prelim$ParticipantID)
data.Prelim$Answer <- gsub("(\\.).*","\\1",data.Prelim$Answer)
data.Prelim$Answer<- str_to_sentence(data.Prelim$Answer)
data.Prelim$Answer<- str_remove(data.Prelim$Answer, "\\.")
fullN <- length(unique(data.Prelim$ParticipantID))

data.Prelim$condC <- ifelse(grepl("then", data.Prelim$Premise1Content, fixed = TRUE), 
                            data.Prelim$Premise1Content, data.Prelim$Premise2Content)
data.Prelim$PropositionP <- gsub(".*then ", "", data.Prelim$condC)
data.Prelim.AC <- subset(data.Prelim, Cond == "AC")
data.Prelim.AC$correctAC <- paste("Believes that knows that", data.Prelim.AC$PropositionP, sep =" ")
data.Prelim.AC$PassedAC <- ifelse(data.Prelim.AC$Answer == data.Prelim.AC$correctAC, 1, 0)
missedAC <- c(as.character(subset(data.Prelim.AC, PassedAC==0)$ParticipantID))
failedAC <- missedAC[duplicated(missedAC)]
data.Prelim$FailedAC <- ifelse(data.Prelim$ParticipantID %in% failedAC, 1, 0)

data <- subset(data.Prelim, FailedAC==0)
useableN <- length(unique(data$ParticipantID))
subjLoss <- 1 - (useableN / fullN)

# Substantive responses from subjects that aren't nonsense
responseOptions <- list(`Q`               = "Q",
                        `X believes Q`    = "X believes that Q",
                        `X knows Q`       = "X knows that Q",
                        `Nothing follows` = "Nothing follows")

data$ResponseFormat <- str_replace(data$Answer, data$IndividX, "X")
data$ResponseFormat <- str_replace(data$ResponseFormat, regex(data$PropositionP, ignore_case = TRUE), "Q")
data$ResponseFormat <- gsub("\\..*","\\1",data$ResponseFormat)

data$AnswerType <- ifelse(data$ResponseFormat %in% responseOptions,
                          names(responseOptions)[match(data$ResponseFormat, responseOptions)], "Nonsense")

data$AnswerType[data$Cond == "AC"] <- "AC"

# Accuracy Rubric
# Cor = 1, Inc = 0
# ProblemType       | AnswerType      | Acc
# Believes-Believes | Nonsense        | Inc
# Believes-Believes | Nothing follows | Inc
# Believes-Believes | Q               | Inc
# Believes-Believes | X believes Q    | Cor
# Believes-Believes | X knows Q       | Inc
# 
# Believes-Knows    | Nonsense        | Inc
# Believes-Knows    | Nothing follows | Inc
# Believes-Knows    | Q               | Inc
# Believes-Knows    | X believes Q    | Cor
# Believes-Knows    | X knows Q       | Inc
# 
# Knows-Believes    | Nonsense        | Inc
# Knows-Believes    | Nothing follows | Inc
# Knows-Believes    | Q               | Inc
# Knows-Believes    | X believes Q    | Cor
# Knows-Believes    | X knows Q       | Inc
# 
# Knows-Knows       | Nonsense        | Inc
# Knows-Knows       | Nothing follows | Inc
# Knows-Knows       | Q               | Cor
# Knows-Knows       | X believes Q    | Cor
# Knows-Knows       | X knows Q       | Cor
# 
# Believes-NoEp     | Nonsense        | Inc
# Believes-NoEp     | Nothing follows | Cor
# Believes-NoEp     | Q               | Inc
# Believes-NoEp     | X believes Q    | Inc
# Believes-NoEp     | X knows Q       | Inc
# 
# Knows-NoEp        | Nonsense        | Inc
# Knows-NoEp        | Nothing follows | Inc
# Knows-NoEp        | Q               | Cor
# Knows-NoEp        | X believes Q    | Inc
# Knows-NoEp        | X knows Q       | Inc

data$Acc[data$AnswerType == "Nonsense"] <- 0
data$Acc[data$AnswerType == "Nothing follows"] <- 0
data$Acc[data$AnswerType == "Nothing follows" & data$ProblemType == "Believes-NoEp"] <- 1
data$Acc[data$AnswerType == "Q" & data$ProblemType != "Knows-Knows"] <- 0
data$Acc[data$AnswerType == "Q" & data$ProblemType == "Knows-Knows"] <- 1
data$Acc[data$AnswerType == "Q" & data$ProblemType == "Knows-NoEp"] <- 1
data$Acc[data$AnswerType == "X believes Q"] <- 1
data$Acc[data$AnswerType == "X believes Q" & data$ProblemType == "Believes-NoEp"] <- 0
data$Acc[data$AnswerType == "X believes Q" & data$ProblemType == "Knows-NoEp"] <- 0
data$Acc[data$AnswerType == "X knows Q" & data$ProblemType != "Knows-Knows"] <- 0
data$Acc[data$AnswerType == "X knows Q" & data$ProblemType == "Knows-Knows"] <- 1

data <- transform(data, Acc = as.numeric(Acc))

data.noAC <- subset(data, ProblemType != "Attn-Chk")

responses.Acc <- group_by(data.noAC, ProblemType) %>% 
  summarise(n=n(), mean=mean(Acc), sd=sd(Acc)) %>% 
  mutate(se=sd/sqrt(n)) %>% 
  mutate(ic=se * qt((1-0.05)/2 + .5, n-1))

resp.Acc <- responses.Acc[,3:6]*100

responses.Acc <- cbind(responses.Acc[1:2], resp.Acc)

responses.Acc2 <- group_by(data.noAC, EpistemicCategorical) %>% 
  summarise(n=n(), mean=mean(Acc), sd=sd(Acc)) %>%
  mutate(se=sd/sqrt(n)) %>%
  mutate(ic=se * qt((1-0.05)/2 + .5, n-1))

resp.Acc2 <- responses.Acc2[,3:6]*100

responses.Acc2 <- cbind(responses.Acc2[1:2], resp.Acc2)

responses.Acc3 <- group_by(data.noAC, ConditionalOrder) %>% 
  summarise(n=n(), mean=mean(Acc), sd=sd(Acc)) %>%
  mutate(se=sd/sqrt(n)) %>%
  mutate(ic=se * qt((1-0.05)/2 + .5, n-1))

resp.Acc3 <- responses.Acc3[,3:6]*100

responses.Acc3 <- cbind(responses.Acc3[1:2], resp.Acc3)

# --------------------------------------------------------------------------------------------------
# 1. Omnibus table
# --------------------------------------------------------------------------------------------------

data.dummy <- subset(data, ProblemType != "Attn-Chk") %>% mutate(var = 1) %>% 
  spread(AnswerType, var, fill = 0, sep = "_") %>% 
  left_join(data) %>% 
  select(ProblemType, AnswerType, ProblemNumber, ParticipantID,
         `AnswerType_Nonsense`, `AnswerType_Nothing follows`,
         `AnswerType_Q`, `AnswerType_X believes Q`, `AnswerType_X knows Q`) %>%
  gather(AnswerType, Response, AnswerType_Nonsense:`AnswerType_X knows Q`)

responses.table <- group_by(data.dummy, ProblemType, AnswerType) %>% summarise(Response = mean(Response))

#       ProblemType      AnswerType                  Response
# 1  Believes-Believes        AnswerType_Nonsense 0.000000000
# 2  Believes-Believes AnswerType_Nothing follows 0.035714286
# 3  Believes-Believes               AnswerType_Q 0.047619048
# 4  Believes-Believes    AnswerType_X believes Q 0.892857143
# 5  Believes-Believes       AnswerType_X knows Q 0.023809524
# 6     Believes-Knows        AnswerType_Nonsense 0.000000000
# 7     Believes-Knows AnswerType_Nothing follows 0.023809524
# 8     Believes-Knows               AnswerType_Q 0.059523810
# 9     Believes-Knows    AnswerType_X believes Q 0.773809524
# 10    Believes-Knows       AnswerType_X knows Q 0.142857143
# 11     Believes-NoEp        AnswerType_Nonsense 0.000000000
# 12     Believes-NoEp AnswerType_Nothing follows 0.053571429
# 13     Believes-NoEp               AnswerType_Q 0.029761905
# 14     Believes-NoEp    AnswerType_X believes Q 0.863095238
# 15     Believes-NoEp       AnswerType_X knows Q 0.053571429
# 16    Knows-Believes        AnswerType_Nonsense 0.000000000
# 17    Knows-Believes AnswerType_Nothing follows 0.000000000
# 18    Knows-Believes               AnswerType_Q 0.023809524
# 19    Knows-Believes    AnswerType_X believes Q 0.785714286
# 20    Knows-Believes       AnswerType_X knows Q 0.190476190
# 21       Knows-Knows        AnswerType_Nonsense 0.000000000
# 22       Knows-Knows AnswerType_Nothing follows 0.023809524
# 23       Knows-Knows               AnswerType_Q 0.166666667
# 24       Knows-Knows    AnswerType_X believes Q 0.023809524
# 25       Knows-Knows       AnswerType_X knows Q 0.785714286
# 26        Knows-NoEp        AnswerType_Nonsense 0.005952381
# 27        Knows-NoEp AnswerType_Nothing follows 0.035714286
# 28        Knows-NoEp               AnswerType_Q 0.178571429
# 29        Knows-NoEp    AnswerType_X believes Q 0.053571429
# 30        Knows-NoEp       AnswerType_X knows Q 0.726190476


responses.table <- group_by(data.dummy, ProblemType, AnswerType) %>% 
  summarise(n=n(), mean=mean(Response), sd=sd(Response)) %>%
  mutate(se=sd/sqrt(n))  %>%
  mutate(ic=se * qt((1-0.05)/2 + .5, n-1))

responses.table$ProblemType <- factor(responses.table$ProblemType, 
                                      levels=c("Knows-Knows", "Believes-Knows", "Knows-Believes", 
                                      "Believes-Believes", "Believes-NoEp", "Knows-NoEp"))

# --------------------------------------------------------------------------------------------------
# 2. Analyses
# --------------------------------------------------------------------------------------------------

# 2.1) Wilcoxon test comparing accuracy between verbs
data %>% group_by(ProblemType) %>% summarise(Acc = mean(Acc))
#   ProblemType           Acc
# 2 Believes-Believes  0.893 
# 6 Knows-Knows        0.976 

BB_acc <- data %>% filter(ProblemType=="Believes-Believes")
BB_acc <- aggregate(Acc ~ ParticipantID, data=BB_acc, FUN=mean)

KK_acc <- data %>% filter(ProblemType=="Knows-Knows")
KK_acc <- aggregate(Acc ~ ParticipantID, data=KK_acc, FUN=mean)

wilcoxsign_test(BB_acc$Acc ~ KK_acc$Acc)
# Z = -2.4485, p-value = 0.01434

cliff.delta(BB_acc$Acc,KK_acc$Acc,conf.level = 0.95)
# delta estimate: -0.1213152 (negligible)

#   ProblemType           Acc
# 4 Believes-NoEp      0.0536
# 7 Knows-NoEp         0.179 

BNoEp_acc <- data %>% filter(ProblemType=="Believes-NoEp")
BNoEp_acc <- aggregate(Acc ~ ParticipantID, data=BNoEp_acc, FUN=mean)

KNoEp_acc <- data %>% filter(ProblemType=="Knows-NoEp")
KNoEp_acc <- aggregate(Acc ~ ParticipantID, data=KNoEp_acc, FUN=mean)

wilcoxsign_test(BNoEp_acc$Acc ~ KNoEp_acc$Acc)
# Z = -2.2081, p-value = 0.02724

cliff.delta(BNoEp_acc$Acc,KNoEp_acc$Acc,conf.level = 0.95)
# delta estimate: -0.2148526 (small)

# 2.2)  Wilcoxon test comparing accuracy between epistemic vs no epistemic problems
ep_acc <- data %>% filter(EpistemicCategorical=="Y")
ep_acc <- aggregate(Acc ~ ParticipantID, data=ep_acc, FUN=mean)

noEp_acc <- data %>% filter(EpistemicCategorical=="N")
noEp_acc <- aggregate(Acc ~ ParticipantID, data=noEp_acc, FUN=mean)

wilcoxsign_test(ep_acc$Acc ~ noEp_acc$Acc)
# Z = 5.6428, p-value = 1.673e-08

cliff.delta(ep_acc$Acc,noEp_acc$Acc,conf.level = 0.95)
# delta estimate: 0.9756236 (large)

# 2.3)  Wilcoxon test comparing accuracy between conditional orders
CF_acc <- data %>% filter(ConditionalOrder=="Conditional First")
CF_acc <- aggregate(Acc ~ ParticipantID, data=CF_acc, FUN=mean)

CS_acc <- data %>% filter(ConditionalOrder=="Conditional Second")
CS_acc <- aggregate(Acc ~ ParticipantID, data=CS_acc, FUN=mean)

wilcoxsign_test(CF_acc$Acc ~ CS_acc$Acc)
# Z = 1.9287, p-value = 0.05377

cliff.delta(CF_acc$Acc,CS_acc$Acc,conf.level = 0.95)
# delta estimate: 0.1689342 (small)

# --------------------------------------------------------------------------------------------------
# 3. Figures
# --------------------------------------------------------------------------------------------------

# 3.1)  Average accuracy of each problem type
accxprob.barplot <- ggplot(responses.Acc, aes(y=mean, x=ProblemType)) +
  geom_bar(stat="identity", position=position_dodge(), fill="darkorange2", size=.5) +
  geom_errorbar(aes(x=ProblemType, ymin=mean-ic, ymax=mean+ic), 
                position=position_dodge(width=0.9), width=0.4, alpha=0.9, size=.5) +
  scale_y_continuous(name = "Accuracy", limits = c(0, 105), breaks = c(0, 50, 100)) +
  scale_x_discrete(name = "", labels=c("Believes-Believes", "Believes-Knows", "Believes-NoEp", "Knows-Believes", "Knows-Knows", "Knows-NoEp")) +
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position=c(0.26, 0.72),
        legend.key.size = unit(.5, 'cm'),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1))

accxprob.barplot

# 3.2)  Average accuracy of epistemic vs fact
accxep.barplot <- ggplot(responses.Acc2, aes(y=mean, x=EpistemicCategorical)) +
  geom_bar(stat="identity", position=position_dodge(), fill="cadetblue4", size=.5) +
  geom_errorbar(aes(x=EpistemicCategorical, ymin=mean-ic, ymax=mean+ic), 
                position=position_dodge(width=0.9), width=0.4, alpha=0.9, size=.5) +
  scale_y_continuous(name = "Accuracy", limits = c(0, 105), breaks = c(0, 50, 100)) +
  scale_x_discrete(name = "", labels=c("Fact", "Epistemic Statement")) +
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position=c(0.26, 0.72),
        legend.key.size = unit(.5, 'cm'),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

accxep.barplot

# 3.3) Average accuracy of each conditional order
accxCO.barplot <- ggplot(responses.Acc3, aes(y=mean, x=ConditionalOrder)) +
  geom_bar(stat="identity", position=position_dodge(), fill="dodgerblue4", size=.5) +
  geom_errorbar(aes(x=ConditionalOrder, ymin=mean-ic, ymax=mean+ic), 
                position=position_dodge(width=0.9), width=0.4, alpha=0.9, size=.5) +
  scale_y_continuous(name = "Accuracy", limits = c(0, 105), breaks = c(0, 50, 100)) +
  scale_x_discrete(name = "", labels=c("Conditional First", "Conditional Second")) +
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position=c(0.26, 0.72),
        legend.key.size = unit(.5, 'cm'),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

accxCO.barplot
