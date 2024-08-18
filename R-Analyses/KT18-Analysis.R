# --------------------------------------------------------------------------------------------------
# Knowing Thinking Experiment 18 Analysis
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
#  2. Wilcoxon tests comparing proportion of yes responses in verb by modulation
#  3. Wilcoxon tests comparing proportion of yes responses in verb by condition
#  4. Wilcoxon test comparing proportion of yes responses in conditions
#  5. Wilcoxon test comparing proportion of yes responses between verbs
#  6. Wilcoxon test comparing proportion of yes responses in modulated vs unmodulated
#  7. Wilcoxon test comparing reaction times in modulated vs unmodulated
#  8. Wilcoxon test comparing reaction times in believes vs knows
#  9. Figures
#     9.1) Proportion of yes responses for condition by modulation
#     9.2) Proportion of yes responses for verb vs modulation
#     9.3) Proportion of yes responses for verb vs condition
#     9.4) Proportion of yes responses for conditions
#     9.5) Proportion of yes responses for verbs
#     9.6) Proportion of yes responses for modulation

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
rawData <- read.csv("./KT18-Aggregate.csv", header=T)
rawData$ParticipantID <- as.factor(rawData$ParticipantID)
rawData$Cond <- factor(rawData$Cond,levels = c("AffirmConsequent", "DenyAntecedent", "AC"))
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
finalN <- length(unique(data$ParticipantID))
subjLoss <- 1 - (useableN / fullN)

demoData       <- read.csv("./KT18-Demographics.csv", header=T)
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

data.frame(fullN, finalN, meanAge, minAge, maxAge, femaleCount, 
           maleCount, otherCount, preferNotCount, nologicCount, logicCount)

#   fullN finalN meanAge minAge maxAge femaleCount maleCount otherCount preferNotCount nologicCount logicCount
# 1    92     88   39.65     20     71          36        51          1              0           66         22

# --------------------------------------------------------------------------------------------------
# 1. Wilcoxon test on "yes" responses for modulation
# --------------------------------------------------------------------------------------------------

data %>% group_by(Modulation) %>% summarise(AnswerY = mean(AnswerY))

# # A tibble: 2 × 2
#   Modulation  AnswerY
#   <fct>         <dbl>
# 1 Modulated     0.713
# 2 Unmodulated   0.261

test <- data %>% group_by(ParticipantID, Modulation) %>% summarise(AnswerY = mean(AnswerY))

wilcoxsign_test(subset(test, Modulation=="Modulated")$AnswerY ~
                subset(test, Modulation=="Unmodulated")$AnswerY)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test
#
# data:  y by x (pos, neg)
# 	 stratified by block
# Z = 6.9432, p-value = 3.833e-12
# alternative hypothesis: true mu is not equal to 0

cliff.delta(subset(test, Modulation=="Modulated")$AnswerY,
            subset(test, Modulation=="Unmodulated")$AnswerY)

# Cliff's Delta
#
# delta estimate: 0.6526343 (large)
# 95 percent confidence interval:
#     lower     upper
# 0.5122584 0.7590462

# --------------------------------------------------------------------------------------------------
# 2. Wilcoxon test on "yes" responses for epistemic verb
# --------------------------------------------------------------------------------------------------

data %>% group_by(Verb) %>% summarise(AnswerY = mean(AnswerY))

# # A tibble: 2 × 2
#   Verb     AnswerY
#   <fct>      <dbl>
# 1 Believes   0.438
# 2 Knows      0.537

test <- data %>% group_by(ParticipantID, Verb) %>% summarise(AnswerY = mean(AnswerY))

wilcoxsign_test(subset(test, Verb=="Believes")$AnswerY ~
                subset(test, Verb=="Knows")$AnswerY)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test
#
# data:  y by x (pos, neg)
# 	 stratified by block
# Z = -2.8019, p-value = 0.00508
# alternative hypothesis: true mu is not equal to 0

cliff.delta(subset(test, Verb=="Believes")$AnswerY,
            subset(test, Verb=="Knows")$AnswerY)

# Cliff's Delta
#
# delta estimate: -0.1960227 (small)
# 95 percent confidence interval:
#       lower       upper
# -0.34874443 -0.03316015

# --------------------------------------------------------------------------------------------------
# 3. Wilcoxon test on "yes" responses for problem type
# --------------------------------------------------------------------------------------------------

data %>% group_by(Cond) %>% summarise(AnswerY = mean(AnswerY))

# # A tibble: 2 × 2
#   Cond             AnswerY
#   <fct>              <dbl>
# 1 AffirmConsequent   0.528
# 2 DenyAntecedent     0.446

test <- data %>% group_by(ParticipantID, Cond) %>% summarise(AnswerY = mean(AnswerY))

wilcoxsign_test(subset(test, Cond=="AffirmConsequent")$AnswerY ~
                subset(test, Cond=="DenyAntecedent")$AnswerY)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test
#
# data:  y by x (pos, neg)
# 	 stratified by block
# Z = 2.3054, p-value = 0.02114
# alternative hypothesis: true mu is not equal to 0

cliff.delta(subset(test, Cond=="AffirmConsequent")$AnswerY,
            subset(test, Cond=="DenyAntecedent")$AnswerY)

# Cliff's Delta
#
# delta estimate: 0.163094 (small)
# 95 percent confidence interval:
#        lower        upper
# 0.0008763749 0.3169482206

# --------------------------------------------------------------------------------------------------
# 4. Wilcoxon test on "yes" responses for interaction between modulation and verb
# --------------------------------------------------------------------------------------------------

data %>% group_by(Modulation, Verb) %>% summarise(AnswerY = mean(AnswerY))

# # A tibble: 4 × 3
# # Groups:   Modulation [2]
#   Modulation  Verb     AnswerY
#   <fct>       <fct>      <dbl>
# 1 Modulated   Believes   0.636
# 2 Modulated   Knows      0.790
# 3 Unmodulated Believes   0.239
# 4 Unmodulated Knows      0.284

test <- data %>% group_by(ParticipantID, Modulation, Verb) %>% summarise(AnswerY = mean(AnswerY))

A <- subset(test, Modulation=="Modulated" & Verb=="Believes")$AnswerY -
     subset(test, Modulation=="Unmodulated" & Verb=="Believes")$AnswerY
B <- subset(test, Modulation=="Modulated" & Verb=="Knows")$AnswerY -
     subset(test, Modulation=="Unmodulated" & Verb=="Knows")$AnswerY

wilcoxsign_test(A ~ B)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test
#
# data:  y by x (pos, neg)
# 	 stratified by block
# Z = -1.8517, p-value = 0.06407
# alternative hypothesis: true mu is not equal to 0

cliff.delta(A, B)

# Cliff's Delta
#
# delta estimate: -0.1278409 (negligible)
# 95 percent confidence interval:
#       lower       upper
# -0.28529974  0.03633746

# --------------------------------------------------------------------------------------------------
# 5. Wilcoxon test on "yes" responses for interaction between modulation and problem type
# --------------------------------------------------------------------------------------------------

data %>% group_by(Modulation, Cond) %>% summarise(AnswerY = mean(AnswerY))

#   Modulation  Cond             AnswerY
#   <fct>       <fct>              <dbl>
# 1 Modulated   AffirmConsequent   0.778
# 2 Modulated   DenyAntecedent     0.648
# 3 Unmodulated AffirmConsequent   0.278
# 4 Unmodulated DenyAntecedent     0.244

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
# Z = 1.3731, p-value = 0.1697
# alternative hypothesis: true mu is not equal to 0

cliff.delta(A, B)

# Cliff's Delta
#
# delta estimate: 0.08910124 (negligible)
# 95 percent confidence interval:
#       lower       upper
# -0.07471391  0.24823377

# --------------------------------------------------------------------------------------------------
# 6. Wilcoxon test on "yes" responses for interaction between conditional order and problem type
# --------------------------------------------------------------------------------------------------

data %>% group_by(Verb, Cond) %>% summarise(AnswerY = mean(AnswerY))

# # Groups:   Verb [2]
#   Verb     Cond             AnswerY
#   <fct>    <fct>              <dbl>
# 1 Believes AffirmConsequent   0.483
# 2 Believes DenyAntecedent     0.392
# 3 Knows    AffirmConsequent   0.574
# 4 Knows    DenyAntecedent     0.5

test <- data %>% group_by(ParticipantID, Verb, Cond) %>% summarise(AnswerY = mean(AnswerY))

A <- subset(test, Verb=="Believes" & Cond=="AffirmConsequent")$AnswerY -
     subset(test, Verb=="Knows" & Cond=="AffirmConsequent")$AnswerY
B <- subset(test, Verb=="Believes" & Cond=="DenyAntecedent")$AnswerY -
     subset(test, Verb=="Knows" & Cond=="DenyAntecedent")$AnswerY

wilcoxsign_test(A ~ B)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test
#
# data:  y by x (pos, neg)
# 	 stratified by block
# Z = 0.37091, p-value = 0.7107
# alternative hypothesis: true mu is not equal to 0

cliff.delta(A, B)

# Cliff's Delta
#
# delta estimate: 0.04145145 (negligible)
# 95 percent confidence interval:
#      lower      upper
# -0.1066988  0.1878011

# --------------------------------------------------------------------------------------------------
# 7. Wilcoxon test on "yes" responses for three-way interaction
# --------------------------------------------------------------------------------------------------

data %>% group_by(Modulation, Verb, Cond) %>% summarise(AnswerY = mean(AnswerY))

#   Modulation  Verb     Cond             AnswerY
#   <fct>       <fct>    <fct>              <dbl>
# 1 Modulated   Believes AffirmConsequent   0.705
# 2 Modulated   Believes DenyAntecedent     0.568
# 3 Modulated   Knows    AffirmConsequent   0.852
# 4 Modulated   Knows    DenyAntecedent     0.727
# 5 Unmodulated Believes AffirmConsequent   0.261
# 6 Unmodulated Believes DenyAntecedent     0.216
# 7 Unmodulated Knows    AffirmConsequent   0.295
# 8 Unmodulated Knows    DenyAntecedent     0.273

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

wilcoxsign_test(A ~ B)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test
#
# data:  y by x (pos, neg)
# 	 stratified by block
# Z = -0.1351, p-value = 0.8925
# alternative hypothesis: true mu is not equal to 0

cliff.delta(A, B)

# Cliff's Delta
#
# delta estimate: -0.01497934 (negligible)
# 95 percent confidence interval:
#      lower      upper
# -0.1590330  0.1296988

# --------------------------------------------------------------------------------------------------
# 8. Planned comparisons
# --------------------------------------------------------------------------------------------------

test <- data %>% group_by(ParticipantID, Modulation, Verb, Cond) %>% summarise(AnswerY = mean(AnswerY))

A <- subset(test, Modulation == "Modulated" & Verb=="Believes")$AnswerY
B <- subset(test, Modulation == "Modulated" & Verb=="Knows")$AnswerY

wilcoxsign_test(A ~ B)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test
#
# data:  y by x (pos, neg)
# 	 stratified by block
# Z = -3.7808, p-value = 0.0001564
# alternative hypothesis: true mu is not equal to 0

cliff.delta(A, B)

# Cliff's Delta
#
# delta estimate: -0.1534091 (small)
# 95 percent confidence interval:
#       lower       upper
# -0.24536006 -0.05872241

# --------------------------------------------------------------------------------------------------
# 9) Figures
# --------------------------------------------------------------------------------------------------

# 9.1) Proportion of yes responses for condition by modulation
condxmod.barplot <- ggplot(response.table.condxmod, aes(fill=Modulation, y=mean, x=Cond)) +
  geom_bar(stat="identity", position=position_dodge(), color="black", size=.5) +
  geom_errorbar(aes(x=Cond, ymin=mean-ic, ymax=mean+ic), 
                position=position_dodge(width=0.9), width=0.4, colour="grey20", alpha=0.9, size=.5) +
  scale_y_continuous(name = "Proportion of responses", limits = c(0, 1), breaks = c(0, .5, 1.0)) +
  scale_x_discrete(name = "", labels=c("Affirming the Consequent", "Denying the Antecedent")) +
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position=c(0.9, 0.9),
        legend.key.size = unit(.5, 'cm'),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

condxmod.barplot

response.table.verbxmod <- data %>% group_by(Modulation, Verb) %>% summarise(mean = mean(AnswerY))

# 9.2) Proportion of yes responses for verb vs modulation
verbxmod.barplot <- ggplot(response.table.verbxmod, aes(fill=Modulation, y=mean, x=Verb)) +
  geom_bar(stat="identity", position=position_dodge(), color="black", size=.5) +
  #geom_errorbar(aes(x=Verb, ymin=mean-ic, ymax=mean+ic), 
  #              position=position_dodge(width=0.9), width=0.4, colour="grey20", alpha=0.9, size=.5) +
  scale_y_continuous(name = "Proportion of responses", limits = c(0, 1), breaks = c(0, .5, 1.0)) +
  scale_x_discrete(name = "", labels=c("Believes", "Knows")) +
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position=c(0.9, 0.9),
        legend.key.size = unit(.5, 'cm'),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

verbxmod.barplot

# 9.3) Proportion of yes responses for verb vs condition
verbxcond.barplot <- ggplot(response.table.verbxcond, aes(fill=Verb, y=mean, x=Cond)) +
  geom_bar(stat="identity", position=position_dodge(), color="black", size=.5) +
  geom_errorbar(aes(x=Cond, ymin=mean-ic, ymax=mean+ic), 
                position=position_dodge(width=0.9), width=0.4, colour="grey20", alpha=0.9, size=.5) +
  scale_y_continuous(name = "Proportion of responses", limits = c(0, 1), breaks = c(0, .5, 1.0)) +
  scale_x_discrete(name = "", labels=c("Affirm Consequent", "Deny Antecedent")) +
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position=c(0.9, 0.9),
        legend.key.size = unit(.5, 'cm'),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

verbxcond.barplot

# 9.4) Proportion of yes responses for conditions
cond.barplot <- ggplot(response.table.cond, aes(y=mean, x=Cond)) +
  geom_bar(stat="identity", position=position_dodge(), color="black", size=.5) +
  geom_errorbar(aes(x=Cond, ymin=mean-ic, ymax=mean+ic), 
                position=position_dodge(width=0.9), width=0.4, colour="grey20", alpha=0.9, size=.5) +
  scale_y_continuous(name = "Proportion of responses") +
  scale_x_discrete(name = "", labels=c("Affirm Consequent", "Deny Antecedent")) +
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position=c(0.9, 0.9),
        legend.key.size = unit(.5, 'cm'),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

cond.barplot

# 9.5) Proportion of yes responses for verbs
verb.barplot <- ggplot(response.table.verb, aes(y=mean, x=Verb)) +
  geom_bar(stat="identity", position=position_dodge(), color="black", size=.5) +
  geom_errorbar(aes(x=Verb, ymin=mean-ic, ymax=mean+ic), 
                position=position_dodge(width=0.9), width=0.4, colour="grey20", alpha=0.9, size=.5) +
  scale_y_continuous(name = "Proportion of responses") +
  scale_x_discrete(name = "", labels=c("Believes", "Knows")) +
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position=c(0.9, 0.9),
        legend.key.size = unit(.5, 'cm'),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

verb.barplot

# 9.6) Proportion of yes responses for modulation
mod.barplot <- ggplot(response.table.mod, aes(y=mean, x=Modulation)) +
  geom_bar(stat="identity", position=position_dodge(), color="black", size=.5) +
  geom_errorbar(aes(x=Modulation, ymin=mean-ic, ymax=mean+ic), 
                position=position_dodge(width=0.9), width=0.4, colour="grey20", alpha=0.9, size=.5) +
  scale_y_continuous(name = "Proportion of responses") +
  scale_x_discrete(name = "", labels=c("Modulated", "Unmodulated")) +
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position=c(0.9, 0.9),
        legend.key.size = unit(.5, 'cm'),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

mod.barplot

write.csv(finalN, "KT18-useable-subs.csv", row.names = FALSE)


