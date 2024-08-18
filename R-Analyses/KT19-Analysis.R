# --------------------------------------------------------------------------------------------------
# Knowing Thinking Experiment 19 Analysis
# --------------------------------------------------------------------------------------------------
# Subject pool: Amazon Mechanical Turk
# Date(s):      July 2022
# Design:       Branden Bio & Sunny Khemlani
# Code:         Branden Bio
# Experimenter:	Knexus Research Corporation
# --------------------------------------------------------------------------------------------------
# Contents:
#  X. Preprocessing; filter subjs by attention check; reformatting responses
#  X. Demographics summary
#  1. Analyses
#     1.1)   Wilcoxon test comparing accuracy between verbs
#     1.2)   Wilcoxon test comparing accuracy between epistemic vs fact problems
#     1.3)   Wilcoxon test comparing accuracy for interaction between verb and knowledge state
#     1.4)   Wilcoxon test comparing omniscience errors in fact only
#     1.5)   Wilcoxon test comparing accuracy in verbs for epistemic only
#  2. Figures
#     2.1)   Violin plot of omniscience errors for each verb [found as fig.1 in Omniscience Errors CogSci paper]
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

rawData <- read.csv("./KT19-Aggregate.csv", header=T)
data.Prelim <- rawData
data.Prelim$ParticipantID <- as.factor(data.Prelim$ParticipantID)
data.Prelim$Answer <- gsub("(\\.).*","\\1",data.Prelim$Answer)
data.Prelim$Answer <- str_to_sentence(data.Prelim$Answer)
data.Prelim$Answer <- str_remove(data.Prelim$Answer, "\\.")
allSubs <- unique(rawData$ParticipantID)
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
useableSubs  <- unique(data$ParticipantID)
finalN       <- length(unique(data$ParticipantID))
excludedN    <- fullN - finalN
subjLoss     <- round(1 - (finalN / fullN),2)
debrief.out  <- data.debrief %>% select(ParticipantID, ProblemNumber, FreeResp, Answer)

debrief.filename <- "KT19-debrief-resps.csv"
if (file.exists(debrief.filename) == FALSE) {
  write.csv(debrief.out, debrief.filename, row.names = FALSE)
} else {
  print(paste(debrief.filename, "was not written because it already exists"))
}

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

data$Verb <- ifelse(grepl("Believes", data$ProblemType), "Believes", 
                    ifelse(grepl("Knows", data$ProblemType), "Knows", "Believes"))

data$Verb <- as.factor(data$Verb)

# Assign error of omniscience scores
data$OmniscienceError <- 0
data$OmniscienceError[data$AnswerType == "X believes Q" & data$ProblemType == "Believes-Fact"] <- 1
data$OmniscienceError[data$AnswerType == "X knows Q" & data$ProblemType == "Believes-Fact"] <- 1
data$OmniscienceError[data$AnswerType == "X believes Q" & data$ProblemType == "Knows-Fact"] <- 1
data$OmniscienceError[data$AnswerType == "X knows Q" & data$ProblemType == "Knows-Fact"] <- 1

data <- transform(data, OmniscienceError = as.numeric(OmniscienceError))

# --------------------------------------------------------------------------------------------------
# X. Demographics summary
# --------------------------------------------------------------------------------------------------

demoSummary <- function(keepers) {
  # Import & subset data
  demoData <- read.csv("./KT19-Demographics.csv", header=T)
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
#    75     59        16   41.39     20     70          28        29          0              2

# Accuracy Rubric
# Cor = 1, Inc = 0
# ProblemType       | AnswerType      | Acc
# Believes-Believes | Nonsense        | Inc
# Believes-Believes | Nothing follows | Inc
# Believes-Believes | Q               | Inc
# Believes-Believes | X believes Q    | Cor
# Believes-Believes | X knows Q       | Inc
# 
# Knows-Knows       | Nonsense        | Inc
# Knows-Knows       | Nothing follows | Inc
# Knows-Knows       | Q               | Cor
# Knows-Knows       | X believes Q    | Cor
# Knows-Knows       | X knows Q       | Cor
# 
# Believes-Fact     | Nonsense        | Inc
# Believes-Fact     | Nothing follows | Cor
# Believes-Fact     | Q               | Inc
# Believes-Fact     | X believes Q    | Inc
# Believes-Fact     | X knows Q       | Inc
# 
# Knows-Fact        | Nonsense        | Inc
# Knows-Fact        | Nothing follows | Inc
# Knows-Fact        | Q               | Cor
# Knows-Fact        | X believes Q    | Inc
# Knows-Fact        | X knows Q       | Inc

data$Acc[data$AnswerType == "Nonsense"] <- 0
data$Acc[data$AnswerType == "Nothing follows"] <- 0
data$Acc[data$AnswerType == "Nothing follows" & data$ProblemType == "Believes-Fact"] <- 1
data$Acc[data$AnswerType == "Q" & data$ProblemType != "Knows-Knows"] <- 0
data$Acc[data$AnswerType == "Q" & data$ProblemType == "Knows-Knows"] <- 1
data$Acc[data$AnswerType == "Q" & data$ProblemType == "Knows-Fact"] <- 1
data$Acc[data$AnswerType == "X believes Q"] <- 1
data$Acc[data$AnswerType == "X believes Q" & data$ProblemType == "Believes-Fact"] <- 0
data$Acc[data$AnswerType == "X believes Q" & data$ProblemType == "Knows-Fact"] <- 0
data$Acc[data$AnswerType == "X knows Q" & data$ProblemType != "Knows-Knows"] <- 0
data$Acc[data$AnswerType == "X knows Q" & data$ProblemType == "Knows-Knows"] <- 1

data <- transform(data, Acc = as.numeric(Acc))

data <- subset(data, ProblemType != "Attn-Chk")

fact <- data %>% filter(KnowledgeState == "Fact")
epi <- data %>% filter(KnowledgeState == "Epistemic")

# --------------------------------------------------------------------------------------------------
# Analyses
# --------------------------------------------------------------------------------------------------

# 2.1) Wilcoxon test comparing accuracy between verbs
data %>% group_by(Verb) %>% summarise(Acc = mean(Acc))
#   Verb        Acc
# 1 Believes  0.436
# 2 Knows     0.564

B_acc <- data %>% filter(Verb=="Believes")
B_acc <- aggregate(Acc ~ ParticipantID, data=B_acc, FUN=mean)

K_acc <- data %>% filter(Verb=="Knows")
K_acc <- aggregate(Acc ~ ParticipantID, data=K_acc, FUN=mean)

wilcoxsign_test(B_acc$Acc ~ K_acc$Acc)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = -3.2239, p-value = 0.001265
# alternative hypothesis: true mu is not equal to 0

cliff.delta(B_acc$Acc,K_acc$Acc,conf.level = 0.95)
# delta estimate: -0.3016375 (small)

# 2.2) Wilcoxon test comparing accuracy between epistemic vs fact problems
data %>% group_by(KnowledgeState) %>% summarise(Acc = mean(Acc))
# KnowledgeState    Acc
# 1 Epistemic      0.915 
# 2 Fact           0.0847

ep_acc <- data %>% filter(KnowledgeState=="Epistemic")
ep_acc <- aggregate(Acc ~ ParticipantID, data=ep_acc, FUN=mean)

Fact_acc <- data %>% filter(KnowledgeState=="Fact")
Fact_acc <- aggregate(Acc ~ ParticipantID, data=Fact_acc, FUN=mean)

wilcoxsign_test(ep_acc$Acc ~ Fact_acc$Acc)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = 6.8702, p-value = 6.41e-12
# alternative hypothesis: true mu is not equal to 0

cliff.delta(ep_acc$Acc,Fact_acc$Acc,conf.level = 0.95)
# delta estimate: 0.9896581 (large)

# 2.3) Wilcoxon test on accuracy for interaction between verb and knowledge state
data %>% group_by(Verb, KnowledgeState) %>% summarise(Acc = mean(Acc))
#   Verb     KnowledgeState    Acc
# 1 Believes Epistemic      0.856 
# 2 Believes Fact           0.0169
# 3 Knows    Epistemic      0.975 
# 4 Knows    Fact           0.153 

VerbxKnowl_Acc <- data %>% group_by(ParticipantID, Verb, KnowledgeState) %>% summarise(Acc = mean(Acc))

Verb_Epist_Acc <- subset(VerbxKnowl_Acc, Verb=="Believes" & KnowledgeState=="Epistemic")$Acc - 
  subset(VerbxKnowl_Acc, Verb=="Knows" & KnowledgeState=="Epistemic")$Acc
Verb_Fact_Acc <- subset(VerbxKnowl_Acc, Verb=="Believes" & KnowledgeState=="Fact")$Acc - 
  subset(VerbxKnowl_Acc, Verb=="Knows" & KnowledgeState=="Fact")$Acc

wilcoxsign_test(Verb_Epist_Acc ~ Verb_Fact_Acc)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = 0.25587, p-value = 0.798
# alternative hypothesis: true mu is not equal to 0

cliff.delta(Verb_Epist_Acc, Verb_Fact_Acc, conf.level = 0.95)
# delta estimate: 0.004021833 (negligible)

# 2.4) Wilcoxon test comparing omniscience errors in fact only
fact %>% group_by(Verb) %>% summarise(OmniscienceError = mean(OmniscienceError))
# Verb     OmniscienceError
# 1 Believes            0.924
# 2 Knows               0.805

B_omni <- fact %>% filter(Verb=="Believes")
B_omni <- aggregate(OmniscienceError ~ ParticipantID, data=B_omni, FUN=mean)

K_omni <- fact %>% filter(Verb=="Knows")
K_omni <- aggregate(OmniscienceError ~ ParticipantID, data=K_omni, FUN=mean)

wilcoxsign_test(B_omni$OmniscienceError ~ K_omni$OmniscienceError)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = 2.9043, p-value = 0.003681
# alternative hypothesis: true mu is not equal to 0

cliff.delta(B_omni$OmniscienceError,K_omni$OmniscienceError,conf.level = 0.95)
# delta estimate: 0.1534042 (small)

# 2.5) Wilcoxon test comparing accuracy in verbs for epistemic only
epi %>% group_by(Verb) %>% summarise(Acc = mean(Acc))
# Verb       Acc
# 1 Believes 0.856
# 2 Knows    0.975

B_epi <- epi %>% filter(Verb=="Believes")
B_epi <- aggregate(Acc ~ ParticipantID, data=B_epi, FUN=mean)

K_epi <- epi %>% filter(Verb=="Knows")
K_epi <- aggregate(Acc ~ ParticipantID, data=K_epi, FUN=mean)

wilcoxsign_test(B_epi$Acc ~ K_epi$Acc)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = -2.6567, p-value = 0.007892
# alternative hypothesis: true mu is not equal to 0

cliff.delta(B_epi$Acc,K_epi$Acc,conf.level = 0.95)
# delta estimate: -0.168917 (small)

# 2.6) Wilcoxon test comparing omniscience errors against 0
Z_omni <- B_omni
Z_omni$OmniscienceError <- Z_omni$OmniscienceError * 0

# For believes
wilcoxsign_test(B_omni$OmniscienceError ~ Z_omni$OmniscienceError)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = 7.3378, p-value = 2.172e-13
# alternative hypothesis: true mu is not equal to 0

cliff.delta(B_omni$OmniscienceError,Z_omni$OmniscienceError,conf.level = 0.95)
# Cliff's Delta
# delta estimate: 0.9491525 (large)
# 95 percent confidence interval:
#     lower     upper 
# 0.8550441 0.9827325 

# For knows
wilcoxsign_test(K_omni$OmniscienceError ~ Z_omni$OmniscienceError)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = 6.9148, p-value = 4.684e-12
# alternative hypothesis: true mu is not equal to 0

cliff.delta(K_omni$OmniscienceError,Z_omni$OmniscienceError,conf.level = 0.95)
# Cliff's Delta
# delta estimate: 0.8644068 (large)
# 95 percent confidence interval:
#   lower     upper 
# 0.7465976 0.9296514

# 2.7) Wilcoxon test comparing omniscience errors against all other errors
fact$ErrorOther <- 0
fact$ErrorOther[fact$OmniscienceError==0 & fact$Acc==0] <- 1

wilcoxsign_test(fact$OmniscienceError ~ fact$ErrorOther)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = 13.064, p-value < 2.2e-16
# alternative hypothesis: true mu is not equal to 0

cliff.delta(fact$OmniscienceError,fact$ErrorOther,conf.level = 0.95)
# Cliff's Delta
# delta estimate: 0.8135593 (large)
# 95 percent confidence interval:
#     lower     upper 
# 0.7547314 0.8594078 

# --------------------------------------------------------------------------------------------------
# Figures
# --------------------------------------------------------------------------------------------------

fig1 <- aggregate(OmniscienceError ~ ParticipantID * Verb, data=fact, FUN=function(x){ round(mean(x), digits=2) })

OmniErr.fig1 <- ggplot(fig1, aes(x=Verb, y=OmniscienceError)) +
  geom_violin(trim=TRUE, fill='white', adjust=1.8, size=.4) +
  geom_jitter(shape=16, alpha=.09, size=1.2, position=position_jitter(width=0.04, height=0.13)) +
  theme_bw(12) +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width=.1) +
  stat_summary(fun = "mean", geom = "point", size=2) + 
  scale_x_discrete(name="", labels=c("Believes", "Knows")) +
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

OmniErr.fig1

# Omniscience Errors barplot
omni.verb1 <- data %>% filter(KnowledgeState=="Fact") %>% group_by(Verb) %>% 
  summarise( 
    n=n(),
    mean=mean(OmniscienceError),
    sd=sd(OmniscienceError)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

omni.verb2 <- omni.verb1[,3:6]*100

omni.verb <- cbind(omni.verb1[1:2], omni.verb2)

verb.omni.barplot <- ggplot(omni.verb, aes(y=mean, x=Verb)) +
  geom_bar(stat="identity", position=position_dodge(), fill="steelblue", size=.5, width=.75) +
  geom_errorbar(aes(x=Verb, ymin=mean-ic, ymax=mean+ic), 
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

# Accuracy barplot
acc.probtype1 <- data %>% group_by(KnowledgeState) %>% 
  summarise( 
    n=n(),
    mean=mean(Acc),
    sd=sd(Acc)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

acc.probtype2 <- acc.probtype1[,3:6]*100

acc.probtype <- cbind(acc.probtype1[1:2], acc.probtype2)

prob.acc.barplot <- ggplot(acc.probtype, aes(y=mean, x=KnowledgeState)) +
  geom_bar(stat="identity", position=position_dodge(), fill="steelblue", size=.5, width=.75) +
  geom_errorbar(aes(x=KnowledgeState, ymin=mean-ic, ymax=mean+ic), 
                position=position_dodge(width=0.9), width=0.2, alpha=0.9, size=.5) +
  scale_y_continuous(name = "Accuracy", limits = c(0, 105), breaks = c(0, 50, 100)) +
  scale_x_discrete(name = "", labels=c("Epistemic", "Factual")) +
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position=c(0.26, 0.72),
        legend.key.size = unit(.5, 'cm'),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(size = 20))

prob.acc.barplot

# Fact response type barplot
sums <- colSums(fact[25:26])
sums <- round((sums/length(fact$OmniscienceError))*100)
# custom_order <- c("OmniscienceError","ErrorOther","Acc")
fact_sum_table <- data.frame(Column = names(sums), N = sums)
rownames(fact_sum_table) <- NULL
# fact_sum_table$Column <- factor(fact_sum_table$Column, levels = custom_order)

fact.resps.barplot <- ggplot(fact_sum_table, aes(y=N, x=reorder(Column, -N))) +
  geom_bar(stat="identity", position=position_dodge(), fill="steelblue", size=.5, width=.75) +
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

fact.resps.barplot

#########
# table #
#########

# ----------------------------------------------------------------------------------
# Type of error           %%             Example
# -------------           --             -------
# Omniscience errors      XX             duh
# Nonsense                XX             "nonsense response"
# Nothing follows
#   when something
#   actually follows      XX             "nothing follows" for factual condition
# Logical error           XX             "not P" for [MP: If P then Q; P]
# ----------------------------------------------------------------------------------
# Incorrect
err_Tot <- length(data$Acc) - sum(data$Acc)
# Omniscience errors - 86%
err_OE <- (sum(data$OmniscienceError)/err_Tot)*100
# Nonsense - 1%
err_NS <- (length(data$Answer[data$AnswerType=="Nonsense"])/err_Tot)*100
# Nothing follows - 3%
err_NF <- (length(data$Answer[data$Acc==0 & data$AnswerType=="Nothing follows"])/err_Tot)*100
# Logical error - 10%
err_LE <- 100 - sum(err_OE,err_NS,err_NF)
