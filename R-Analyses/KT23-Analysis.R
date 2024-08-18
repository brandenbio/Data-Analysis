# --------------------------------------------------------------------------------------------------
# Knowing Thinking Experiment 23 Analysis
# --------------------------------------------------------------------------------------------------
# Subject pool: Amazon Mechanical Turk
# Date(s):      Dec 2023
# Design:       Branden Bio & Sunny Khemlani
# Code:         Branden Bio
# Experimenter:	Knexus Research Corporation
# --------------------------------------------------------------------------------------------------
# Contents:
#  X. Preprocessing; filter subjs by attention check; reformatting responses
#  1. Analyses
#     1.1) Wilcoxon test on response pattern for modulation                                      ***
#     1.2) Wilcoxon test on response pattern for problem type                                    n.s.
#     1.3) Wilcoxon test on response pattern for epistemic verb                                  ***
#     1.4) Wilcoxon test on response pattern for interaction between modulation and problem type n.s.
#     1.5) Wilcoxon test on response pattern for interaction between modulation and verb          **
#     1.6) Wilcoxon test on response pattern for interaction between verb and problem type         *
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
library(forcats)
#library(rstudioapi)

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
project <- "KT23"
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
#   Believes           46  6   2
#   Knows              25 22   7

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
  #demoData <- subset(demoData, ParticipantID %in% keepers)
  
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
#    60     54         6   36.77     22     62          29        31          0              0


# --------------------------------------------------------------------------------------------------
# 1. Analyses
# --------------------------------------------------------------------------------------------------

# 1.1) Wilcoxon test on response pattern for modulation

data %>% group_by(Modulation) %>% summarise(RespPattern = mean(RespPattern))

# Modulation  ACorDAPattern
# 1 Modulated           0.606
# 2 Unmodulated         0.356

test <- data %>% group_by(ParticipantID, Modulation) %>% summarise(RespPattern = mean(RespPattern))

wilcoxsign_test(subset(test, Modulation=="Modulated")$RespPattern ~
                  subset(test, Modulation=="Unmodulated")$RespPattern)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test

# data:  y by x (pos, neg) 
# stratified by block
# Z = 4.9942, p-value = 5.909e-07
# alternative hypothesis: true mu is not equal to 0

cliff.delta(subset(test, Modulation=="Modulated")$RespPattern ~
              subset(test, Modulation=="Unmodulated")$RespPattern)

# Cliff's Delta

# delta estimate: -0.4754902 (large)
# 95 percent confidence interval:
#   lower       upper 
# -0.75039004 -0.06036078 

# 1.2) Wilcoxon test on response pattern for problem type

data %>% group_by(Cond) %>% summarise(RespPattern = mean(RespPattern))

# Cond               RespPattern
# 1 AffirmConsequent 0.491
# 2 DenyAntecedent   0.472

test <- data %>% group_by(ParticipantID, Cond) %>% summarise(RespPattern = mean(RespPattern))

wilcoxsign_test(subset(test, Cond=="AffirmConsequent")$RespPattern ~
                  subset(test, Cond=="DenyAntecedent")$RespPattern)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test

# data:  y by x (pos, neg) 
# stratified by block
# Z = 0.52301, p-value = 0.601
# alternative hypothesis: true mu is not equal to 0

cliff.delta(subset(test, Cond=="AffirmConsequent")$RespPattern,
            subset(test, Cond=="DenyAntecedent")$RespPattern)

# Cliff's Delta

# delta estimate: 0.04526749 (negligible)
# 95 percent confidence interval:
#   lower      upper 
# -0.1679082  0.2543986 

# 1.3) Wilcoxon test on response pattern for epistemic verb

data %>% group_by(Verb) %>% summarise(RespPattern = mean(RespPattern))

#   Verb       RespPattern
# 1 Believes 0.347
# 2 Knows    0.616

test <- data %>% group_by(ParticipantID, Verb) %>% summarise(RespPattern = mean(RespPattern))

wilcoxsign_test(subset(test, Verb=="Believes")$RespPattern ~
                  subset(test, Verb=="Knows")$RespPattern)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test

# data:  y by x (pos, neg)
# 	 stratified by block
# Z = -4.3263, p-value = 1.516e-05
# alternative hypothesis: true mu is not equal to 0

cliff.delta(subset(test, Verb=="Believes")$RespPattern,
            subset(test, Verb=="Knows")$RespPattern)

# Cliff's Delta

# delta estimate: -0.425583 (medium)
# 95 percent confidence interval:
#   lower      upper 
# -0.5979132 -0.2156482  

# 1.4) Wilcoxon test on response pattern for interaction between modulation and problem type

data %>% group_by(Modulation, Cond) %>% summarise(RespPattern = mean(RespPattern))

#   Modulation  Cond             RespPattern
# 1 Modulated   AffirmConsequent 0.602
# 2 Modulated   DenyAntecedent   0.611    
# 3 Unmodulated AffirmConsequent 0.380
# 4 Unmodulated DenyAntecedent   0.333

test <- data %>% group_by(ParticipantID, Modulation, Cond) %>% summarise(RespPattern = mean(RespPattern))

A <- subset(test, Modulation=="Modulated" & Cond=="AffirmConsequent")$RespPattern -
  subset(test, Modulation=="Unmodulated" & Cond=="AffirmConsequent")$RespPattern
B <- subset(test, Modulation=="Modulated" & Cond=="DenyAntecedent")$RespPattern -
  subset(test, Modulation=="Unmodulated" & Cond=="DenyAntecedent")$RespPattern

wilcoxsign_test(A ~ B)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test

# data:  y by x (pos, neg) 
# stratified by block
# Z = -1.0705, p-value = 0.2844
# alternative hypothesis: true mu is not equal to 0

cliff.delta(A, B)

# Cliff's Delta

# delta estimate: -0.09533608 (negligible)
# 95 percent confidence interval:
#   lower      upper 
# -0.2900844  0.1069944 

# 1.5) Wilcoxon test on response pattern for interaction between modulation and verb

data %>% group_by(Modulation, Verb) %>% summarise(RespPattern = mean(RespPattern))

#   Modulation  Verb     RespPattern
# 1 Modulated   Believes 0.417
# 2 Modulated   Knows    0.796
# 3 Unmodulated Believes 0.278
# 4 Unmodulated Knows    0.435  

test <- data %>% group_by(ParticipantID, Modulation, Verb) %>% summarise(RespPattern = mean(RespPattern))

A <- subset(test, Modulation=="Modulated" & Verb=="Believes")$RespPattern -
  subset(test, Modulation=="Unmodulated" & Verb=="Believes")$RespPattern
B <- subset(test, Modulation=="Modulated" & Verb=="Knows")$RespPattern -
  subset(test, Modulation=="Unmodulated" & Verb=="Knows")$RespPattern

wilcoxsign_test(A ~ B)

# 	Asymptotic Wilcoxon-Pratt Signed-Rank Test

# data:  y by x (pos, neg) 
# stratified by block
# Z = -2.6599, p-value = 0.007815
# alternative hypothesis: true mu is not equal to 0

cliff.delta(A, B)

# Cliff's Delta

# delta estimate: -0.2726337 (small)
# 95 percent confidence interval:
#   lower       upper 
# -0.45442121 -0.06904564 

# 1.6) Wilcoxon test on response pattern for interaction between verb and problem type

data %>% group_by(Verb, Cond) %>% summarise(RespPattern = mean(RespPattern))

#   Verb     Cond             RespPattern
# 1 Believes AffirmConsequent 0.315
# 2 Believes DenyAntecedent   0.380
# 3 Knows    AffirmConsequent 0.667
# 4 Knows    DenyAntecedent   0.565

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
# Z = -2.4193, p-value = 0.01555
# alternative hypothesis: true mu is not equal to 0

cliff.delta(A, B)

# Cliff's Delta

# delta estimate: -0.1875857 (small)
# 95 percent confidence interval:
#   lower       upper 
# -0.37867689  0.01884457 

# 1.7) Wilcoxon test on three-way interaction

data %>% group_by(Cond, Modulation, Verb) %>% summarise(RespPattern = mean(RespPattern))
# Cond             Modulation  Verb       RespPattern
# 1 AffirmConsequent Modulated   Believes 0.389
# 2 AffirmConsequent Modulated   Knows    0.815
# 3 AffirmConsequent Unmodulated Believes 0.241
# 4 AffirmConsequent Unmodulated Knows    0.519
# 5 DenyAntecedent   Modulated   Believes 0.444    
# 6 DenyAntecedent   Modulated   Knows    0.778    
# 7 DenyAntecedent   Unmodulated Believes 0.315
# 8 DenyAntecedent   Unmodulated Knows    0.352

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
# Z = 1.3176, p-value = 0.1876
# alternative hypothesis: true mu is not equal to 0

cliff.delta(A, B)
# Cliff's Delta

# delta estimate: 0.1138546 (negligible)
# 95 percent confidence interval:
#   lower       upper 
# -0.08593906  0.30484505 

# 1.8) Omnibus GLMM

model <- glmer(RespPattern ~ Modulation + Cond + Verb + Modulation:Verb + Modulation:Cond + Cond:Verb + (1 | ParticipantID), data=data, family=binomial(link="logit"))
summary(model)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#  Family: binomial  ( logit )
# Formula: RespPattern ~ Modulation + Cond + Verb + Modulation:Verb + (1 |      ParticipantID) + (1 + Modulation | ParticipantID)
#    Data: data
#
#      AIC      BIC   logLik deviance df.resid
#    499.9    536.5   -241.0    481.9      423
#
# Scaled residuals:
#     Min      1Q  Median      3Q     Max
# -4.6435 -0.5661 -0.2077  0.6135  2.9484
#
# Random effects:
#  Groups          Name                  Variance  Std.Dev. Corr
#  ParticipantID   (Intercept)           0.0002567 0.01602
#  ParticipantID.1 (Intercept)           2.1704890 1.47326
#                  ModulationUnmodulated 0.0076516 0.08747  -1.00
# Number of obs: 432, groups:  ParticipantID, 54
#
# Fixed effects:
#                                 Estimate Std. Error z value Pr(>|z|)
# (Intercept)                      -0.4272     0.3337  -1.280  0.20050
# ModulationUnmodulated            -0.8161     0.3642  -2.241  0.02503 *
# CondDenyAntecedent               -0.1182     0.2432  -0.486  0.62708
# VerbKnows                         2.3675     0.4244   5.579 2.42e-08 ***
# ModulationUnmodulated:VerbKnows  -1.4335     0.5511  -2.601  0.00929 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Correlation of Fixed Effects:
#             (Intr) MdltnU CndDnA VrbKnw
# MdltnUnmdlt -0.486
# CndDnyAntcd -0.360  0.007
# VerbKnows   -0.451  0.441 -0.018
# MdltnUnm:VK  0.352 -0.693  0.008 -0.781
# optimizer (Nelder_Mead) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')

# --------------------------------------------------------------------------------------------------
# X. Figure
# --------------------------------------------------------------------------------------------------

fig1 <- aggregate(RespPattern ~ ParticipantID * Modulation * Verb, data=data,
                  FUN=function(x){ round(mean(x), digits=2) })
fig1$VerbLabel <- ifelse(fig1$Verb=="Knows", "\"knows\"", "\"believes\"")
fig1$Color <- ifelse(fig1$Verb=="Knows" & fig1$Modulation=="Modulated", "highlight", "no-highlight")

# fig1$Condition <- ifelse(fig1$SentenceType=="sentence-initial", "Sentence-initial condition", "Sentence-final condition")

fig1.plot <- ggplot(fig1, aes(x=Modulation, y=RespPattern, color=Color)) +
          geom_violin(trim=TRUE, fill='white', adjust=1.8, size=.7) +
		  geom_jitter(shape=16, alpha=.3, size=1.2, position=position_jitter(width=0.04, height=0.13)) +
          theme_bw(12) +
		  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width=.3) +
		  stat_summary(fun = "mean", geom = "point", size=2) + 
#		  geom_dotplot(binaxis='y', stackdir='center', dotsize=.3) +
          scale_colour_manual(values=c("blue", "black", "black", "black")) +
          scale_x_discrete(name="", labels=c("Mod.", "Unmod.")) +
          scale_y_continuous(name="Proportion of accepted inferences", limits=c(-.3,1.3),
                             breaks=c(0.0, .50, 1.0)) +
		  facet_wrap(~ fct_rev(fig1$VerbLabel), nrow=1) +
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
