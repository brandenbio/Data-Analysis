# --------------------------------------------------------------------------------------------------
# Omniscience Errors Experiment 5 Analysis
# --------------------------------------------------------------------------------------------------
# Subject pool: Amazon Mechanical Turk
# Date(s):      December 2023
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
project <- "OE5"
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

# fullN finalN excludedN meanAge minAge maxAge femaleCount maleCount otherCount preferNotCount
#    65     45        20   41.72     24     76          28        36          0              1

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
# 1 nonFactive  Modus Tollens   180 0.283 0.452 0.0337 0.0665
# 2 Factive     Modus Tollens   180 0.272 0.446 0.0333 0.0656
# 3 nonFactive  Modus Ponens    180 0.267 0.443 0.0331 0.0652
# 4 Factive     Modus Ponens    180 0.25  0.434 0.0324 0.0639

# --------------------------------------------------------------------------------------------------
# 2. Errors of Omniscience (OE)
# --------------------------------------------------------------------------------------------------
data %>% filter(OmniscienceError==1) %>% group_by(ProblemType) %>% 
  summarize(OE_N=n(),
            Resp_Believes = round(mean(str_detect(AnswerType, "believes")) * 100,2), 
            Resp_Knows = round(mean(str_detect(AnswerType, "knows")) * 100,2))

#   ProblemType  OE_N Resp_Believes Resp_Knows
# 1 Factive        94          73.4       26.6
# 2 nonFactive     99          91.9       8.08

# 2.1) Wilcoxon test comparing errors of omniscience - ProblemType
data %>% group_by(ProblemType) %>% summarise(OmniscienceError = mean(OmniscienceError))
# ProblemType     OmniscienceError
# 1 Factive                0.261
# 2 nonFactive             0.275

nonFactive.OE <- aggregate(OmniscienceError ~ ParticipantID, data=data[data$ProblemType=="nonFactive",], FUN=mean)
factive.OE <- aggregate(OmniscienceError ~ ParticipantID, data=data[data$ProblemType=="Factive",], FUN=mean)

wilcoxsign_test(nonFactive.OE$OmniscienceError ~ factive.OE$OmniscienceError)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = 1.0434, p-value = 0.2968
# alternative hypothesis: true mu is not equal to 0

cliff.delta(nonFactive.OE$OmniscienceError,factive.OE$OmniscienceError,conf.level = 0.95)
# Cliff's Delta
# 
# delta estimate: 0.01679012 (negligible)
# 95 percent confidence interval:
#   lower      upper 
# -0.2085657  0.2404529 

# 2.2) Wilcoxon test comparing errors of omniscience - Construction
data %>% group_by(Construction) %>% summarise(OmniscienceError = mean(OmniscienceError))
# Construction    OmniscienceError
# 1 Modus Ponens             0.258
# 2 Modus Tollens            0.278

MP.OE <- aggregate(OmniscienceError ~ ParticipantID, data=data[data$Construction=="Modus Ponens",], FUN=mean)
MT.OE <- aggregate(OmniscienceError ~ ParticipantID, data=data[data$Construction=="Modus Tollens",], FUN=mean)

wilcoxsign_test(MP.OE$OmniscienceError ~ MT.OE$OmniscienceError)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = -0.81871, p-value = 0.413
# alternative hypothesis: true mu is not equal to 0

cliff.delta(MP.OE$OmniscienceError,MT.OE$OmniscienceError,conf.level = 0.95)
# Cliff's Delta
# 
# delta estimate: -0.02617284 (negligible)
# 95 percent confidence interval:
#      lower      upper 
# -0.2501368  0.2004498 

# 2.4) Wilcoxon test on errors of omniscience for interaction between ProblemType and Construction
data %>% group_by(ProblemType, Construction) %>% summarise(OmniscienceError = mean(OmniscienceError))
#   ProblemType   Construction  OmniscienceError
# 1 Factive     Modus Ponens             0.25 
# 2 Factive     Modus Tollens            0.272
# 3 nonFactive  Modus Ponens             0.267
# 4 nonFactive  Modus Tollens            0.283

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
# Z = 0.12721, p-value = 0.8988
# alternative hypothesis: true mu is not equal to 0

cliff.delta(VerbxMP.OE, VerbxMT.OE, conf.level = 0.95)
# Cliff's Delta
# 
# delta estimate: -0.03802469 (negligible)
# 95 percent confidence interval:
#   lower     upper 
# -0.238981  0.166055 
# 
# # 2.8) Wilcoxon test comparing omniscience errors against 0
zeroes.OE <- data.frame(OmniscienceError = rep(0, 720))

wilcoxsign_test(data$OmniscienceError ~ zeroes.OE$OmniscienceError)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg)
# stratified by block
# Z = 13.892, p-value < 2.2e-16
# alternative hypothesis: true mu is not equal to 0

cliff.delta(data$OmniscienceError, zeroes.OE$OmniscienceError, conf.level = 0.95)
# Cliff's Delta
#
# delta estimate: 0.2680556 (small)
# 95 percent confidence interval:
#   lower     upper 
# 0.2353654 0.3001399

# # 2.8) Wilcoxon test comparing verb switches
wilcoxsign_test(data$Acc ~ data$OmniscienceError)
# Asymptotic Wilcoxon-Pratt Signed-Rank Test
# data:  y by x (pos, neg) 
# stratified by block
# Z = 5.5396, p-value = 3.031e-08
# alternative hypothesis: true mu is not equal to 0

cliff.delta(data$Acc, data$OmniscienceError, conf.level = 0.95)
# Cliff's Delta
# delta estimate: 0.1708333 (small)
# 95 percent confidence interval:
#   lower      upper 
# 0.1218131 0.2190222 

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
sums <- colSums(data[29:30])
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
