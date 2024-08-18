# --------------------------------------------------------------------------------------------------
# Pragmatic Spatial Reasoning Experiment 1 Analysis
# --------------------------------------------------------------------------------------------------
# Subject pool: Amazon Mechanical Turk
# Date(s):      February 2023
# Design:       Branden Bio & Sunny Khemlani
# Code:         Branden Bio
# Experimenter:	Knexus Research Corporation
# --------------------------------------------------------------------------------------------------
# Contents:
#  X. Preprocessing; filter subjs by attention check; reformatting responses
#  1. Analyses
# --------------------------------------------------------------------------------------------------

library(tidyr)
library(stringr)
library(coin)
library(reshape2)
library(ggplot2)
library(dplyr)
library(lme4)
library(effsize)
library(rstudioapi)

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
project <- "PSR1"
##########

analysisType <- "hum" # hum = human behavioral data, sim = simulated data, combo = combined

if (analysisType != "sim") {
  rawData <- read.csv(paste("./", project, "-Aggregate.csv", sep=""), header=T)
} else if (analysisType == "sim") {
  rawData <- read.csv(paste("./", project, "-Simulations.csv", sep=""), header=T)
}

if (analysisType != "sim") {
  workingData <- rawData
  workingData$ParticipantID <- as.factor(workingData$ParticipantID)
  workingData$Answer <- gsub(";$", "", workingData$Answer)
  fullN <- length(unique(workingData$ParticipantID))
  
  # Subject exclusion based on AC performance
  data.AC <- workingData
  data.AC$AC1 <- ifelse(data.AC$Condition == "AC1" & data.AC$Answer == 2, 1, 0)
  data.AC$AC2 <- ifelse(data.AC$Condition == "AC2" & data.AC$Answer == 3, 1, 0)
  data.AC <- subset(data.AC, ProblemType == "AC") %>% select(ParticipantID, AC1, AC2)
  data.AC.Corr <- melt(data.AC, id = "ParticipantID") %>% group_by(ParticipantID) %>% 
                  summarise(Combined = sum(value))
  data.AC.Corr$AC50 <- ifelse(data.AC.Corr$Combined >= 1, 1, 0)
  data.AC.Corr$AC100 <- ifelse(data.AC.Corr$Combined == 2, 1, 0)
  
  failedAC <- c(as.character(subset(data.AC.Corr, Combined == 0)$ParticipantID))
  AC50 <- c(as.character(subset(data.AC.Corr, Combined == 1)$ParticipantID))
  AC100 <- c(as.character(subset(data.AC.Corr, Combined == 2)$ParticipantID))
  
  workingData$FailedAC <- ifelse(workingData$ParticipantID %in% failedAC, 1, 0)
  #workingData$AC50 <- ifelse(workingData$ParticipantID %in% AC50, 1, 0)
  workingData$AC100 <- ifelse(workingData$ParticipantID %in% AC100, 1, 0)
  
  #Percentage correct needed on AC trials to be included in analysis
  AC.Threshold <- 100
  
  if (AC.Threshold == 50) {
    # This option includes P's who missed one of the two AC trials
    data <- subset(workingData, FailedAC==0)
    excl.data <- subset(workingData, FailedAC==1)
  } else {
    # Default includes only P's who got 100% correct on AC trials
    data <- subset(workingData, AC100==1)
    excl.data <- subset(workingData, AC100!=1)
  }
  
  excl.data <- subset(excl.data, ProblemType != "AC")
  excludedN <- length(unique(excl.data$ParticipantID))
  finalN <- length(unique(data$ParticipantID))
  subjLoss <- round(1 - (finalN / fullN),2)
  useableSubs <- unique(data$ParticipantID)
  
  # Data subsets
  data <- subset(data, ProblemType == "EXP") %>% select(ParticipantID, ProblemNumber, Condition, Direction, Answer)
} else if (analysisType == "sim") {
  data <- rawData
}
if (analysisType == "combo") {
    dataSim <- read.csv(paste("./", project, "-Simulations.csv", sep=""), header=T)
    data <- bind_rows(data, dataSim)
    data$studyType <- ifelse(str_detect(data$ParticipantID, "S"), "Simulation", "Human")
    data$studyType <- factor(data$studyType, levels = c("Simulation", "Human"))
    data <- data[order(data$ParticipantID, data$ProblemNumber),]
}

data$Condition <- factor(data$Condition, levels = c("Ambiguous", "Unambiguous"))
data$Direction <- factor(data$Direction, levels = c("Above", "Right", "Below", "Left"))
# data$ResponseType <- ifelse(str_detect(data$Answer, ";"), "Broad", "Narrow")
# data$ResponseType <- factor(data$ResponseType, levels = c("Broad", "Narrow"))
# data$Narrow <- ifelse(str_detect(data$Answer, ";"), 0, 1)
# data$Broad <- ifelse(str_detect(data$Answer, ";"), 1, 0)

# Assign accuracy scores
data$Acc <- 0

data$Acc[data$ProblemNumber == 1 & (data$Answer == "carrot" | 
                                      data$Answer == "broccoli" | 
                                      data$Answer == "carrot;broccoli" | 
                                      data$Answer == "broccoli;carrot")] <- 1
data$Acc[data$ProblemNumber == 2 & (data$Answer == "broccoli" | 
                                      data$Answer == "nothing" | 
                                      data$Answer == "nothing;broccoli" | 
                                      data$Answer == "broccoli;nothing")] <- 1
data$Acc[data$ProblemNumber == 3 & (data$Answer == "carrot" | 
                                      data$Answer == "broccoli" | 
                                      data$Answer == "carrot;broccoli" | 
                                      data$Answer == "broccoli;carrot")] <- 1
data$Acc[data$ProblemNumber == 4 & (data$Answer == "broccoli" | 
                                      data$Answer == "nothing" | 
                                      data$Answer == "nothing;broccoli" | 
                                      data$Answer == "broccoli;nothing")] <- 1
data$Acc[data$ProblemNumber == 5 & (data$Answer == "carrot" | 
                                      data$Answer == "broccoli" | 
                                      data$Answer == "carrot;broccoli" | 
                                      data$Answer == "broccoli;carrot")] <- 1
data$Acc[data$ProblemNumber == 6 & (data$Answer == "carrot" | 
                                      data$Answer == "nothing" | 
                                      data$Answer == "nothing;carrot" | 
                                      data$Answer == "carrot;nothing")] <- 1
data$Acc[data$ProblemNumber == 7 & (data$Answer == "carrot" | 
                                      data$Answer == "broccoli" | 
                                      data$Answer == "carrot;broccoli" | 
                                      data$Answer == "broccoli;carrot")] <- 1
data$Acc[data$ProblemNumber == 8 & (data$Answer == "carrot" | 
                                      data$Answer == "nothing" | 
                                      data$Answer == "nothing;carrot" | 
                                      data$Answer == "carrot;nothing")] <- 1

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
if (analysisType == "hum") {
  demoSummary(useableSubs)
}
#  fullN finalN excludedN meanAge minAge maxAge femaleCount maleCount otherCount preferNotCount
#     60     57         3   35.75     23     62          31        29          0              0

# --------------------------------------------------------------------------------------------------
# 1. Analyses
# --------------------------------------------------------------------------------------------------

if (analysisType != "combo") {
  # Acc
  data %>% group_by(ParticipantID) %>% summarise(Acc = mean(Acc)*100)
  #     ParticipantID   Acc
  # 1             P11  75.0
  # 2             P12 100.0
  # 3             P13 100.0
  # 4             P14 100.0
  # 5             P15 100.0
  # 6             P17  87.5
  # 7             P18 100.0
  # 8             P19 100.0
  # 9              P2 100.0
  # 10            P20 100.0
  # 11            P21 100.0
  # 12            P22 100.0
  # 13            P23 100.0
  # 14            P24  87.5
  # 15            P25  75.0
  # 16            P26 100.0
  # 17            P27 100.0
  # 18            P28 100.0
  # 19            P29 100.0
  # 20             P3 100.0
  # 21            P30 100.0
  # 22            P31 100.0
  # 23            P32 100.0
  # 24            P33  87.5
  # 25            P34 100.0
  # 26            P35  75.0
  # 27            P36  62.5
  # 28            P37 100.0
  # 29            P38 100.0
  # 30            P39 100.0
  # 31             P4 100.0
  # 32            P40 100.0
  # 33            P41 100.0
  # 34            P42 100.0
  # 35            P43 100.0
  # 36            P44 100.0
  # 37            P45 100.0
  # 38            P46 100.0
  # 39            P47 100.0
  # 40            P48 100.0
  # 41            P49 100.0
  # 42             P5 100.0
  # 43            P50 100.0
  # 44            P51 100.0
  # 45            P52 100.0
  # 46            P53 100.0
  # 47            P54  75.0
  # 48            P55 100.0
  # 49            P56  87.5
  # 50            P58 100.0
  # 51            P59 100.0
  # 52            P60 100.0
  # 53            P61 100.0
  # 54            P62 100.0
  # 55             P7 100.0
  # 56             P8  50.0
  # 57             P9 100.0
  # 58            SP1  75.0
  # 59           SP10  87.5
  # 60          SP100 100.0
  # 61           SP11  62.5
  # 62           SP12  75.0
  # 63           SP13  37.5
  # 64           SP14  87.5
  # 65           SP15  87.5
  # 66           SP16  87.5
  # 67           SP17  87.5
  # 68           SP18  62.5
  # 69           SP19  75.0
  # 70            SP2  75.0
  # 71           SP20  75.0
  # 72           SP21 100.0
  # 73           SP22  75.0
  # 74           SP23  87.5
  # 75           SP24  75.0
  # 76           SP25  87.5
  # 77           SP26  62.5
  # 78           SP27  75.0
  # 79           SP28  75.0
  # 80           SP29  50.0
  # 81            SP3  75.0
  # 82           SP30  62.5
  # 83           SP31  75.0
  # 84           SP32  62.5
  # 85           SP33  75.0
  # 86           SP34  50.0
  # 87           SP35  62.5
  # 88           SP36  62.5
  # 89           SP37  62.5
  # 90           SP38  62.5
  # 91           SP39  62.5
  # 92            SP4  62.5
  # 93           SP40  87.5
  # 94           SP41  87.5
  # 95           SP42  75.0
  # 96           SP43  62.5
  # 97           SP44  75.0
  # 98           SP45  75.0
  # 99           SP46 100.0
  # 100          SP47  75.0
  # 101          SP48  75.0
  # 102          SP49  87.5
  # 103           SP5  62.5
  # 104          SP50  87.5
  # 105          SP51  87.5
  # 106          SP52  87.5
  # 107          SP53  50.0
  # 108          SP54  87.5
  # 109          SP55  87.5
  # 110          SP56  75.0
  # 111          SP57  62.5
  # 112          SP58  75.0
  # 113          SP59 100.0
  # 114           SP6  50.0
  # 115          SP60  75.0
  # 116          SP61  75.0
  # 117          SP62  62.5
  # 118          SP63  87.5
  # 119          SP64  87.5
  # 120          SP65  75.0
  # 121          SP66  75.0
  # 122          SP67  62.5
  # 123          SP68  75.0
  # 124          SP69  62.5
  # 125           SP7  87.5
  # 126          SP70  62.5
  # 127          SP71  87.5
  # 128          SP72  62.5
  # 129          SP73  75.0
  # 130          SP74  50.0
  # 131          SP75  75.0
  # 132          SP76  75.0
  # 133          SP77  75.0
  # 134          SP78  75.0
  # 135          SP79  75.0
  # 136           SP8  62.5
  # 137          SP80  87.5
  # 138          SP81  75.0
  # 139          SP82  75.0
  # 140          SP83  62.5
  # 141          SP84  87.5
  # 142          SP85  75.0
  # 143          SP86  75.0
  # 144          SP87  75.0
  # 145          SP88  75.0
  # 146          SP89  50.0
  # 147           SP9  87.5
  # 148          SP90  75.0
  # 149          SP91  87.5
  # 150          SP92  87.5
  # 151          SP93  62.5
  # 152          SP94  87.5
  # 153          SP95  87.5
  # 154          SP96  75.0
  # 155          SP97  87.5
  # 156          SP98  75.0
  # 157          SP99  75.0
  
  # Acc Prob x Cond
  data %>% group_by(ProblemNumber, Condition) %>% summarise(Acc = round(mean(Acc)*100,2))
  #   ProblemNumber   Condition   Acc
  # 1             1   Ambiguous 96.18
  # 2             2 Unambiguous 67.52
  # 3             3   Ambiguous 87.90
  # 4             4 Unambiguous 86.62
  # 5             5   Ambiguous 99.36
  # 6             6 Unambiguous 70.70
  # 7             7   Ambiguous 99.36
  # 8             8 Unambiguous 51.59
  
  # Acc for conditions
  data %>% group_by(Condition) %>% summarise(Acc = round(mean(Acc)*100,2))
  #     Condition   Acc
  # 1   Ambiguous 95.70
  # 2 Unambiguous 69.11
  
  # Wilcoxon comparing proportion of each response type
  table(data$ResponseType)
  
  # Hum              # Sim
  # Broad Narrow     # Broad Narrow 
  #    69    387     #     0    598  
  
  a <- aggregate(Broad ~ ParticipantID, data=data, FUN=mean)
  
  b <- aggregate(Narrow ~ ParticipantID, data=data, FUN=mean)
  
  wilcoxsign_test(a$Broad ~ b$Narrow)
  # Asymptotic Wilcoxon-Pratt Signed-Rank Test
  # 
  # data:  y by x (pos, neg) 
  # stratified by block
  # Z = -5.8356, p-value = 5.358e-09
  # alternative hypothesis: true mu is not equal to 0
  
  cliff.delta(a$Broad, b$Narrow, conf.level = 0.95)
  # Cliff's Delta
  # 
  # delta estimate: -0.848261 (large)
  # 95 percent confidence interval:
  #      lower      upper 
  # -0.9249608 -0.7051713 
  
  # Wilcoxon comparing proportion of response type by condition
  table(data$Condition, data$ResponseType)
  
  # Hum                          # Sim
  #             Broad Narrow     #             Broad Narrow
  # Ambiguous      56    172     # Ambiguous       0    374
  # Unambiguous    13    215     # Unambiguous     0    224
  
  bResps <- aggregate(Broad ~ Condition, data=data, FUN=mean)
  
  nResps <- aggregate(Narrow ~ Condition, data=data, FUN=mean)
  
  wilcoxsign_test(bResps$Broad ~ nResps$Narrow)
  # Asymptotic Wilcoxon-Pratt Signed-Rank Test
  # 
  # data:  y by x (pos, neg) 
  # stratified by block
  # Z = -1.3416, p-value = 0.1797
  # alternative hypothesis: true mu is not equal to 0
  
  # Wilcoxon comparing proportion of each condition within broad response type
  
  data %>% group_by(Condition) %>% summarise(Broad = mean(Broad))
  # Condition   Broad
  # Ambiguous    0.246
  # Unambiguous  0.0570
  
  amb <- data %>% filter(Condition == "Ambiguous")
  a <- aggregate(Broad ~ ParticipantID, data=amb, FUN=mean)
  una <- data %>% filter(Condition == "Unambiguous")
  b <- aggregate(Broad ~ ParticipantID, data=una, FUN=mean)
  
  wilcoxsign_test(a$Broad ~ b$Broad)
  # Asymptotic Wilcoxon-Pratt Signed-Rank Test
  # 
  # data:  y by x (pos, neg) 
  # stratified by block
  # Z = 3.9841, p-value = 6.775e-05
  # alternative hypothesis: true mu is not equal to 0
  
  cliff.delta(a$Broad, b$Broad, conf.level = 0.95)
  # Cliff's Delta
  # 
  # delta estimate: 0.2585411 (small)
  # 95 percent confidence interval:
  #   lower     upper 
  # 0.1111269 0.3948231 
  
  # Wilcoxon comparing proportion of each condition within narrow response type
  
  data %>% group_by(Condition) %>% summarise(Narrow = mean(Narrow))
  # Condition   Narrow
  # Ambiguous    0.754
  # Unambiguous  0.943
  
  a <- aggregate(Narrow ~ ParticipantID, data=amb, FUN=mean)
  
  b <- aggregate(Narrow ~ ParticipantID, data=una, FUN=mean)
  
  wilcoxsign_test(a$Narrow ~ b$Narrow)
  # Asymptotic Wilcoxon-Pratt Signed-Rank Test
  # 
  # data:  y by x (pos, neg) 
  # stratified by block
  # Z = -3.9841, p-value = 6.775e-05
  # alternative hypothesis: true mu is not equal to 0
  
  cliff.delta(a$Narrow, b$Narrow, conf.level = 0.95)
  # Cliff's Delta
  # 
  # delta estimate: -0.2585411 (small)
  # 95 percent confidence interval:
  #   lower      upper 
  # -0.3948231 -0.1111269 
  
  # Wilcoxon comparing proportion of each response type within ambiguous condition
  
  table(amb$ResponseType)
  # Broad Narrow 
  #    56    172 
  
  a <- aggregate(Broad ~ ParticipantID, data=amb, FUN=mean)
  
  b <- aggregate(Narrow ~ ParticipantID, data=amb, FUN=mean)
  
  wilcoxsign_test(a$Broad ~ b$Narrow)
  # Asymptotic Wilcoxon-Pratt Signed-Rank Test
  # 
  # data:  y by x (pos, neg) 
  # stratified by block
  # Z = -4.217, p-value = 2.476e-05
  # alternative hypothesis: true mu is not equal to 0
  
  cliff.delta(a$Broad, b$Narrow, conf.level = 0.95)
  # Cliff's Delta
  # 
  # delta estimate: -0.5980302 (large)
  # 95 percent confidence interval:
  #      lower      upper 
  # -0.7326018 -0.4184651 
  
  # Wilcoxon comparing proportion of each response type within unambiguous condition
  
  table(una$ResponseType)
  # Broad Narrow 
  #    13    215  
  
  a <- aggregate(Broad ~ ParticipantID, data=una, FUN=mean)
  
  b <- aggregate(Narrow ~ ParticipantID, data=una, FUN=mean)
  
  wilcoxsign_test(a$Broad ~ b$Narrow)
  # Asymptotic Wilcoxon-Pratt Signed-Rank Test
  # 
  # data:  y by x (pos, neg) 
  # stratified by block
  # Z = -6.686, p-value = 2.294e-11
  # alternative hypothesis: true mu is not equal to 0
  
  cliff.delta(a$Broad, b$Narrow, conf.level = 0.95)
  # Cliff's Delta
  # 
  # delta estimate: -0.8928901 (large)
  # 95 percent confidence interval:
  #   lower      upper 
  # -0.9511732 -0.7731257 
  
  # Directions
  
  # Wilcoxon comparing proportion of responses for each direction
  table(data$Direction, data$ResponseType)
  #       Broad Narrow
  # Above    15     99
  # Right    18     96
  # Below    19     95
  # Left     17     97
} else if (analysisType == "combo") {
  # Combo Analyses
  humData <- data %>% filter(studyType == "Human")
  simData <- data %>% filter(studyType == "Simulation")
  # Wilcoxon comparing proportion of each response type
  table(data$studyType, data$ResponseType)
  #            Broad Narrow
  # Simulation     0    598
  # Human         69    387
  
  # Mann-Whitney Test for broad response comparison between populations
  a <- aggregate(Broad ~ ParticipantID, data=humData, FUN=mean)
  b <- aggregate(Broad ~ ParticipantID, data=simData, FUN=mean)
  
  wilcox.test(a$Broad, b$Broad)
  # Wilcoxon rank sum test with continuity correction
  # 
  # data:  a$Broad and b$Broad
  # W = 3800, p-value = 9.416e-10
  # alternative hypothesis: true location shift is not equal to 0
  
  cliff.delta(a$Broad, b$Broad, conf.level = 0.95)
  #   Cliff's Delta
  # 
  # delta estimate: 0.3333333 (medium)
  # 95 percent confidence interval:
  #      lower      upper 
  # 0.2041263 0.4511257 

  # Mann-Whitney Test for narrow response comparison between populations
  a <- aggregate(Narrow ~ ParticipantID, data=humData, FUN=mean)
  b <- aggregate(Narrow ~ ParticipantID, data=simData, FUN=mean)
  
  wilcox.test(a$Narrow, b$Narrow)
  # Wilcoxon rank sum test with continuity correction
  # 
  # data:  a$Narrow and b$Narrow
  # W = 1900, p-value = 9.416e-10
  # alternative hypothesis: true location shift is not equal to 0
  
  cliff.delta(a$Narrow, b$Narrow, conf.level = 0.95)
  #   Cliff's Delta
  # 
  # delta estimate: -0.3333333 (medium)
  # 95 percent confidence interval:
  #   lower      upper 
  # -0.4511257 -0.2041263 
  
  # Test for interaction between population (study) and condition
  # test <- data %>% group_by(studyType, Condition) %>% summarise(Response = mean(Broad))
  # 
  # A <- subset(test, studyType=="Simulation" & Condition=="Ambiguous")$Response -
  #   subset(test, studyType=="Simulation" & Condition=="Unambiguous")$Response
  # B <- subset(test, studyType=="Human" & Condition=="Ambiguous")$Response -
  #   subset(test, studyType=="Human" & Condition=="Unambiguous")$Response
  # 
  # wilcoxsign_test(A ~ B)
  
}

