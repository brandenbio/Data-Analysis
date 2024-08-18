# --------------------------------------------------------------------------------------------------
# Wilson DDM Analysis
# --------------------------------------------------------------------------------------------------
# Date written: June 2024
# Script by:    Branden Bio
# --------------------------------------------------------------------------------------------------

library(RWiener)
library(Metrics)
library(tidyverse)

# Set which agent to simulate - h = human, ai = ai
runFor <- "ai"
runOn <- "aware"
group <- "combined"
focus <- "int"

# Setup data
agent  <- c("ai","ai","ai","ai","ai","ai","ai","human","human","human","human","human","human","human")
concept  <- c("wrongness","responsibility","awareness","intentionality","justifiability","permissibility","blameworthy",
              "wrongness","responsibility","awareness","intentionality","justifiability","permissibility","blameworthy")
rating    <- c(3.60,2.64,2.14,2.44,2.38,2.42,2.67,3.87,4.00,3.26,3.36,2.25,2.25,3.55)

data <- data.frame(agent,concept,rating)
data$concept <- factor(data$concept, levels=c("wrongness","responsibility","awareness","intentionality",
                                              "justifiability","permissibility","blameworthy"))
# print(data)
#        concept agent rating
# 1        wrong human   3.87
# 2  responsible human   4.00
# 3        aware human   3.26
# 4  intentional human   3.36
# 5  blameworthy human   3.55
# 6    justified human   2.25
# 7  permissible human   2.27
# 8        wrong    ai   3.60
# 9  responsible    ai   2.64
# 10       aware    ai   2.14
# 11 intentional    ai   2.44
# 12 blameworthy    ai   2.67
# 13   justified    ai   2.38
# 14 permissible    ai   2.42

# Split data by agent
humData <- data %>% filter(agent=="human") %>% select(concept,rating)
aiData  <- data %>% filter(agent=="ai") %>% select(concept,rating)

# Convert to same scale
ConvertToNewRange <- function(Value, OriginalMin, OriginalMax, NewMin, NewMax) {
    NewValue <- (((Value - OriginalMin) * (NewMax - NewMin)) / (OriginalMax - OriginalMin)) + NewMin
    return(NewValue)
}
  
# Set parameters for each type of agent
# alpha = boundary separation, beta = initial bias, delta = drift rate, tau = nondecision time

# Wrong
# Human: Expectation = 9/10, Observed = 2/10, Drift = (2-9)/10=-0.7
# AI:    Expectation = 8.5/10, Observed = 2/10, Drift = (2-8.5)/10=-0.65

# Responsible
# Human: Expectation = 9/10, Observed = 2/10, Drift = (2-9)/10=-0.7
# AI:    Expectation = 6/10, Observed = 2/10, Drift = (2-6)/10=-0.4

# Aware
# Human: Expectation = 8/10, Observed = 2/10, Drift = (2-8)/10=-0.6
# AI:    Expectation = 4/10, Observed = 2/10, Drift = (2-4)/10=-0.2

# Intentional
# Human: Expectation = 8/10, Observed = 2/10, Drift = (2-8)/10=-0.6
# AI:    Expectation = 5/10, Observed = 2/10, Drift = (2-5)/10=-0.3

# Blameworthy
# Human: Expectation = 8.25/10, Observed = 2/10, Drift = (2-8.25)/10=-0.625
# AI:    Expectation = 6/10, Observed = 2/10, Drift = (2-6)/10=-0.4

# Justified
# Human: Expectation = 4.5/10, Observed = 2/10, Drift = (2-4.5)/10=-0.25
# AI:    Expectation = 5/10, Observed = 2/10, Drift = (2-5)/10=-0.3

# Permissible
# Human: Expectation = 4.5/10, Observed = 2/10, Drift = (2-4.5)/10=-0.25
# AI:    Expectation = 5.1/10, Observed = 2/10, Drift = (2-5.1)/10=-0.31

if (runFor=="hum") {
    currentAgent  <- "human"
    currentData   <- humData
    currentParams <- case_when (
        runOn == "wrong"       ~ c(2,0.9,-0.7,0.3),
        runOn == "responsible" ~ c(2,0.9,-0.7,0.3),
        runOn == "aware"       ~ c(2,0.8,-0.6,0.3),
        runOn == "intentional" ~ c(2,0.8,-0.6,0.3),
        runOn == "blameworthy" ~ c(2,0.825,-0.625,0.3),
        runOn == "justified"   ~ c(2,0.45,-0.25,0.3),
        runOn == "permissible" ~ c(2,0.45,-0.25,0.3),
    )
} else if (runFor=="ai") {
    currentAgent  <- "ai"
    currentData   <- aiData
    currentParams <- case_when (
        runOn == "wrong"       ~ c(2,0.85,-0.65,0.3),
        runOn == "responsible" ~ c(2,0.6,-0.4,0.3),
        runOn == "aware"       ~ c(2,0.4,-0.2,0.3),
        runOn == "intentional" ~ c(2,0.5,-0.3,0.3),
        runOn == "blameworthy" ~ c(2,0.6,-0.4,0.3),
        runOn == "justified"   ~ c(2,0.5,-0.3,0.3),
        runOn == "permissible" ~ c(2,0.51,-0.3,0.31),
    )
}

# Combined parameters
if (group == "combined" & runFor == "hum") {
    if (focus == "int") {
        # Internal
        currentParams <- c(2,0.8,-0.6,0.3)
        currentData   <- data.frame("concept"= "internal", "rating" = mean(humData[c(2:5),"rating"]))
        runOn <- "internal"
            } else {
        # External
        currentParams <- c(2,0.4,0.1,0.3)
        currentData   <- data.frame("concept"= "external", "rating" = mean(humData[c(1,6,7),"rating"]))
        runOn <- "external"
    }
}
if (group == "combined" & runFor == "ai") {
    if (focus == "int") {
        # Internal
        currentParams <- c(2,0.5,0.3,0.3)
        currentData   <- data.frame("concept"= "internal", "rating" = mean(humData[c(2:5),"rating"]))
        runOn <- "internal"
    } else {
        # External
        currentParams <- c(2,0.3,0.3,0.3)
        currentData   <- data.frame("concept"= "external", "rating" = mean(humData[c(1,6,7),"rating"]))
        runOn <- "external"
    }
}

#outputFile <- paste0("wilsonDDM_", currentAgent, "-", format(Sys.time(), "%d-%m-%Y-%H.%M"), ".csv")

# write.table(cbind("alpha","beta","delta","tau","simResponse","actualResponse","fit"),
#             file = outputFile,
#             col.names = F,
#             row.names = F, 
#             sep = ",")

# Run sims
runSim <- function (n=10000, alpha=currentParams[1], beta=currentParams[2], delta=currentParams[3], tau=currentParams[4]) {
    tempData <- rwiener(n=n, alpha=alpha, beta=beta, delta=delta, tau=tau)
    upperCount <- table(tempData$resp)[1]
    simResponse <- ConvertToNewRange(upperCount, 0, 10000, 1, 5)
    # actNum <- case_when (
    #     runOn == "wrong"       ~ 1,
    #     runOn == "responsible" ~ 2,
    #     runOn == "aware"       ~ 3,
    #     runOn == "intentional" ~ 4,
    #     runOn == "blameworthy" ~ 5,
    #     runOn == "justified"   ~ 6,
    #     runOn == "permissible" ~ 7,
    #     .default = 1
    # )
    # actualResponse <- currentData$rating[actNum]
    # Calculate fit (rmse)
    fit <- rmse(actualResponse, simResponse)
    # output <- cbind(alpha, beta, delta, tau, simResponse, actualResponse, fit)

    # write.table(output,
    #             file = outputFile,
    #             col.names = F,
    #             row.names = F, 
    #             sep = ",",
    #             append = TRUE)
    # print(paste("Run for: ", runFor, runOn))
    # print(output)
    
    return(c(simResponse, fit))
}

# runSim()

# wiener_likelihood(x=currentParams,dat=currentData)

# Calibrate parameters
# betas  <- seq(0.2, 0.8, by = 0.1)
# deltas <- seq(-0.5, 0.5, by = 0.1)
# 
# for (b in unique(betas)) {
#     for (d in unique(deltas)) {
#         runSim(beta = b, delta = d)
#     }
# }

# Wrong
# Human
#       alpha beta delta tau simResponse actualResponse    fit
# upper     2  0.9  -0.7 0.3      3.9536           3.87 0.0836
# AI
#       alpha beta delta tau simResponse actualResponse    fit
# upper     2 0.85 -0.65 0.3      3.6468            3.6 0.0468

# Responsible
# Human
#       alpha beta delta tau simResponse actualResponse   fit
# upper     2  0.9  -0.7 0.3       3.954              4 0.046
# AI
#       alpha beta delta tau simResponse actualResponse   fit
# upper     2  0.6  -0.4 0.3      2.6536           2.64 0.0136

# Aware
# Human
#       alpha beta delta tau simResponse actualResponse   fit
# upper     2  0.8  -0.6 0.3       3.304           3.26 0.044
# AI
#       alpha beta delta tau simResponse actualResponse   fit
# upper     2  0.4  -0.2 0.3      2.2228           2.14 0.0828

# Intentional
# Human
#       alpha beta delta tau simResponse actualResponse    fit
# upper     2  0.8  -0.6 0.3      3.3148           3.36 0.0452
# AI
#       alpha beta delta tau simResponse actualResponse   fit
# upper     2  0.5  -0.3 0.3       2.458           2.44 0.018

# Blameworthy
# Human
#       alpha  beta  delta tau simResponse actualResponse    fit
# upper     2 0.825 -0.625 0.3      3.4516           3.55 0.0984
# AI
#       alpha beta delta tau simResponse actualResponse    fit
# upper     2  0.6  -0.4 0.3      2.6468           2.67 0.0232

# Justified
# Human
#       alpha beta delta tau simResponse actualResponse    fit
# upper     2 0.45 -0.25 0.3      2.3296           2.25 0.0796
# AI
#       alpha beta delta tau simResponse actualResponse    fit
# upper     2  0.5  -0.3 0.3      2.3912           2.38 0.0112

# Permissible
# Human
#       alpha beta delta tau simResponse actualResponse    fit
# upper     2 0.45 -0.25 0.3      2.3224           2.27 0.0524
# AI
#       alpha beta delta  tau simResponse actualResponse   fit
# upper     2 0.51  -0.3 0.31       2.496           2.42 0.076

## Combined concepts
# Internal (Responsibility, Awareness, Intentionality, Blameworthy)
# Human
#       alpha beta delta tau simResponse actualResponse    fit
# upper     2  0.8  -0.6 0.3      3.3364         3.5425 0.2061
# AI
#       alpha beta delta tau simResponse actualResponse    fit
# upper     2  0.5   0.3 0.3      3.5868         3.5425 0.0443

# External (Justifiability, Permissibility, Wrongness)
# Human
#       alpha beta delta tau simResponse actualResponse         fit
# upper     2  0.4   0.1 0.3      2.7992       2.796667 0.002533333
# AI
#       alpha beta delta tau simResponse actualResponse        fit
# upper     2  0.3   0.3 0.3      2.7292       2.796667 0.06746667

for (i in rownames(data)) {
    temp <- runSim()
    data$sim[i] <- temp[1]
    data$fit[i] <- temp[2]
}

means.barplot <- ggplot(data, aes(x=concept, y=rating, fill=agent)) +
    geom_bar(stat="identity", position=position_dodge(), size=.5, width=.75) +
    geom_point(data, aes(x=concept, y=sim, color = name), size = 5)
    scale_y_continuous(name = "rating", limits = c(0, 5)) +
    theme_minimal() + 
    theme(legend.title = element_blank(),
          legend.position="top",
          legend.key.size = unit(.5, 'cm'),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          text = element_text(size = 20))

means.barplot
