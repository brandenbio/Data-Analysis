# --------------------------------------------------------------------------------------------------
# Pragmatic Spatial Reasoning Experiment 2 - Modeling and Simulation Analysis
# --------------------------------------------------------------------------------------------------
# Subject pool: Amazon Mechanical Turk
# Date:         June 2023
# Update:       February 2024
# Design:       Branden Bio & Sunny Khemlani
# Code:         Branden Bio
# Experimenter:	Knexus Research Corporation
# --------------------------------------------------------------------------------------------------
# Contents:
#   i. Read in and preprocess human subject data
#  ii. Read in and preprocess simulation data
# iii. Bind human vs. simulation data for modeling analyses (to be continued)
#   1. Comparison for # of responses between human and machine
# --------------------------------------------------------------------------------------------------

library(tidyr)
library(stringr)
library(reshape2)
library(ggplot2)
library(dplyr)
library(rstudioapi)
library(car)
library(gridExtra)
library(grid)
library(yardstick)
library(gifski)

# --------------------------------------------------------------------------------------------------
# i. Read in and preprocess human subject data
# --------------------------------------------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
project <- "PSR2"
data <- read.csv(paste("./", project, "-Aggregate.csv", sep=""), header=T)
data$ParticipantID <- as.factor(data$ParticipantID)
data$AnswerContent <- gsub(";$", "", data$AnswerContent)
data$AnswerAbstract <- gsub("NA", "Nothing", data$AnswerAbstract)

# Remove Ps who didn't score 100% on attention check trials
ac.data <- data
ac.data$AC1 <- ifelse(ac.data$Condition == "AC1" & ac.data$AnswerContent == "two", 1, 0)
ac.data$AC2 <- ifelse(ac.data$Condition == "AC2" & ac.data$AnswerContent == "three", 1, 0)
ac.data <- subset(ac.data, ProblemType == "AC") %>% select(ParticipantID, AC1, AC2)
ac.data.cor <- melt(ac.data, id = "ParticipantID") %>% group_by(ParticipantID) %>% 
  summarise(Combined = sum(value))
ac.data.cor$AC100 <- ifelse(ac.data.cor$Combined == 2, 1, 0)
AC100 <- c(as.character(subset(ac.data.cor, Combined == 2)$ParticipantID))
data$AC100 <- ifelse(data$ParticipantID %in% AC100, 1, 0)
data <- subset(data, AC100==1)

# Remove Ps who didn't score 100% on control trials
ctrl.data <- subset(data, Condition == "CTRL")
ctrl.data$missedCTRL <- ifelse(ctrl.data$AnswerContent != "nothing", 1, 0)
missedCTRL <- unique(c(as.character(subset(ctrl.data, missedCTRL == 1)$ParticipantID)))
data$missedCTRL <- ifelse(data$ParticipantID %in% missedCTRL, 1, 0)
data <- subset(data, missedCTRL==0)

# Remove control trials from analysis
data <- subset(data, ProblemType == "EXP" & Condition != "CTRL")

# Re-level columns; dummy code responses
data$Condition <- factor(data$Condition, levels = c("Ambiguous", "Unambiguous"))
data$Direction <- factor(data$Direction, levels = c("Above", "Right", "Below", "Left"))
data$SelectedA <- ifelse(str_detect(data$AnswerAbstract, "A"), 1, 0)
data$SelectedB <- ifelse(str_detect(data$AnswerAbstract, "B"), 1, 0)
data$SelectedC <- ifelse(str_detect(data$AnswerAbstract, "C"), 1, 0)
data$SelectedP1.1 <- ifelse(str_detect(data$AnswerPosition, "Position_1_1"), 1, 0)
data$SelectedP1.2 <- ifelse(str_detect(data$AnswerPosition, "Position_1_2"), 1, 0)
data$SelectedP4.1 <- ifelse(str_detect(data$AnswerPosition, "Position_4_1"), 1, 0)
data$SelectedP4.2 <- ifelse(str_detect(data$AnswerPosition, "Position_4_2"), 1, 0)
data$SelectedNothing <- ifelse(str_detect(data$AnswerAbstract, "Noth"), 1, 0)
data$ResponseCount <- str_count(data$AnswerAbstract, ";")

# --------------------------------------------------------------------------------------------------
# ii. Read in and preprocess simulation data
# --------------------------------------------------------------------------------------------------

# Concat simulation data
cellsizes <- 250#c(seq(50, 400, by = 50))
celloffset <- 25#c(seq(0, 100, by = 25))
distances <- 2#c(seq(2, 8, by = 2))
compression <- c(seq(0.0, 1.0, by = 0.1))
paramOff <- 1
if (paramOff == 1) {
  #celloffset <- 0
  compression <- 0
}
paramName <- "sims_lesion_com"
stats <- data.frame(cellsizes = c(rep(0,length(cellsizes) * length(distances) * length(celloffset) * length(compression))),
                    celloffset = c(rep(0,length(cellsizes) * length(distances) * length(celloffset) * length(compression))),
                    distances = c(rep(0,length(cellsizes) * length(distances) * length(celloffset) * length(compression))),
                    compression = c(rep(0,length(cellsizes) * length(distances) * length(celloffset) * length(compression))),
                    r = c(rep(0,length(cellsizes) * length(distances) * length(celloffset) * length(compression))),
                    rmse = c(rep(0,length(cellsizes) * length(distances) * length(celloffset) * length(compression))),
                    ambiguous.r = c(rep(0,length(cellsizes) * length(distances) * length(celloffset) * length(compression))),
                    ambiguous.rmse = c(rep(0,length(cellsizes) * length(distances) * length(celloffset) * length(compression))),
                    unambiguous.r = c(rep(0,length(cellsizes) * length(distances) * length(celloffset) * length(compression))),
                    unambiguous.rmse = c(rep(0,length(cellsizes) * length(distances) * length(celloffset) * length(compression)))
                    )
counter <- 1
for (cs in cellsizes) {
  for (co in celloffset) {
    for (d in distances) {
      for (com in compression) {
        current_file_pattern <- paste("cs", as.character(cs), "_co", as.character(co), 
                                      "_d", as.character(d), "_com", as.character(com), ".csv", sep="")
        simulation <- read.csv(paste("./simulations/", paramName, "/", project, "-Simulations_", current_file_pattern, sep=""), header=T)
        simulation$ParticipantID <- as.factor(simulation$ParticipantID)
        simulation$AnswerContent[is.na(simulation$AnswerContent)] <- "nothing"
        simulation$AnswerPosition[is.na(simulation$AnswerPosition)] <- "NA"
        simulation$AnswerAbstract[is.na(simulation$AnswerAbstract)] <- "Nothing"
        simulation$AnswerPosition <- paste(simulation$AnswerPosition, ";", sep = "")
        simulation$AnswerAbstract <- paste(simulation$AnswerAbstract, ";", sep = "")
        simulation$SelectedA <- ifelse(str_detect(simulation$AnswerAbstract, "A"), 1, 0)
        simulation$SelectedB <- ifelse(str_detect(simulation$AnswerAbstract, "B"), 1, 0)
        simulation$SelectedC <- ifelse(str_detect(simulation$AnswerAbstract, "C"), 1, 0)
        simulation$SelectedP1.1 <- ifelse(str_detect(simulation$AnswerPosition, "Position_1_1"), 1, 0)
        simulation$SelectedP1.2 <- ifelse(str_detect(simulation$AnswerPosition, "Position_1_2"), 1, 0)
        simulation$SelectedP4.1 <- ifelse(str_detect(simulation$AnswerPosition, "Position_4_1"), 1, 0)
        simulation$SelectedP4.2 <- ifelse(str_detect(simulation$AnswerPosition, "Position_4_2"), 1, 0)
        simulation$SelectedNothing <- ifelse(str_detect(simulation$AnswerAbstract, "Noth"), 1, 0)
        simulation$ResponseCount <- str_count(simulation$AnswerAbstract, ";")
        
        # --------------------------------------------------------------------------------------------------
        # iii. Bind human vs. simulation data for modeling analyses (to be continued)
        # --------------------------------------------------------------------------------------------------
        
        human.versus.machine <- data.frame()
        human.versus.machine <- bind_rows(data, simulation)
        human.versus.machine$Population <- ifelse(str_detect(human.versus.machine$ParticipantID, "S"), "Simulation", "Human")
        
        # --------------------------------------------------------------------------------------------------
        # 1. Comparison for # of responses between human and machine
        # --------------------------------------------------------------------------------------------------
        
        human.versus.machine %>% group_by(Population, Condition) %>% summarise(ResponseCount = mean(ResponseCount), .groups = "keep")
        
        #   Population Condition   ResponseCount
        # 1 Human      Ambiguous            1.95
        # 2 Human      Unambiguous          1.33
        # 3 Simulation Ambiguous            2.48
        # 4 Simulation Unambiguous          1.03
        
        # --------------------------------------------------------------------------------------------------
        # 2. Figure code 
        # --------------------------------------------------------------------------------------------------
        human.table.abstract <- human.versus.machine %>% filter(Population=="Human") %>% 
                       select(ResponseCount, SelectedA, SelectedB, SelectedC, ProblemNumber) %>%
                       group_by(ProblemNumber) %>%
                       summarise(A = mean(SelectedA), B = mean(SelectedB), C = mean(SelectedC), .groups = "keep")
    
        human.table.position <- human.versus.machine %>% filter(Population=="Human") %>% 
          select(Condition, SelectedP1.1, SelectedP1.2, SelectedP4.1, SelectedP4.2, ProblemNumber) %>%
          group_by(ProblemNumber, Condition) %>%
          summarise(Position1.1 = mean(SelectedP1.1), Position1.2 = mean(SelectedP1.2),
                    Position4.1 = mean(SelectedP4.1), Position4.2 = mean(SelectedP4.2), .groups = "keep")
    
        simulation.table.abstract <- human.versus.machine %>% filter(Population=="Simulation") %>% 
                            select(ResponseCount, SelectedA, SelectedB, SelectedC, ProblemNumber) %>%
                            group_by(ProblemNumber) %>%
                            summarise(A = mean(SelectedA), B = mean(SelectedB), C = mean(SelectedC), .groups = "keep")
    
        simulation.table.position <- human.versus.machine %>% filter(Population=="Simulation") %>% 
          select(Condition, SelectedP1.1, SelectedP1.2, SelectedP4.1, SelectedP4.2, ProblemNumber) %>% 
          group_by(ProblemNumber, Condition) %>%
          summarise(Position1.1 = mean(SelectedP1.1), Position1.2 = mean(SelectedP1.2),
                    Position4.1 = mean(SelectedP4.1), Position4.2 = mean(SelectedP4.2), .groups = "keep")
    
        # createPSRFigure <- function(table, figureName, data, problemNumber)
        # # This figure takes a table as input, e.g.,
        # #
        # #    table1.data <- data %>% group_by(ProblemNumber) %>%
        # #                            select(ResponseCount, SelectedA, SelectedB, SelectedC, ProblemNumber) %>%
        # #                            summarise(A = mean(SelectedA), B = mean(SelectedB), C = mean(SelectedC))
        # #    createPSRFigure(table1.data, "Human (N = 60)", data=data, 1)
        # #    createPSRFigure(simulation.table, "Simulation-Problem 1", data=human.versus.machine, 1)
        # #
        # # It pulls out relevant labels (from data variable) based on positions for each unique problemNumber,
        # # pulls out relevant %s for both labels and alpha values, and creates figure.
        # {
        # 	# figure.data represents a data.frame in the same 2 x 4 format as PSR2 experimental materials
        # 	figure.data <- data.frame(x = rep(c(1,2),times=4))
        # 	figure.data$y <- rev(c(1,1,2,2,3,3,4,4))
        # 	figure.data$labelName <- c(data$Position_1_1[data$ProblemNumber==problemNumber][1],
        # 	                                  data$Position_1_2[data$ProblemNumber==problemNumber][1],
        # 	                                  "", "", "", "",
        # 	                                  data$Position_4_1[data$ProblemNumber==problemNumber][1],
        # 	                                  data$Position_4_2[data$ProblemNumber==problemNumber][1])
        # 
        # 	columnNumberLookup <- c()
        # 	for (l in figure.data$labelName) {
        # 	 if (l == "A") { columnNumberLookup <- c(columnNumberLookup, 2) }
        # 	 else if (l == "B") { columnNumberLookup <- c(columnNumberLookup, 3) }
        # 	 else if (l == "C") { columnNumberLookup <- c(columnNumberLookup, 4) }
        # 	}
        # 
        # 	# Referent refers to the 'B' item that was the intended target of the problem
        # 	referent <- list(c(2,8), 8, c(1,7), 1, c(7,8), 7, c(1,2), 1)
        # 
        # 	figure.data$alpha <- c(table[table$ProblemNumber==problemNumber, columnNumberLookup[1]],
        # 			                      table[table$ProblemNumber==problemNumber, columnNumberLookup[2]],
        # 			                        -1, -1, -1, -1,
        # 			                      table[table$ProblemNumber==problemNumber, columnNumberLookup[3]],
        # 			                      table[table$ProblemNumber==problemNumber, columnNumberLookup[4]])
        # 
        # 	figure.data$alpha <- as.numeric(figure.data$alpha)
        # 
        # 	# Blank label if it's a middle cell, otherwise labeled by both a name + the proportion selected
        # 	figure.data$label <- ifelse(figure.data$alpha==-1, "",
        # 	                            paste(figure.data$labelName, " (", round(figure.data$alpha, digits=2)*100, "%)", sep=""))
        # 	figure.data$color <- ifelse(figure.data$alpha>0.5, "white", "black")
        # 	figure.data$border <- ifelse(figure.data$labelName=="", "white", "black")
        # 	figure.data$fontface <- ifelse(figure.data$alpha>0, "bold", "plain")
        # 
        # 	# Highlight referents
        # 	for (r in referent[problemNumber]) { figure.data$border[r] <- "red" }
        # 
        # 	ggplot(data = figure.data, aes(x,y)) +
        # 	    geom_tile(alpha=figure.data$alpha, colour = figure.data$border, width=1, height=.9, linewidth=1.0) +
        # 	    scale_size(range = c(2, 20)) +
        # 	    geom_text(size=3, nudge_y = 0, label = figure.data$label, colour=figure.data$color, fontface=figure.data$fontface) +
        # 	    scale_y_continuous(name="", limits=c(0, 4.5), breaks=c(0, 4), labels=rep("", 2)) +
        # 	    scale_x_continuous(name=figureName, limits=c(0, 3), breaks=c(0, 1, 2, 3), labels=rep("", 4), position="top") +
        # 	    theme_minimal(10) +
        # 	    theme(panel.grid = element_blank(),
        # 	          axis.line.x = element_blank())
        # }
        # Full combined heatmaps
        hum.long <- pivot_longer(human.table.position, cols = starts_with("Position"), names_to = "Position", values_to = "Value")
        
        hum.heatmap <- ggplot(data = hum.long, aes(x = Position, y = ProblemNumber)) +
          geom_tile(aes(fill = Value), color = "white") +
          scale_fill_gradient(low = "blue", high = "red") +
          labs(title = "Human Heatmap") +
          theme_minimal() +
          theme(panel.grid = element_blank()) +
          scale_y_continuous(breaks = 1:8, labels = c("1", "2", "3", "4", "5", "6", "7", "8"))
    
        sim.long <- pivot_longer(simulation.table.position, cols = starts_with("Position"), names_to = "Position", values_to = "Value")
    
        sim.heatmap <- ggplot(data = sim.long, aes(x = Position, y = ProblemNumber)) +
          geom_tile(aes(fill = Value), color = "white") +
          scale_fill_gradient(low = "blue", high = "red") +
          labs(title = "Simulation Heatmap") +
          theme_minimal() +
          theme(panel.grid = element_blank()) +
          scale_y_continuous(breaks = 1:8, labels = c("1", "2", "3", "4", "5", "6", "7", "8"))
  
        # grid.arrange(hum.heatmap, sim.heatmap, ncol=2, top=textGrob(paste("Heatmap_", current_file_pattern, sep=""), gp=gpar(fontsize=20,font=3)))
        
        ambiguous.human <- hum.long %>% filter(Condition=="Ambiguous")
        unambiguous.human <- hum.long %>% filter(Condition=="Unambiguous")

        ambiguous.simulation <- sim.long %>% filter(Condition=="Ambiguous")
        unambiguous.simulation <- sim.long %>% filter(Condition=="Unambiguous")
        
        # Ambiguous Heatmaps
        ambiguous.hum.heatmap <- ggplot(data = ambiguous.human, aes(x = Position, y = ProblemNumber)) +
          geom_tile(aes(fill = Value), color = "white") +
          scale_fill_gradient(low = "blue", high = "red") +
          labs(title = "Human Ambiguous Heatmap") +
          theme_minimal() +
          theme(panel.grid = element_blank()) +
          scale_y_continuous(breaks = 1:8, labels = c("1", "", "3", "", "5", "", "7", ""))
        
        ambiguous.sim.heatmap <- ggplot(data = ambiguous.simulation, aes(x = Position, y = ProblemNumber)) +
          geom_tile(aes(fill = Value), color = "white") +
          scale_fill_gradient(low = "blue", high = "red") +
          labs(title = "Simulation Ambiguous Heatmap") +
          theme_minimal() +
          theme(panel.grid = element_blank()) +
          scale_y_continuous(breaks = 1:8, labels = c("1", "", "3", "", "5", "", "7", ""))
        
        # grid.arrange(ambiguous.hum.heatmap, ambiguous.sim.heatmap, ncol=2, 
        # top=textGrob(paste("Heatmap_", current_file_pattern, sep=""), gp=gpar(fontsize=20,font=3)))
        
        # Unambiguous Heatmaps
        unambiguous.hum.heatmap <- ggplot(data = unambiguous.human, aes(x = Position, y = ProblemNumber)) +
          geom_tile(aes(fill = Value), color = "white") +
          scale_fill_gradient(low = "blue", high = "red") +
          labs(title = "Human Unambiguous Heatmap") +
          theme_minimal() +
          theme(panel.grid = element_blank()) +
          scale_y_continuous(breaks = 1:8, labels = c("", "2", "", "4", "", "6", "", "8"))
        
        unambiguous.sim.heatmap <- ggplot(data = unambiguous.simulation, aes(x = Position, y = ProblemNumber)) +
          geom_tile(aes(fill = Value), color = "white") +
          scale_fill_gradient(low = "blue", high = "red") +
          labs(title = "Simulation Unambiguous Heatmap") +
          theme_minimal() +
          theme(panel.grid = element_blank()) +
          scale_y_continuous(breaks = 1:8, labels = c("", "2", "", "4", "", "6", "", "8"))
        
        # grid.arrange(unambiguous.hum.heatmap, unambiguous.sim.heatmap, ncol=2,
        # top=textGrob(paste("Heatmap_", current_file_pattern, sep=""), gp=gpar(fontsize=20,font=3)))
        
        ambiguous.correlation <- cor.test(ambiguous.human$Value, ambiguous.simulation$Value)
        ambiguous.rmse <- rmse_vec(ambiguous.human$Value, ambiguous.simulation$Value)
        
        unambiguous.correlation <- cor.test(unambiguous.human$Value, unambiguous.simulation$Value)
        unambiguous.rmse <- rmse_vec(unambiguous.human$Value, unambiguous.simulation$Value)
        
        correlation <- cor.test(hum.long$Value, sim.long$Value)
        rmse <- rmse_vec(hum.long$Value, sim.long$Value)
        
        # fill in stats matrix
        stats$cellsizes[counter] <- cs
        stats$celloffset[counter] <- co
        stats$distances[counter] <- d
        stats$compression[counter] <- com
        stats$r[counter] <- correlation$estimate
        stats$rmse[counter] <- rmse
        stats$ambiguous.r[counter] <- ambiguous.correlation$estimate
        stats$ambiguous.rmse[counter] <- ambiguous.rmse
        stats$unambiguous.r[counter] <- unambiguous.correlation$estimate
        stats$unambiguous.rmse[counter] <- unambiguous.rmse
        counter <- counter + 1
      }
   }
  }
}

# stats.heatmap <- ggplot(data = stats, aes(x = cellsizes, y = distances)) +
#   geom_tile(aes(fill = r), color = "white") +
#   scale_fill_gradient(low = "blue", high = "red") +
#   labs(title = "Parameter Heatmap") +
#   theme_minimal() +
#   theme(panel.grid = element_blank())
# 
# stats.heatmap

stats <- stats[order(stats$r, decreasing = TRUE),]
ambiguous.stats <- stats[order(stats$ambiguous.r, decreasing = TRUE),]
unambiguous.stats <- stats[order(stats$unambiguous.r, decreasing = TRUE),]
write.csv(stats, paste("./", project, "-Stats_", paramName, ".csv", sep=""), row.names=FALSE)
# --------------------------------------------------------------------------------------------------
# 1. Animated GIF of simulation (problem 7)
# --------------------------------------------------------------------------------------------------

# gif_frame <- 1
# fig1.data <- c()
# fig2.simulation <- c()
# blank <- grid.rect(gp=gpar(col="white"))
# 
# for (frame in 1:length(levels(data$ParticipantID))) {
# 	# Pull only a subset of P data and construct table based on that subset
# 	data.subset <- subset(data, ParticipantID %in% levels(data$ParticipantID)[1:frame])
# 	table1.data <- data.subset %>% group_by(ProblemNumber) %>% 
# 	               select(ResponseCount, SelectedA, SelectedB, SelectedC, ProblemNumber) %>% 
# 	               summarise(A = mean(SelectedA), B = mean(SelectedB), C = mean(SelectedC))
#    	# Create figure based on subset of human data
# 	fig1.data <- createPSRFigure(table1.data, paste("Human\n(N = ", frame, ")", sep=""), data=data.subset)
# 	fig1 <- grid.arrange(fig1.data, blank, ncol=2)
# 
# 	# Save frame and increment
# 	ggsave(paste("fig1-", str_pad(gif_frame, 4, pad = "0"), ".png", sep=""), fig1)
# 	gif_frame <<- gif_frame + 1
# }
# 
# for (frame in 1:length(levels(simulation$ParticipantID))) {
# 	# Pull only a subset of simulated data and construct table based on that subset
# 	simulation.subset <- subset(simulation, ParticipantID %in% levels(simulation$ParticipantID)[1:frame])
# 	table1.simulation <- simulation.subset %>% select(ResponseCount, SelectedA, SelectedB, SelectedC, ProblemNumber) %>% 
# 	                     group_by(ProblemNumber) %>%
# 	                     summarise(A = mean(SelectedA), B = mean(SelectedB), C = mean(SelectedC))
#    	# Create figure based on subset of simulated data
# 	fig1.simulation <- createPSRFigure(table1.simulation, paste("Simulation\n(N = ", frame, ")", sep=""), data=data)
# 	fig1 <- grid.arrange(fig1.data, fig1.simulation, ncol=2)
# 	# Save frame and increment
# 	ggsave(paste("fig1-", str_pad(gif_frame, 4, pad = "0"), ".png", sep=""), fig1)
# 	gif_frame <- gif_frame + 1
# }
# 
# png_files <- sprintf(file.path(".", "fig1-%04d.png"), 1:160)
# gif_file <- file.path(".", "fig1.gif")
# gifski(png_files, gif_file, width = 5.12*200, height = 2.16*200, delay = .03, loop=FALSE)
