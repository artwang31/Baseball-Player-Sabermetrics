library(tidyverse)
library(dplyr)

batting <- Batting
salaries <- Salaries
colnames(batting)
colnames(salaries)
head(batting)
head(salaries)

batting$BA <- batting$H / batting$AB
batting$OBP <- (batting$H + batting$BB + batting$HBP) / (batting$AB + batting$BB + batting$HBP + batting$SF)
batting$`1B` <- batting$H - batting$`2B` - batting$`3B` - batting$HR
batting$SLG <- ((batting$`1B`) + (2 * batting$`2B`) + (3 * batting$`3B`) + (batting$HR)) / batting$AB

batting <- subset(batting, yearID >= 1985)
salaries <- subset(salaries, yearID >= 1985)
combo <- merge(batting, salaries, by = c('playerID', 'yearID'))
summary(combo)
colnames(combo)
players <- c('giambja01', 'damonjo01', 'saenzol01')
playerslost <- filter(combo, playerID %in% players, yearID == 2001)
playerslost <- playerslost[, c('playerID','H','2B','3B','HR','OBP','SLG','BA','AB','salary')]
# Find 3 Replacement Players
# Total Cannot Exceed 15,000,000
# Combined AB >= playerslost
# Mean OBP >= playerslost
sumsalary <- sum(playerslost$salary) # Sum salary of Giambi, Damon, Saenzol = 11,493,333
playerslostAB <- sum(playerslost$AB) # Sum of AB 1469
playerslostOBP <- mean(playerslost$OBP) # Mean On Base Percentage (OBP) = 0.3638687

possibleplayers <- filter(combo, combo$salary < 5000000, combo$AB > 500, combo$OBP > 0.365, combo$yearID == 2001)
replacements <- possibleplayers[sample(nrow(possibleplayers),3),]
replacements

