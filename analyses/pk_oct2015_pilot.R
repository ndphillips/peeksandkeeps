
# October 2015 Peeks Keeps Pilot
# 16 participants

# Set global optoins

options(stringsAsFactors = F)

# load packages

library('dplyr')

# Read data
{
trial <- read.table("data/oct2015/peekkeep_oct2015_trialdata.txt", 
                    sep = "\t", 
                    header = T)

distributions <- read.table("data/oct2015/peekkeep_oct2015_gamedist.txt", 
                            sep = "\t", 
                            header = T)

optionorder <- read.table("data/oct2015/peekkeep_oct2015_optionorder.txt", 
                            sep = "\t", 
                            header = T)

unipark <- read.table("data/oct2015/peekkeep_oct2015_unipark.txt", 
                          sep = "\t", 
                          header = T)

mturk <- read.table("data/oct2015/peekkeep_oct2015_mturk.txt", 
                    sep = "\t", 
                    header = T)
}

# condition.table
# Table showing a participant's game type, number of options, and 
#  option standard deviation as a function of their unipark condition

condition.table <- data.frame(uni.condition = 1:12,
                              game.type = rep(c("ptk", "k", "pak"), each = 4),
                              n.options = rep(c(2, 4), each = 2, times = 3),
                              option.sd = rep(c(5, 20), times = 6)
                              )

# Determine valid.works who completed the study and entered
#  code on mturk

valid.workers <- mturk$WorkerId


# Subset datasets to only include valid workers
trial <- subset(trial, workerid %in% valid.workers)
optionorder <- subset(optionorder, workerid %in% valid.workers)
unipark <- subset(unipark, workerid %in% valid.workers)

# participants

participants <- unipark

participants <- participants %>%
  mutate(
    bis = bimp_11 + bimp_32 + bimp_05 + (5 - bimp_09) +
      (5 - bimp_21) + bimp_17 + v_529 + bimp_23 +
      bimp_03 + bimp_02 + bimp_28 + bimp_04 +
      (5 - bimp_12) + (5 - bimp_01) + (5 - bimp_08) +
      (5 - bimp_07) + (5 - bimp_13) + bimp_14 +
      (5 - bimp_15) + (5 - bimp_33) + (5 - bimp_10) +
      bimp_31 + bimp_18 + bimp_22 + bimp_16 + (5 - bimp_34) +
      bimp_24 + bimp_30 + bimp_06 + bimp_25,
    maximizing = max_nen_01m + max_nen_02 + max_nen_03 +
      max_nen_04m + max_nen_05 + max_nen_06,
    ant.score = ifelse(ant1 == 25 & ant2 == 20, 4, 
                      ifelse(ant1 == 25 & ant2 != 20 & ant2 == 50, 4, 
                             ifelse(ant1 == 25 & ant2 != 20 & ant2 != 50, 3,
                                    ifelse(ant1 != 25 & ant4 == 30, 2, 1))))
    
    )


participants <- merge(participants, condition.table, 
                      by.x = "conditionNr", by.y = "uni.condition")

# p.game

p.game <- expand.grid(workerid = valid.workers, 
                      gamenum = 1:2)

for(i in 1:nrow(p.game)) {
  
  workerid.i <- p.game$workerid[i]
  gamenum.i <- p.game$gamenum[i]
  
  order.i <- subset(optionorder, workerid == workerid.i)$optionorder[gamenum.i]
  trial.i <- subset(trial, workerid == workerid.i & playergamenum == gamenum.i)  
  
  uni.condition.i <- subset(unipark, workerid == workerid.i)$conditionNr
  
  game.type.i <- condition.table$game.type[condition.table$uni.condition == uni.condition.i]
  n.options.i <- condition.table$n.options[condition.table$uni.condition == uni.condition.i]
  option.sd <- condition.table$option.sd[condition.table$uni.condition == uni.condition.i]
  
  n.keeps.i <- sum(trial.i$mode == "keep")
  n.peeks.i <- sum(trial.i$mode == "peek")
  final.points.i <- trial.i$cumreward[trial.i$trial == max(trial.i$trial)]
  
  p.game$n.keeps[i] <- n.keeps.i
  p.game$n.peeks[i] <- n.peeks.i
  p.game$n.trials[i] <- max(trial.i$trial)
  p.game$final.points[i] <- final.points.i
  

}


# Add game data to participants table

participants <- merge(participants, subset(p.game, gamenum == 1), by = "workerid")
participants <- participants[, names(participants) != "gamenum"]
names(participants)[names(participants) %in% c("n.keeps", "n.peeks", "n.trials", "final.points")] <- c("g1.n.keeps", "g1.n.peeks", "g1.n.trials", "g1.final.points")


participants <- merge(participants, subset(p.game, gamenum == 2), by = "workerid")
participants <- participants[, names(participants) != "gamenum"]
names(participants)[names(participants) %in% c("n.keeps", "n.peeks", "n.trials", "final.points")] <- c("g2.n.keeps", "g2.n.peeks", "g2.n.trials", "g2.final.points")

participants <- participants %>%
  mutate(
    all.points = g1.final.points + g2.final.points,
    game1.reward = ifelse(g1.final.points < 0, 0, g1.final.points / 10) / 100,
    game2.reward = ifelse(g2.final.points < 0, 0, g2.final.points / 10) / 100,
    all.reward = round(game1.reward + game2.reward, 2)
    )




# Analysis
library(beanplot)
beanplot(g1.final.points ~ n.options, data = participants, col = "white")
beanplot(g1.final.points ~ option.sd + n.options, data = participants, col = "white")
beanplot(g1.final.points ~ game.type, data = participants, col = "white")


