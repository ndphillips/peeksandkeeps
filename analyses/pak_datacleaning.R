# Peeks and Keeps data cleaning

library("xtable")
library("dplyr")
library("snowfall")
library("RColorBrewer")
library("yarrr")
library("beanplot")
library("MCMCglmm")
library("RColorBrewer")
library("BEST")
library("xtable")
library("stringr")
options(stringsAsFactors = F)


# --------------------------------
#  Read Data
# --------------------------------
{
  
  actions <- read.table("data/oct2015/peekkeep_oct2015_trialdata.txt", 
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

# --------------------------------
#  Clean data and create tables
# --------------------------------
{
  
  optionorder <- optionorder %>%
    mutate(
      worker.game = paste(workerid, playergamenum)
    )
  
  
  # condition.table
  # Table showing a participant's game type, number of options, and 
  #  option standard deviation as a function of their unipark condition
  
  condition.table <- data.frame(uni.condition = 1:12,
                                environment = rep(1:4, times = 3),
                                game.type = rep(c("ptk", "k", "pak"), each = 4),
                                n.options = rep(c(2, 4), each = 2, times = 3),
                                option.sd = rep(c(5, 20), times = 6)
  )
  
  
  
  # Only include first game when players played more than once!
  
  unipark <- unipark[with(unipark, order(workerid, lfdn)),]
  unipark$doublepart <- duplicated(unipark$workerid)
  
  single.game.workers <- unique(unipark$workerid[unipark$doublepart == F])
  
  
  # Determine valid.works who completed the study and entered
  #  code on mturk
  
  valid.workers <- intersect(single.game.workers, mturk$WorkerId)
  
  
  # Subset datasets to only include valid workers
  actions <- subset(actions, workerid %in% valid.workers)
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
      maximizing = max_nen_01 + max_nen_02 + max_nen_03 +
        max_nen_04 + max_nen_05 + max_nen_06,
      ant.score = ifelse(ant1 == 25 & ant2 == 20, 4, 
                         ifelse(ant1 == 25 & ant2 != 20 & ant2 == 50, 4, 
                                ifelse(ant1 == 25 & ant2 != 20 & ant2 != 50, 3,
                                       ifelse(ant1 != 25 & ant4 == 30, 2, 1))))
      
    )
  
  
  participants <- merge(participants, mturk[c("WorkerId", "HITId")], 
                        by.x = "workerid", by.y = "WorkerId")
  
  participants$workerid <- paste(participants$workerid)
  
  participants <- participants %>%
    mutate(
      conditionNr.c = ifelse(HITId != "3XEIP58NL0O04ODNMEQV1T04TKLLZD" & 
                               conditionNr == 4, 2, conditionNr)
    )
  
  
  gameid.lu <- data.frame(
    
    gameid = c(635, 636, 637, 638, 639, 640, 641, 642, 651, 652, 653, 654, 660),
    n.options = c(2, 2, 4, 4, 2, 2, 4, 4, 2, 2, 4, 4, 4),
    option.sd = c(5, 20, 5, 5, 5, 20, 5, 20, 5, 20, 5, 20, 20),
    environment = c(1, 2, 3, 3, 1, 2, 3, 4, 1, 2, 3, 4, 4),
    game.type = c("ptk", "ptk", "ptk", "ptk", "k", "k", "k", "k", "pak", "pak", "pak", "pak", "ptk")
    
  )
  
  
  participants <- merge(participants, worker.gameid)
  participants <- merge(participants, gameid.lu)
  
  
  # Find participants who played multiple games with different gameids
  
  
  multiple.gameids <- actions %>%
    group_by(workerid) %>%
    summarise(
      first.game = gameid[1],
      n.games = length(unique(gameid))
    ) %>%
    filter(n.games > 1)
  
  
  
  
  # Delete data for participants with multiple gameids
  
  for(i in 1:nrow(multiple.gameids)) {
    
    actions <- actions[actions$workerid != multiple.gameids$workerid[i] | 
                         (actions$workerid == multiple.gameids$workerid[i] & 
                            actions$gameid == multiple.gameids$first.game[i])
                       ,]
    
    # get lfdn for paticipant
    
    lfdn.vec <- participants$lfdn[participants$workerid == multiple.gameids$workerid[i]]
    first.lfdn <- min(lfdn.vec)
    
    participants <- participants[participants$workerid != multiple.gameids$workerid[i] | 
                                   (participants$workerid == multiple.gameids$workerid[i] & 
                                      participants$lfdn == first.lfdn)
                                 ,]
    
    
  }
  
  # Add important participant data to trial
  
  actions <- merge(actions, participants[c("workerid", "game.type", "n.options", "option.sd", "conditionNr.c")])
  
  
  
  # Add columns to trial
  
  actions <- actions[order(actions$workerid, actions$playergamenum, actions$trial),]
  
  actions$worker.game <- paste(actions$workerid, actions$playergamenum)
  
  workergame.vec <- unique(actions$worker.game)
  
  for (i in 1:length(workergame.vec)) {
    print(i)
    workergame.i <- workergame.vec[i]
    option.order.i <- optionorder$optionorder[optionorder$worker.game == workergame.i][1]
    option.order.i <- str_replace_all(option.order.i, ",", replacement = "")
    option.order.i <- unlist(str_split(option.order.i, pattern = ""))
    best.option <- toupper(letters[which(option.order.i == 1)])
    
    if(length(best.option) != 1) {print(i)}
    
    d <- subset(actions, worker.game == workergame.i)
    n.trials <- nrow(d)
    n.options <- length(option.order.i)
    
    d$option.switch <- c(NA, d$selection[2:n.trials] != d$selection[1:(n.trials - 1)])
    d$mode.switch <- c(NA, d$mode[2:n.trials] != d$mode[1:(n.trials - 1)])
    d$peek.outcome <- d$outcome
    d$peek.outcome[d$mode == "keep"] <- 0
    d$select.best <- d$selection == best.option
    
    d$ptp <- c(NA, d$mode[1:(n.trials - 1)] == "peek" & d$mode[2:(n.trials)] == "peek")
    d$ptk <- c(NA, d$mode[1:(n.trials - 1)] == "peek" & d$mode[2:(n.trials)] == "keep")
    d$ktk <- c(NA, d$mode[1:(n.trials - 1)] == "keep" & d$mode[2:(n.trials)] == "keep")
    d$ktp <- c(NA, d$mode[1:(n.trials - 1)] == "keep" & d$mode[2:(n.trials)] == "peek")
    
    d$peek.points.cum <- cumsum(d$peek.outcome)
    d$keep.points.cum <- d$cumreward
    
    
    
    # Add last option outcome and cumulative option mean
    cum.outcomes.ls <- vector("list", length = n.options)
    
    for(trial.i in 1:n.trials) {
      
      current.selection <- d$selection[trial.i]
      current.selection.num <- which(LETTERS == current.selection)
      
      n.prior.samples <- length(cum.outcomes.ls[[current.selection.num]])
      
      if(n.prior.samples == 0) {
        
        prior.mean <- NA
        prior.outcome <- NA
        prior.sd <- NA
        
      }
      
      
      
      if(n.prior.samples > 0) {
        
        prior.mean <- mean(cum.outcomes.ls[[current.selection.num]])
        prior.sd <- sd(cum.outcomes.ls[[current.selection.num]])
        prior.outcome <- cum.outcomes.ls[[current.selection.num]][n.prior.samples]
        
      }
      
      
      current.outcome <- d$outcome[trial.i]
      
      cum.outcomes.ls[[current.selection.num]] <- c(cum.outcomes.ls[[current.selection.num]], current.outcome)
      
      d$option.prior.outcome[trial.i] <- prior.outcome
      d$option.prior.mean[trial.i] <- prior.mean
      d$option.prior.sd[trial.i] <- prior.sd
      d$option.prior.samples[trial.i] <- n.prior.samples
      
      
      # Trials since last sample
      
      last.sample.trial.vec <- d$trial[d$selection == current.selection & d$trial < trial.i]
      
      if(length(last.sample.trial.vec) == 0) {trials.since.last.samp <- NA}
      if(length(last.sample.trial.vec) > 0) {trials.since.last.samp <- trial.i - max(last.sample.trial.vec)}
      
      d$trials.since.last.samp[trial.i] <- trials.since.last.samp
      
    }
    
    
    if(i == 1) {actions.n <- d}
    if(i > 1) {actions.n <- rbind(actions.n, d)}
    
  }
  
  actions <- actions.n
  
  
  
  actions$trial10.cut <- cut(actions$trial, seq(0, 100, 10))
  
  # games
  
  games <- expand.grid(workerid = unique(participants$workerid), 
                       gamenum = 1:2, stringsAsFactors = F)
  
  for(i in 1:nrow(games)) {
    
    workerid.i <- games$workerid[i]
    gamenum.i <- games$gamenum[i]
    
    order.i <- subset(optionorder, workerid == workerid.i)$optionorder[gamenum.i]
    trial.i <- subset(actions, workerid == workerid.i & playergamenum == gamenum.i)  
    
    uni.condition.i <- subset(unipark, workerid == workerid.i)$conditionNr
    
    game.type.i <- condition.table$game.type[condition.table$uni.condition == uni.condition.i]
    n.options.i <- condition.table$n.options[condition.table$uni.condition == uni.condition.i]
    option.sd <- condition.table$option.sd[condition.table$uni.condition == uni.condition.i]
    
    if(nrow(trial.i) == 100 & max(trial.i$trial) == 100) {
      
      n.keeps.i <- sum(trial.i$mode == "keep")
      n.peeks.i <- sum(trial.i$mode == "peek")
      final.points.i <- trial.i$cumreward[trial.i$trial == max(trial.i$trial)]
      n.trials.i <- max(trial.i$trial)
      
      option.switch.n <- sum(trial.i$option.switch, na.rm = T)
      mode.switch.n <-  sum(trial.i$mode.switch, na.rm = T)
      ptk.n <- sum(trial.i$ptk, na.rm = T)
      ptp.n <- sum(trial.i$ptp, na.rm = T)
      ktk.n <- sum(trial.i$ktk, na.rm = T)
      ktp.n <- sum(trial.i$ktp, na.rm = T)
      
      change.option.peek.n <- sum(trial.i$option.switch == TRUE & trial.i$mode == "peek", na.rm = T)
      change.option.keep.n <- sum(trial.i$option.switch == TRUE & trial.i$mode == "keep", na.rm = T)
      
      same.option.peek.n <- sum(trial.i$option.switch == FALSE & trial.i$mode == "peek", na.rm = T)
      same.option.keep.n <- sum(trial.i$option.switch == FALSE & trial.i$mode == "keep", na.rm = T)
      
      mode.switch.same.option.n <- sum(trial.i$mode.switch == TRUE & trial.i$option.switch == FALSE, na.rm = T)
      option.switch.same.mode.n <- sum(trial.i$mode.switch == FALSE & trial.i$option.switch == TRUE, na.rm = T)
      option.switch.same.mode.n <- sum(trial.i$mode.switch == FALSE & trial.i$option.switch == TRUE, na.rm = T)
      
    }
    
    if(nrow(trial.i) != 100 | max(trial.i$trial) != 100) {
      
      n.keeps.i <- NA
      n.peeks.i <- NA
      final.points.i <- NA
      n.trials.i <-NA
      option.switch.n <- NA
      mode.switch.n <-  NA
      ptk.n <- NA
      ptp.n <- NA
      ktk.n <- NA
      ktp.n <- NA
      change.option.peek.n <- NA
      change.option.keep.n <- NA
      
      same.option.peek.n <- NA
      same.option.keep.n <- NA
      
      mode.switch.same.option.n <- NA
      option.switch.same.mode.n <- NA
      option.switch.same.mode.n <- NA
      
      
      
    }
    
    
    if(length(final.points.i) > 1) {print(i)}
    
    games$n.keeps[i] <- n.keeps.i
    games$n.peeks[i] <- n.peeks.i
    games$n.trials[i] <- n.trials.i
    games$final.points[i] <- final.points.i
    games$option.switch.n[i] <- option.switch.n
    games$mode.switch.n[i] <- mode.switch.n
    games$ptp.n[i] <- ptp.n
    games$ptk.n[i] <- ptk.n
    games$ktp.n[i] <- ktp.n
    games$ktk.n[i] <- ktk.n
    games$change.option.peek.n[i] <- change.option.peek.n
    games$change.option.keep.n[i] <- change.option.keep.n
    
    games$same.option.peek.n[i] <- same.option.peek.n
    games$same.option.keep.n[i] <- same.option.keep.n
    
    games$mode.switch.same.option.n[i] <- mode.switch.same.option.n
    games$option.switch.same.mode.n[i] <- option.switch.same.mode.n
    games$option.switch.same.mode.n[i] <- option.switch.same.mode.n
    
  }
  
  games <- merge(games, participants[c("workerid", "game.type", "n.options", "option.sd")])
  
  
  # Add game data to participants table
  
  games.red <- games[c("n.keeps", "n.peeks", "n.trials", "final.points", "gamenum", "workerid")]
  
  participants <- merge(participants, subset(games.red, gamenum == 1), by = "workerid")
  participants <- participants[, names(participants) != "gamenum"]
  names(participants)[names(participants) %in% c("n.keeps", "n.peeks", "n.trials", "final.points")] <- c("g1.n.keeps", "g1.n.peeks", "g1.n.trials", "g1.final.points")
  
  
  participants <- merge(participants, subset(games.red, gamenum == 2), by = "workerid")
  participants <- participants[, names(participants) != "gamenum"]
  names(participants)[names(participants) %in% c("n.keeps", "n.peeks", "n.trials", "final.points")] <- c("g2.n.keeps", "g2.n.peeks", "g2.n.trials", "g2.final.points")
  
  participants <- participants %>%
    mutate(
      all.points = g1.final.points + g2.final.points,
      game1.reward = ifelse(g1.final.points < 0, 0, g1.final.points / 10) / 100,
      game2.reward = ifelse(g2.final.points < 0, 0, g2.final.points / 10) / 100,
      all.reward = round(game1.reward + game2.reward, 2),
      n.peeks.d = g2.n.peeks - g1.n.peeks,
      all.points.d = g2.final.points - g1.final.points
    )
}

# --------------------------------
#  Write Results
# --------------------------------

write.table(participants, "data/participants.txt", sep = "\t")
write.table(actions, "data/actions.txt", sep = "\t")
write.table(condition.table, "data/conditiontable.txt", sep = "\t")
