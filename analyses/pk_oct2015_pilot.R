
# October 2015 Peeks Keeps Pilot
# 16 participants



# --------------------------------
#  Load packages
# --------------------------------

options(stringsAsFactors = F)
library(dplyr)
library(beanplot)
#library(yarrr)
library(MCMCglmm)
library(stringr)


# --------------------------------
#  Custom Functions
# --------------------------------

betabinom.hdi <- function(vec, alpha.prior = 1, beta.prior = 1, quant = .95, na.rm = T) {
  
  vec <- as.vector(vec)
  
  if(na.rm) {
    
    vec <- vec[is.finite(vec)]
    
  }
    
  
  n <- length(vec)
  successes <- sum(vec == 1)
  failures <- sum(vec == 0)
  
  alpha <- successes + alpha.prior
  beta <- failures + beta.prior
  
  lb <- qbeta((1 - quant) / 2, shape1 = alpha, shape2 = beta)
  ub <- qbeta(1 - ((1 - quant) / 2), shape1 = alpha, shape2 = beta)
  mode <- alpha / (beta + alpha)
  
  
  return(list("lb" = lb, "ub" = ub, "mode" = mode))
  
}
  
add.binom.p.bar <- function(vec, alpha.prior = 1, beta.prior = 1, 
                            quant = .95, bar.col, location = 1, 
                            bar.width = .5, add.text = T) {
  
  
  lb <- betabinom.hdi(vec)$lb
  ub <- betabinom.hdi(vec)$ub
  
  rect(location - bar.width / 2, 0, location + bar.width / 2, mean(vec), col = bar.col)
  segments(location, lb, location, ub, lwd = 2)
  
  if(add.text) {
    
    text(location + bar.width / 2, mean(vec), labels = round(mean(vec), 2), pos = 4)
  }
    
  }



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
                              game.type = rep(c("ptk", "k", "pak"), each = 4),
                              n.options = rep(c(2, 4), each = 2, times = 3),
                              option.sd = rep(c(5, 20), times = 6)
                              )

# Determine valid.works who completed the study and entered
#  code on mturk

valid.workers <- mturk$WorkerId


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

participants <- participants %>%
  mutate(
    conditionNr.c = ifelse(HITId != "3XEIP58NL0O04ODNMEQV1T04TKLLZD" & 
                             conditionNr == 4, 2, conditionNr)
  )



participants <- merge(participants, condition.table, 
                      by.x = "conditionNr.c", by.y = "uni.condition")


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
  
  workergame.i <- workergame.vec[i]
  option.order.i <- optionorder$optionorder[optionorder$worker.game == workergame.i][1]
  option.order.i <- str_replace_all(option.order.i, ",", replacement = "")
  option.order.i <- unlist(str_split(option.order.i, pattern = ""))
  best.option <- toupper(letters[which(option.order.i == 1)])
  
  if(length(best.option) != 1) {print(i)}
  
  d <- subset(actions, worker.game == workergame.i)
  n.trials <- nrow(d)
  
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
  
  if(i == 1) {actions.n <- d}
  if(i > 1) {actions.n <- rbind(actions.n, d)}

}

actions <- actions.n


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
#  Analyses
# --------------------------------



# --------------------------------
# Plot individual participant's data
# pak participant curves.pdf
# --------------------------------
{
plot.participant <- function(worker.i, 
                             col.vec = c("blue", "black", "green")
                             ) {
  
if(length(col.vec) < 6) {col.vec <- rep(col.vec, length.out = 6)}
  
  
worker.data <- subset(trial, workerid == worker.i & playergamenum == game.num)

game.type.i <- worker.data$game.type[1]
n.options.i <- worker.data$n.options[1]
option.sd.i <- worker.data$option.sd[1]


layout(matrix(c(1, 1, 2, 3), nrow = 2, ncol = 2, byrow = T), 
       widths = c(4, 4), heights = c(.85, 4))

# Top plot

par(mar = rep(0, 4))

plot(1, xlim = c(0, 1), ylim = c(0, 1), 
     xaxt = "n", yaxt = "n", xlab = "", ylab = "", type = "n", bty = "n")

text(rep(.5, 4), 
     c(.7, .45, .25, .05), 
     c(worker.i, paste("Type =", game.type.i), 
       paste("N =", n.options.i), 
       paste("sd =", option.sd.i)),
     cex = c(2, 1.5, 1.5, 1.5)
     )
     
#paste(worker.i, "\n", game.type.i, ", n = ", n.options.i, ", sd = ", option.sd.i, sep = ""), cex = 2)

par(mar = c(5, 4, 4, 1) + .1)


for(game.num in c(1, 2)) {

game.data <- subset(trial, workerid == worker.i & playergamenum == game.num)
game.data <- game.data[order(game.data$trial),]

par(mar = c(5, 5, 4, 3))

plot(1, xlim = c(0, 120), ylim = c(-500, 500), type = "n", 
     main = paste("Game", game.num), 
     ylab = "Cumulative Points", xlab = "Trial", 
     xaxt = "n", yaxt = "n", bty = "n", cex.main = 1.5)


par(xpd = NA)

segments(20, 510, 40, 510, col = col.vec[2], lwd = 2)
text(20, 530, "Peeks", adj = 0)

segments(60, 510, 80, 510, col = col.vec[1], lwd = 2)
text(60, 530, "Keeps", adj = 0)

par(xpd = FALSE)

axis(1, at = seq(0, 100, 20))
axis(2, at = seq(-500, 500, 100), las = 1)

# Add margin text for final points

text(103, game.data$keep.points.cum[nrow(game.data)], 
     game.data$keep.points.cum[nrow(game.data)], adj = 0)


text(103, game.data$peek.points.cum[nrow(game.data)], 
     game.data$peek.points.cum[nrow(game.data)], adj = 0)


h.line.loc <- seq(-500, 500, 100)
v.line.loc <- seq(0, 100, 20)

segments(rep(0, length(h.line.loc)), 
         h.line.loc, 
         rep(100, length(h.line.loc)),
         h.line.loc, col = gray(.9)
)

segments(
         v.line.loc, 
         rep(-500, length(v.line.loc)), 
         v.line.loc, 
         rep(500, length(v.line.loc)),
         col = gray(.9)
)

abline(h = 0, col = "red", lty = 2)

lines(1:100, game.data$keep.points.cum, col = col.vec[1], lwd = 2)
lines(1:100, game.data$peek.points.cum, col = col.vec[2], lwd = 2)

peek.trials <- game.data$trial[game.data$mode == "peek"]
keep.trials <- game.data$trial[game.data$mode == "keep"]

pch.vec <- rep(16, nrow(game.data))
#pch.vec[game.data$option.switch == TRUE] <- 24

points(peek.trials, 
       game.data$peek.points.cum[peek.trials], 
       pch = pch.vec[peek.trials], col = col.vec[2], cex = 1.2)

#points(keep.trials, game.data$keep.points.cum[keep.trials], pch = pch.vec[keep.trials], bg = keep.col, cex = .8)

# Add select best line

text(102, -195, "Select Best", adj = 0)
select.best.trials <- game.data$trial[game.data$select.best == TRUE]
rect(select.best.trials - .5, 
     rep(-200, length(select.best.trials)),
     select.best.trials + .5,
     rep(-190, length(select.best.trials)),
     col = col.vec[4], lwd = .5
     )

# Add option switch line

text(102, -245, "Option Change", adj = 0)
option.switch.trials <- game.data$trial[game.data$option.switch == TRUE]
rect(option.switch.trials - .5, 
     rep(-250, length(option.switch.trials)),
     option.switch.trials + .5,
     rep(-240, length(option.switch.trials)),
     col = col.vec[5], lwd = .5
)


# Add option switch line

text(102, -295, "Mode Change", adj = 0)
mode.switch.trials <- game.data$trial[game.data$mode.switch == TRUE]
rect(mode.switch.trials - .5, 
     rep(-300, length(mode.switch.trials)),
     mode.switch.trials + .5,
     rep(-290, length(mode.switch.trials)),
     col = col.vec[6], lwd = .5
)


# Add mean totals
# {
# bar.min <- -450
# bar.height.max <- 200 
# 
# rect(0, bar.min, 30, bar.min + bar.height.max, col = "white", border = NA)
# 
# rect(0, bar.min, 10, 
#      bar.min + sum(game.data$mode == "peek") / 100 * bar.height.max)
# 
# text(5, bar.min + sum(game.data$mode == "peek") / 100 * bar.height.max,
#      sum(game.data$mode == "peek"), pos = 3)
# 
# text(5, bar.min, labels = "p", pos = 1)
# 
# rect(10, bar.min, 20,
#      bar.min + sum(game.data$option.switch, na.rm = T) / 100 * bar.height.max)
# 
# text(15, bar.min + sum(game.data$option.switch, na.rm = T) / 100 * bar.height.max,
#      sum(game.data$option.switch, na.rm = T), pos = 3)
# 
# text(15, bar.min, labels = "os", pos = 1)
# 
# rect(20, bar.min, 30,
#      bar.min +  sum(game.data$mode.switch, na.rm = T) / 100 * bar.height.max)
# 
# text(25, bar.min + sum(game.data$mode.switch, na.rm = T) / 100 * bar.height.max,
#      sum(game.data$mode.switch, na.rm = T), pos = 3)
# 
# text(25, bar.min, labels = "ms", pos = 1)
# }

}

}


pdf("figures/pak participant curves.pdf", width = 12, height = 8)
worker.vec <- unique(subset(participants, game.type %in% c("pak"))$workerid)

for(i in worker.vec) {
  
  if(setequal(table(trial$playergamenum[trial$workerid == i]), c(100, 100)) &
    setequal(names(table(trial$playergamenum[trial$workerid == i])), c("1", "2"))
       ) {
  
  plot.participant(i, piratepal(palette = "google", trans = .3))
  
  }
  
}

dev.off()
}


# --------------------------------
# Icon Array by Participant
# participant icons.pdf
# --------------------------------

{
  
pdf("figures/participant peeks over time.pdf", width = 16, height = 12)

cond.vec <- c(1:4, 9:12)

col.1 <- gray(.9)
col.2 <- "white"

line.col.vec <- rep(piratepal("info")[c(1, 2, 4, 5)], 4)

    my.text <- "Peek Rate"
    cond.vec <- cond.vec <- c(1:4, 9:12)
    layout(matrix(c(1:5, 1, 6:9, 1, 10:13, 1, 14:17), nrow = 5, ncol = 4, byrow = F), 
           widths = rep(5, 4), heights = c(2, rep(5, 4)))

  

# Header plot
  
  par(mar = rep(0, 4))
  
  plot(1, type = "n", xaxt = "n", xlab = "", xlim = c(0, 1), ylim = c(0, 1), bty = "n")
  
  text(.5, .5, my.text,  cex = 3)

  
for(cond.i in cond.vec) {

game.type.i <- condition.table$game.type[condition.table$uni.condition == cond.i]  
n.options.i <- condition.table$n.options[condition.table$uni.condition == cond.i]
option.sd.i <- condition.table$option.sd[condition.table$uni.condition == cond.i]

data <- subset(actions, conditionNr.c == cond.i)
data$peek.bin <- data$mode == "peek"

worker.vec <- unique(data$workerid)


# Top plot
par(mar = c(3, 4, 4, 1) + .1)
plot(1, xlim = c(0, 100), ylim = c(0, 1), type = "n", bty = "n", 
     xlab = "", ylab = "p(Peek)", 
     main = paste("Game = ", game.type.i, "\n",
                  "N = ", n.options.i, "\n",
                  "SD = ", option.sd.i, sep = ""
                  ), 
     cex.main = 1)

abline(h = seq(0, 1, .1), lwd = c(1, 2), col = gray(.9))
abline(v = seq(0, 100, 10), lwd = c(1, 2), col = gray(.9))



avg <- data %>%
  group_by(trial) %>%
  summarise(
    peek.mean = mean(peek.bin)
  )

lines(1:100, avg$peek.mean, col = line.col.vec[cond.i], lwd = 5)

mtext(paste("Mean", round(mean(data$peek.bin), 2), sep = "\n"), side = 3, at = 90, line = 1)

# Bottom Plot

par(mar = c(3, 4, 0, 1) + .1)
plot(1, xlim = c(0, 100), ylim = c(0, length(worker.vec) + 2), 
     type = "n", ylab = "", yaxt = "n", bty = "n")

for(i in 1:length(worker.vec)) {
 
vec <- data$peek.bin[data$workerid == worker.vec[i]]
   
 col.vec <- rep(col.1, 100)
 col.vec[vec == FALSE] <- col.2
 
 rect(1:100 - .5, 
      rep(i, 100) - .5, 
      1:100 + .5, 
      rep(i, 100) + .5,
      col = col.vec, border = "white", lwd = .5)
 
}

mtext(text = 1:length(worker.vec), at = 1:length(worker.vec), side = 2, cex = .4, las = 1)

}


dev.off()
}


{
pdf("figures/participant option switch over time.pdf", width = 24, height = 12)


col.1 <- gray(.9)
col.2 <- "white"
line.col.vec <- rep(piratepal("info")[c(1, 2, 4, 5)], 4)

my.text <- "Option Switch Rate"
cond.vec <- cond.vec <- c(1:4, 9:12, 5:8)

layout(matrix(c(1:5, 1, 6:9, 1, 10:13, 1, 14:17, 1, 18:21, 1, 22:25), nrow = 5, ncol = 6, byrow = F), 
       widths = rep(5, 6), heights = c(1.5, rep(5, 4)))

# Header plot

par(mar = rep(0, 4))

plot(1, type = "n", xaxt = "n", xlab = "", xlim = c(0, 1), ylim = c(0, 1), bty = "n")

text(.5, .5, my.text,  cex = 3)

for(cond.i in cond.vec) {
  
  game.type.i <- condition.table$game.type[condition.table$uni.condition == cond.i]  
  n.options.i <- condition.table$n.options[condition.table$uni.condition == cond.i]
  option.sd.i <- condition.table$option.sd[condition.table$uni.condition == cond.i]
  
  data <- subset(actions, conditionNr.c == cond.i)
  data$peek.bin <- data$mode == "peek"
  
  worker.vec <- unique(data$workerid)
  
  
  # Top plot
  par(mar = c(3, 4, 4, 1) + .1)
  plot(1, xlim = c(0, 100), ylim = c(0, 1), type = "n", bty = "n", 
       xlab = "", ylab = "p(Peek)", 
       main = paste("Game = ", game.type.i, "\n",
                    "N = ", n.options.i, "\n",
                    "SD = ", option.sd.i, sep = ""
       ), 
       cex.main = 1)
  
  mtext(paste("Mean", round(mean(data$option.switch, na.rm = T), 2), sep = "\n"), side = 3, at = 90, line = 1)
  
  
  abline(h = seq(0, 1, .1), lwd = c(1, 2), col = gray(.9))
  abline(v = seq(0, 100, 10), lwd = c(1, 2), col = gray(.9))
  

    
    avg <- data %>%
      group_by(trial) %>%
      summarise(
        switch.mean = mean(option.switch)
      )
    
    lines(1:100, avg$switch.mean, col = line.col.vec[cond.i], lwd = 5)
    

  
  # Bottom Plot
  
  par(mar = c(3, 4, 1, 1) + .1)
  plot(1, xlim = c(0, 100), ylim = c(0, length(worker.vec) + 2), 
       type = "n", ylab = "Participants", yaxt = "n", bty = "n")
  
  for(i in 1:length(worker.vec)) {
    

   vec <- data$option.switch[data$workerid == worker.vec[i]]
    
    col.vec <- rep(col.1, 100)
    col.vec[vec == FALSE] <- col.2
    
    rect(1:100 - .5, 
         rep(i, 100) - .5, 
         1:100 + .5, 
         rep(i, 100) + .5,
         col = col.vec, border = "white", lwd = .5)
    
  }
  
  mtext(text = 1:length(worker.vec), at = 1:length(worker.vec), side = 2, cex = .4, las = 1)
  
}


dev.off()
}


# --------------------------------
# Points over time by condition
# points over time.pdf
# --------------------------------
{
pdf("figures/points over time.pdf", width = 20, height = 20)

par(mfcol = c(4, 3))
par(mar = c(5, 4, 4, 1))

for (i in 1:12) {
  
  game.type.i <- condition.table$game.type[i]
  n.options.i <- condition.table$n.options[i]
  option.sd.i <- condition.table$option.sd[i]

  plot(1, xlim = c(0, 101), ylim = c(-300, 500), type = "n", 
       main = paste(game.type.i, n.options.i, option.sd.i))
  
  abline(h = 0, lty = 2)
  
  # Game 1

  dat.temp <- subset(trial, conditionNr.c == i & playergamenum == 1)
  
  workers.vec <- unique(dat.temp$workerid)
  
  for(worker.i in workers.vec) {
    
  worker.data <- dat.temp %>%
              subset(workerid == worker.i) %>%
              arrange(trial)
  
  lines(worker.data$trial, worker.data$cumreward, col = gray(.1, alpha = .1))
    
  }
  
  avg <- dat.temp %>% 
    group_by(trial) %>%
    summarise(
      mean.points = mean(cumreward),
      median.points = median(cumreward)
      )
  
  lines(avg$trial, avg$median.points, lwd = 2)
  
  
  # Game 2
  
  dat.temp <- subset(trial, conditionNr.c == i & playergamenum == 2)
  
  workers.vec <- unique(dat.temp$workerid)
  
  for(worker.i in workers.vec) {
    
    worker.data <- dat.temp %>%
      subset(workerid == worker.i) %>%
      arrange(trial)
    
    lines(worker.data$trial, worker.data$cumreward, col = transparent("blue", .2))
    
  }
  
  avg <- dat.temp %>% 
    group_by(trial) %>%
    summarise(
      mean.points = mean(cumreward),
      median.points = median(cumreward)
    )
  
  lines(avg$trial, avg$median.points, col = transparent("blue"), lwd = 2)
  
  
}
    
dev.off()
}    
    
# --------------------------------
# Peek Rate by condition
# peeks over time.pdf
# --------------------------------
{
pdf("figures/peeks over time.pdf", width = 8.5, height = 6.5)


plot(1, xlim = c(1, 10), ylim = c(0, .6), type = "n", 
     xlab = "trial", ylab = "p(Peek)", main = "Peek Rates by Trial Block", xaxt = "n")

mtext("Bars are 95\\% HDIs", side = 3)

col.vec <- piratepal("google")

#rect(-100, -100, 100, 100, col = gray(.97))

abline(h = seq(0, .6, .1), col = gray(.9), lwd = 2)
abline(h = seq(.05, .65, .1), col = gray(.9), lwd = 1)
abline(v = 1:10, col = gray(.9), lwd = 1)

legend("topright", 
       c("n = 2, sd = 5", "n = 2, sd = 20", "n = 4, sd = 5", "n = 4, sd = 20"), 
       lty = 1, col = col.vec, lwd = 3, bg = "white")

rect.min <- .4
rect.max <- .6
locations <- c(4, 5, 6, 7)
rect.width <- .7


rect(3.5, .38, 7.5, .55, col = gray(level = 1, alpha = .7), border = NA)


for(i in 1:4) {
  
 binom.hdi <- actions %>%
          filter(conditionNr.c == 8 + i) %>%
          mutate(
            trial.cut = cut(trial, breaks = seq(0, 100, 10)),
            peek = mode == "peek"
          ) %>%
    group_by(trial.cut) %>%
    summarise(
      lb = betabinom.hdi(vec = peek)$lb,
      ub = betabinom.hdi(vec = peek)$ub
    )
    
  
 agg <- aggregate(mode == "peek" ~ trial.cut, data = subset(data, conditionNr.c == 8 + i), FUN = mean)
  
 lines(agg[,1], agg[,2], col = col.vec[i], lwd = 3)
 
 polygon(c(1:10, 10:1), c(binom.hdi$lb, rev(binom.hdi$ub)), 
         col = transparent(col.vec[i], trans.val = .05), border = NA)
 
 overall <- betabinom.hdi(subset(actions, conditionNr.c == 8 + i)$mode == "peek")
 
 # Add overall bin
 
 rect(locations[i] - rect.width / 2, 
      rect.min, 
      locations[i] + rect.width / 2,
      rect.min + overall$mode * (rect.max - rect.min),
      col = col.vec[i])
 
 text(locations[i], rect.min + overall$mode * (rect.max - rect.min), round(overall$mode, 2), pos = 3)
 segments(locations[i] + .5, rect.min + overall$lb * (rect.max - rect.min), 
          locations[i] + .5, rect.min + overall$ub * (rect.max - rect.min), col = col.vec[i], lwd = 2)
 
 
}

axis(side = 1, labels = binom.hdi$trial.cut, at = 1:10)
text(5.5, .52, "All Trials", cex = 1.5)


dev.off()

}

  
# --------------------------------
# miscellaneous
# miscellaneous.pdf
# --------------------------------
{
pdf("figures/option and mode switching.pdf", width = 12, height = 12)


my.col <- piratepal("google", trans = .5)

games.p <- subset(games, game.type %in% c("pak"))
actions.p <- subset(actions, game.type %in% c("pak"))

layout(matrix(1:4, nrow = 2, ncol = 2, byrow = T), widths = c(6, 6), heights = c(6, 6))

# Peeks by option switching

with(games.p, 
     plot(n.peeks, option.switch.n, xlab = "N Peeks", ylab = "N Option Switch",
          main = "Peeks v. Option Switch",
          pch = 16, col = gray(.2, .2)
          ))
abline(lm(option.switch.n ~ n.peeks, 
          data = games.p), lwd = 2)

mtext(text = paste("cor =", round(with(subset(games.p, is.finite(n.peeks) & is.finite(option.switch.n)), cor(n.peeks, option.switch.n)), 2),
                   "p = ", round(with(subset(games.p, is.finite(n.peeks) & is.finite(option.switch.n)), cor.test(n.peeks, option.switch.n)$p.value), 3)
                   ), 3)


# Mode Switch by option switch

plot(1, xlim = c(0, 9), ylim = c(0, .6), xaxt = "n", 
     xlab = "Switch Options", type = "n", main = "Mode Switching by Option Switching", ylab = "Mode Switch")

rect(-100, -100, 100, 100, col = gray(.97))
abline(h = seq(0, 1, .1), col = "white", lwd = c(1, 2))


cond.vec <- 9:12

for(i in 1:length(cond.vec)) {

add.binom.p.bar(subset(actions.p, option.switch == F & conditionNr.c == cond.vec[i])$mode.switch, bar.col = my.col[i], 
                location = (i - 1) * 2 + 1, bar.width = .4)

add.binom.p.bar(subset(actions.p, option.switch == T & conditionNr.c == cond.vec[i])$mode.switch, bar.col = my.col[i], 
                location = (i) * 2, bar.width = .4)

mtext(paste(condition.table$n.options[i], ", ", condition.table$option.sd[i], sep = ""), side = 1, at = (i - 1) * 2 + 1 + .5)


}

mtext(c("No", "Yes"), side = 1, at = 1:8, line = 1.5)




# Peek by option switch

# Mode Switch by option switch

plot(1, xlim = c(0, 9), ylim = c(0, .6), xaxt = "n", 
     xlab = "Switch Options", type = "n", main = "Mode Switching by Option Switching", ylab = "Peek")

rect(-100, -100, 100, 100, col = gray(.97))
abline(h = seq(0, 1, .1), col = "white", lwd = c(1, 2))


cond.vec <- 9:12

for(i in 1:length(cond.vec)) {
  
  add.binom.p.bar(subset(actions.p, option.switch == F & conditionNr.c == cond.vec[i])$mode == "peek", bar.col = my.col[i], 
                  location = (i - 1) * 2 + 1, bar.width = .4)
  
  add.binom.p.bar(subset(actions.p, option.switch == T & conditionNr.c == cond.vec[i])$mode == "peek", bar.col = my.col[i], 
                  location = (i) * 2, bar.width = .4)
  
  mtext(paste(condition.table$n.options[i], ", ", condition.table$option.sd[i], sep = ""), side = 1, at = (i - 1) * 2 + 1 + .5)
  
  
}

mtext(c("No", "Yes"), side = 1, at = 1:8, line = 1.5)





dev.off()

}










## Analysis


mod1 <- MCMCglmm(g1.final.points ~ game.type * as.factor(n.options), 
                 random = ~workerid,
                 data = subset(participants, option.sd == 5)
                 )

mod1b <- MCMCglmm(g1.final.points ~ game.type, 
                  random = ~workerid,
                  data = subset(participants, option.sd == 5 & n.options == 4)
)


mod3 <- MCMCglmm(g2.final.points ~ game.type * n.options, 
                 random = ~workerid,
                 data = subset(participants, option.sd == 5)
)





# DV = N.peeks

col.vec <- c(transparent("green", .2), "white")

beanplot(g1.n.peeks ~ game.type + n.options + option.sd, 
         data = subset(participants, option.sd == 5 & game.type %in% c("pak", "ptk")), beanlines = "median", 
         main = "Game 1 and Game 2 Peeks", 
         col = col.vec[1], at = 1:4 * 2 - .5, xlim = c(1, 10))

beanplot(g2.n.peeks ~ game.type + n.options + option.sd, 
         data = subset(participants, option.sd == 5 & game.type %in% c("pak", "ptk")), 
         col = col.vec[2], beanlines = "median", 
         at = 1:4 * 2 + .5, add = T, xaxt = "n")

abline(h = 0, col = "red", lty = 2)

legend("bottomright", legend = c("Game 1", "Game 2"), pt.bg = c(col.vec), pch = 21)




col.vec <- c(transparent("green", .2), "white")

beanplot(g1.n.peeks ~ game.type + n.options + option.sd, 
         data = subset(participants, option.sd == 20 & game.type %in% c("pak", "ptk")), beanlines = "median", 
         main = "Game 1 and Game 2 Peeks", 
         col = col.vec[1], at = 1:4 * 2 - .5, xlim = c(1, 10))

beanplot(g2.n.peeks ~ game.type + n.options + option.sd, 
         data = subset(participants, option.sd == 20 & game.type %in% c("pak", "ptk")), 
         col = col.vec[2], beanlines = "median", 
         at = 1:4 * 2 + .5, add = T, xaxt = "n")

abline(h = 0, col = "red", lty = 2)

legend("bottomright", legend = c("Game 1", "Game 2"), pt.bg = c(col.vec), pch = 21)







beanplot(g1.n.peeks ~ n.options + option.sd + game.type, 
         data = subset(participants, game.type %in% c("pak", "ptk")), col = "white")

beanplot(g2.n.peeks ~ n.options + option.sd + game.type, 
         data = subset(participants, game.type %in% c("pak", "ptk")), col = "white")

beanplot(n.peeks.d ~ n.options + option.sd + game.type, 
         data = subset(participants, game.type %in% c("pak", "ptk")), col = "white")
abline(h = 0, col = "red", lty = 2)


## Analysis


mod1 <- MCMCglmm(g1.n.peeks ~ game.type * n.options, 
                 random = ~workerid, family = "poisson",
                 data = subset(participants, option.sd == 5 & game.type %in% c("pak", "ptk"))
)


mod3 <- MCMCglmm(g2.final.points ~ game.type * n.options, 
                 random = ~workerid,
                 data = subset(participants, option.sd == 5)
)




pak <- trial %>%
  filter(game.type == "pak") %>%
  group_by(trial, playergamenum) %>%
  summarise(
    peek.p = mean(mode == "peek"),
    n = n()
    
    )

ptk <- trial %>%
  filter(game.type == "ptk") %>%
  group_by(trial, playergamenum) %>%
  summarise(
    peek.p = mean(mode == "peek"),
    n = n()
    
  )

with(subset(pak, playergamenum == 1), plot(trial, peek.p, ylim = c(0, .7)))
with(subset(ptk, playergamenum == 1), points(trial, peek.p, ylim = c(0, .7), col = "red"))


with(subset(pak, playergamenum == 2), plot(trial, peek.p, ylim = c(0, .7)))
with(subset(ptk, playergamenum == 2), points(trial, peek.p, ylim = c(0, .7), col = "red"))


summary(lm(g1.final.points ~ g1.n.peeks * game.type, 
   data = subset(participants, game.type %in% c("pak", "ptk"))))


beanplot(g1.final.points ~ ant.score, 
         data = subset(participants, game.type %in% c("pak", "ptk")))


par(mfrow = c(2, 2))

with(participants[participants$game.type == "pak",], plot(g1.n.peeks, g1.final.points))
with(participants[participants$game.type == "ptk",], plot(g1.n.peeks, g1.final.points))

with(participants[participants$game.type == "pak",], plot(g2.n.peeks, g2.final.points))
with(participants[participants$game.type == "ptk",], plot(g2.n.peeks, g2.final.points))



