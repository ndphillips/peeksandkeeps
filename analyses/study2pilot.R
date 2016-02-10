# Study 2 Pilot

library(dplyr)

s2 <- read.table("data/study2_jan2016/pilotdata.txt", sep = "\t", header = T, stringsAsFactors = F)
s2.dist <- read.table("data/study2_jan2016/pilotdistributions.txt", sep = "\t", header = T, stringsAsFactors = F)
s2.uni <- read.table("data/study2_jan2016/unipark.txt", sep = "\t", header = T, stringsAsFactors = F)
s2.cond <- read.table("data/study2_jan2016/conditionlu.txt", sep = "\t", header = T, stringsAsFactors = F)

s2.cond$experiment.num <- rep(c(1, 2), each = 9)

valid.workers <- s2.uni$workerid
valid.workers <- valid.workers[substr(valid.workers, 1, 1) == "A"]

s2 <- subset(s2, workerid %in% valid.workers)
s2.uni <- subset(s2.uni,workerid %in% valid.workers)

s2.uni$experiment.num[s2.uni$lfdn <= 36] <- 1
s2.uni$experiment.num[s2.uni$lfdn > 36] <- 2

s2.workerlu <- s2.uni[c("workerid", "conditionNr", "experiment.num")]
names(s2.workerlu)[2] <- "unicondition"

s2.workerlu <- merge(s2.workerlu, s2.cond, by = c("unicondition", "experiment.num"))

s2 <- merge(s2, s2.workerlu[c("unicondition", "workerid", "options", "gametype", "optionsd", "experiment.num")], by = "workerid")


peek.summaries <- s2 %>%
  group_by(workerid, workergame) %>%
  summarise(
    n.peeks = sum(action == "peek")
  )

point.summaries <- s2 %>%
  group_by(workerid, workergame) %>%
  filter(trial == 100) %>%
  summarise(
    cumreward = cumreward[1]
  )


game.summaries <- merge(point.summaries, 
                        peek.summaries, by = c("workerid", "workergame"))


game.summaries <- merge(game.summaries,
                        s2.workerlu, by = "workerid"
                        )



library(yarrr)

pirateplot("cumreward" ,iv.name = "gametype", 
           data = subset(game.summaries, options == 6), ylim = c(-400, 600)
           )



final <- subset(s2, trial == 100)

total.games <- final %>% 
  subset(workerid != "A2J5DN5UMU0A3Z") %>%
  group_by(workerid) %>%
  
  summarise(
    total.points = sum(cumreward),
    condition = condition[1],
    game.type = game.type[1],
    options = options[1]
    
    
  )



point.totals <- final %>% group_by(workerid) %>%
  subset(workergame <= 3) %>%
  summarise(
    total.points = sum(cumreward),
    games = n()
    
    
  )