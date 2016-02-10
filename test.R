select.fun <- function(past.result.df, 
                       statistic = "mean",
                       select.rule = "egreedy"
                       ) {
  

  
  
  if(select.rule == "egreedy") {
    
    rand <- runif(1, min = 0, max = 1)
    if(rand < .05) {action.mode <- "explore"}
    if(rand >= .05) {action.mode <- "exploit"}
    
  }
  
  
if(statistic == "p.pos") {  
  
  statistic.vec <- unlist(lapply(1:ncol(past.result.df), function(x) {mean(past.result.df[,x] > 0, na.rm = T)}))
  statistic.vec[is.na(statistic.vec)] <- .5
 
}
 
  
if(statistic == "mean") {  
  
  statistic.vec <- unlist(lapply(1:ncol(past.result.df), function(x) {mean(past.result.df[,x], na.rm = T)}))
  statistic.vec[is.na(statistic.vec)] <- 0
  
}
  
best <- which(statistic.vec == max(statistic.vec))

if(length(best) > 1) {best <- sample(best, size = 1)}

  
if(action.mode == "explore") {
  
  all.options <- 1:length(statistic.vec)
  
  selection <- sample(all.options[all.options != best], 1)
  
}


if(action.mode == "exploit") {
  
  selection <- best
  
}
  
if(statistic == "p.pos") {
  
  if(statistic.vec[selection] > .5) {action <- "keep"}
  if(statistic.vec[selection] <= .5) {action <- "peek"}
  
  
}

if(statistic == "mean") {
  
  if(statistic.vec[selection] > 0) {action <- "keep"}
  if(statistic.vec[selection] <= 0) {action <- "peek"}
  
  
}


 return(list("selection" = selection, "action" = action))
  
}


# n.bad = 1
# trial.n = 500
# bad.mean = -5
# good.mean = 5
# option.sd = 10
# n.options <- 20
# n.peeks <- 0


my.sim <- function(
                   n.options = 4,
                   trial.n = 100,
                   bad.mean = -5,
                   good.mean = 5,
                   option.sd = 10,
                   peek.rule = "mean",
                   strategy = "pak",
                   strategy.par = 10
                   ) {
  
  
#   n.options = 4
#   trial.n = 100
#   bad.mean = -5
#   good.mean = 5
#   option.sd = 10
#   peek.rule <- "p.pos"
#   

  outcome.ls <- lapply(1:n.options, function(x) {
    
    if(x == 1) {return(rnorm(trial.n, mean = good.mean, sd = option.sd))}
    if(x != 1) {return(rnorm(trial.n, mean = bad.mean, sd = option.sd))}
    
    
  })
  
  
  outcome.vec <- rep(NA, trial.n)
  reward.vec <- rep(NA, trial.n)
  selection.vec <- rep(NA, trial.n)
  action.vec <- rep(NA, trial.n)
  
  outcome.df <- matrix(NA, nrow = trial.n, ncol = n.options)
  
  
  
  for(trial.i in 1:trial.n) {
    
    if(trial.i == 1) {past.result.df <- as.data.frame(matrix(NA, nrow = 1, ncol = n.options))}
    if(trial.i == 2) {past.result.df <- matrix(outcome.df[1,], nrow = 1, ncol = n.options)}
    if(trial.i > 2) {past.result.df <- outcome.df[1:(trial.i - 1),]}
    
    
    decision.result <- select.fun(past.result.df, 
                                  statistic = "p.pos", 
                                  select.rule = "egreedy"
                                  )
    
    
    if(strategy == "ptk" & trial.i <= strategy.par) {action.i <- "peek"}
    if(strategy == "ptk" & trial.i > strategy.par) {action.i <- "keep"}
    if(strategy == "pak") {action.i <- decision.result$action}
    if(strategy == "k") {action.i <- "keep"}
    
    
    selection.i <- decision.result$selection
    outcome.i <- outcome.ls[[selection.i]][trial.i]
    
    
    if(action.i == "peek") {reward.i <- 0}
    if(action.i == "keep") {reward.i <- outcome.i}
    
    
    outcome.df[trial.i, selection.i] <- outcome.i
    selection.vec[trial.i] <- selection.i
    outcome.vec[trial.i] <- outcome.i
    reward.vec[trial.i] <- reward.i
    action.vec[trial.i] <- action.i
    
    
    
    
  }
  
  result.df <- data.frame("outcome" = outcome.vec,
                          "reward" = reward.vec,
                          "selection" = selection.vec,
                          "action" = action.vec
  )  

  return(result.df)
  
}



n.sims <- 500

result.df <- expand.grid(n.options = c(2:10),
                         sim = 1:n.sims,
                         strategy = c("k", "pak"),
                         reward = NA,
                         peeks = NA
                         )


cluster.fun <- function(i) {
  
  result <- my.sim(n.options = result.df$n.options[i],
                   strategy = result.df$strategy[i]
                   )
  
  return(c(sum(result$action == "peek"), sum(result$reward)))
  
  
}



library(snowfall)
sfInit(parallel = T, cpus = 16)

sfExport("result.df")
sfExport("cluster.fun")
sfExport("select.fun")
sfExport("my.sim")

cluster.output <- sfLapply(1:nrow(result.df), cluster.fun)
cluster.result <- matrix(unlist(cluster.output), nrow = length(cluster.output), ncol = 2, byrow = T)

result.df$peeks <- cluster.result[,1]
result.df$reward <- cluster.result[,2]

library(dplyr)

agg.result <- result.df %>%
  group_by(n.options, strategy) %>%
  summarise(
    reward.mean = mean(reward),
    peeks.mean = mean(peeks),
    n = n()
  )



# Plotting
col.vec <- piratepal("basel", length.out = 12)
plot(1, xlim = c(0, 100), ylim = c(0, 5), type = "n")

for(option.i in unique(agg.result$n.options)) {
  
dat <- subset(agg.result, n.options == option.i)  
  
 lines(dat$n.peeks, dat$reward.mean, col = col.vec[which(unique(agg.result$n.options) == option.i)]) 
  
  
  
}

legend("topright", legend = unique(agg.result$n.options), 
       col = col.vec[1:which(unique(agg.result$n.options) == option.i)], lty = 1)





best.peeks <- data.frame("n.options" = 1:10,
                         "best.peeks" = NA,
                         "best.reward" = NA
                         )

for(option.i in best.peeks$n.options) {
  
  data.temp <- subset(agg.result, n.options == option.i)
  
  best.peeks.i <- min(data.temp$n.peeks[data.temp$reward.mean == max(data.temp$reward.mean)])
  best.reward.i <- data.temp$reward.mean[data.temp$n.peeks == best.peeks.i]
  
  best.peeks$best.peeks[option.i] <- best.peeks.i
  best.peeks$best.reward[option.i] <- best.reward.i
  
  
  
}

best.peeks


