# Optimal Peeks and Keeps Simulation


setwd(dir = "/Users/Nathaniel/Dropbox/Git/PeeksAndKeeps")

library(snowfall)
library(dplyr)


# Simulation 1
{

# Set parameters

n.sim <- 2000   # Number of simulations for each parameter combination
n.trials <- 100 # Number of total trials in game
good.mean <- 5
bad.mean <- -5

peeks.vec <- seq(0, 100, 5)
n.good.vec <- 1
n.bad.vec <- c(1, 3)  # 1:4
sd.vec <- c(5, 10, 20) # 0 means random sd from 1 to 30
keep.select.strategy.vec <- c(paste("egreedy-", c(".05"), sep = ""))

action.strategy.vec <- c("trial", "expectation")

peek.select.strategy.vec <- "uniform"
update.method.vec <- c("mean")   #c("rl.3", "mean"),  # mean or rl.3

environments.df <- expand.grid(n.good = n.good.vec,
                               n.bad = n.bad.vec,
                               sd = sd.vec, 
                               keep.select.strategy = keep.select.strategy.vec,
                               peek.select.strategy = peek.select.strategy.vec,
                               action.strategy = action.strategy.vec,
                               update.method = update.method.vec,
                               stringsAsFactors = F
)

  # Run n.sim simulations for each environment

for(env.i in 1:nrow(environments.df)) {
  
  print(paste("environment", env.i, "out of ", nrow(environments.df), date()))
  
  n.good.i <- environments.df$n.good[env.i]
  n.bad.i <- environments.df$n.bad[env.i]
  sd.i <- environments.df$sd[env.i]
  keep.select.strategy.i <- environments.df$keep.select.strategy[env.i]
  peek.select.strategy.i <- environments.df$peek.select.strategy[env.i]
  update.method.i <- environments.df$update.method[env.i]
  action.strategy.i <- environments.df$action.strategy[env.i]
  
  peeks.sim <- expand.grid(
    n.trials = n.trials,
    n.peeks = peeks.vec,
    action.strategy = action.strategy.i,
    peek.select.strategy = peek.select.strategy.i,# uniform, egreedy or softmax
    keep.select.strategy = keep.select.strategy.i,
    update.method = update.method.i,
    good.mean = good.mean, 
    bad.mean = bad.mean,
    n.good = n.good.i,
    n.bad = n.bad.i,
    sd = sd.i,
    sim = 1:n.sim
  )
  
  peeks.sim <- peeks.sim[!(peeks.sim$action.strategy == "expectation" & peeks.sim$n.peeks > min(peeks.sim$n.peeks)), ]
  
  
  peek.sim.fun <- function(i) {
    
    # Print current run
 #   sfCat(paste("Iteration ", i, "out of ", nrow(peeks.sim)), sep="\n")
    
    soft.max <- function(exp.vec, trial, sens = 1) {
      
      prob.vec <- exp((trial / 10) ^ sens * exp.vec) / sum(exp((trial / 10) ^ sens * exp.vec))
      
      return(prob.vec)
      
    }
    
    egreedy <- function(exp.vec, trial, e = .05) {
      
      prob.vec <- rep(e / (length(exp.vec) - 1), length(exp.vec))
      prob.vec[exp.vec == max(exp.vec)] <- 1 - e
      prob.vec <- prob.vec / sum(prob.vec)
      return(prob.vec)
      
    }
    
    # Get agent parameters
    
    n.peeks.i <- peeks.sim$n.peeks[i]
    n.trials.i <- peeks.sim$n.trials[i] 
    action.strategy.i <- peeks.sim$action.strategy[i]
    keep.select.strategy.i <- peeks.sim$keep.select.strategy[i]
    peek.select.strategy.i <- peeks.sim$peek.select.strategy[i]
    update.method.i <- peeks.sim$update.method[i]
    
    
    # Create environment 
    
    n.good.i <- peeks.sim$n.good[i]
    n.bad.i <- peeks.sim$n.bad[i]
    good.mean.i <- peeks.sim$good.mean[i]
    bad.mean.i <- peeks.sim$bad.mean[i]
    sd.i <- peeks.sim$sd[i]
    
    n.options <- n.good.i + n.bad.i
    
    # Create sample outcomes
    
    good.mtx <- matrix(unlist(lapply(1:n.good.i, function(x) {rnorm(n.trials.i, mean = good.mean.i, sd = sd.i)})), 
                       nrow = n.trials.i, ncol = n.good.i)
    
    bad.mtx <- matrix(unlist(lapply(1:n.bad.i, function(x) {rnorm(n.trials.i, mean = bad.mean.i, sd = sd.i)})), 
                      nrow = n.trials.i, ncol = n.bad.i)
    
    environment.outcomes <- cbind(good.mtx, bad.mtx)
    
    # Randomize option order
    rand.order <- sample(n.options, size = n.options, replace = F)
    
    environment.outcomes <- environment.outcomes[,rand.order]
    
    best.option <- which(rand.order == 1)
    
    agent.result <- data.frame("trial" = 1:(n.trials.i + 1), 
                               selection = NA, 
                               action = NA,
                               outcome = NA,
                               reward = NA,
                               reward.cum = NA)
    
    agent.result[paste("option", 1:n.options, ".exp", sep = "")] <- NA
    agent.result[1, paste("option", 1:n.options, ".exp", sep = "")] <- rep(0, n.options)
    
    for(trial.i in 1:n.trials.i) {
      
      # Get current expectations
      
      current.expectations <- as.numeric(agent.result[trial.i, paste("option", 1:n.options, ".exp", sep = "")])
      
      if(action.strategy.i == "trial") {
        
        # Step 1: Determine Action  
        
        if(trial.i <= n.peeks.i) {
          
          current.action <- "peek"
          
        }
        
        if(trial.i > n.peeks.i) {
          
          current.action <- "keep"
          
        }
        
        # Step 2: Determine Selection
        
        if(current.action == "peek") {
          
          if(peek.select.strategy.i == "uniform") {
          
          
          current.selection <- rep(1:n.options, length.out = n.peeks.i)[trial.i]
          select.probs <- rep(0, n.options)
          select.probs[current.selection] <- 1
          
          }
          
          if(substr(peek.select.strategy.i, 1, 2) == "eg") {
            
            e.par <- as.numeric(substr(peek.select.strategy.i, 9, 11))
            
            select.probs <- egreedy(current.expectations, 
                                     trial = trial.i, 
                                     e = e.par)
            
          }
          
          
          if(substr(peek.select.strategy.i, 1, 2) == "so") {
            
            sm.par <- as.numeric(substr(peek.select.strategy.i, 9, 12))
            
            select.probs <- soft.max(current.expectations, trial = trial.i, sens = sm.par)
            
          }
          
        }
        
        if(current.action == "keep") {
          
          if (substr(keep.select.strategy.i, 1, 2) == "so") {
            
            sm.par <- as.numeric(substr(keep.select.strategy.i, 9, 12))
            
            select.probs <- soft.max(current.expectations, trial = trial.i, sens = sm.par)
            
          }
          
          if(substr(keep.select.strategy.i, 1, 2) == "eg") {
            
            e.par <- as.numeric(substr(keep.select.strategy.i, 9, 11))
            
            select.probs <- egreedy(current.expectations, 
                                     trial = trial.i, 
                                     e = e.par)
            
          }
        }
        
        
        # Set probs to uniform if there is an NA
        
        if(mean(is.na(select.probs)) != 0) {
          
          select.probs <- rep(1 / n.options, n.options)
        
      }
      
      current.selection <- sample(1:n.options, size = 1, prob = select.probs)
      
      }
      
      if(action.strategy.i == "expectation") {
        
        # Step 1: Determine Selection
        
        if(substr(keep.select.strategy.i, 1, 2) == "eg") {
          
          e.par <- as.numeric(substr(keep.select.strategy.i, 8, 10))
          
          select.probs <- egreedy(current.expectations, 
                                   trial = trial.i, 
                                   e = e.par)
          
        }
        
        if(substr(keep.select.strategy.i, 1, 2) == "so") {
          
          sm.par <- as.numeric(substr(keep.select.strategy.i, 9, 12))
          
          select.probs <- soft.max(current.expectations, trial = trial.i, sens = sm.par)
          
        }
        
        # Set probs to uniform if there is an NA
        
        if(mean(is.na(select.probs)) != 0) {
          
          current.selection <- sample(1:n.options, size = 1, prob = rep(1 / n.options, n.options))
          
        }

        current.selection <- sample(1:n.options, size = 1, prob = select.probs)
        
        
        
        # Step 2: Determine Action
        
        if(current.expectations[current.selection] <= 0) {current.action <- "peek"}
        if(current.expectations[current.selection] > 0) {current.action <- "keep"}  
        
      }
      
      # Determine outcome
      
      current.outcome <- sample(environment.outcomes[,current.selection], size = 1, replace = T)
      
      # Update expectations
      
      if(update.method.i == "mean") {
        
        new.expectations <- current.expectations
        
        prior.selections <- sum(agent.result$selection == current.selection, na.rm = T)
        
        if(prior.selections == 0) {new.expectations[current.selection] <- current.outcome}
        if(prior.selections >= 1) {new.expectations[current.selection] <- (prior.selections ) / (prior.selections + 1) * current.expectations[current.selection] + 1 / (prior.selections + 1) * current.outcome}
        
      }
      
      if(substr(update.method.i, 1, 2) == "rl") {
        
        update.rate <- as.numeric(substr(update.method.i, 3, 4))
        
        new.expectations <- current.expectations
        new.expectations[current.selection] <- (1 - update.rate) * new.expectations[current.selection] + update.rate  * current.outcome
        
        
      }
      
      
      if(current.action == "peek") {current.reward <- 0}
      if(current.action == "keep") {current.reward <- current.outcome}
      
      # Write results to table
      
      agent.result$selection[trial.i] <- current.selection
      agent.result$action[trial.i] <- current.action
      agent.result$outcome[trial.i] <- current.outcome
      agent.result$reward[trial.i] <- current.reward
      agent.result[trial.i + 1, paste("option", 1:n.options, ".exp", sep = "")] <- new.expectations 
      
      

    
    }
    
    final.impressions <- agent.result[n.trials.i + 1, grepl(".exp", names(agent.result))]
    

    
    true.means <- rep(bad.mean.i, n.options)
    true.means[rand.order <= n.good.i] <- good.mean.i
    
    agent.result[,paste("option", 1:n.options, ".mu", sep = "")] <- matrix(rep(true.means, times = nrow(agent.result)), 
                                                                           nrow = nrow(agent.result), 
                                                                           ncol = n.options, byrow = T)
    
    agent.result[,paste("impression", 1:n.options, ".absdev", sep = "")] <- abs(agent.result[,paste("option", 1:n.options, ".mu", sep = "")] - agent.result[,paste("option", 1:n.options, ".exp", sep = "")])
    agent.result$impression.mad <- rowMeans(agent.result[,paste("impression", 1:n.options, ".absdev", sep = "")])
    
    agent.result$reward.cum <- cumsum(agent.result$reward)
    
    
    # Earnings in first keep trial
    
    first.keep.points <- agent.result$reward[agent.result$action == "keep"][1]
    
    
    # Get final output  
    
    total.points <- agent.result$reward.cum[n.trials.i]
    final.impression.mad <- agent.result$impression.mad[n.trials.i + 1]
    
    final.impressions <- agent.result[n.trials.i + 1, grepl(".exp", names(agent.result))]
    final.prefer.best <- which(final.impressions == max(final.impressions)) ==  best.option
    
    if(n.peeks.i == 0) { 
      
      final.peek.impression.mad <- NA
      final.peek.prefer.best <- NA
      final.peek.rank.best <- NA
       
    }
    
    if(n.peeks.i > 1) {
  
    final.peek.impression.mad <- agent.result$impression.mad[n.peeks.i + 1]
      
    expectations.final.peek <- as.numeric(agent.result[n.peeks.i + 1, grepl(".exp", x = names(agent.result))])
    final.peek.prefer.best <- which(expectations.final.peek == max(expectations.final.peek)) == best.option
    final.peek.rank.best <- n.options + 1 - rank(expectations.final.peek)[best.option]

    }
    
    
    agent.result <- agent.result[1:n.trials.i,]
    
    n.keep.best <- sum(agent.result$selection == best.option & agent.result$action == "keep")
    n.peek.best <- sum(agent.result$selection == best.option & agent.result$action == "peek")
    p.select.best <- mean(agent.result$selection == best.option)
    
    print(c(i, total.points))
    
    return(c(total.points, 
             final.impression.mad, 
             final.prefer.best, 
             n.keep.best, 
             n.peek.best, 
             p.select.best, 
             final.peek.impression.mad, 
             final.peek.prefer.best, 
             final.peek.rank.best, 
             first.keep.points))
    
  }

  result <- c()
  for(i in 1:nrow(peeks.sim)) {result <- c(result, peek.sim.fun(i)[1])}
  
  
  sfInit(parallel = T, cpus = 32, slaveOutfile = "slaveupdate.txt")
  sfExport("peeks.sim")
  sfLibrary("snowfall", character.only = TRUE)
  
  # Run simulation!
  date() # Print starting time
 cluster.result <- sfClusterApplySR(x = 1:nrow(peeks.sim), fun = peek.sim.fun, perUpdate = 1)
  cluster.result.r <- matrix(unlist(cluster.result), nrow = nrow(peeks.sim), ncol = 10, byrow = T)
  
  # Append to peeks.sim
  peeks.sim[, c("total.points", 
                "final.impression.mad", 
                "final.prefer.best",
                "n.keep.best", 
                "n.peek.best", 
                "p.select.best", 
                "final.peek.impression.mad", 
                "final.peek.prefer.best", 
                "final.peek.rank.best", 
                "first.keep.points")] <- cluster.result.r
  
  # Write results
  
  write.table(peeks.sim, file = paste("simulations/temp/sim_result ", substr(date(), 1, 10), " env", env.i, " out of ", nrow(environments.df), ".txt", sep = ""), sep = "\t")

  
}

  # Reload raw data and aggregate results
{
  files.to.use  <- list.files("simulations/temp/")
  files.to.use <- files.to.use[grepl("result", files.to.use)]
  
  
  for(i in 1:length(files.to.use)) {
    
    current.df <- read.table(paste("simulations/temp/", files.to.use[i], sep = ""), 
                             stringsAsFactors = F, 
                             header = T, 
                             sep = "\t")
    
    if(i == 1) {peeks.sim <- current.df}
    if(i > 1) {peeks.sim <- rbind(peeks.sim, current.df)}
    
  }
  
  points.agg <- peeks.sim %>%
    group_by(n.bad, sd, n.peeks, action.strategy, peek.select.strategy, keep.select.strategy, update.method) %>%
    summarise(
      total.points.mean = mean(total.points),
      final.impression.mad.mean = mean(final.impression.mad),
 #     final.peek.impression.mad = mean(final.peek.impression.mad, na.rm = T),
#      final.peek.prefer.best.mean = mean(final.peek.prefer.best, na.rm = T),
#      first.keep.points.mean = mean(first.keep.points),
      mean.0 = mean(total.points == 0),
      mean.100 = mean(total.points == 100),
      n = n()
    )
  
  
  write.table(points.agg, paste("simulations/temp/agg_result ", substr(date(), 1, 10), ".txt", sep = ""), sep = "\t")
}
  

}




# Simulation 2
{
  
  
library(plyr)
library(dplyr)
library(snowfall)
library(matrixStats)
# Set parameters

n.sim <- 1000   # Number of simulations for each parameter combination
good.mean <- 5
bad.mean <- -5

trials.vec <- seq(5, n.trials, 10) # Number of trials people could take
n.good.i <- 1
n.bad.i <- 1:4
sd.i <- c(5, 15, 30)
  
  exploration.sim <- expand.grid(
    n.trials = trials.vec,
    select.strategy = c("uniform"), # uniform, egreedy or softmax
    update.method = "mean",  #c("rl.3", "mean"),  # mean or rl.3
    good.mean = good.mean, 
    bad.mean = bad.mean,
    n.good = n.good.i,
    n.bad = n.bad.i,
    sd = sd.i,
    sim = 1:n.sim,
    total.points = NA, 
    prefer.best = NA, 
    impression.mad = NA,
    stringsAsFactors = F
  )
  
  exploration.sim$agent <- 1:nrow(exploration.sim)
  
  exploration.sim.fun <- function(i) {
    
    # Print current run
    sfCat(paste("Iteration ", i, "out of ", nrow(exploration.sim)), sep="\n")
    
    soft.max <- function(exp.vec, trial, sens = 1) {
      
      prob.vec <- exp((trial / 10) ^ sens * exp.vec) / sum(exp((trial / 10) ^ sens * exp.vec))
      
      return(prob.vec)
      
    }
    
    egreedy <- function(exp.vec, trial, e = .05) {
      
      prob.vec <- rep(e / (length(exp.vec) - 1), length(exp.vec))
      prob.vec[exp.vec == max(exp.vec)] <- 1 - e
      prob.vec <- prob.vec / sum(prob.vec)
      return(prob.vec)
      
    }
    
    n.trials.i <- exploration.sim$n.trials[i]
    select.strategy.i <- exploration.sim$select.strategy[i]
    update.method.i <- exploration.sim$update.method[i]
    environment.i <- exploration.sim$environment[i]
    agent.i <- exploration.sim$agent[i]
    
    # Create environment 
    
    n.good.i <- exploration.sim$n.good[i]
    n.bad.i <- exploration.sim$n.bad[i]
    good.mean.i <- exploration.sim$good.mean[i]
    bad.mean.i <- exploration.sim$bad.mean[i]
    sd.i <- exploration.sim$sd[i]
    
    n.options <- n.good.i + n.bad.i
    
    # Create sample outcomes
    
    good.mtx <- matrix(unlist(lapply(1:n.good.i, function(x) {rnorm(n.trials.i, mean = good.mean.i, sd = sd.i)})), 
                       nrow = n.trials.i, ncol = n.good.i)
    
    bad.mtx <- matrix(unlist(lapply(1:n.bad.i, function(x) {rnorm(n.trials.i, mean = bad.mean.i, sd = sd.i)})), 
                      nrow = n.trials.i, ncol = n.bad.i)
    
    environment.outcomes <- cbind(good.mtx, bad.mtx)
    
    # Randomize order
    rand.order <- sample(n.options, size = n.options, replace = F)
    
    environment.outcomes <- environment.outcomes[,rand.order]
    
    best.option <- which(rand.order == 1)
    
    agent.result <- data.frame(
                               agent = rep(agent.i, n.trials.i),
                               n.trials = n.trials.i,
                               select.strategy = select.strategy.i,
                               update.method = update.method.i,
                               best.option = best.option,
                               trial = 1:n.trials.i, 
                               selection = NA, 
                               outcome = NA,
                               outcome.cum = NA)
    
    agent.result[paste("option", 1:n.options, ".exp", sep = "")] <- NA
    agent.result[1, paste("option", 1:n.options, ".exp", sep = "")] <- rep(0, n.options)
    
    agent.result$current.preference <- NA
    
    for(trial.i in 1:n.trials.i) {
      
      # Get current expectations
      
      current.expectations <- as.numeric(agent.result[trial.i, paste("option", 1:n.options, ".exp", sep = "")])
      
      current.preference <- which(current.expectations == max(current.expectations))
      if(length(current.preference) > 1) {current.preference <- sample(current.preference, 1)}
      
  
        # Step 2: Determine Selection
        
          if(select.strategy.i == "uniform") {
            
            current.selection <- rep(1:n.options, length.out = n.trials.i)[trial.i]
            select.probs <- rep(0, n.options)
            select.probs[current.selection] <- 1
            
          }

          if(substr(select.strategy.i, 1, 2) == "eg") {
            
            e.par <- as.numeric(substr(select.strategy.i, 8, 10))
            
            select.probs <- egreedy(current.expectations, 
                                     trial = trial.i, 
                                     e = e.par)
          }
          
          if(select.strategy.i == "softmax") {
            
            select.probs <- soft.max(current.expectations, trial = trial.i, sens = 2)
            
          }
          
        
        # Set probs to uniform if there is an NA
        
        if(mean(is.na(select.probs)) != 0) {
          
          select.probs <- rep(1 / n.options, n.options)
          
        }
        
        current.selection <- sample(1:n.options, size = 1, prob = select.probs)

      # Determine outcome
      
      current.outcome <- sample(environment.outcomes[,current.selection], size = 1, replace = T)
      
      # Update expectations
      
      if(update.method.i == "mean") {
        
        new.expectations <- current.expectations
        
        prior.selections <- sum(agent.result$selection == current.selection, na.rm = T)
        
        if(prior.selections == 0) {new.expectations[current.selection] <- current.outcome}
        if(prior.selections >= 1) {new.expectations[current.selection] <- (prior.selections - 1) / prior.selections * current.expectations[current.selection] + 1 / prior.selections * current.outcome}
        
      }
      
      if(update.method.i == "rl.3") {
        
        new.expectations <- current.expectations
        new.expectations[current.selection] <- .7 * new.expectations[current.selection] + .3 * current.outcome
        
        
      }
    
      
      # Write results to table
      
      agent.result$selection[trial.i] <- current.selection
      agent.result$outcome[trial.i] <- current.outcome
      agent.result[trial.i + 1, paste("option", 1:n.options, ".exp", sep = "")] <- new.expectations
      agent.result$current.preference[trial.i] <- current.preference
      
      
      
    }
    
    agent.result <- agent.result[1:n.trials.i,]
    
    true.means <- rep(bad.mean.i, n.options)
    true.means[rand.order <= n.good.i] <- good.mean.i
    
    agent.result[,paste("option", 1:n.options, ".mu", sep = "")] <- matrix(rep(true.means, times = nrow(agent.result)), 
                                                                           nrow = nrow(agent.result), 
                                                                           ncol = n.options, byrow = T)
    
    agent.result[,paste("impression", 1:n.options, ".absdev", sep = "")] <- abs(agent.result[,paste("option", 1:n.options, ".mu", sep = "")] - agent.result[,paste("option", 1:n.options, ".exp", sep = "")])
    agent.result$impression.mad <- rowMeans(agent.result[,paste("impression", 1:n.options, ".absdev", sep = "")])
    
    agent.result$outcome.cum <- cumsum(agent.result$outcome)
    
    prefer.best <- agent.result$current.preference[n.trials.i] == agent.result$best.option[n.trials.i]
    
    return(c(prefer.best, agent.result$impression.mad[n.trials.i]))
    
  }
  
  sfInit(parallel = T, cpus = 32, slaveOutfile = "slaveupdate.txt")
  sfExport("exploration.sim")
  sfLibrary("snowfall", character.only = TRUE)
  
  # Run simulation!
  date() # Print starting time
  cluster.result <- sfClusterApplySR(x = 1:nrow(exploration.sim), fun = exploration.sim.fun, perUpdate = 1)
  cluster.result <- matrix(unlist(cluster.result), nrow = length(cluster.result), ncol = length(cluster.result[[1]]), byrow = T)
exploration.sim$prefer.best <- cluster.result[,1]
exploration.sim$impression.mad <- cluster.result[,2]




# Plotting

agg <- aggregate(impression.mad ~ n.bad + sd + n.trials + select.strategy, FUN = mean, data = exploration.sim)

n.bad.vec <- sort(unique(agg$n.bad))
sd.vec <- sort(unique(agg$sd))

par(mfrow = c(length(n.bad.vec), length(sd.vec)))

for(n.bad.i in 1:length(n.bad.vec)) {
  for(sd.i in 1:length(sd.vec)) {
    
  data <- subset(agg, n.bad == n.bad.vec[n.bad.i] & sd == sd.vec[sd.i])
    
plot(1, xlim = c(0, 100), ylim = c(0, 15), type = "n", 
     main = paste("n.bad = ", n.bad.vec[n.bad.i], " sd = ", sd.vec[sd.i], sep = ""))

lines(subset(data, select.strategy == "uniform")$n.trials, 
      subset(data, select.strategy == "uniform")$impression.mad, col = "red")

# lines(subset(data, select.strategy == "egreedy.25")$n.trials, 
#       subset(data, select.strategy == "egreedy.25")$impression.mad, col = "blue")
# 
# lines(subset(data, select.strategy == "egreedy.05")$n.trials, 
#       subset(data, select.strategy == "egreedy.05")$impression.mad, col = "blue")

}
}





  # Write results
  
#  write.table(exploration.sim, file = paste("simulations/sim_result ", substr(date(), 1, 10), " env", env.i, " out of ", nrow(environments.df), ".txt", sep = ""), sep = "\t")
  
  


}


# Simplified learning simulation
# How well can you learn an environment in N samples?


{
  
  # Set parameters
  
  n.sim <- 5000   # Number of simulations for each parameter combination
  good.mean <- 5
  bad.mean <- -5
  
  peeks.vec <- seq(0, 100, 5) # Number of peeks people could take
  n.good.vec <- 1
  n.bad.vec <- 1:4  # 1:4
  sd.vec <- c(5, 10, 20, 30) # 0 means random sd from 1 to 30
  
  environments.df <- expand.grid(n.good = n.good.vec,
                                 n.bad = n.bad.vec,
                                 sd = sd.vec, stringsAsFactors = F
  )
  
  for(env.i in 1:nrow(environments.df)) {
    
    print(paste("environment", env.i, "out of ", nrow(environments.df), date()))
    
    n.good.i <- environments.df$n.good[env.i]
    n.bad.i <- environments.df$n.bad[env.i]
    sd.i <- environments.df$sd[env.i]
    
    peeks.sim <- expand.grid(
      n.peeks = peeks.vec,
      peek.select.strategy = c("uniform"),# uniform, egreedy or softmax
      update.method = c("mean"),  #c("rl.3", "mean"),  # mean or rl.3
      good.mean = good.mean, 
      bad.mean = bad.mean,
      n.good = n.good.i,
      n.bad = n.bad.i,
      sd = sd.i,
      sim = 1:n.sim
    )
    
    
    
    peek.sim.fun <- function(i) {
      
      # Print current run
      sfCat(paste("Iteration ", i, "out of ", nrow(peeks.sim)), sep="\n")
      
      soft.max <- function(exp.vec, trial, sens = 1) {
        
        prob.vec <- exp((trial / 10) ^ sens * exp.vec) / sum(exp((trial / 10) ^ sens * exp.vec))
        
        return(prob.vec)
        
      }
      
      egreedy <- function(exp.vec, trial, e = .05) {
        
        prob.vec <- rep(e / (length(exp.vec) - 1), length(exp.vec))
        prob.vec[exp.vec == max(exp.vec)] <- 1 - e
        prob.vec <- prob.vec / sum(prob.vec)
        return(prob.vec)
        
      }
      
      n.peeks.i <- peeks.sim$n.peeks[i]
      peek.select.strategy.i <- peeks.sim$peek.select.strategy[i]
      update.method.i <- peeks.sim$update.method[i]
      n.good.i <- peeks.sim$n.good[i]
      n.bad.i <- peeks.sim$n.bad[i]
      good.mean.i <- peeks.sim$good.mean[i]
      bad.mean.i <- peeks.sim$bad.mean[i]
      sd.i <- peeks.sim$sd[i]
      n.trials.i <- n.peeks.i
      
      n.options <- n.good.i + n.bad.i
      
      # Randomize order
      rand.order <- sample(n.options, size = n.options, replace = F)      
      best.option <- which(rand.order == 1)
      
      
      
      if(n.peeks.i > 0) {
      
      
      # Create environment 
      

      # Create sample outcomes
      
      good.mtx <- matrix(unlist(lapply(1:n.good.i, function(x) {rnorm(n.peeks.i, mean = good.mean.i, sd = sd.i)})), 
                         nrow = n.peeks.i, ncol = n.good.i)
      
      bad.mtx <- matrix(unlist(lapply(1:n.bad.i, function(x) {rnorm(n.peeks.i, mean = bad.mean.i, sd = sd.i)})), 
                        nrow = n.peeks.i, ncol = n.bad.i)
      
      environment.outcomes <- cbind(good.mtx, bad.mtx)
      environment.outcomes <- environment.outcomes[,rand.order]
      
      agent.result <- data.frame("trial" = 1:n.peeks.i, 
                                 selection = NA,
                                 outcome = NA)
      
      agent.result[paste("option", 1:n.options, ".exp", sep = "")] <- NA
      agent.result[1, paste("option", 1:n.options, ".exp", sep = "")] <- rep(0, n.options)
      
      for(trial.i in 1:n.peeks.i) {
        
        # Get current expectations
        
        current.expectations <- as.numeric(agent.result[trial.i, paste("option", 1:n.options, ".exp", sep = "")])
        
            
            if(peek.select.strategy.i == "uniform") {
              
              
              current.selection <- rep(1:n.options, length.out = n.peeks.i)[trial.i]
              select.probs <- rep(0, n.options)
              select.probs[current.selection] <- 1
              
            }
            
            if(substr(peek.select.strategy.i, 1, 2) == "eg") {
              
              e.par <- as.numeric(substr(peek.select.strategy.i, 8, 10))
              
              select.probs <- egreedy(current.expectations, 
                                      trial = trial.i, 
                                      e = e.par)
              
            }
            
            
            if(peek.select.strategy.i == "softmax") {
              
              select.probs <- soft.max(current.expectations, trial = trial.i, sens = 1)
              
            }
            
          

          
          
          # Set probs to uniform if there is an NA
          
          if(mean(is.na(select.probs)) != 0) {
            
            select.probs <- rep(1 / n.options, n.options)
            
          }
          
          current.selection <- sample(1:n.options, size = 1, prob = select.probs)
          
        
        
        
        # Determine outcome
        
        current.outcome <- sample(environment.outcomes[,current.selection], size = 1, replace = T)
        
        # Update expectations
        
        if(update.method.i == "mean") {
          
          new.expectations <- current.expectations
          
          prior.selections <- sum(agent.result$selection == current.selection, na.rm = T)
          
          if(prior.selections == 0) {new.expectations[current.selection] <- current.outcome}
          if(prior.selections >= 1) {new.expectations[current.selection] <- (prior.selections - 1) / prior.selections * current.expectations[current.selection] + 1 / prior.selections * current.outcome}
          
        }
        
        if(update.method.i == "rl.3") {
          
          new.expectations <- current.expectations
          new.expectations[current.selection] <- .7 * new.expectations[current.selection] + .3 * current.outcome
          
          
        }
        
        
        # Write results to table
        
        agent.result$selection[trial.i] <- current.selection
        agent.result$outcome[trial.i] <- current.outcome
        agent.result[trial.i + 1, paste("option", 1:n.options, ".exp", sep = "")] <- new.expectations 
        
        
        
      }
      
      final.impressions <- agent.result[n.trials.i + 1, grepl(".exp", names(agent.result))]
      
      
      
      true.means <- rep(bad.mean.i, n.options)
      true.means[rand.order <= n.good.i] <- good.mean.i
      
      agent.result[,paste("option", 1:n.options, ".mu", sep = "")] <- matrix(rep(true.means, times = nrow(agent.result)), 
                                                                             nrow = nrow(agent.result), 
                                                                             ncol = n.options, byrow = T)
      
      agent.result[,paste("impression", 1:n.options, ".absdev", sep = "")] <- abs(agent.result[,paste("option", 1:n.options, ".mu", sep = "")] - agent.result[,paste("option", 1:n.options, ".exp", sep = "")])
      agent.result$impression.mad <- rowMeans(agent.result[,paste("impression", 1:n.options, ".absdev", sep = "")])

      
      # Get final output  
      
      final.impression.mad <- agent.result$impression.mad[n.peeks.i + 1]
              
        expectations.final.peek <- as.numeric(agent.result[n.peeks.i + 1, grepl(".exp", x = names(agent.result))])
        final.peek.prefer.best <- which(expectations.final.peek == max(expectations.final.peek)) ==  best.option
        
      }
      
      if(n.peeks.i == 0) {
        

        true.means <- rep(bad.mean.i, n.options)
        true.means[rand.order <= n.good.i] <- good.mean.i
        
        final.impression.mad <- mean(abs(true.means))
        final.peek.prefer.best <- sample(c(rep(0, n.bad.i), rep(1, n.good.i)), size = 1)
        
      }

      
      return(c(final.impression.mad, final.peek.prefer.best))
      
    }
    
#    test <- unlist(lapply(1:nrow(peeks.sim), peek.sim.fun))
    
    sfInit(parallel = T, cpus = 32, slaveOutfile = "slaveupdate.txt")
    sfExport("peeks.sim")
    sfLibrary("snowfall", character.only = TRUE)
    
    # Run simulation!
    date() # Print starting time
    cluster.result <- sfClusterApplySR(x = 1:nrow(peeks.sim), fun = peek.sim.fun, perUpdate = 1)
    cluster.result.r <- matrix(unlist(cluster.result), nrow = nrow(peeks.sim), ncol = 2, byrow = T)
    
    # Append to peeks.sim
    peeks.sim[, c("final.impression.mad", "final.peek.prefer.best")] <- cluster.result.r
    
    # Write results
    
    write.table(peeks.sim, file = paste("simulations/sim_learning_result ", substr(date(), 1, 10), " env", env.i, " out of ", nrow(environments.df), ".txt", sep = ""), sep = "\t")
    
    
  }
  
  
}


