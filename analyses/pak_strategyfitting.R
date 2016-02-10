#PAK Strategy Fitting



# Load Libraries
{library("xtable")
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
  library("R2jags")
  library("BayesFactor")
  options(stringsAsFactors = F)}

# Load Data
{
participants <- read.table("data/participants.txt", sep = "\t", header = T)
actions <- read.table("data/actions.txt", sep = "\t", header = T)
condition.table <- read.table("data/conditiontable.txt", sep = "\t", header = T)
distributions <- read.table("data/oct2015/peekkeep_oct2015_gamedist.txt", 
                            sep = "\t", 
                            header = T)

optionorder <- read.table("data/oct2015/peekkeep_oct2015_optionorder.txt", 
                          sep = "\t", 
                          header = T)


}

# Define strategy functions
{
  
  ## Selection Functions
  
  softmax1a.sel.fun <- function(
    par.vec,
    exp.vec, 
    current.trial,
    current.action,
    show.pars = F
  ) {
    
    if(show.pars == F) {
      
      sens <- par.vec[1]
      
      if(mean(exp.vec == 0) == 1 | mean(is.finite(unlist(exp.vec))) != 1) {exp.vec <- rep(1, length(exp.vec))}
      
      prob.vec <- exp((current.trial / 10) ^ sens * exp.vec) / sum(exp((current.trial / 10) ^ sens * exp.vec))
      
      return(prob.vec)}
    
    if(show.pars) {
      
      output.df <- data.frame(
        start = runif(1, -1, 1),
        lb = -10,
        ub = 10
      )
        
      return(output.df)}
    
  }

softmax1b.sel.fun <- function(
  par.vec,
  exp.vec, 
  current.trial,
  current.action,
  show.pars = F
) {
  
  if(show.pars == F) {
    
    if(mean(exp.vec == 0) == 1 | mean(is.finite(unlist(exp.vec))) != 1) {exp.vec <- rep(1, length(exp.vec))}
    
    sens <- par.vec[1]
    
    prob.vec <- exp(exp.vec * sens) / sum(exp(exp.vec * sens))
    
    
    return(prob.vec)}
  
  if(show.pars) {
    
    
    output.df <- data.frame(
      start = runif(1, -1, 1),
      lb = -10,
      ub = 10
    )
    
    
    return(output.df)
    
    }
  
}


softmax2b.sel.fun <- function(
  par.vec,
  exp.vec, 
  current.trial,
  current.action,
  show.pars = F
) {
  
  if(show.pars == F) {
    
    if(mean(exp.vec == 0) == 1 | mean(is.finite(unlist(exp.vec))) != 1) {exp.vec <- rep(1, length(exp.vec))}
    
    peek.sens <- par.vec[1]
    keep.sens <- par.vec[2]
    
    if(current.action == "peek") {sens <- peek.sens}
    if(current.action == "keep") {sens <- keep.sens}
    
    prob.vec <- exp(exp.vec * sens) / sum(exp(exp.vec * sens))
    
    
    return(prob.vec)}
  if(show.pars) {
    
    
    output.df <- data.frame(
      start = runif(2, -1, 1),
      lb = c(-10 -10),
      ub = c(10, 10)
    )
    
    
    
    return(output.df)
    }
  
}
  
  egreedy.sel.fun <- function(
    par.vec,
    exp.vec, 
    current.trial,
    current.action,
    show.pars = F
  ) {
    
  
    if(show.pars == F) {
      
      e <- par.vec[1]
      
      prob.vec <- rep(e / (length(exp.vec) - 1), length(exp.vec))
      
      best.options <- which(exp.vec == max(exp.vec))
      
      if(length(best.options) > 1) {
        
        prob.vec[sample(best.options, size = 1)] <- 1 - e
        
      }
      
      if(length(best.options == 1)) {
        
        prob.vec[best.options] <- 1 - e
        
        
      }
      
      prob.vec <- prob.vec / sum(prob.vec)
      
      
      return(prob.vec)}
    
    if(show.pars) {
      
      
      output.df <- data.frame(
        start = runif(1, .4, .6),
        lb = c(0),
        ub = c(1)
      )
      
      
      return(output.df)}
    
  }
  
  uniform.sel.fun <- function(
    par.vec,
    exp.vec, 
    current.trial,
    current.action,
    show.pars = F
  ) {
    

    
    if(show.pars == F) {
      
      prob.vec <- rep(1 / length(exp.vec), times = length(exp.vec))
      return(prob.vec)}
    
    if(show.pars) {
      
      
      output.df <- data.frame(
        start = c(NULL),
        lb = c(NULL),
        ub = c(NULL)
      )
      
      return(output.df)}
    
  }
  
  ## Action Functions
  
  ## Action then Selection
  
  trial.act.fun <- function(
    par.vec,
    current.trial, 
    total.trials,
    option.exp,
    last.action,
    show.pars = F
  ) {
    

    if(show.pars == F) {
      
      
      p.start <- par.vec[1]
      p.end <- par.vec[2]
      
      keep.prob <- (total.trials + 1 - current.trial) / total.trials * p.start + (current.trial - 1) / (total.trials - 1) * p.end
      
      return(keep.prob)}
    if(show.pars) {
      
      
      output.df <- data.frame(
        start = runif(2, .4, .6),
        lb = c(0, 0),
        ub = c(1, 1)
      )
      
      
      return(output.df)}
    
  }
  
  trialfixed.act.fun <- function(
    par.vec,
    current.trial, 
    total.trials,
    option.exp,
    last.action,
    show.pars = F
  ) {
    

    if(show.pars == F) {
      
      peektrial.percentage <- par.vec[1]
      keep.rates <- c(1 - par.vec[2], par.vec[2])
      
      if(current.trial <= floor(total.trials * peektrial.percentage)) {keep.prob <- keep.rates[1]}
      if(current.trial > floor(total.trials * peektrial.percentage)) {keep.prob <- keep.rates[2]}
      
      
      return(keep.prob)
    }
    
    if(show.pars) {
      
      
      output.df <- data.frame(
        start = runif(2, .4, .6),
        lb = c(0, 0),
        ub = c(1, 1)
      )
      
      return(output.df)}
    
  }
  
  markov.act.fun <- function(
    par.vec,
    current.trial,
    total.trials,
    option.exp,
    last.action,
    show.pars = F) {
    
    

    
    if(show.pars == F) {
      
      p.kgp <- par.vec[1]
      p.pgk <- par.vec[2]
      
      if(last.action == "none") {keep.prob <- .5}
      if(last.action == "peek") {keep.prob <- p.kgp}
      if(last.action == "keep") {keep.prob <- 1 - p.pgk}
      
      return(keep.prob)}
    if(show.pars) {
      
      
      output.df <- data.frame(
        start = runif(2, .4, .6),
        lb = c(0, 0),
        ub = c(1, 1)
      )
      
      return(output.df)}
  }
  
    stable.act.fun <- function(
    par.vec,
    current.trial,
    total.trials,
    option.exp,
    last.action,
    show.pars = F) {
    
    
    if(show.pars == F) {
      
      p.keep <- par.vec[1]
      
      return(p.keep)}
    if(show.pars) {
      
      
      output.df <- data.frame(
        start = runif(1, .4, .6),
        lb = c(0),
        ub = c(1)
      )
      
      return(output.df)}
  }
  
  
  ## Selection then Action
  
  exp.act.fun <- function(
    par.vec,
    current.trial, 
    total.trials, 
    option.exp,
    last.action,
    show.pars = F) {
    

    
    if(show.pars == F) {
      
      temp.par <- par.vec[1]
      thresh.par <- par.vec[2]
      
      keep.prob <- as.numeric(1 / (1 + exp(-(temp.par * (option.exp - thresh.par)))))
      
      
      return(keep.prob)}
    if(show.pars) {
      
      
      output.df <- data.frame(
        start = c(runif(1, 0, 2), runif(1, -2, 2)),
        lb = c(0, -10),
        ub = c(10, 10)
      )
      
      return(output.df)}
    
  }
  
  ## Update Functions
  
  mean.upd.fun <-   function(
    par.vec,
    prior.exp.vec, 
    current.trial, 
    current.selection, 
    current.outcome, 
    current.action,
    prior.selections, 
    prior.outcomes,
    show.pars = F
  ) {
    
    

    
    if(show.pars == F) {
      new.expectations <- prior.exp.vec
      
      if(sum(prior.selections == current.selection) == 0) {new.expectations[current.selection] <- current.outcome}
      if(sum(prior.selections == current.selection) > 0) {
        
        new.expectations[current.selection] <- mean(c(current.outcome, prior.outcomes[prior.selections == current.selection]), na.rm = T)
        
      }
      
      
      return(new.expectations)}
    if(show.pars) {
      
      
      output.df <- data.frame(
        start = c(NULL),
        lb = c(NULL),
        ub = c(NULL)
      )
      
      return(output.df)}
    
  }
  
  
  rla2.upd.fun <- function(
    par.vec, 
    prior.exp.vec, 
    current.trial, 
    current.selection, 
    current.outcome,
    current.action,
    prior.selections, 
    prior.outcomes,
    show.pars = F
  ) {
    
    
    # Return result
    
    if(show.pars == F) {
      
      new.expectations <- prior.exp.vec
      
      if(sum(prior.selections == current.selection) == 0) {new.expectations[current.selection] <- current.outcome}
      if(sum(prior.selections == current.selection) > 0) {
        
        
        if(current.action == "peek") {update.rate <- par.vec[1]}
        if(current.action == "keep") {update.rate <- par.vec[2]}
        
        # Calculate updated impression
        
        new.expectations[current.selection] <- (1 - update.rate) * new.expectations[current.selection] + update.rate * current.outcome
        
        
      }
      
      
      return(new.expectations)}
    if(show.pars) {
      
      
      
      output.df <- data.frame(
        start = runif(2, .4, .6),
        lb = c(0, 0),
        ub = c(1, 1)
      )
      
      return(output.df)}
    
  }
  
  
  rlb2.upd.fun <- function(
    par.vec, 
    prior.exp.vec, 
    current.trial, 
    current.selection, 
    current.outcome, 
    current.action,
    prior.selections, 
    prior.outcomes,
    show.pars = F
  ) {
    

    
    if(show.pars == F) {
      
      new.expectations <- prior.exp.vec
      
      if(sum(prior.selections == current.selection) == 0) {new.expectations[current.selection] <- current.outcome}
      if(sum(prior.selections == current.selection) > 0) {
        
        # Determine update rate  
        
        if(current.action == "peek") {theta <- par.vec[1]}
        if(current.action == "keep") {theta <- par.vec[2]}
        
        # Get number of observations of selected option
        
        n.obs <- sum(prior.selections == current.selection)
        
        # Calculate updated impression
        
        omega <- (1 / (n.obs + 1)) ^ theta
        
        new.expectations[current.selection] <- (omega) * new.expectations[current.selection] + (1 - omega) * current.outcome
        
      }
      
      
      
      return(new.expectations)}
    
    if(show.pars) {
      
      
      output.df <- data.frame(
        start = runif(2, 0, 2),
        lb = c(0, 0),
        ub = c(10, 10)
      )
      
      return(output.df)}
    
  }
  
  rla1.upd.fun <- function(
    par.vec, 
    prior.exp.vec, 
    current.trial, 
    current.selection, 
    current.outcome,
    current.action,
    prior.selections, 
    prior.outcomes,
    show.pars = F
  ) {
    

    
    if(show.pars == F) {
      new.expectations <- prior.exp.vec
      
      if(sum(prior.selections == current.selection) == 0) {new.expectations[current.selection] <- current.outcome}
      if(sum(prior.selections == current.selection) > 0) {
        
        update.rate <- par.vec[1]
        
        
        # Calculate updated impression
        
        new.expectations[current.selection] <- (1 - update.rate) * new.expectations[current.selection] + update.rate * current.outcome
        
        
      }
      
      # Return result
      
      return(new.expectations)}
    if(show.pars) {
      
      
      output.df <- data.frame(
        start = runif(1, .4, .6),
        lb = c(0),
        ub = c(1)
      )
      
      return(output.df)}
    
  }
  
  
  rlb1.upd.fun <- function(
    par.vec, 
    prior.exp.vec, 
    current.trial, 
    current.action,
    current.selection, 
    current.outcome, 
    prior.selections, 
    prior.outcomes,
    show.pars = F
  ) {
    
  
    
    if(show.pars == F) {
      
      
      new.expectations <- prior.exp.vec
      
      if(sum(prior.selections == current.selection) == 0) {new.expectations[current.selection] <- current.outcome}
      if(sum(prior.selections == current.selection) > 0) {
        
        # Determine update rate  
        
        theta <- par.vec[1]
        
        # Get number of observations of selected option
        
        n.obs <- sum(prior.selections == current.selection)
        
        
        # Calculate updated impression
        
        omega <- (1 / n.obs) ^ theta
        
        new.expectations[current.selection] <- (1 - omega) * new.expectations[current.selection] + omega * current.outcome
        
      }
      
      return(new.expectations)}
    if(show.pars) {
      
      
      output.df <- data.frame(
        start = runif(1, -2, 2),
        lb = c(-10),
        ub = c(10)
      )
      
      return(output.df)}
    
  }
}

# pak.sim.fun
# Simulate data
{
pak.sim.fun <- function(n.trials = 100,
                        action.strat = "stable",
                        action.pars = c(.8),
                        update.strat = "mean",
                        update.pars = c(.5, .5),
                        select.strat = "egreedy",
                        select.pars = .05,
                        n.bad = 3,
                        n.good = 1,
                        bad.mean = -5,
                        good.mean = 5,
                        option.sd = 10
) {
  

  
  # Create environment 
  
  n.options <- n.good + n.bad
  
  # Create sample outcomes
  
  good.mtx <- matrix(unlist(lapply(1:n.good, function(x) {rnorm(n.trials, mean = good.mean, sd = option.sd)})), 
                     nrow = n.trials, ncol = n.good)
  
  bad.mtx <- matrix(unlist(lapply(1:n.bad, function(x) {rnorm(n.trials, mean = bad.mean, sd = option.sd)})), 
                    nrow = n.trials, ncol = n.bad)
  
  environment.outcomes <- cbind(good.mtx, bad.mtx)
  
  # Randomize option order
  rand.order <- sample(n.options, size = n.options, replace = F)
  
  environment.outcomes <- environment.outcomes[,rand.order]
  
  best.option <- which(rand.order == 1)
  
  agent.result <- data.frame("trial" = 1:(n.trials + 1), 
                             selection = NA, 
                             action = NA,
                             outcome = NA,
                             reward = NA,
                             reward.cum = NA)
  
  agent.result[paste("option", 1:n.options, ".exp", sep = "")] <- NA
  agent.result[paste("option", 1:n.options, ".selprob", sep = "")] <- NA
  
  agent.result[1, paste("option", 1:n.options, ".exp", sep = "")] <- rep(0, n.options)
  
  for(trial.i in 1:n.trials) {
    
    # Get current expectations
    
    current.expectations <- as.numeric(agent.result[trial.i, paste("option", 1:n.options, ".exp", sep = "")])
    if(trial.i > 1) {last.action <- agent.result$action[trial.i - 1]}
    if(trial.i == 1) {last.action <- "none"}
    
    # Determine Selection - Action order
    
    if(action.strat %in% c("trialfixed", "trial", "stable", "markov")) {sa.order <- "Action then Selection"}
    if(action.strat %in% c("exp")) {sa.order <- "Selection then Action"}
    
    # Determine Selection and Action 
    
    if(sa.order == "Action then Selection") {
      
      
      keep.prob <- get(paste(action.strat, ".act.fun", sep = ""))(
        par.vec = action.pars,
        current.trial = trial.i,
        last.action = last.action,
        option.exp = current.expectations,
        total.trials = n.trials)
      
      current.action <- sample(c("peek", "keep"), size = 1, prob = c(1 - keep.prob, keep.prob))
      
      # Get Selection
      

        
        select.probs <- get(paste(select.strat, ".sel.fun", sep = ""))(par.vec = select.pars,
                                                                        current.expectations, 
                                                                        current.trial = trial.i,
                                                                        current.action = current.action
                                                                        
                                                                        
        )
        

      current.selection <- sample(1:n.options, size = 1, prob = select.probs)
      
      
    }
    
    if(sa.order == "Selection then Action") {
      
      select.probs <- get(paste(select.strat, ".sel.fun", sep = ""))(
        par.vec = select.pars,
        exp.vec = current.expectations,
        current.trial = trial.i,
        current.action = "none"
        
      )
      
      
      current.selection <- sample(1:n.options, size = 1, prob = select.probs)
      
      
      if(trial.i > 1) {last.action <- agent.result$action[trial.i - 1]}
      if(trial.i == 1) {last.action <- "none"}
      
      keep.prob <- get(paste(action.strat, ".act.fun", sep = ""))(par.vec = action.pars,
                                                                  current.trial = trial.i,
                                                                  last.action = last.action,
                                                                  option.exp = current.expectations[current.selection],
                                                                  total.trials = n.trials)
      
      current.action <- sample(c("peek", "keep"), size = 1, prob = c(1 - keep.prob, keep.prob))
      
      
    }
    
    # Determine outcome
    
    current.outcome <- sample(environment.outcomes[,current.selection], size = 1, replace = T)
    
    # Update expectations
    
    if(trial.i == 1) {
      
      prior.selections <- "Z"
      prior.outcomes <- 999
      
    }
    
    if(trial.i > 1) {
      
      prior.selections <- agent.result$selection[1:(trial.i - 1)]
      prior.outcomes <- agent.result$outcome[1:(trial.i - 1)]
      
    }
    
    new.expectations <- get(paste(update.strat, ".upd.fun", sep = ""))(
      par.vec = update.pars,
      prior.exp.vec = current.expectations,
      current.trial = trial.i,
      current.selection = current.selection,
      current.outcome = current.outcome,
      current.action = current.action,
      prior.selections = prior.selections,
      prior.outcomes = prior.outcomes
    )
    
    
    if(current.action == "peek") {current.reward <- 0}
    if(current.action == "keep") {current.reward <- current.outcome}
    
    # Write results to table
    
    agent.result$selection[trial.i] <- current.selection
    agent.result$action[trial.i] <- current.action
    agent.result$outcome[trial.i] <- current.outcome
    agent.result$reward[trial.i] <- current.reward
    agent.result[trial.i + 1, paste("option", 1:n.options, ".exp", sep = "")] <- new.expectations 
    agent.result[trial.i, paste("option", 1:n.options, ".selprob", sep = "")] <- select.probs 
    
  }
  
  true.means <- rep(bad.mean, n.options)
  true.means[rand.order <= n.good] <- good.mean
  
  agent.result[,paste("option", 1:n.options, ".mu", sep = "")] <- matrix(rep(true.means, times = nrow(agent.result)), 
                                                                         nrow = nrow(agent.result), 
                                                                         ncol = n.options, byrow = T)
  
  agent.result[,paste("impression", 1:n.options, ".absdev", sep = "")] <- abs(agent.result[,paste("option", 1:n.options, ".mu", sep = "")] - agent.result[,paste("option", 1:n.options, ".exp", sep = "")])
  
  
  
  agent.result$impression.mad <- rowMeans(agent.result[,paste("impression", 1:n.options, ".absdev", sep = "")])
  
  agent.result$reward.cum <- cumsum(agent.result$reward)
  
  agent.result <- merge(agent.result, data.frame("selection" = 1:4, "selection.s" = c("A", "B", "C", "D"), stringsAsFactors = F))
  
  agent.result <- agent.result[order(agent.result$trial),]
  
  return(list("result" = agent.result,
              "best.option" = LETTERS[best.option]
  ))
  
}
}

# pak.fit.fun
# Fit one (or more) model(s) to a participants' data
{

pak.fit.fun <- function(
  action.strat.to.fit = "exp",
  update.strat.to.fit = "mean",
  select.strat.to.fit = "uniform",
  action.pars = NA,
  update.pars = NA,
  select.pars = NA,
  option.vec = c("A", "B", "C", "D"), 
  selection.vec = sample(c("A", "B", "C", "D"), size = 100, replace = T), 
  outcome.vec = rnorm(100, 0, 1), 
  action.vec = sample(c("peek", "keep"), size = 100, replace = T),
  what = "fit", # fit or optim
  print.results = F # print results?
) {
  

  
  
  
  # Get design matrix of models
  {
    
    models.to.test <- expand.grid("action.strat" = action.strat.to.fit,
                                  "update.strat" = update.strat.to.fit,
                                  "select.strat" = select.strat.to.fit,
                                  n.obs = NA, n.pars = NA, dev = NA, bic = NA, fitted.pars = NA,
                                  stringsAsFactors = F
    )
  

    
  }
  
  # Perform action on each model
  
  for(mod.i in 1:nrow(models.to.test)) {
    
   # print(mod.i)
    
    action.strat.i <- models.to.test$action.strat[mod.i]
    update.strat.i <- models.to.test$update.strat[mod.i]
    select.strat.i <- models.to.test$select.strat[mod.i]

    # Create vector of starting parameters
    
      start.pars <- c(get(paste(action.strat.i, ".act.fun", sep = ""))(show.pars = T)$start,
                      get(paste(update.strat.i, ".upd.fun", sep = ""))(show.pars = T)$start,
                      get(paste(select.strat.i, ".sel.fun", sep = ""))(show.pars = T)$start)
    
    # model.dev  - Calculate the deviance for a given model and parameters
    
    model.dev <- function(pars) {
      
      action.strat.i <- models.to.test$action.strat[mod.i]
      update.strat.i <- models.to.test$update.strat[mod.i]
      select.strat.i <- models.to.test$select.strat[mod.i]

      n.action.pars <- length(get(paste(action.strat.i, ".act.fun", sep = ""))(show.pars = T)$start)
      n.update.pars <- length(get(paste(update.strat.i, ".upd.fun", sep = ""))(show.pars = T)$start)
      n.select.pars <- length(get(paste(select.strat.i, ".sel.fun", sep = ""))(show.pars = T)$start)

      if(n.action.pars > 0) {action.pars <- pars[1:n.action.pars]}
      if(n.action.pars == 0) {action.pars <- NA}
      
      if(n.update.pars > 0) {update.pars <- pars[(n.action.pars + 1):(n.update.pars + n.action.pars)]}
      if(n.update.pars == 0) {update.pars <- NA}
      
      if(n.select.pars > 0) {select.pars <- pars[(n.action.pars + n.update.pars + 1):(n.action.pars + n.update.pars + 1 + n.select.pars - 1)]}
      if(n.select.pars == 0) {select.pars <- NA}
      
      
      
      # Convert selections to numbers
      
      selection.vec.num <- merge(data.frame("order" = 1:length(selection.vec),
                                            "LETTERS" = selection.vec, stringsAsFactors = F), 
                                 cbind(LETTERS, 1:length(LETTERS)))
      
      selection.vec.num <- as.numeric(paste(selection.vec.num[order(selection.vec.num$order),3]))
      
      # Extract important variables
      
      n.trials <- length(outcome.vec)
      n.options <- length(option.vec)
      
      # Create fit.df
      
      fit.df <- data.frame("selection" = selection.vec.num, 
                           "outcome" = outcome.vec, 
                           "action" = action.vec
      )
      
      old.exp.mtx <- as.data.frame(matrix(NA, nrow = n.trials, ncol = n.options))
      names(old.exp.mtx) <- paste(option.vec, ".oldexp", sep = "")
      new.exp.mtx <- as.data.frame(matrix(NA, nrow = n.trials, ncol = n.options))
      names(new.exp.mtx) <- paste(option.vec, ".newexp", sep = "")
      
      select.prob.mtx <- as.data.frame(matrix(NA, nrow = n.trials, ncol = n.options))
      names(select.prob.mtx) <- paste(option.vec, ".selprob", sep = "")
      
      
      fit.df <- cbind(fit.df, old.exp.mtx, new.exp.mtx, select.prob.mtx)
      
      
      fit.df[1,grepl("old", names(fit.df))] <- 0
      
      # Loop over trials
      
      for(trial.i in 1:n.trials) {
        
        current.action <- fit.df$action[trial.i]
        current.selection <- fit.df$selection[trial.i]
        current.outcome <- fit.df$outcome[trial.i]
        
        old.exp <- fit.df[trial.i, grepl("old", names(fit.df))]
        
        
        # Expectations   
        
        if(trial.i == 1) {
          
          prior.selections <- "Z"
          prior.outcomes <- 999
          last.action <- "none"
          
        }
        
        if(trial.i > 1) {
          
          prior.selections <- fit.df$selection[1:(trial.i - 1)]
          prior.outcomes <- fit.df$outcome[1:(trial.i - 1)]
          last.action <- fit.df$action[trial.i - 1]
          
        }

        new.exp <- get(paste(update.strat.i, ".upd.fun", sep = ""))(
          par.vec = update.pars,
          prior.exp.vec = old.exp,
          current.trial = trial.i,
          current.action = current.action,
          current.selection = current.selection,
          current.outcome = current.outcome,
          prior.selections = prior.selections,
          prior.outcomes = prior.outcomes
        )
        
        
        # Add new expectations to fit.df                       
        fit.df[trial.i, grepl("new", names(fit.df))] <- new.exp
        
        # Add old expectations to next column
        if(trial.i < n.trials) {
          
          fit.df[trial.i + 1, grepl("old", names(fit.df))] <- new.exp
          
        }
        
        # Selection Probabilities
          
          selection.probabilities <- get(paste(select.strat.i, ".sel.fun", sep = ""))(
            par.vec = select.pars,
            exp.vec = fit.df[trial.i, grepl("oldexp", names(fit.df))],
            current.trial = trial.i,
            current.action = current.action)

        # Add new expectations to fit.df                       
        fit.df[trial.i, grepl(".selprob", names(fit.df))] <- selection.probabilities
        
        
        # Action Probabilities
        
        selected.option <- fit.df$selection[trial.i]
        option.exp <- fit.df[trial.i, grepl("oldexp", names(fit.df))][selected.option]
        
        keep.prob.i <- get(paste(action.strat.i, ".act.fun", sep = ""))(
          par.vec = action.pars,
          current.trial = trial.i,
          total.trials = n.trials,
          option.exp = option.exp,
          last.action = last.action
        )
        
        
        fit.df$keep.prob[trial.i] <- keep.prob.i
        
        
      }
      
      # Calculate joint probabilities
      
      fit.df <- fit.df %>%
        mutate(
          action.prob = ifelse(action == "keep", keep.prob, 1 - keep.prob),
          selection.prob = ifelse(selection == 1, A.selprob, 
                                  ifelse(selection == 2, B.selprob, 
                                         ifelse(selection == 3, C.selprob,
                                                ifelse(selection == 4, D.selprob, NA)))),
          joint.prob = action.prob * selection.prob,
          joint.loglik = log(joint.prob)
        )
      
      
      dev <-  -2 * sum(fit.df$joint.loglik)
      
      
      # Create large deviances for bad parameter values
      
      action.pars.lb <- get(paste(action.strat.i, ".act.fun", sep = ""))(show.pars = T)$lb
      select.pars.lb <- get(paste(select.strat.i, ".sel.fun", sep = ""))(show.pars = T)$lb
      update.pars.lb <- get(paste(update.strat.i, ".upd.fun", sep = ""))(show.pars = T)$lb
      
      action.pars.ub <- get(paste(action.strat.i, ".act.fun", sep = ""))(show.pars = T)$ub
      select.pars.ub <- get(paste(select.strat.i, ".sel.fun", sep = ""))(show.pars = T)$ub
      update.pars.ub <- get(paste(update.strat.i, ".upd.fun", sep = ""))(show.pars = T)$ub
      
      if(mean(action.pars >= action.pars.lb & action.pars <= action.pars.ub) != 1 |
         mean(select.pars >= select.pars.lb & select.pars <= select.pars.ub) != 1 |
         mean(update.pars >= update.pars.lb & update.pars <= update.pars.ub) != 1
         ) {dev <- 9999999}
      
      

      if(print.results) {print(c(pars, dev))}
      
      return(dev)
      
    }
    
    
    if(what == "fit") {
      
      start.pars <- c(action.pars, update.pars, select.pars)
                     
      mod.i.dev <- model.dev(start.pars)
      models.to.test$dev[mod.i] <- mod.i.dev
      n.pars <- length(start.pars)
      n.obs <- length(selection.vec)
      bic <- NA
      models.to.test$bic[mod.i] <- NA
      models.to.test$fitted.pars[mod.i] <- paste(start.pars, collapse = ", ")
      
    }
    
    if(what == "optim") {
      
      mod.i.fit <- optim(start.pars, fn = model.dev, hessian = T, method = "BFGS")
      
      models.to.test$dev[mod.i] <- mod.i.fit$value
      models.to.test$n.obs[mod.i] <- length(selection.vec)
      models.to.test$n.pars[mod.i] <- length(start.pars)
      models.to.test$bic[mod.i] <- mod.i.fit$value + length(start.pars) * log(length(selection.vec))
      models.to.test$fitted.pars[mod.i] <- paste(round(mod.i.fit$par, 3), collapse = ", ")
    }
    
    
  }
  
  
  random.sumloglik <- sum(log(rep(.5 * (1 / length(option.vec)), length(selection.vec))))
  
  models.to.test[nrow(models.to.test) + 1, c(1:8)] <- c(rep("uniform", 3), rep(NA, 5))
  models.to.test[nrow(models.to.test), c(4:8)] <- c(length(selection.vec), 0, -2 * random.sumloglik, -2 * random.sumloglik, NA)
  
  models.to.test$mod.posterior <- exp( - .5 * as.numeric(models.to.test$bic)) / sum(exp(-.5 * as.numeric(models.to.test$bic)))
  
  return(models.to.test)
  
}
}


# ----------------------------------------
# Individual Fitting
# Fit one participant to models
# ----------------------------------------
{
  
  
action.strat.true <- "stable"
update.strat.true <- "rlb2"
select.strat.true <- "softmax2b" 
action.pars.true <- c(.8)
update.pars.true <- c(1, 2)
select.pars.true <- c(0, 1)
  
  
# Generate data

temp.data <- pak.sim.fun(n.trials = 100, 
                         action.strat = action.strat.true, 
                         update.strat = update.strat.true,
                         select.strat = select.strat.true,
                         action.pars = action.pars.true,
                         update.pars = update.pars.true,
                         select.pars = select.pars.true,
                         n.good = 1, 
                         n.bad = 1, 
                         option.sd = 10, 
                         bad.mean = -5, 
                         good.mean = 5
)


# Fit model(s)

# Fit true model

true.fit <- pak.fit.fun(option.vec = c("A", "B", "C", "D"),
                        selection.vec = temp.data$result$selection.s,
                        outcome.vec = temp.data$result$outcome,
                        action.vec = temp.data$result$action,
                        action.strat.to.fit = action.strat.true,
                        update.strat.to.fit = update.strat.true,
                        select.strat.to.fit = select.strat.true,
                        action.pars = action.pars.true,
                        update.pars = update.pars.true,
                        select.pars = select.pars.true,
                        what = "fit",
                        print.results = T
)

# Fit several models

action.strat.to.fit = c("exp", "stable")
update.strat.to.fit = c("rlb2")
select.strat.to.fit = c("softmax2b")

  

part.fit <- pak.fit.fun(option.vec = c("A", "B", "C", "D"),
                    selection.vec = temp.data$result$selection.s,
                    outcome.vec = temp.data$result$outcome,
                    action.vec = temp.data$result$action,
                    action.strat.to.fit = action.strat.to.fit,
                    update.strat.to.fit = update.strat.to.fit,
                    select.strat.to.fit = select.strat.to.fit,
                    what = "optim",
                    print.results = T
)





}

# ----------------------------------------
# Parallel Fitting
# Fit multiple participants to a model
# ----------------------------------------

# Parallel fittin
{


action.strat.to.fit <- c("exp", "trialfixed", "stable")
update.strat.to.fit <- c("rlb2", "rlb1")
select.strat.to.fit <- c("softmax2b", "softmax1b")

cluster.fun <- function(workerid.i) {
  
  data <- subset(actions, workerid == workerid.i & playergamenum == 1)
  n.options.i <- data$n.options[1]
  
  options.i <- LETTERS[1:n.options.i]
  outcomes.i <- data$outcome
  actions.i <- data$mode
  selections.i <- data$selection
  
  
  valid.data <- length(outcomes.i) == 100 & 
    length(actions.i == 100) & 
    length(selections.i == 100) &
    mean(selections.i %in% options.i) == 1 &
    mean(is.finite(outcomes.i)) == 1 &
    mean(actions.i %in% c("peek", "keep")) == 1
  
  
  if(valid.data) {
    
    model.fits <- pak.fit.fun(
                              action.strat.to.fit = action.strat.to.fit,
                              update.strat.to.fit = update.strat.to.fit,
                              select.strat.to.fit = select.strat.to.fit,
                              option.vec = options.i,
                              selection.vec = selections.i,
                              outcome.vec = outcomes.i,
                              action.vec = actions.i,
                              what = "optim", print.results = T
                              
                              
    )
  }
  
  if(valid.data == FALSE) {model.fits <- NA}
  
  
  
  return(model.fits)
}

#Loop test

#workers.to.use <- unique(participants$workerid[participants$game.type %in% c("pak", "ptk")])
#test.ls <- lapply(1:2, function(x) {cluster.fun(workers.to.use[x])})


# Start parallel fitting


sfInit(parallel = T, cpus = 10, slaveOutfile = "slaveupdate.txt")
sfExport("action.strat.to.fit")
sfExport("update.strat.to.fit")
sfExport("select.strat.to.fit")
sfExport("pak.fit.fun")
sfExport("actions")
sfExport("uniform.sel.fun")
sfExport("softmax.sel.fun")
sfExport("softmax1b.sel.fun")
sfExport("softmax2b.sel.fun")
sfExport("egreedy.sel.fun")
sfExport("trialfixed.act.fun")
sfExport("stable.act.fun")
sfExport("trial.act.fun")
sfExport("exp.act.fun")
sfExport("markov.act.fun")
sfExport("mean.upd.fun")
sfExport("rl.upd.fun")
sfExport("rlb2.upd.fun")
sfExport("rlb1.upd.fun")
sfExport("cluster.fun")

sfLibrary("snowfall", character.only = TRUE)
sfLibrary(dplyr)
sfLibrary(stringr)


# Run simulation!
date() # Print starting time

workers.to.use <- unique(participants$workerid)[1:2]

cluster.result <- sfLapply(x = workers.to.use, 
                           fun = cluster.fun)


}



