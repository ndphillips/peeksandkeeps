# Generating Distributions for Peeks and Keeps Study

library("beanplot")
library("stringr")
sampling.method <- "unstructured"

# Set rejection thresholds for random samples - R will continually generate samples until the 
# sample mean and standard deviations are within this threshold of the parameter values

N <- 100
mean.thresh <- .05
median.thresh <- .05
sd.thresh <- 1

# Define the environments

environments <- c(
  "N,5,5; N,-5,5",
  "N,5,20; N,-5,20",
  "N,5,5; N,-5,5; N,-5,5; N,-5,5",
  "N,5,20; N,-5,20; N,-5,20; N,-5,20"
  )


# env.ls = list of final distributions
env.ls <- vector("list", length(environments))
for (env.i in 1:length(environments)) {
  
  print(env.i)

  env.description <- environments[env.i]
  
env.list <- str_replace_all(env.description, pattern = " ", "")
env.list <- unlist(str_split(env.list, ";"))
  

current.outcomes <- matrix(NA, nrow = N, ncol = length(env.list))

for (option.i in 1:length(env.list)) {
  
  option.description <- unlist(str_split(env.list[option.i], pattern = ","))
  
  dist.mean <- as.numeric(option.description[2])
  dist.sd <- as.numeric(option.description[3])
  

if(sampling.method == "structured") {

total.samp <- 100
n.breaks <- 100
dist.min <- -100
dist.max <- 100

lb <- seq(dist.min, dist.max, length.out = n.breaks + 1)[-(n.breaks + 1)]
ub <- seq(dist.min, dist.max, length.out = n.breaks + 1)[-1]

prob.vec <- pnorm(ub, dist.mean, dist.sd) - pnorm(lb, dist.mean, dist.sd)

distribution <- c()

for (i in 1:length(lb)) {
  coin <- sample(c(0, 1), 1)
  
  if(coin == 0) {distribution <- c(distribution, runif(round(prob.vec[i] * total.samp, 0), lb[i], ub[i]))}
  if(coin == 1) {distribution <- c(distribution, runif(round(prob.vec[i] * total.samp, 0), lb[i], ub[i]))}
}

dist.df[,i] <- distribution

}


if(sampling.method == "unstructured") {
  
count <- 1
distribution <- round(rnorm(N, dist.mean, dist.sd), 0)

distribution[distribution > 99] <- 99
distribution[distribution < -99] <- -99
  
while(abs(mean(distribution) - dist.mean) > mean.thresh | 
        abs(median(distribution) - dist.mean) > median.thresh | 
        abs(sd(distribution)) - dist.sd > sd.thresh
        ) {
  
  print(count)
  distribution <- round(rnorm(N, dist.mean, dist.sd), 0)
  distribution[distribution > 99] <- 99
  distribution[distribution < -99] <- -99
  count <- count + 1
  
}



current.outcomes[,option.i] <- distribution

}

}

env.ls[[env.i]] <- current.outcomes


}

# env.formatted = vector of outcomes for administrator page
env.formatted <- c()
konstanz.format <- function(df) {
  
  output <- c()
  
  for(i in 1:ncol(df)) {
    
    if(i < ncol(df)) { output <- c(output, c(paste(as.character(as.numeric(df[,i])), collapse = ","), ";"))}
    if(i == ncol(df)) { output <- c(output, c(paste(as.character(as.numeric(df[,i])), collapse = ",")))}
    
    
  }
  
  output <- paste(output, collapse = "")
  
  return(output)
  
}

for(env.i in 1:length(environments)) {
  
  env.formatted[env.i] <- konstanz.format(env.ls[[env.i]])
  
}
  

# - Plot distributions

par(mfrow = c(1, length(env.ls)))

for(i in 1:length(env.ls)) {
  
  dat <- env.ls[[i]]
  
  beanplot(lapply(1:ncol(dat), function(x) {dat[,x]}), ylim = c(-80, 80))
  mtext(paste("mu\n", colMeans(dat)), side = 3, at = 1:ncol(dat))
  
}
  

##





}


# Write results to tables

write.table(experiment.stim.1, "/Users/Nathaniel/Dropbox/Git/PeeksAndKeeps/stimuli/Peeks and Keeps Stimuli 1.txt", sep = "\t")
write.table(experiment.stim.2, "/Users/Nathaniel/Dropbox/Git/PeeksAndKeeps/stimuli/Peeks and Keeps Stimuli 2.txt", sep = "\t")


# Plot final results
{
pdf("Peeks and Keeps Stimuli.pdf", height = 16, width = 24)


experiment.stim.2 <- experiment.stim.2[with(experiment.stim.2, order(environment, difficulty, option.order)),]
par(mfrow = c(4, 6))

require("RColorBrewer")
col.vec <- brewer.pal(7, "Set3")[5:7]

for (i in 1:nrow(experiment.stim.2)) {
  
  environment.i <- experiment.stim.2$environment[i]
  difficulty.i <- experiment.stim.2$difficulty[i]
  option.order.i <- experiment.stim.2$option.order[i]
  
  trial.df <- experiment.stim.1[experiment.stim.1$environment == environment.i & 
                                experiment.stim.1$difficulty == difficulty.i &
                                experiment.stim.1$option.order == option.order.i
                                , 5:7]
  
  plot(1, xlim = c(0, 201), ylim = c(-75, 75), type = "n", xlab = "Trial", ylab = "Outcome", 
       main = paste(environment.i, difficulty.i, option.order.i))
  
  running.mean <- function(x, lag = 5) {
    result <- rep(NA, length(x))
    
    for (k in 1:length(x)) {if (k >= lag) {result[k] <- mean(x[(k - (lag - 1)):k])}}
    
    return(result)
  }
    
  # Plot individual points
  
  for (k in 1:3) {
  
  lines(1:200, trial.df[,k], col = col.vec[k], lwd = .5)
  lines(1:200, running.mean(trial.df[,k], lag = 10), col = col.vec[k], lwd = 2, lty = 2)
  segments(1, mean(trial.df[1:100, k]), 100, mean(trial.df[1:100, k]), col = col.vec[k], lwd = 2)
  segments(101, mean(trial.df[101:200, k]), 200, mean(trial.df[101:200, k]), col = col.vec[k], lwd = 2)
  
  }  
  
  legend("topright", c("a", "b", "c"), col = col.vec, lty = 1, lwd = 2)
  
}

dev.off()

}