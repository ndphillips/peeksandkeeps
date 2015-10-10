# Generating Distributions for Peeks and Keeps Study

library("beanplot")

sampling.method <- "unstructured"

# - Set parameters for mean of distributions
bad.mean <- -5
middle.mean <- 0
good.mean <- 5

# - Set parameters for standard deviations of distributions

easy.sd <- 10
hard.sd <- 30

# Set rejection thresholds for random samples - R will continually generate samples until the 
# sample mean and standard deviations are within this threshold of the parameter values

mean.thresh <- .05
median.thresh <- .05
sd.thresh <- 1


# - Create sample distributions

dist.names <- c("be", "bh", "me", "mh", "ge", "gh")
dist.df <- as.data.frame(matrix(NA, nrow = 200, ncol = length(dist.names)))
names(dist.df) <- dist.names

for (i in 1:length(dist.names)) {
  print(i)
dist.name <- dist.names[i]  
  
mean.char <- substr(dist.name, 1, 1)
sd.char <- substr(dist.name, 2, 2)  
  
if(mean.char == "b") {dist.mean <- bad.mean}
if(mean.char == "m") {dist.mean <- middle.mean}
if(mean.char == "g") {dist.mean <- good.mean}

if(sd.char == "h") {dist.sd <- hard.sd}
if(sd.char == "e") {dist.sd <- easy.sd}

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

distribution <- round(rnorm(200, dist.mean, dist.sd), 0)

distribution[distribution > 99] <- 99
distribution[distribution < -99] <- -99
  
while(abs(mean(distribution[1:100]) - dist.mean) > mean.thresh | 
        abs(median(distribution[1:100]) - dist.mean) > median.thresh | 
        abs(sd(distribution[1:100])) - dist.sd > sd.thresh |
        abs(mean(distribution[101:200]) - dist.mean) > mean.thresh | 
        abs(median(distribution[101:200]) - dist.mean) > median.thresh | 
        abs(sd(distribution[101:200]) - dist.sd) > sd.thresh 
        ) {
  
  distribution <- round(rnorm(200, dist.mean, dist.sd), 0)
  distribution[distribution > 99] <- 99
  distribution[distribution < -99] <- -99
  
}

dist.df[,i] <- distribution

}


}

# - Plot distributions

beanplot(dist.df, col = "white", 
         names = c("Bad-Easy", "Bad-Hard", "Middle-Easy", "Middle-Hard", "Good-Easy", "Good-Hard")
)

# - Create Stimuli for experiment

experiment.stim.1 <- expand.grid("environment" = c("dynamic", "stable"),
                               "difficulty" = c("easy", "hard"),
                               "option.order" = c("bmg", "bgm", "gmb", "gbm", "mgb", "mbg"),
                               "trial" = 1:200,
                               "a" = NA,
                               "b" = NA,
                               "c" = NA,
                               stringsAsFactors = F
                                )


experiment.stim.1 <- experiment.stim.1[with(experiment.stim.1, order(environment, difficulty, option.order, trial)),]



experiment.stim.2 <- expand.grid("environment" = c("dynamic", "stable"),
                               "difficulty" = c("easy", "hard"),
                               "option.order" = c("bmg", "bgm", "gmb", "gbm", "mgb", "mbg"),
                               stringsAsFactors = F
)


for (i in 1:nrow(experiment.stim.2)) {
  
  environment.i <- experiment.stim.2$environment[i]
  difficulty.i <- experiment.stim.2$difficulty[i]
  option.order.i <- experiment.stim.2$option.order[i]

# Start with the order bmg

if(environment.i == "stable" & difficulty.i == "easy") {
  
  trial.df <- cbind(dist.df$be[1:200], 
                    dist.df$me[1:200], 
                    dist.df$ge[1:200])
  
}

if(environment.i == "stable" & difficulty.i == "hard") {
  
  trial.df <- cbind(dist.df$bh[1:200], 
                    dist.df$mh[1:200], 
                    dist.df$gh[1:200])
  
}

if(environment.i == "dynamic" & difficulty.i == "easy") {
  
  trial.df <- cbind(c(dist.df$be[1:100], dist.df$ge[101:200]), 
                    c(dist.df$me[1:100], dist.df$me[101:200]), 
                    c(dist.df$ge[1:100], dist.df$be[101:200])
  )
  
}

if(environment.i == "dynamic" & difficulty.i == "hard") {
  
  trial.df <- cbind(c(dist.df$bh[1:100], dist.df$gh[101:200]), 
                    c(dist.df$mh[1:100], dist.df$mh[101:200]), 
                    c(dist.df$gh[1:100], dist.df$bh[101:200])
  )
                    
}

if(option.order.i == "bmg") {trial.df <- trial.df[,c(1, 2, 3)]}
if(option.order.i == "bgm") {trial.df <- trial.df[,c(1, 3, 2)]}
if(option.order.i == "gmb") {trial.df <- trial.df[,c(3, 2, 1)]}
if(option.order.i == "gbm") {trial.df <- trial.df[,c(3, 1, 2)]}
if(option.order.i == "mgb") {trial.df <- trial.df[,c(2, 3, 1)]}
if(option.order.i == "mbg") {trial.df <- trial.df[,c(2, 1, 3)]}

experiment.stim.1[experiment.stim.1$environment == environment.i & 
                experiment.stim.1$difficulty == difficulty.i & 
                experiment.stim.1$option.order == option.order.i, 5:7] <- trial.df 

konstanz.format <- function(df) {
  
  output <- c()
  
  for(i in 1:ncol(df)) {
    
    if(i < ncol(df)) { output <- c(output, c(paste(as.character(as.numeric(df[,i])), collapse = ","), ";"))}
    if(i == ncol(df)) { output <- c(output, c(paste(as.character(as.numeric(df[,i])), collapse = ",")))}
    
    
  }
  
  output <- paste(output, collapse = "")
  
  return(output)
  
}

experiment.stim.2$game.input[i] <- konstanz.format(trial.df)

}


# Write results to tables

#write.table(experiment.stim.1, "/Users/Nathaniel/Dropbox/Git/PeeksAndKeeps/stimuli/Peeks and Keeps Stimuli 1.txt", sep = "\t")
#write.table(experiment.stim.2, "/Users/Nathaniel/Dropbox/Git/PeeksAndKeeps/stimuli/Peeks and Keeps Stimuli 2.txt", sep = "\t")


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