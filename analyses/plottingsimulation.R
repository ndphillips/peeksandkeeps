
# Plotting Simulation

setwd(dir = "/Users/Nathaniel/Dropbox/Git/PeeksAndKeeps")

library(RColorBrewer)

#dates.to.use <- c("Jul 30", "Jul 31")

#all.files <- list.files("simulations")
#files.to.use <- all.files[grepl(paste(dates.to.use, sep = "|"), all.files)]

files.to.use  <- list.files("simulations")

for(i in 1:length(files.to.use)) {
  print(i)
  
  current.df <- read.table(paste("simulations/", files.to.use[i], sep = ""), 
                           stringsAsFactors = F, header = T, sep = "\t")
  
  if(i == 1) {peeks.sim <- current.df}
  if(i > 1) {peeks.sim <- rbind(peeks.sim, current.df)}
  
}


environments.df <- expand.grid(n.good = sort(unique(peeks.sim$n.good)),
                               n.bad = sort(unique(peeks.sim$n.bad)),
                               sd = sort(unique(peeks.sim$sd)), stringsAsFactors = F
)

n.environments <- nrow(environments.df)

par(mfcol = c(length(unique(peeks.sim$n.bad)), length(unique(peeks.sim$sd))))

for(env.i in 1:n.environments) {
  
  n.good.i <- environments.df$n.good[env.i]
  n.bad.i <- environments.df$n.bad[env.i]
  sd.i <- environments.df$sd[env.i]

  par(mar = c(5, 4, 4, 1))
  
  # Draw optimal samples
{
    plot(1, xlim = c(0, 100),
         ylim = c(-300, 500), type = "n", 
         main = paste("n.bad =", n.bad.i, ",sd =", sd.i))
    
    abline(h = seq(-300, 500, 100), col = gray(.5, alpha = .2), lty = 2)
    abline(h = seq(-350, 550, 100), col = gray(.5, alpha = .2), lty = 1)
    
    abline(v = seq(0, 100, 20), col = gray(.5, alpha = .2), lty = 2)
    abline(v = seq(10, 110, 20), col = gray(.5, alpha = .2), lty = 1)
    
    abline(h = 0, lwd = 2)
    
    for(peek.strategy.i in c("uniform", "softmax")) {
      
      peeks.df <- subset(peeks.sim, 
                                 n.good == n.good.i & 
                                   n.bad == n.bad.i & 
                                   sd == sd.i &
                                   peek.select.strategy == peek.strategy.i &
                                   keep.select.strategy == "softmax" &
                                   action.strategy == "trial" & 
                                   update.method == "mean")
      
    
    agg <- tapply(peeks.df$total.points, peeks.df$n.peeks, median)
    
    if(peek.strategy.i == "uniform") {line.lty = 1; line.col = "blue"}
    if(peek.strategy.i == "softmax") {line.lty = 1; line.col = "red"}
    
    lines(names(agg), agg, lwd = 2, type = "b", lty = line.lty, col = line.col)
    
    points(names(agg)[which(agg == max(agg))], agg[which(agg == max(agg))],
           cex = 2, col = "red"
    )
    
    text(names(agg)[which(agg == max(agg))], agg[which(agg == max(agg))],
         names(agg)[which(agg == max(agg))], pos = 3)
  }

}


}