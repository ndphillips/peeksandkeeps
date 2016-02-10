
  
  pdf("pak_pres_plot1.pdf", width = 12, height = 8)
  {
  
  for (sd.i in c(0, 5, 20)) {
    
    
    par(mar = c(5, 6, 10, 1))  
    col.vec <- piratepal(palette = "info", trans = .2)
    
    
    
    plot(1, xlim = c(1, 13), ylim = c(-500, 550), xaxt = "n", xlab = "", type = "n", yaxt = "n", ylab = "Total Points")
    
    mtext(text = rep(c("Keep", "P and K", "P then K"), 2), at = seq(2, 12, 2), side = 3, line = 2.5, cex = 2)
    mtext(text = rep(c("N = 2", "N = 4"), each = 3, times = 2), at = seq(2, 12, 2), side = 3, line = .5, cex = 1.9)
    
    axis(side = 2, labels = seq(-500, 500, 500), at = seq(-500, 500, 500), las = 1, cex.axis = 2)
    
    rect(-1000, -1000, 1000, 1000, col = gray(.99), border = NA)
    abline(h = seq(-500, 500, 100), col = gray(.9), lwd = c(1, 3))
    abline(h = 0, col = "red", lwd = 2)
    

    if(sd.i > 0) {
    
   # abline(v = 7, lty = 2)
    
    beanplot(g1.final.points ~ game.type + n.options + option.sd, 
             data = subset(participants, option.sd == sd.i & is.finite(g1.final.points)), beanlines = "median", 
             ylim = c(-500, 600), main = "", 
             col = lapply(1:6, FUN = function(x) {c(transparent(col.vec[x], .5), gray(0), "white", "black")}), 
             at = 1:6 * 2, xaxt = "n", cex.main = 1.5,
             beanlinewd = 5, what = c(0, 1, 1, 1), cutmax = 510, add = T, maxwidth = 1)
    
    
    medians <- aggregate(g1.final.points ~ game.type + n.options + option.sd, FUN = median, data = subset(participants, option.sd == sd.i & is.finite(g1.final.points)))
    
    text(x = 1:6 * 2 - .5, y = medians[,4], pos = 2, labels = medians[,4], cex = 1.5)
    
    
#     beanplot(g2.final.points ~ game.type + n.options + option.sd, 
#              data = subset(participants, option.sd == sd.i & is.finite(g2.final.points)), 
#              col = lapply(1:6, FUN = function(x) {c(transparent(col.vec[x], .5), gray(0), "white", "black")}), 
#              beanlines = "mean", 
#              ylim = c(-500, 600), cutmax = 510, xaxt = "n",
#              at = 1:6 * 2 + .5, add = T, xaxt = "n", beanlinewd = 5, what = c(1, 1, 1, 1))
    
    }
  }  
    dev.off()
    
  }
  
  
  

  {
  
  for(peek.cond.i in c("pak", "ptk")) {  
    
    pdf(paste("pak_pres_plot2.pdf", peek.cond.i, ".pdf", sep = ""), width = 8.5, height = 6.5)
    
    par(mar = c(5, 5, 4, 2))
    for(k in 0:4) {
    
    
    
    data <- actions
    data$trial.cut <- cut(data$trial, seq(0, 100, 10))
    
    plot(1, xlim = c(1, 10), ylim = c(0, .6), type = "n", 
         xlab = "Trial Block (10 trials per block)", ylab = "p(Peek)", 
         main = "", xaxt = "n", cex.lab = 1.5)
    
  #  mtext("Bars are 95\\% HDIs", side = 3)
    
    col.vec <- piratepal("google")
    
    rect(-100, -100, 100, 100, col = gray(.97))
    
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
    
    
    rect(3.5, .38, 7.5, .55, col = gray(level = 1, alpha = .7), border = "black")
    
    if(k > 0) {
    
    for(i in 1:k) {
      
      if(peek.cond.i == "pak") {adj <- 8}  
      if(peek.cond.i == "ptk") {adj <- 0}  
      
      binom.hdi <- actions %>%
        filter(conditionNr.c == adj + i) %>%
        mutate(
          trial.cut = cut(trial, breaks = seq(0, 100, 10)),
          peek = mode == "peek"
        ) %>%
        group_by(trial.cut) %>%
        summarise(
          lb = betabinom.hdi(vec = peek)$lb,
          ub = betabinom.hdi(vec = peek)$ub
        )
      
      
      agg <- aggregate(mode == "peek" ~ trial.cut, data = subset(data, conditionNr.c == adj + i), FUN = mean)
      
      lines(agg[,1], agg[,2], col = col.vec[i], lwd = 3)
      
      polygon(c(1:10, 10:1), c(binom.hdi$lb, rev(binom.hdi$ub)), 
              col = transparent(col.vec[i], trans.val = .1), border = NA)
      
      overall <- betabinom.hdi(subset(actions, conditionNr.c == adj + i)$mode == "peek")
      
      # Add overall bin
      
      rect(locations[i] - rect.width / 2, 
           rect.min, 
           locations[i] + rect.width / 2,
           rect.min + overall$mode * (rect.max - rect.min),
           col = col.vec[i])
      
      text(locations[i], rect.min + overall$mode * (rect.max - rect.min), round(overall$mode, 2), pos = 3)
      # segments(locations[i] + .5, rect.min + overall$lb * (rect.max - rect.min), 
      #          locations[i] + .5, rect.min + overall$ub * (rect.max - rect.min), col = col.vec[i], lwd = 2)
      
      
    }
    }
    
    axis(side = 1, labels = 1:10, at = 1:10, cex = 1.5)
    text(5.5, .52, "All Trials", cex = 1.5)

    }
    
    
    dev.off()
    
    
  }
  
  }
  
  

  
  pdf("pak_pres_plot3.pdf", width = 12, height = 4)
  
  for(k in 0:3) {
  # ---------------------
  {     
    col.vec <- piratepal("google")
    
    ## Action
    
    par(mfrow = c(1, 3))
    
    # Trial
    { 
    
    plot(1, xlim = c(1, 100), ylim = c(0, 1), 
         type = "n", ylab = "p(Keep)", 
         main = "Trial", xlab = "Trial")
    
    rect(-10, -10, 1000, 1000, col = gray(.96))
    abline(h = seq(0, 1, .1), lwd = c(1, 2), col = gray(1))
    abline(v = seq(0, 100, 10), lwd = c(1, 2), col = gray(1))  
    
    abline(h = .5, lty  = 2)
    
    mtext("p.start, p.end", side = 3, line = .5, cex = .8)
    
    p.start.vec <- c(.8, .1, .4)
    p.end.vec <- c(.2, .6, .4)
    
    if(k > 0) {
    
    
    for(i in 1:k) {
      
      lines(1:100, 
            trial.actionfun(1:100, total.trials = 100, 
                            p.start = p.start.vec[i], 
                            p.end = p.end.vec[i]), 
            col = col.vec[i], lwd = 2)  
      
    }
    
    
    legend("topright", 
           legend = unlist(lapply(1:k, FUN = function(x) {paste("start = ", p.start.vec[x], ", end = ", p.end.vec[x], sep = "")})),
           col = col.vec[1:length(p.start.vec)], lwd = 2, bg = "white")
    
#     legend("topright", 
#            legend = c("p.start = .8, p.end = .2",
#                       "p.start = .1, p.end = .8"),
#            col = col.vec[1:2], lty = 1)
    }
    
    }
    
    # Expectation
    {  
    plot(1, xlim = c(-10, 10), ylim = c(0, 1), 
         type = "n", ylab = "p(Keep)", 
         main = "Expectation", xlab = "Current Expectation")
    
    rect(-100, -100, 1000, 1000, col = gray(.96))
    abline(h = seq(0, 1, .1), lwd = c(1, 2), col = gray(1))
    abline(v = seq(-10, 10, 2.5), lwd = c(1, 2), col = gray(1))  
    
    
    abline(v = 0, lty = 2)
    abline(h = .5, lty = 2)
    mtext("temp, thresh", side = 3, line = .5, cex = .8)
    
    temp.vec <- c(1, .3, 4)
    thresh.vec <- c(0, -3, 5)
    
    if(k > 0) {
    
    for(i in 1:k) {
      
      lines(seq(-10, 10, .5), 
            exp.actionfun(option.exp = seq(-10, 10, .5), 
                          temp.par = temp.vec[i],
                          thresh.par = thresh.vec[i]), 
            col = col.vec[i], lwd = 2)  
      
    }
    
    legend("topleft", 
           legend = unlist(lapply(1:k, FUN = function(x) {paste("temp = ", temp.vec[x], ", thresh = ", thresh.vec[x], sep = "")})),
           col = col.vec[1:k], lwd = 2, bg = "white")
    }  
    }
    
    # Markov
    {
    n <- 20
    
    plot(1, xlim = c(0, n), ylim = c(-3.5, -.5),
         main = "Markov", type = "n", yaxt = "n", ylab = "", xlab = "Trial"
    )
    
    mtext(text = c("p(K|P), p(P|K)"), side = 3, line = .5, cex = .8)
    
    p.pgk.vec <- c(.5, .2, .9)
    p.kgp.vec <- c(.5, .2, .9)
    
    
    if(k > 0) {
    
    for(i in 1:k) {
      
      outcome.vec <- sample(c(0, 1), size = 1)
      
      for(j in 2:n) {
        
        prior <- outcome.vec[j - 1]
        
        if(prior == 0) {
          
          outcome.vec[j] <- sample(c(0, 1), 
                                   size = 1, 
                                   prob = c(1 - p.kgp.vec[i], p.kgp.vec[i]))
          
        }
        
        if(prior == 1) {
          
          outcome.vec[j] <- sample(c(0, 1), 
                                   size = 1, 
                                   prob = c(p.pgk.vec[i], 1 - p.pgk.vec[i]))}
        
      }
      
      points(1:n, rep(-i, n), bg = gray(outcome.vec), pch = 21, cex = 1.5)
      
      text(n / 2, 
           -i + .1, 
           paste("p(P|K) = ", p.pgk.vec[i], ", p(K|P) = ",p.kgp.vec[i], sep = ""), 
           pos = 3, cex = 1.5)  
      
      
      
    }
    
    }
    
    
    }
  }
  }
  
  dev.off()
  
  
  
  
  # Optimal
  
  
  col.vec <- piratepal("google")
  
  points.agg <- read.table("simulations/oct 1 2015/oct 1 2015 agg.txt")
  
  # Plotting
  
  pdf("pak_pres_plot4.pdf", width = 9, height = 5)
  
  {
  
  n.bad.vec <- c(0, 1, 3)
  sd.vec <- c(5, 20)
  
  par(mfrow = c(1, 2))
  par(mar = c(5, 5, 3, 1))
  
  for(k in 1:3) {
  
  for(sd.i in 1:length(sd.vec)) {
    
    plot(1, 
         xlim = c(-5, 100), 
         ylim = c(-100, 500), 
         type = "n",
         ylab = "Mean points earned",
         xlab = "Number of peeking trials",
         main = paste("sd = ", sd.vec[sd.i], sep = ""),
         cex.main = 1
    )

    
    mtext("Pos ~ N(5, sd), Neg ~ N(-5, sd)", side = 3, line = .2, cex = .5)
    
    rect(-10000, -1000, 10000, 10000, col = gray(.95))
    
    abline(h = seq(-1000, 1000, 100), col = gray(1), lwd = 1.5)
    abline(h = seq(-1050, 1050, 100), col = gray(1), lwd = .75)
    
    abline(v = seq(0, 100, 10), col = gray(1), lwd = 1.5)
    abline(v = seq(50, 150, 10), col = gray(1), lwd = .75)
    
    abline(h = 0)
    
    legend("topright",
           paste("N = ", c(2, 4), sep = ""),
           pch = 16,
           col = col.vec,
           lty = 1, bg = "white"
    )
    
if(k > 1) {
    
    for(n.bad.i in 2:k) {
      
      data <- subset(points.agg, n.bad == n.bad.vec[n.bad.i] & sd == sd.vec[sd.i])
      
      lines(data$n.peeks, 
            data$total.points.mean, 
            col = col.vec[n.bad.i - 1], 
            type = "b", pch = 16, lty = 1)
      
      optimal.peeks <- min(data$n.peeks[data$total.points.mean == max(data$total.points.mean)], na.rm = T)
      optimal.points <- max(data$total.points.mean, na.rm = T)
      
      points(optimal.peeks, optimal.points, pch = 21, cex = 1.5)
      text(optimal.peeks, 
           optimal.points, 
           labels = paste(optimal.peeks, ", ", round(optimal.points, 0), sep = ""), 
           pos = 3)
      
      # lines(subset(data, select.strategy == "egreedy.25")$n.trials, 
      #       subset(data, select.strategy == "egreedy.25")$impression.mad, col = "blue")
      # 
      # lines(subset(data, select.strategy == "egreedy.05")$n.trials, 
      #       subset(data, select.strategy == "egreedy.05")$impression.mad, col = "blue")
      
      
    }
   
}
   
  }
  }
  dev.off()
  
  }
  
  
  # Peek rates and earnings
  
  pdf("pak_pres_plot6.pdf", width = 12, height = 6)
  
  
  for(i in c(0, 1, 2)) {
  
  par(mfrow = c(1, 2))
  
  col.vec <- piratepal("google", length.out = 4, trans = .1)
  
  for(game.type.i in c("pak", "ptk")) {
    
    plot(1, xlim = c(1, 100), ylim = c(-500, 500), type = "n", main = game.type.i, 
         bty = "n", yaxt = "n", ylab = "Final Points", xlab = "Number of Peeks")
    axis(2, at = seq(-500, 500, 100), las = 1)
    abline(h = seq(-500, 500, 100), lwd = c(.5, 1), col = gray(.5))
    
    if(i > 0) {
    
    for(env.i in 1:4) {
      
      data <- subset(participants, game.type == game.type.i & environment == env.i & is.finite(g1.final.points) & is.finite(g1.n.peeks))
      with(data, points(g1.n.peeks, g1.final.points, pch = 16, col = col.vec[env.i]))
      
      
      # Add simulation curve
      
      
      if(i > 1) {
      sim.agg <- subset(points.agg, n.bad == condition.table$n.options[condition.table$environment == env.i] & 
                          sd == condition.table$option.sd[condition.table$environment == env.i])
      
      lines(sim.agg$n.peeks, sim.agg$total.points.mean, col = col.vec[env.i], lwd = 3)
      
      }
      
      
      
    }
    }
    
    legend("topright", 
           legend = c("N = 2, sd = 5", "N = 2, sd = 20", "N = 4, sd = 5", "N = 4, sd = 20"),
           col = col.vec,
           lty = 1, lwd = 2, bg = "white")
    
    
  }
  }
    dev.off()
    

    # Peek rates and earnings
    
    pdf("pak_pres_plot5.pdf", width = 12, height = 6)
    
    
    for(i in c(0, 1, 2)) {
      
      par(mfrow = c(1, 2))
      
      col.vec <- piratepal("google", length.out = 4, trans = .1)
      
      for(game.type.i in c("pak", "ptk")) {
        
        plot(1, xlim = c(1, 10), ylim = c(-500, 500), type = "n", main = game.type.i, 
             bty = "n", yaxt = "n", ylab = "Final Points", xlab = "Number of Peeks")
        axis(2, at = seq(-500, 500, 100), las = 1)
        abline(h = seq(-500, 500, 100), lwd = c(.5, 1), col = gray(.5))
        
        if(i > 0) {
          
          for(env.i in 1:4) {
            
            data <- subset(participants, game.type == game.type.i & environment == env.i & is.finite(g1.final.points) & is.finite(g1.n.peeks))
            data$peeks.cut <- cut(data$g1.n.peeks, breaks = seq(0, 100, 5))
            
            get.lb <- function(x) {
              
              if(length(x) > 5) {
              return(hdi(BESTmcmc(x, numSavedSteps = 1000, burnInSteps = 10, verbose = F))[1])}
              
              if(length(x) <= 5) {return(NA)}
            }
              
            
            agg <- data %>% group_by(peeks.cut) %>% 
              summarise(
                g1.points.med = median(g1.final.points),
                n = n(),
                lb = get.lb(g1.final.points)
              )
                
            
      with(agg, lines(1:nrow(agg), agg$g1.points.med, col = col.vec[env.i]))

            
            
          }
        }
        
        legend("topright", 
               legend = c("N = 2, sd = 5", "N = 2, sd = 20", "N = 4, sd = 5", "N = 4, sd = 20"),
               col = col.vec,
               lty = 1, lwd = 2, bg = "white")
        
        
      }
    }
    dev.off()

    
    
    # Distributions
    
    par(mar = c(4, 4, 1, 1))
    
    col.vec <- piratepal("google", trans = 0)
    pdf("pak_pres_plot6.pdf", width = 8, height = 8)
    
    for(i in 1:2) {
    
    
    par(mfrow = c(2, 2))

    plot(1, xlim = c(-60, 60), ylim = c(0, .15), 
         type = "n", xlab = "outcome", 
         ylab = "likelihood", main = "N = 2, sd = 5")
    
    rect(-100, -100, 100, 100, col = gray(.96))
    abline(h = seq(0, .2, .025), lwd = c(1, 2), col = gray(1))
    abline(v = seq(-60, 60, 10), lwd = c(1, 2), col = gray(1))
    
    if(i > 1) {
    
  fun <- function(x) {return(dnorm(x, mean = 5, sd = 5))}
    curve(expr = fun, from = -20, to = 20, add = T, col = col.vec[1], lwd = 3)
  
    fun <- function(x) {return(dnorm(x, mean = -5, sd = 5))}
    curve(expr = fun, from = -20, to = 20, add = T, col = col.vec[2], lwd = 3)  
        
    abline(v = c(5, -5), lty = 2, col = col.vec[1:2], lwd = 2)
    
    add.rect.fun <- function(mean, sd, height, rect.height, col) {
      
      lb <- qnorm(.025, mean, sd)
      ub <- qnorm(.975, mean, sd)
      
      rect(lb, height - rect.height / 2, ub, 
           height + rect.height / 2, col = col, border = NA)
      
    }
    
    add.rect.fun(5, 5, .125, .005, col.vec[1])
    add.rect.fun(-5, 5, .115, .005, col.vec[2])
    
    }
    
        
    plot(1, xlim = c(-60, 60), ylim = c(0, .15), 
         type = "n", xlab = "outcome", 
         ylab = "likelihood", main = "N = 4, sd = 5")
    
    rect(-100, -100, 100, 100, col = gray(.96))
    abline(h = seq(0, .2, .025), lwd = c(1, 2), col = gray(1))
    abline(v = seq(-60, 60, 10), lwd = c(1, 2), col = gray(1))
    
    if(i > 1) {
    
    fun <- function(x) {return(dnorm(x, mean = 5, sd = 5))}
    curve(expr = fun, from = -50, to = 50, add = T, col = col.vec[1], lwd = 3)
    
    fun <- function(x) {return(dnorm(x, mean = -5, sd = 5))}
    curve(expr = fun, from = -50, to = 50, add = T, col = col.vec[2], lwd = 3)  
    
    fun <- function(x) {return(dnorm(x + 1, mean = -5, sd = 5))}
    curve(expr = fun, from = -50, to = 50, add = T, col = col.vec[3], lwd = 3)  
    
    fun <- function(x) {return(dnorm(x - 1, mean = -5, sd = 5))}
    curve(expr = fun, from = -50, to = 50, add = T, col = col.vec[4], lwd = 3)  
    
    
    abline(v = c(5, -5, -6, -4), lty = 2, col = col.vec[1:4], lwd = 2)
    
    add.rect.fun(5, 5, .125, .005, col.vec[1])
    add.rect.fun(-5, 5, .115, .005, col.vec[2])
    add.rect.fun(-5, 5, .105, .005, col.vec[3])
    add.rect.fun(-5, 5, .095, .005, col.vec[4])
    
    }
    
    
    plot(1, xlim = c(-60, 60), ylim = c(0, .15), 
         type = "n", xlab = "outcome", 
         ylab = "likelihood", main = "N = 2, sd = 20")
    
    rect(-100, -100, 100, 100, col = gray(.96))
    abline(h = seq(0, .2, .025), lwd = c(1, 2), col = gray(1))
    abline(v = seq(-60, 60, 10), lwd = c(1, 2), col = gray(1))
    
    
    if(i > 1) {
    fun <- function(x) {return(dnorm(x, mean = 5, sd = 20))}
    curve(expr = fun, from = -50, to = 50, add = T, col = col.vec[1], lwd = 3)
    
    fun <- function(x) {return(dnorm(x, mean = -5, sd = 20))}
    curve(expr = fun, from = -50, to = 50, add = T, col = col.vec[2], lwd = 3)  

    abline(v = c(5, -5), lty = 2, col = col.vec[1:2], lwd = 2)
    
    
    add.rect.fun(5, 20, .125, .005, col.vec[1])
    add.rect.fun(-5, 20, .115, .005, col.vec[2])
}
    
    
  
    plot(1, xlim = c(-60, 60), ylim = c(0, .15), 
         type = "n", xlab = "outcome", 
         ylab = "likelihood", main = "N = 4, sd = 20")
    
    rect(-100, -100, 100, 100, col = gray(.96))
    abline(h = seq(0, .2, .025), lwd = c(1, 2), col = gray(1))
    abline(v = seq(-60, 60, 10), lwd = c(1, 2), col = gray(1))
    
    if(i > 1) {
    
    fun <- function(x) {return(dnorm(x, mean = 5, sd = 20))}
    curve(expr = fun, from = -50, to = 50, add = T, col = col.vec[1], lwd = 3)
    
    fun <- function(x) {return(dnorm(x, mean = -5, sd = 20))}
    curve(expr = fun, from = -50, to = 50, add = T, col = col.vec[2], lwd = 3)  
    
    fun <- function(x) {return(dnorm(x + 2, mean = -5, sd = 20))}
    curve(expr = fun, from = -50, to = 50, add = T, col = col.vec[3], lwd = 3)  
    
    fun <- function(x) {return(dnorm(x - 2, mean = -5, sd = 20))}
    curve(expr = fun, from = -50, to = 50, add = T, col = col.vec[4], lwd = 3)  
    
    abline(v = c(5, -5, -6, -4), lty = 2, col = col.vec[1:4], lwd = 2)
    
    
    add.rect.fun(5, 20, .125, .005, col.vec[1])
    add.rect.fun(-5, 20, .115, .005, col.vec[2])
    add.rect.fun(-5, 20, .105, .005, col.vec[3])
    add.rect.fun(-5, 20, .095, .005, col.vec[4])
    
    
    }
    }
   dev.off() 
    
  