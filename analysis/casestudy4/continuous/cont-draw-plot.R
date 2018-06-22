
plotDist <- function(param,chains,paneltext) {
  
  its <- param$its
  
  # prior & posterior over mu for everyone in the chain
  pre_mean <- chains[[1]][(its-10):its,3,]
  post_mean <- chains[[1]][(its-10):its,1,] 
  
  # prior & posterior over sig for everone in the chain
  pre_sd <- chains[[1]][(its-10):its,4,]
  post_sd <- chains[[1]][(its-10):its,2,]
  
  ns <- length(pre_mean)
  
  # sample eta using the priors 
  pre_draw <- pre_mean + rnorm(ns, 0, pre_sd)
  
  # sample eta using the iterated-posteriors
  post_draw <- post_mean + rnorm(ns, 0, post_sd)
  
  yl <- .8
  xl <- c(-7,7)
  br <- seq(-30,50,.25)
  
  # histograms for the main plot
  ind <- pre_draw > min(br) & pre_draw < max(br)
  pre_hist <- hist(pre_draw[ind], breaks = br, plot = FALSE)
  
  ind <- post_draw > min(br) & post_draw < max(br)
  post_hist <- hist(post_draw[ind], breaks = br, plot = FALSE)
  
  # sey up the main plot
  plot.new() 
  plot.window(xlim=xl,ylim=c(0,yl))
  
  # prior histogram
  lines(pre_hist$mids,pre_hist$density,col="black",lwd=2)
  polygon(pre_hist$mids,pre_hist$density,col="grey90")
  
  # iterated histogram
  lines(post_hist$mids,post_hist$density,col="black",lwd=2)
  polygon(post_hist$mids,post_hist$density,density=20)
  
  # prettiness
  legend(x=2.5, y=.3, 
         legend=c("Prior","Iterated"), 
         col=c("black","black"), bty="n", 
         fill=c("grey90","black"), density = c(NA,40))
  axis(1)
  #axis(2)
  title(xlab="Learner Hypothesis")
  
  xshift <- 10
  
  # draw the subplots showing the population prior
  x0 <- -6 + xshift
  y0 <- .23 * (yl/.3)
  xs <- 3 
  ys <- .05 * (yl/.3)
  
  polygon(c(x0,x0+xs,x0+xs,x0),c(y0,y0,y0+ys,y0+ys))
  h <- hist(pre_mean,br,plot=FALSE)
  bs <- h$mid-xl[1]; bs <- bs/(xl[2]-xl[1])
  ind <- bs >= 0 & bs <= 1
  yscf <- 1.9
  polygon(x0 + bs[ind]*xs ,y0 +  h$density[ind] * ys * yscf, col="grey90")
  lines(x0 + bs[ind]*xs ,y0 +  h$density[ind] * ys * yscf, lwd=2)
  lines(c(1,1)*x0 + xs/2, y0+c(0,ys), lty=3, lwd=2)
  text(-4.5+ xshift,.29  * (yl/.3),"Bias")
  
  
  
  # draw the subplots showing the strength
  x0 <- -6 + xshift
  y0 <- .15  * (yl/.3)
  xs <- 3
  ys <- .05  * (yl/.3)
  
  polygon(c(x0,x0+xs,x0+xs,x0),c(y0,y0,y0+ys,y0+ys))
  str <- 1/param$strength(h$mid)
  yscf <- .075
  polygon(c(x0, x0 + bs[ind]*xs, x0 + xs), c(y0, y0 +  str[ind] * ys * yscf, y0), col="grey90")
  lines(x0 + bs[ind]*xs ,y0 +  str[ind] * ys * yscf, lwd=2)
  lines(c(1,1)*x0 + xs/2, y0+c(0,ys), lty=3, lwd=2)
  text(-4.5+ xshift,.21  * (yl/.3),"Strength")
  
  abline(v=0, lty=3, lwd=2)
  box()
  
  text(-6.25,.75,paneltext,font = 2,cex = 1.5)
  
  ysh <- .05
  text(-7, .5-ysh, paste0("mean_P = ", round(mean(pre_draw),1)), pos = 4)
  text(-7, .425-ysh, paste0("mean_I = ", round(mean(post_draw),1)), pos = 4)
  
  text(-7, .3-ysh, paste0("sd_P = ", round(sd(pre_draw),1)), pos = 4)
  text(-7, .22-ysh, paste0("sd_I = ", round(sd(post_draw),1)), pos = 4)
  
    
  return(list(pre_draw=pre_draw, post_draw=post_draw))
  
}

op <- par(no.readonly = TRUE)
par(mar=c(5,1,1,1))
layout(matrix(1:6,2,3,byrow = TRUE))
for(i in 1:6) {
  load(paste0("cont-draw-data-",i,".Rdata"))
  x <- plotDist(param, chains, 
                paste0("(",letters[i],")"))
  # print(i)
  # print(mean(x$pre_draw))
  # print(mean(x$post_draw))
  # 
  # print(sd(x$pre_draw))
  # print(sd(x$post_draw))
}
layout(1)
par(op)

dev.print(pdf,file="./cont-draw-plot.pdf",
          width=8,height=5)
