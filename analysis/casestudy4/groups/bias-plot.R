
# load the simulation data
load("bias-data.Rdata")

gpmu <- groupMeans
sd2 <- sig2
nb <- length(sig2)

col1 <- "grey50"
col2 <- "black"
addLegend <- FALSE

plot(1/sd2,gpmu[1,],type="o",col=col1,ylim=c(0,1), pch=19,
     bty="n",xlab="Precision of Group 2 (1/sig)",ylab="Group Mean")
lines(1/sd2,gpmu[2,],type="o",col=col2,pch=19)
lines(1/sd2,rep.int(0,nb),col=col1,lty=3,pch=21,bg="white",type="o")
lines(1/sd2,rep.int(1,nb),col=col2,lty=3,pch=21,bg="white",type="o")
#abline(v = 1, col="black",lty=2)

title(main="(a) Strength of Belief")

if(addLegend) { 
  legend(x=3, y=.4,
         legend=c("group 1 iterated", "group 1 prior", "group 2 iterated", "group 2 prior"),
         lty=c(1,3,1,3),col=c(col1,col1,col2,col2),pt.bg=c(col1,"white",col2,"white"),pch=21)
}

box()

