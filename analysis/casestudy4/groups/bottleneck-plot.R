
# load the simulation data
load("bottleneck-data.Rdata")

gpmu <- groupMeans
nb <- length(N)

col1 <- "grey50"
col2 <- "black"
addLegend <- FALSE


plot(N,gpmu[1,],type="o",col=col1,ylim=c(0,1), pch=19,
     bty="n",xlab="Size of Bottleneck (N)",ylab="Group Mean")
lines(N,gpmu[2,],type="o",col=col2,pch=19)
lines(N,rep.int(0,nb),col=col1,lty=3,pch=21,bg="white",type="o")
lines(N,rep.int(1,nb),col=col2,lty=3,pch=21,bg="white",type="o")


title(main="(d) Amount of Input")

if(addLegend) {
  legend(x=6, y = .35,
         legend=c("group 1 iterated", "group 1 prior", "group 2 iterated", "group 2 prior"),
         lty=c(1,3,1,3),col=c(col1,col1,col2,col2),pt.bg=c(col1,"white",col2,"white"),pch=21)
}

box()