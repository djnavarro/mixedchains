
# load the simulation data
load("separation-data.Rdata")

gpmu <- groupMeans
nb <- length(mu2)

col1 <- "grey50"
col2 <- "black"
addLegend <- TRUE

plot(mu2,gpmu[1,],type="o",col=col1,ylim=c(0,1), pch=19,
     bty="n",xlab="Prior Mean of Group 2",ylab="Group Mean")
lines(mu2,gpmu[2,],type="o",col=col2,pch=19)
lines(mu2,rep.int(0,nb),col=col1,lty=3,pch=21,bg="white",type="o")
lines(mu2,mu2,col=col2,lty=3,pch=21,bg="white",type="o")


title(main="(b) Separation Between Groups")

if(addLegend) {
  legend(x=0, y = 1,
         legend=c("group 1 iterated", "group 1 prior", "group 2 iterated", "group 2 prior"),
         lty=c(1,3,1,3),col=c(col1,col1,col2,col2),pt.bg=c(col1,"white",col2,"white"),pch=21,
         bty="n")
}

box()
