# draw the transition matrices for the two learner types

# function to compute transition probabilities
getTP <- function(a,b,n=10) {
  
  th <- seq(0,1,.01)
  TP <- matrix(NA,n+1,n+1,dimnames = list(y=0:n,x=0:n))
  for(x in 0:n) {
    for(y in 0:n) {
      TP[y+1,x+1] <- sum( choose(n,y) * th^y * (1-th)^(n-y) *dbeta(th,a+x,b+n-x))
    }
    TP[,x+1] <- TP[,x+1] / sum(TP[,x+1])
  }
  return(t(TP))
}
  
# plotting function
drawTP <- function(P,...) {

  n <- dim(P)[1]-1
  plot.new()
  plot.window(xlim=c(-.5,n+.5),ylim=c(0,n+.5))
  for(x in 0:n) {
    for(y in 0:n) {
      if(P[y+1,x+1]>.01) {
        polygon(x=x+c(0,1,1,0)-.5,y=y+c(0,0,1,1)*P[y+1,x+1]*1.25,col = "black")
      }
    }
  }
  axis(1)
  axis(2)
  box()
  title(xlab="Rule consistent responses, y",ylab="Rule consistent training data, x",...)
  
}

# draw the figure
layout(matrix(1:2,1,2))

P <- getTP(2,1)
drawTP(P,main="Weak Bias Learner")

P <- getTP(1,10)
drawTP(P,main="Strong Bias Learner")

layout(1)

#dev.print(pdf,file="./betabinomialtransitions.pdf",width=7,height=6)

