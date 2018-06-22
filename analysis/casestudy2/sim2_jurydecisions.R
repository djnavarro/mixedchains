
#### Parameters defining the simulations

# simulation parameters
nits <- 100000  # paper used 100000
n <- 12

# homogenous chain of sheep - 
rs <- .9
ths <- .6

# homogenous chain of goats - 
rg <- .5
thg <- .4

# 50-50 mixture
w <- .5
r <- c(rs,rg)
th <- c(ths,thg)




#### Define the Bayesian juror

# prior sample
prior <- function(th) {
  v <- as.numeric((runif(1) <= th))
  #v <- as.numeric(runif(1)<.5)
  return(v)
}

# given input vote v, posterior sample 
iterate <- function(v,r,th) {
  if(v==1) { # last person voted for plaintiff
    h1 <- r*th # it was plaintiff + previous person corret
    h2 <- (1-r)*(1-th) # defenande + previous wrong
    
  } 
  
  if(v==0) { # last person voted for defendant
    h1 <- (1-r)*th # plaintiff + previous wrong
    h2 <- r*(1-th) # defentant + previous right
  }
  
  p <- h1 / (h1 + h2)
  v2 <- as.numeric((runif(1) <= p))
  return(v2)
}



#### Functions to simulate a single chain, either pure or mixed

purechain <- function(r,th,n) {
  v <- numeric(length=n)
  v[1] <- prior(th)
  for(i in 2:n) {
    v[i] <- iterate(v[i-1],r,th)
  }
  return(v)
}

mixedchain <- function(r,th,w,n) {
  
  v <- numeric(length=n)
  l <- as.numeric(runif(n)<=w) # assign learner types
  
  v[1] <- prior(th[l[1]+1])
  for(i in 2:n) {
    v[i] <- iterate(v[i-1],r[l[i]+1],th[l[i]+1])
  }
  return(list(v=v,l=l))
  
}



#### Functions to simulate a many chain, either pure or mixed

homogenouschains <- function(r,th,nits,n) {
  
  # run many chains
  V <- matrix(NA,nits,n)
  for( i in 1:nits ) {
    V[i,] <- purechain(r,th,n)
  }
  return(V)
  
}

heterogenouschains <- function(r,th,nits,w,n) {
  
  # run many chains
  V <- matrix(NA,nits,n)
  L <- matrix(NA,nits,n)
  for( i in 1:nits ) {
    c <- mixedchain(r,th,w,n)
    V[i,] <- c$v
    L[i,] <- c$l
  }
  return(list(V=V,L=L))
  
}



#### Run the three chains

sheepVotes <- homogenouschains(rs,ths,nits,n)
goatVotes <- homogenouschains(rg,thg,nits,n)
c <- heterogenouschains(r,th,nits,w,n)
mixedVotes <- c$V
mixedLearners <- c$L



#### Define plotting functions

sequencePlot <- function(V,ind=NULL,mstr) {
  if(is.null(ind)) {
    val <- colMeans(V)
  } else {
    val <- colSums(V*ind) / colSums(ind)
  }
  plot(1:n,val,xlab="Juror Number",
       ylab="Probability of Voting for Plaintiff",pch=19, type="o",
       cex.axis=1.5, cex.lab=1.5, cex.main=1.5,
       ylim=c(.3,.7),main=mstr)
}

tallyPlot <- function(V,mstr) {
  n <- dim(V)[2]
  f <- tabulate(rowSums(V)+1,n+1)
  f <- f/sum(f)
  names(f) <- 0:n
  barplot(f,names.arg = 0:n,ylim=c(0,.45),
          xlab="Total Votes for Plaintiff",
          ylab="Probability",
          cex.axis=1.5, cex.lab=1.5, cex.main=1.5,cex.names=1.5,
          col="grey50",main=mstr)
  box()
}


#### Draw the figure plotting the overall behaviour of the two 
#### heterogenous chains and the one mixed chain

layout(matrix(1:6,2,3))

sequencePlot(goatVotes,mstr="100% Goats")
abline(h=thg,lty=2)
tallyPlot(goatVotes,mstr="100% Goats")

sequencePlot(sheepVotes,mstr="100% Sheep")
abline(h=ths,lty=2)
tallyPlot(sheepVotes,mstr="100% Sheep")

mstr <- paste0((1-w)*100, "% Sheep, ", w*100, "% Goat")
sequencePlot(mixedVotes,mstr=mstr)
abline(h=(1-w)*th[1] + w*th[2],lty=2)
tallyPlot(mixedVotes,mstr=mstr)

layout(1)


#### Draw the figure plotting the goats and sheep from the mixed chain separately

layout(matrix(1:2,1,2))
sequencePlot(mixedVotes,ind=mixedLearners,mstr="Goats in Mixed Chain")
abline(h=thg,lty=2)

sequencePlot(mixedVotes,ind=1-mixedLearners,mstr="Sheep in Mixed Chain")
abline(h=ths,lty=2)
layout(1)


