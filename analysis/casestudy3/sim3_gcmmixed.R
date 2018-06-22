
its <- 15 # number of iterations in the chain
nruns <- 30000 # number of simulated chains: paper used 300000
n <- 8 # number of objects
lmb <- c(.1, 1, 10) # lambda values

dosim <- FALSE # should we run a simulation? (or use saved file)
dosave <- FALSE # should we save the simulation data to file


### Function defining the GCM

gcm <- function(train,test,labels,lmb,soft=FALSE) {

  ntest <- length(test)
  testlab <- p0 <- vector(length=ntest)
  
  for( i in 1:ntest) {
    sim0 <- sum(exp(-lmb*abs(train[labels==0]-test[i])))
    sim1 <- sum(exp(-lmb*abs(train[labels==1]-test[i])))
    p0[i] <- sim0 / (sim0 + sim1) 
    testlab[i] <- ifelse( runif(1)<p0[i], 0,1)
  }
  if(soft) {
    return(p0) 
  } else {
    return(testlab)
  }
}

## Functions defining the coherence score and the size measure for a categorization scheme

score <- function(labels) {
  boundaries <- sum(labels[-1] == labels[-length(labels)])
  return(boundaries)
}

size <- function(labels) {
  n <- length(labels)
  s <- sum(labels)
  return(min(s,n-s))
}


### Functions defining the iterated learning procedure

subsample <- function(labels,m=4) {
  n <- length(labels)
  ind <- sample(n,m)
  while( sum(labels[ind]) == 0 | sum(labels[ind]) == m ) {
    ind <- sample(n,m)
  }
  return(ind)
}

iterate <- function(labels,...) {
  
  ind <- subsample(labels)
  
  n <- length(labels)
  train <- ind
  labs <- labels[ind]
  test <- (1:n)[-ind]
  
  newlabels <- gcm(train,test,labs,...)
  labels[test] <- newlabels
  return(labels)
  
}

baselabel <- function(n) { 
  labels <- round(runif(n))
  while(sum(labels)==0 | sum(labels)==n) {
    labels <- round(runif(n))
  }
  return(labels)
}


### Run the simulations

# run simulation
if(dosim) {
  npar <- length(lmb)
  L <- array(NA,dim=c(its+1,n,nruns))
  LMB <- matrix(NA,its+1,nruns)
  for( r in 1:nruns ) {
    if(r %% 10000 == 0) cat(".")
    L[1,,r] <- baselabel(n)
    for( i in 1:its ) {
      LMB[i+1,r] <- sample(npar,1)
      L[i+1,,r] <- iterate(L[i,,r],lmb=lmb[LMB[i+1,r]])
    }  
  }
}

# either save the file or load from elsewhere
if(dosave){
  save(L,LMB,lmb,file="./sim3_gcmmixed.Rdata")
} else {
  load("./sim3_gcmmixed.Rdata")
}


### Draw plots

plotFun <- function(L,FUN,ylim,yshift) {  
  
  plot.new()
  plot.window(xlim=c(0,its), ylim=ylim)
  axis(1)
  axis(2)
  box()
  
  pch <- c(21,21,21)
  bg <- c("black","grey50","white")
  av <- rep.int(0,length(lmb))
  
  S <- matrix()
  
  for( l in 1:length(lmb)) {
    S <- rep.int(NA,its+1)
    S[1] <- mean(apply(L[1,,],2,FUN))
    for( i in 1:its ) {
      ind <- LMB[i+1,] == l 
      S[i+1] <- mean(apply(L[i+1,,ind],2,FUN))
    }  
    lines(0:its,S,pch=pch[l],type="o",bg=bg[l])
    av[l] <- S[length(S)]
    
    str <- paste0("lambda == ",lmb[l])
    text(13,S[13]+yshift[l],parse(text=str))
  }
  
  abline(h=mean(av),lwd=2,lty=2,col="grey50")
  
}


plotFun(L,score,ylim=c(3,6),yshift=c(1,1,1)*-.13)
title(xlab="Iterations",
      ylab="Adjacent Items in Same Category",
      main="Heterogenous Priors")

plotFun(L,size,ylim=c(2.2,3.0),yshift=c(-1,-1,1)*.025)
title(xlab="Iterations",
      ylab="Size of Smaller Category",
      main="Heterogeneous Priors")

