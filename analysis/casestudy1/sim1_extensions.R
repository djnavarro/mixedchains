
mix <- .95 # proportions of each learner type in population

eps <- .00001 # tiny bias for 0.5 otherwise there is no prior mode!
a1 <- 1+eps  # beta prior for unbiased learner
b1 <- 1+eps

a2 <- 100 # beta prior for very strong bias learner
b2 <- 1

nresp <- 100 # responses per person
nits<-100 # how long to run the chain for

nruns <- 1000 # paper use 10000

###################

# function generating responses from a Bayesian learner with a beta prior
# with a probability matching rule (map=FALSE) or maximising rule (map=TRUE)
# ... note that MAP here means selecting the MAP estimate of theta, not
# sampling the most likely data set given theta. 
learner <- function(counts,a,b,nresp,map=FALSE) { 
  
  # for my sanity
  nheads <- counts[1]
  ntails <- counts[2]
  
  if(!map) {
    # sample from posterior
    theta <- rbeta(1,nheads+a,ntails+b)  
  } else {
    # choose map hypothesis
    theta <- (nheads+a-1)/(nheads+ntails+a+b-2)
  }
  
  # generate new responses by sampling from hypothesis
  newheads <- rbinom(n=1,size = nresp,prob = theta)
  return(c(newheads,nresp-newheads))
  
}

# function to run the iterated learning chain.
chain <- function(map=FALSE) {
  
  obs <- matrix(NA,nits+1,2)
  ltype <- vector(length=nits)
  obs[1,] <- c(nresp/2,nresp/2) # seed the chain at 5 heads, 5 tails
  for( i in 1:nits ) {
    
    ltype[i] <- as.numeric(runif(1)>mix)+1 # learner type
    if( ltype[i] == 1) {
      obs[i+1,] <- learner(obs[i,],a1,b1,nresp,map=map)
    } else {
      obs[i+1,] <- learner(obs[i,],a2,b2,nresp,map=map)
    }
    
  }
  return(data.frame(obs=obs[-1,1], ltype=ltype)) 
}


# initialise figure
layout(matrix(1:6,2,3,byrow = TRUE))
op <- par(no.readonly=TRUE)
par(mar=c(5,5,3,2))

# run the simulation with sampling vs MAP
for( map in c(FALSE,TRUE)) {
  
  
  #  run lots of chains to get average number of heads
  nheads <- vector(length=nits)
  iteratedoutcomes <- matrix(0,nresp+1,2,
                             dimnames=list("heads"=0:nresp,"learner"=1:2))
  nheads_all <- matrix(NA,nits,nruns)
  ltype_all <- matrix(NA,nits,nruns)
  for( i in 1:nruns ){
    if(i %% 1000 == 0 ) cat(".")
    ch <- chain(map = map)
    nheads <- nheads + ch$obs
    nheads_all[,i] <- ch$obs
    ltype_all[,i] <- ch$ltype
    iteratedoutcomes[ch$obs[nits]+1, ch$ltype[nits]] <- 
      iteratedoutcomes[ch$obs[nits]+1, ch$ltype[nits]] + 1
  }
  nheads <- nheads/nruns
  for(i in 1:2) iteratedoutcomes[,i] <- iteratedoutcomes[,i]/sum(iteratedoutcomes[,i])
  
  # mixture of the two priors?
  p1 <- a1/(a1+b1)
  p2 <- a2/(a2+b2)
  mixp <- mix*p1 + (1-mix)*p2
  priornh <- mixp*nresp

  # response distribution from the mixture of priors
  priorsamples <- vector(length=nruns)
  priorltype <- vector(length=nruns)
  for( i in 1:nruns) {
    
    ltype <- as.numeric(runif(1)>mix)+1 # learner type
    if( ltype == 1) {
      priorsamples[i] <- learner(c(0,0),a1,b1,nresp)[1]
    } else {
      priorsamples[i] <- learner(c(0,0),a2,b2,nresp)[1]
    }
    priorltype[i] <- ltype
  }
  
  # plot the trajectory of the mixed chain
  mstr <- "Mixed Chain (95% Unbiased)\n"
  if(map) { 
    mstr <- paste0(mstr,"MAP Hypothesis") 
  } else {
    mstr <- paste0(mstr,"Posterior Sampling") 
  }
  
  plot(c(50,nheads),type="o",pch=19,xlab="Iteration",ylab="Average Response",
       ylim=c(40,100),main=mstr)
  abline(h=priornh)
  
  legend(x = "bottomleft", pch=c(19,NA), col=c("black","black"),
         lty=c(1,1),lwd=c(1,1),
         legend=c("Iterated Learning","Pop. Average Prior"),bty="n")
  
  
  
  # for each learner type, plot them separately
  for(ltype in 1:2) {
    
    nh <- vector()
    for(i in 1:nits){
      nh[i] <- mean( nheads_all[i,ltype_all[i,]==ltype])
    }
    
    if(ltype==1) {
      mstr <- "Unbiased Learners\n"
    } else {
      mstr <- "Very Biased Learners\n"
    }
    if(map) { 
      mstr <- paste0(mstr,"MAP Hypothesis") 
    } else {
      mstr <- paste0(mstr,"Posterior Sampling") 
    }
    
    # plot the trajectory of the chain
    plot(c(50,nh),type="o",pch=19,xlab="Iteration",ylab="Average Response",
         ylim=c(40,100),main=mstr)
    abline(h=ifelse(ltype==1,p1,p2)*nresp)
    
    legend(x = "bottomleft", pch=c(19,NA), col=c("black","black"),
           lty=c(1,1),lwd=c(1,1),
           legend=c("Iterated Learning","Prior"),bty="n")
    
  
    
  }
  
}


layout(1)
par(op)

#dev.print(pdf,file="./coinsfail4.pdf",width=8,height=7)

