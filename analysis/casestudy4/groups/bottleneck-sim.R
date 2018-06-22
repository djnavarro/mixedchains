

# import the model functions
source("./two-group-model.R")

# parameters held fixed
param <- list(
  p = .5,
  n = NA,
  mu = c(0,1),
  sig = c(1,1/3),
  its = 50,
  nchains = 5000
)

# parameter to be varied
N <- 1:10

# set up
npar <- length(N)
chains <- list()
groupMeans <- matrix(NA,2,npar)

# run
for( s in 1:npar ) {
  
  cat(".")
  chains[[s]] <- array(NA, dim = c(param$its + 1, 3, param$nchains))
  
  param$n <- N[s] # set the relevant parameter
  
  # many chains
  for( c in 1:param$nchains ) {
    chains[[s]][,,c] <- mod$chain(param)
  } 
  
  # summary statistics
  all <- apply(chains[[s]],1:2,mean)
  gp1 <- gp2 <- matrix(0,param$its+1,2)
  for(i in 1:(param$its+1)) {
    for(j in 1:2) {
      gp1[i,j] <- mean(chains[[s]][i,j,chains[[s]][i,3,]==1])
      gp2[i,j] <- mean(chains[[s]][i,j,chains[[s]][i,3,]==2])
    }
  }
  groupMeans[1,s] <- mean(gp1[(param$its-10):param$its,1])
  groupMeans[2,s] <- mean(gp2[(param$its-10):param$its,1])
  
}

# save
save(mod,param,N,chains,groupMeans,file="bottleneck-data.Rdata")
