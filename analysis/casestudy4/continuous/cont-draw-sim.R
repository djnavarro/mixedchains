
# import the model functions
source("./continuous-model-b.R")

# parameters
param <- list(
  n = 1,
  its = 50,
  nchains = 10000,
  newDraw = function(w = 1) { 
    mu <- param$mean(w)
    sig <- param$strength(mu) 
    return(c(mu,sig))
  },
  mean = function(w) {
    (w * rnorm(1,mean = 0, sd = 1) + 
      (1-w) * (rexp(n = 1, rate = 1) - 1))*2
  },
  strength = function(bias) {
    1/(abs(bias) + 1)
  }
)

# set up
W <- c(1,1,1,.1,.1,.1)
S <- c(1,2,3,1,2,3)

# strength functions
strengths <- list(
  function(bias) {
    1/(abs(bias) + 1)
  },
  function(bias) {
    rep.int(.3,length(bias))
  },
  function(bias) {
    x <- .1 + 1/((10-abs(2*bias))+4)
    x[x < .1] <- .1
    return(x)
  }
)

npar <- length(W) # not actually varying anything!

for( s in 1:npar ) { # different parameters
  
  chains <- list()
  param$strength <- strengths[[S[s]]]
  
  cat(".")
  chains[[1]] <- array(NA, dim = c(param$its + 1, 4, param$nchains))
  
  for( c in 1:param$nchains ) { # many chains
    chains[[1]][,,c] <- mod$chain(param, w=W[s])
  } 
  
  save(mod,param,chains,file=paste0("cont-draw-data-",s,".Rdata"))
  
}

source("cont-draw-plot.R")
