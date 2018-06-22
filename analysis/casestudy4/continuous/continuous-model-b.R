# the core model is identical except we're
# assuming very litle response variability

# model is a list of functions
mod <- list()

# posterior distribution over the mean
mod$beliefUpdate <- function(observations, parameters, knownsd=.1) {
  s <- sum(observations)
  n <- length(observations)
  
  mu0 <- parameters[1]
  var0 <- parameters[2]^2
  knownvar <- knownsd^2
  
  var1 <- 1/(1/var0 + n/knownvar)
  mu1 <- var1 * (mu0 / var0  + s/knownvar)
  return(c(mu1,sqrt(var1)))
}

# sample a hypothesis, then obs from hypothesis
mod$productionStage <- function(n, parameters, knownsd=.1) {
  
  mu <- rnorm(1, mean = parameters[1], sd = parameters[2])
  observations <- rnorm(n, mu, knownsd)
  return(observations)
  
}

# an iteration 
mod$iterate <- function(state) {
  
  n <- length(state$observations)
  pars <- mod$beliefUpdate(state$observations, state$parameters)
  obs <- mod$productionStage(n, pars)
  return(list(observations=obs, parameters=pars))
  
}

# a chain
mod$chain <- function(param, ...) {
  
  # initialise by picking a random population member and sampling
  # from their prior
  pars <- param$newDraw(...)
  state <- list(
    observations = mod$productionStage(param$n, pars)
  )
  
  # initialise the chain: 
  #   1 = posterior mean, 2 = posterio sd
  #   3 = prior mean, 4 = prior sd
  chain <- matrix(NA, param$its+1, 4)
  chain[1,] <- c(pars,pars)
  
  for( i in 1:param$its) {
    
    
    # new person priors from population disr
    pars <- param$newDraw(...)
    state$parameters <- pars
    
    # iterate the chain
    state <- mod$iterate(state)
    
    # store the results (posterior, prior)
    chain[i+1,] <- c(state$parameters, pars)
    
  }
  
  return(chain)
  
}
