# core model for the last simulation batch
#    x1... xn ~ Normal(d, 1)    - n normally distributed observations
#    d|g ~ Normal(mu_g, sig_g)  - prior over the mean for each group
#    g ~ Bernoulli(p)           - groups with different proportions

# model is a list of functions
mod <- list()

# posterior distribution over the mean
mod$beliefUpdate <- function(observations, parameters, knownsd=1) {
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
mod$productionStage <- function(n, parameters, knownsd=1) {
  
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
mod$chain <- function(param) {
  
  # initialise by picking a random population member and sampling
  # from their prior
  g <- ifelse( runif(1) < param$p, 1, 2)
  pars <- c(param$mu[g], param$sig[g])
  state <- list(
    observations = mod$productionStage(param$n, pars)
  )
  
  chain <- matrix(NA, param$its+1, 3)
  chain[1,] <- c(pars,g)
  
  for( i in 1:param$its) {
    
    # person comes from a random group
    g <- ifelse(runif(1)<param$p, 1, 2)
    
    # read their priors 
    state$parameters <- c(param$mu[g], param$sig[g])
    
    # iterate the chain
    state <- mod$iterate(state)
    
    # store the results
    chain[i+1,] <- c(state$parameters, g)
    
  }
  
  return(chain)
  
}
