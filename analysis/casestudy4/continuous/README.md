
- The core model assumes normally distributed observations with unknown mean and known standard deviation. The prior over the mean is normal, with parameters that depend on a continuous population distribution (specified as a function in the parameter list) 
- Structure of the code `continuous-model.R` defines a single model object that is a list of functions for belief revision and iterated learning chains
- The `continuous-sim.R` function varies the bottleneck and the skewness of the underlying population distribution
- Each `continuous-plot.R` loads the simulation data and draws the plot


