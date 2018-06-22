
- The core model assumes normally distributed observations with unknown mean and known standard deviation. The prior over the mean is normal, with parameters that depend on the group
- Structure of the code `two-group-model.R` defines a single model object that is a list of functions for belief revision and iterated learning chains
- Each `xxx-sim.R` function picks a single parameter to vary, and runs a simulation that shows how that parameter affects the chain
- Each `xxx-plot.R` loads the relevant simulation data and plots it
- The `plot-all.R` function calls all the plot functions and draws the overall picture


