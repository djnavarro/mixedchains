layout(matrix(1:4,2,2))
source("./bias-plot.R")
source("./proportion-plot.R")
source("./separation-plot.R")
source("./bottleneck-plot.R")
layout(1)
dev.print(pdf, file="two-group-sim.pdf",
          width=7, height=7)
