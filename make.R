# Run the whole workflow in batch mode.
# For guidance on how to set up drake projects, visit
# https://ropenscilabs.github.io/drake-manual/projects.html
source("R/packages.R")
source("R/functions.R")
source("R/extract.R")
source("R/load_graph.R")
source("R/match.R")
source("R/label.R")
source("R/export.R")
source("R/plan.R")

r_make()
loadd()
