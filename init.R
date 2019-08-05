setwd("/app/")
if (!requireNamespace("remotes"))
  install.packages("remotes")

remotes::install_github("rstudio/renv")

source("/app/renv/activate.R")
