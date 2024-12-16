install.packages("devtools")
library(devtools)

install.packages("sp")

install.packages('./libs/rgeos_0.6-4.tar.gz', type="source", repos=NULL)
install.packages('./libs/rgdal_1.6-7.tar.gz', type="source", repos=NULL)

install.packages("remotes")
install.packages("git2r")

Sys.setenv(GITHUB_PAT="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")
install_github('johnbaums/rmaxent')

library(sp)
library(rmaxent)
library(rgdal)

remotes::install_version("biomod2", version = "4.1-2", repos = "https://cran.r-project.org")

library(biomod2)

remotes::install_git(
  "https://gitlab.com/ecocommons-australia/ecocommons-platform/ecocommons",
  upgrade = "never") 
library(ecocommons)

install.packages('jpeg')
install.packages('maps')
install.packages('ncdf4')
install.packages('rasterVis')
install.packages('readxl')
install.packages('rgbif')
install.packages('rJava')
install.packages('svMisc')
