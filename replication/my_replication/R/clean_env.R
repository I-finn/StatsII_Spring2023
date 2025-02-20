detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}

pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

clean_env <- function() {# clear environment
  rm(list=ls())
  
  detachAllPackages()

  # load general packages
  lapply(c("ggplot2", "stargazer", "tidyverse", "stringr", 
           "broom", "gridExtra", "patchwork", "crosstable","here"),
         pkgTest)
  
#  lapply(c("AER", "pscl", "nnet","MASS"), pkgTest)
#  lapply(c("foreign", "MASS","Hmisc", "reshape2", "nnet"),  pkgTest)
  
  # setup env/options
#  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

  options(comment="#>", continue=" ")
  theme_set(theme_minimal())

}

# wrapper for stargazer tables
output_stargazer <- function(outputFile, appendVal=TRUE, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=appendVal)}
