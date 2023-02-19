#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2023/blob/main/datasets/climateSupport.RData?raw=true"))#loading data

mod <- glm(choice ~ ., # period functions as omnibus selector (kitchen sink additive model)
           data = climateSupport, 
           family = "binomial")

summary(mod)


nullMod <- glm(choice ~ 1, # 1 = fit an intercept only (i.e. sort of a "mean") 
               data = climateSupport, 
               family = "binomial")

#  Run an anova test on the model compared to the null model 
anova(nullMod, mod, test = "Chisq")


#problem 2 c)
mod2 <- glm(choice ~ countries+sanctions, # additive model
           data = climateSupport, 
           family = "binomial")

mod3 <- glm(choice ~ countries*sanctions, # interactive model
           data = climateSupport, 
           family = "binomial")

anova(mod2, mod3, test = "Chisq") #comparing additive and interactive model
