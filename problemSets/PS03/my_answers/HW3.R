library(tidyverse)
library(ggplot2)
library(nnet)
library(MASS)



gdp <- read.csv(file = "gdpChange.csv")

gdp[, 'GDPWdiff'] <- as.numeric(gdp[, 'GDPWdiff'])
gdp$GDPWdiff2 <- "NA"
print(gdp[,13])

for( i in 1:nrow(gdp)){
  if(gdp[i,10] < -500){ print(i)
    gdp[i,13] <- "1Strong Negative"}
  else if(gdp[i,10] > -500 & gdp[i,10] <= -100 ){ gdp[i,13] <- "2Negative"}
  else if(gdp[i,10] > -100 & gdp[i,10] <= 100 ){ gdp[i,13] <- "3Neutral"}
  else if(gdp[i,10] > 100 & gdp[i,10] <= 300 ){ gdp[i,13] <- "4Positive"}
  else if(gdp[i,10] > 300){ gdp[i,13] <- "5Strong Positive"}
}


gdp[, 'GDPWdiff2'] <- as.factor(gdp[, 'GDPWdiff2'])
gdp[, 'OIL'] <- as.factor(gdp[, 'OIL'])
gdp[, 'REG'] <- as.factor(gdp[, 'REG'])


multimom_model1 <- multinom(GDPWdiff2 ~ OIL + REG, data = gdp, family = multinomial)
summary(multimom_model1)

pol_model1 <- polr(GDPWdiff2 ~ OIL + REG, data = gdp)
summary(pol_model1)
coef(pol_model1)[[1]]



#Question 2
mex <- read.csv(file = "MexicoMuniData.csv")

pos_reg <- glm(PAN.visits.06 ~ competitive.district +marginality.06+PAN.governor.06, data = mex, family = poisson)

summary(pos_reg)





#memory.size()
#gc()
#utils::browseURL("https://localhost:4242/sessions")
#nnet.MaxNWts=10000
#u <- unique(gdp[, 'GDPWdiff'])
#gdp <- gdp[,-c(1,2,3,4,5,8,9,11,12)]
#print(sort(u))
#gdp <- filter(gdp, YEAR < 1960)
#unique(gdp$REG)
#, model = TRUE, size = c(10, 10), linout = TRUE, maxit = 1000)
#install.packages("renv")
#library(renv)
#renv::init()
#memory.limit(size = 8000)
#print(multimom_model1)
#, size = c(10, 10), linout = TRUE, maxit = 1000,,,