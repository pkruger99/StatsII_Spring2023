#loading libraries
library(ggplot2)
library(tidyverse)
library("eha")
library("survival")

#this fit is literally taken from https://cran.r-project.org/web/packages/eha/eha.pdf pg 13 with 1 covariate changed
fit <- coxreg(Surv(enter, exit, event) ~ sex + m.age, data = child, coxph = TRUE)
summary(fit)

#using interactive model
interfit <- coxreg(Surv(enter, exit, event) ~ sex * m.age, data = child, coxph = TRUE)
summary(interfit)

#checking which model is better
drop1(fit, test = "Chisq")
drop1(interfit, test = "Chisq")

#plotting the results
plot(fit, main = "Cumulative probability of a child dying 0-15")
plot(survfit(fit),main = "Cumulative probability of a child surviving 0-15")


     