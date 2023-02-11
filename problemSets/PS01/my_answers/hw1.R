library(tidyverse)
library(ggplot2)

?ecdf
###########
#question 1
###########
set.seed(123) #setting seed
data <- unlist(rcauchy(1000, location = 0, scale = 1)) #initilising data
ECDF <- ecdf(data) #getting the empirical distiribution function
empiricalCDF <-ECDF(data)
D <- max(abs(empiricalCDF - pnorm(data))) #getting the D statistic

a = 0 #initialising "a" which counts the sum
x <- D #setting x in the formula to our D statistic
for (k in 1:10000){ #for loop is the sum in formula, using 10,000 as a stand in for inf
  b = exp(-(((2*k)-1)^2)*(pi^2)/(8*(x^2))) #the part getting summed over
  a = a+b #increase a for the value
  }
c = (sqrt(2*pi)/x)*a #multiply by not summed part
c

print(ks.test(empiricalCDF, pnorm(data)))
######################
#Question 2
##################

set.seed(123) #initialisng seed again
data <- data.frame(x = runif(200,1,10)) #generating x column
data$y <- 0 +2.75*data$x + rnorm(200,0,1.5) #generating y column
print(summary(lm(data$y~data$x))) #getting the a and B with the lm function
ggplot(data, aes(x,y))+ #graphing y against x 
  geom_point() #even if i didnt have the results from lm. I could use the graph
ggsave("hw1_graph.png", width = 5, height = 4, units = 'in', dpi = 300)#to guess initail parameters



ols_reg_func <- function(theta, y, x){ #creating the function to be optomised
  a <- theta[1] #it is a linear regression with one independent variable
  B <- theta[2] #thus it is only a function of alpha and beta
  sigma <- theta[3] #the standard deviation of errors is needed for optomisation too
  
  Res = y-a-B*x #the classic linear regression formula for the residuals
  -sum(dnorm(Res,mean = 0, sigma, log = TRUE)) #and the quasi random distribution of residuals
}

a_guess <- 0.1 #from the graph I know that both alpha and beta are low
B_guess <- 0.1 #this is just setting initaial terms to optomise from
sigma_guess <- 0.1 #I think I need to be careful to keep these under the actual values of the parameters
#otherwise the newton raphson will look for a local miinimum many sd away


#putting the function, data and initial guesses into optim() function
optim_results <- optim(fn = ols_reg_func,par = c(a = a_guess, B = B_guess, sigma = sigma_guess), 
                       y = data$y,x = data$x,method = "BFGS") #setting optim to use BFGS

print(optim_results$par)#printing results




#?glm


#ggplot(data, aes(x,y))+
 # geom_point()

#a <-  seq(from = 0,to = 5, by = (1-0)/1000)
#b <-  seq(from = 1.5, to = 2.5, by = (2.5-1.5)/1000)

#print(b)
#print(a)

#linear.lik <- function(theta, y, X){
 # n <- nrow(X)
  #k <- ncol(X)
  #beta <- theta[1:k]
  #sigma2 <- theta[k+1]^2
  #e <- 
#}
#lem <- lm(data$x~data$y, method = "BNFS")
#?ols
#METHOD = BNFS
#formula = 
#glm(formula = formula, family = "gaussian", data = data, method = "BNFS")

#install.packages("aplore3")
#library(aplore3)
#Define our liklihood function we like to optimise