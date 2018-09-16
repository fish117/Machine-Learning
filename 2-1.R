#PART I
install.packages("tidyverse") 
install.packages("ISLR") 
install.packages("knitr")
library(tibble)
set.seed(87)
dat <- tibble(x = c(rnorm(100), rnorm(100)+5)-3,
              y = sin(x^2/5)/x + rnorm(200)/10 + exp(1))
library(tidyverse)
library(knitr)
kable(head(dat))
plot(y~x,data=dat)

y.0<-subset(dat,x==0)
dat$d<-abs(dat$x-0)
dat<-arrange(dat,d)

#ford d within 1
dat_loess <- filter(dat,d<1)
# mean of y when x=0
mean(dat_loess$y)


# for  first 10 d
k_data <- filter(dat[1:10,])
# mean of y when x=0
mean(k_data$y)

# 3
#if r is too small, we will get a smaller dataset, might have 0 observations.



#4
#trade off:overfitting, if r is too small, dataset is small. 
#our estimation will be high amount of variance, could be unbised.
# if r is larger, we have too much data, variance is small,bias will be high.




#PART II
library(tidyverse)
xgrid <- seq(-5, 4, length.out=1000)
kNN_estimates <- map_dbl(xgrid, function(x){
  dat$d<-abs(dat$x-x)
  dat<-arrange(dat,d)
  KNN_data<-dat[1:5,]
  mean(KNN_data$y)
  ## YOUR CODE HERE FOR kNN
  ## Note: The variable "x" here is a single value along the grid.
  ## Hint1: Extend your code for kNN from the previous exercise.
  ## Hint2: Say you store the prediction in the variable "yhat".
  ##         Then in a new line of code, write: return(yhat)
})
loess_estimates <- map_dbl(xgrid, function(x){
  dat$d<-abs(dat$x-x)
  dat<-arrange(dat,d)
  loess<-filter(dat,d<1)
  yhat<-mean(loess$y)
  ## YOUR CODE HERE FOR LOESS
  ## Note: The variable "x" here is a single value along the grid.
  ## Hint1: Extend your code for loess from the previous exercise.
  ## Hint2: Say you store the prediction in the variable "yhat".
  ##         Then in a new line of code, write: return(yhat)
})-
est <- tibble(x=xgrid, kNN=kNN_estimates, loess=loess_estimates) %>% 
  gather(key="method", value="estimate", kNN, loess)
ggplot() +
  geom_point(data=dat, mapping=aes(x,y)) +
  geom_line(data=est, 
            mapping=aes(x,estimate, group=method, colour=method)) +
  theme_bw()
