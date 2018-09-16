install.packages("scales")
install.packages("tidyverse") 


  
install.packages('tidyverse', dependencies=TRUE, type="source", repos="https://cloud.r-project.org")
install.packages("ISLR") 
install.packages("knitr")
library(tidyverse)
library(tibble)

library(dplyr)
#8.1
#Using both the values of X1 and X2

genreg <- function(n){
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  eps <- rnorm(n)
  y <- 5-x1+2*x2+eps
  tibble(x1=x1, x2=x2, y=y)# tibble means table frame
}
mydata1<-genreg(1000)
dat<-mutate(mydata1,yhat=5,yhat1=5-x1,yhat2=5+2*x2,yhat12=5-x1+2*x2)
mse<-mean((dat$yhat-dat$y)^2)
mse1<-mean((dat$yhat1-dat$y)^2)
mse2<-mean((dat$yhat2-dat$y)^2)
mse12<-mean((dat$yhat12-dat$y)^2)
#when we add the parameters, the error decrases.



#8.2  Oracle classification

# when x=2
gencla <- function(n) {
  x <- rnorm(n)
  pB <- 0.8/(1+exp(-x))
  y <- map_chr(pB, function(t) #maps this number into character
    sample(LETTERS[1:3], size=1, replace=TRUE,
           prob=c(0.2, t, 0.8-t)))
  tibble(x=x, y=y)
}
mydata1<-gencla(1000) 
pa<-0.2
pb<-0.8/(1+exp(-2))
pc<-1-pa-pb


dat2<-mutate(dat2,
             yhat<-sapply(dat2$x, function(x_)
               if(x<0,"c" else "B"))

# error rate
1-mean(dat2$yhat==dat2$y)
