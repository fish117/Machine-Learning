suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(knitr))
opts_chunk$set(fig.width=5, fig.height=3, fig.align="center",
               warning=FALSE)
my_accent <- "#d95f02"
rotate_y <- theme(axis.title.y=element_text(angle=0, vjust=0.5)
set.seed(87)
n <- 200
library(tidyverse)
library(dplyr)
dat <- tibble(x = c(rnorm(n/2), rnorm(n/2)+5)-3,
              y = sin(x^2/5)/x + rnorm(n)/10 + exp(1))
fit <- loess(y ~ x, data=dat, span=0.3)
yhat <- predict(fit)
mean((yhat - dat$y)^2)                  
