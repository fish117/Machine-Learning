x<-c(-19.8 ,-13.6, -12.0  ,-3.5,  -2.8,   4.4 ,  4.5,  15.1,  16.3,  20.2)
library(ggplot2)
library(tidyverse)
ggplot(data.frame(x=x), aes(x)) +
  geom_histogram(binwidth=10, center=min(x)+5,
                 fill=my_accent, colour="black") +
  theme_bw()
x <- rnorm(1000, sd=10)
qplot(x, binwidth=1)  # too small
qplot(x, binwidth=10)  # too big
qplot(x, binwidth=3.5)  # just right
qplot(x, geom="density", bw=2.5)
x<-c(1, 0, 0, 0 ,2 ,0 ,1 ,2, 3, 0)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(Lahman))
suppressPackageStartupMessages(library(knitr))
opts_chunk$set(warning=FALSE, fig.width=6, fig.height=3, fig.align="center")
my_accent <- "#d95f02"

dat <- Teams %>% tbl_df %>% 
  select(runs=R, hits=H)
kable(head(dat))


props <- tibble(Value=x) %>% 
  group_by(Value) %>% 
  summarize(Proportion=length(Value)/length(x))
kable(props)
qplot(x=Value, y=Proportion, data=props, geom="col")
set.seed(2)
x <- rpois(1000, lambda=1)
qplot(factor(x))# get the count instead of proportion
qplot(factor(x), mapping=aes(y=..prop..), group=1)# same as before


ggplot(dat, aes(hits, runs)) +
  geom_point(colour=my_accent, alpha=0.1) +
  geom_vline(xintercept=c(1000+c(-r,r), 1500+c(-r,r)),
             linetype="dashed") +
  theme_bw() +
  labs(x="Number of Hits (X)",
       y="Number of Runs (Y)")

fit_rq <- rq(runs ~ hits, data=dat, tau=c(0.25, 0.5, 0.75)))


# when x=500, there is 25% chance that it will be less than 150.

