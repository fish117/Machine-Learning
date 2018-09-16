suppressPackageStartupMessages(library(tree))
suppressPackageStartupMessages(library(tidyverse))
fit <- tree(Sepal.Width ~ ., data=iris)
summary(fit)

plot(fit)  # Plot the tree, without labels
text(fit)  # Add labels to the tree

predict(fit, newdata=iris) %>% 
  head
fitfull <- tree(Sepal.Width ~ ., data=iris, 
                control=tree.control(nrow(iris), 
                                     mindev=0, minsize=2))# minimal decrease in error and minimal size in every node

mean((predict(fitfull) - iris$Sepal.Width)^2) # MSE

set.seed(4)
fitfull_cv <- cv.tree(fitfull)
plot(fitfull_cv$size, log(fitfull_cv$dev))# dev is the error and size is the size of the sub tree.

# this error is cross validation error.10 fold
fit_pruned <- try(prune.tree(fitfull, best=10))#from the picture above, the error is minimal when it =10
plot(fit_pruned)
text(fit_pruned)
