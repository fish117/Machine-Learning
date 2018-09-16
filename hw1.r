Titanic_test<-read.csv(file.choose())
Titanic_train<-read.csv(file.choose())


# log transformation
Titanic_test$log.Fare<-log(Titanic_test$Fare)
Titanic_train$log.Fare<-log(Titanic_train$Fare)
Titanic_test<-na.omit(Titanic_test)
Titanic_train<-na.omit(Titanic_train)
Titanic_test$log.Fare[Titanic_test$Fare== 0] <- 0
Titanic_train$log.Fare[Titanic_train$Fare== 0] <- 0

mean_test_age<-mean(Titanic_test$Age)
mean_test_log.fare<-mean(Titanic_test$log.Fare)

mean_train_age<-mean(Titanic_train$Age)
mean_train_log.fare<-mean(Titanic_train$log.Fare)

sd_test_age<-sd(Titanic_test$Age)
sd_test_log.fare<-sd(Titanic_test$log.Fare)

sd_train_age<-sd(Titanic_train$Age)
sd_train_log.fare<-sd(Titanic_train$log.Fare)


#3a:scaling
Titanic_test$Age.1<-(Titanic_test$Age-mean_test_age)/sd_test_age
Titanic_test$log.fare.1<-(Titanic_test$Fare-mean_test_log.fare)/sd_test_log.fare

Titanic_train$Age.1<-(Titanic_train$Age-mean_train_age)/sd_train_age
Titanic_train$log.fare.1<-(Titanic_train$Fare-mean_train_log.fare)/sd_train_log.fare



#3b regression for train set
# split into female and male
Titanic_female=subset(Titanic_train,Sex=="female")
Titanic_male=subset(Titanic_train,Sex=="male")
# split into train and test
require(caTools)
set.seed(123)   #  set seed to ensure you always have same random numbers generated
sample = sample.split(Titanic_female,SplitRatio = 0.75) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
Titanic_female_train=subset(Titanic_female,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
Titanic_female_test=subset(Titanic_female, sample==FALSE)

sample = sample.split(Titanic_male,SplitRatio = 0.75) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
Titanic_male_train=subset(Titanic_male,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
Titanic_male_test=subset(Titanic_male, sample==FALSE)



# span for female
MSE_train_female<-list()
MSE_test_female<-list()

for (j in 1:50){
female_j=loess(Survived~Age.1+log.fare.1,data=Titanic_female_train,span=j/50)
residual_female<-resid(female_j)
MSE_train_female[j]<-mean(sum(as.numeric(residual_female)^2))
pd_test_female_j<-predict(female_j,Titanic_female_train)
MSE_test_female[j]<-mean(sum((pd_test_female_j-Titanic_female_test$Survived)^2))
}



#validation set approach
which.min(MSE_test_female)
a<-seq(1,50,1)





a<-seq(1,50,1)/50
min(MSE_train_female)
max(MSE_train_female)
plot(a,MSE_train_female,ylim=c(0,100))
plot(a,MSE_test_female,ylim=c(0,100))
plot(a,as.numeric(MSE_test_female)+as.numeric(MSE_train_female),ylim=c(0,200))
which.min(as.numeric(MSE_test_female)+as.numeric(MSE_train_female))

plot(1,type='n',xlim=c(0,1),ylim=c(0,100),xlab='bandwidth', ylab='MSE')
lines(a,MSE_train_female, type="o", col="blue", lwd=3)
lines(a, MSE_test_female, type="o", col="red", lwd=3)


# span for male
# span
MSE_train_male<-list()
MSE_test_male<-list()

for (j in 1:50){
  male_j=loess(Survived~Age.1+log.fare.1,data=Titanic_male_train,span=j/50)
  pd_test_male<-predict(male_j,Titanic_male_test)
  MSE_test_male[j]<-mean(sum((pd_test_male-Titanic_male_train$Survived)^2))
  residual_male<-resid(loess(Survived~Age.1+log.fare.1,data=Titanic_male_train,span=j/50))
 
  MSE_train_male[j]<-mean(sum(as.numeric(residual_male)^2))
  }



which.min(MSE_test_male)
a<-seq(1,50,1)
min(MSE_train_female)
max(MSE_train_female)
plot(a,MSE_train_male,ylim=c(0,100))
plot(a,MSE_test_male,ylim=c(0,100))



plot(1,type='n',xlim=c(0,1),ylim=c(0,100),xlab='bandwidth', ylab='MSE')
lines(a,MSE_train_male, type="o", col="blue", lwd=3)
lines(a, MSE_test_male, type="o", col="red", lwd=3)
#3C classification

female_0.2=loess(Survived~Age.1+log.fare.1,data=Titanic_female_train,span=0.2)
pd_train_female_0.2<-female_0.2$fitted
pd_test_female_0.2<-predict(female_0.2,Titanic_female_test)

## now use 0.5 as the cut off for predicting live or die
pred__train_binary_female <- as.numeric(pd_train_female_0.2>0.5)
pred__test_binary_female <- as.numeric(pd_test_female_0.2>0.5)
## error rate
Titanic_female_train$isEqual <- ifelse(pred__train_binary_female  ==Titanic_female_train$Survived,1,0)
error_rate_train_female_0.2<-1-(sum(Titanic_female_train$isEqual)/nrow(Titanic_female_train))
Titanic_female_test$isEqual <- ifelse(pred__test_binary_female  ==Titanic_female_test$Survived,1,0)
error_rate_test_female_0.2<-1-(sum(Titanic_female_test$isEqual)/nrow(Titanic_female_test))



error_rate_train_female<-list()
error_rate_test_female<-list()

for (j in 1:50){
  female_j=loess(Survived~Age.1+log.fare.1,data=Titanic_female_train,span=j/50,control=loess.control(surface = "direct"))
  pd_train_female_j<-female_j$fitted
  pd_test_female_j<-predict(female_j,Titanic_female_test)
  
  ## now use 0.5 as the cut off for predicting live or die
  pred__train_binary_female <- as.numeric(pd_train_female_j>0.5)
  pred__test_binary_female <- as.numeric(pd_test_female_j>0.5)
  ## error rate
  Titanic_female_train$isEqual <- ifelse(pred__train_binary_female ==Titanic_female_train$Survived,1,0)
  error_rate_train_female[j]<-1-(sum(Titanic_female_train$isEqual)/nrow(Titanic_female_train))
  Titanic_female_test$isEqual <- ifelse(pred__test_binary_female ==Titanic_female_test$Survived,1,0)
  error_rate_test_female[j]<-1-(sum(Titanic_female_test$isEqual)/nrow(Titanic_female_test))
}



which.min(error_rate_test_female)
a<-seq(1,50,1)/50

plot(a,error_rate_train_female,ylim=c(0.1,0.6))
plot(a,error_rate_test_female,ylim=c(0.1,0.6))

plot(1,type='n',xlim=c(0,1),ylim=c(0,1),xlab='bandwidth', ylab='MSE')
lines(a,error_rate_train_female, type="o", col="blue", lwd=3)
lines(a, error_rate_test_female, type="o", col="red", lwd=3)



# for male
error_rate_train_male<-list()
error_rate_test_male<-list()




for (j in 1:50){
  male_j=loess(Survived~Age.1+log.fare.1,data=Titanic_male_train,span=j/50,control=loess.control(surface = "direct"))
  pd_train_male_j<-male_j$fitted
  pd_test_male_j<-predict(male_j,Titanic_male_test)
 
  ## now use 0.5 as the cut off for predicting live or die
  pred__train_binary_male <- as.numeric(pd_train_male_j>0.5)
  pred__test_binary_male<- as.numeric(pd_test_male_j>0.5)
  ## error rate
  
  Titanic_male_train$isEqual <- ifelse(pred__train_binary_male  ==Titanic_male_train$Survived,1,0)
  error_rate_train_male[j]<-1-(sum(Titanic_male_train$isEqual)/nrow(Titanic_male_train))
  Titanic_male_test$isEqual <- ifelse(pred__test_binary_male  ==Titanic_male_test$Survived,1,0)
  error_rate_test_male[j]<-1-(sum(Titanic_male_test$isEqual)/nrow(Titanic_male_test))
}

which.min(error_rate_train_male)

a<-seq(1,50,1)/50


plot(a,error_rate_train_male)
plot(a,error_rate_test_male)

plot(1,type='n',xlim=c(0,1),ylim=c(0,1),xlab='bandwidth', ylab='MSE')
lines(a,error_rate_train_male, type="o", col="blue", lwd=3)
lines(a, error_rate_test_male, type="o", col="red", lwd=3)





#3d-KNN

library(kknn)  
library(sampling)




library(class)
# for female

Titanic_female_train_subset<-Titanic_female_train[,c(2,14,15)]
Titanic_female_test_subset<-Titanic_female_test[,c(2,14,15)]
test.pre_female<- knn(Titanic_female_train_subset,Titanic_female_test_subset,Titanic_female_train_subset$Survived,k=7)
train.pre_female<- knn(Titanic_female_train_subset,Titanic_female_train_subset,Titanic_female_train_subset$Survived,k=7)
## now use 0.5 as the cut off for predicting win or loss
test.pre_female_binary <- as.numeric(as.numeric(test.pre_female)>1)
train.pre_female_binary <- as.numeric(as.numeric(train.pre_female)>1)
## error rate
Titanic_female_train_subset$isEqual <- ifelse(train.pre_female_binary ==Titanic_female_train_subset$Survived,1,0)
error_rate_train_female_7<-1-((sum(Titanic_female_train_subset$isEqual)/nrow(Titanic_female_train_subset)))
Titanic_female_test_subset$isEqual <- ifelse(test.pre_female_binary   ==Titanic_female_test_subset$Survived,1,0)
error_rate_test_female_7<-1-((sum(Titanic_female_test_subset$isEqual)/nrow(Titanic_female_test_subset)))



error_rate_train_female_knn<-list()
error_rate_test_female_knn<-list()

for (j in 1:50){
  test.pre_female_j<- knn(Titanic_female_train_subset,Titanic_female_test_subset,Titanic_female_train_subset$Survived,k=j)
  train.pre_female_j<- knn(Titanic_female_train_subset,Titanic_female_train_subset,Titanic_female_train_subset$Survived,k=j)
  ## now use 1 as the cut off for predicting win or loss
  test.pre_female_binary <- as.numeric(as.numeric(test.pre_female_j)>1)
  train.pre_female_binary <- as.numeric(as.numeric(train.pre_female_j)>1)
  ## error rate
  Titanic_female_train_subset$isEqual <- ifelse(train.pre_female_binary ==Titanic_female_train_subset$Survived,1,0)
  error_rate_train_female_knn[j]<-1-((sum(Titanic_female_train_subset$isEqual)/nrow(Titanic_female_train_subset)))
  Titanic_female_test_subset$isEqual <- ifelse(test.pre_female_binary   ==Titanic_female_test_subset$Survived,1,0)
  error_rate_test_female_knn[j]<-1-((sum(Titanic_female_test_subset$isEqual)/nrow(Titanic_female_test_subset)))
}


which.min(error_rate_train_female_knn)

a<-seq(1,50,1)
plot(a,error_rate_train_female_knn)
plot(a,error_rate_test_female_knn)
plot(1,type='n',xlim=c(0,50),ylim=c(0,1),xlab='k', ylab='error rate')
lines(a,error_rate_train_female_knn, type="o", col="blue", lwd=3)
lines(a, error_rate_test_female_knn, type="o", col="red", lwd=3)


#for male
Titanic_male_train_subset<-Titanic_male_train[,c(2,14,15)]
Titanic_male_test_subset<-Titanic_male_test[,c(2,14,15)]


error_rate_train_male_knn<-list()
error_rate_test_male_knn<-list()

for (j in 1:50){
  test.pre_male_j<- knn(Titanic_male_train_subset,Titanic_male_test_subset,Titanic_male_train_subset$Survived,k=j)
  train.pre_male_j<- knn(Titanic_male_train_subset,Titanic_male_train_subset,Titanic_male_train_subset$Survived,k=j)
  ## now use 1 as the cut off for predicting win or loss
  test.pre_male_binary <- as.numeric(as.numeric(test.pre_male_j)>1)
  train.pre_male_binary <- as.numeric(as.numeric(train.pre_male_j)>1)
  ## error rate
  Titanic_male_train_subset$isEqual <- ifelse(train.pre_male_binary ==Titanic_male_train_subset$Survived,1,0)
  error_rate_train_male_knn[j]<-1-((sum(Titanic_male_train_subset$isEqual)/nrow(Titanic_male_train_subset)))
  Titanic_male_test_subset$isEqual <- ifelse(test.pre_male_binary   ==Titanic_male_test_subset$Survived,1,0)
  error_rate_test_male_knn[j]<-1-((sum(Titanic_male_test_subset$isEqual)/nrow(Titanic_male_test_subset)))
}

which.min(error_rate_train_male_knn)

a<-seq(1,50,1)
plot(a,error_rate_train_male_knn)
plot(a,error_rate_test_male_knn)
plot(1,type='n',xlim=c(0,50),ylim=c(0,1),xlab='k', ylab='error rate')
lines(a,error_rate_train_male_knn, type="o", col="blue", lwd=3)
lines(a, error_rate_test_male_knn, type="o", col="red", lwd=3)
