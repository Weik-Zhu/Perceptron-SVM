install.packages("e1071")
.packages(all.available = TRUE)

library(e1071)


ups <- read.table('uspsdata.txt',header = FALSE,col.names = c(1:256))
y <- read.table('uspscl.txt',header = FALSE,col.names = "lable")

#set training data and test data
random<-sample(1:200,40)
testdat<-ups[random,]
testy<-y[random,]
traindat<-ups[-random,]
trainy<-y[-random,]

#linear model

tune.linear<-tune(svm, traindat,trainy, kernel='linear', ranges = 
                    list(cost=c( 0.0001,0.001,0.01,0.1,0.5,1,2,3)),scale = T)
performance<-tune.linear$performances
error<- performance[,2]
cost<- performance[,1]
#graph it
plot(cost,error)
#get the best parameter
min_error<- min(error)
#using test data
svm_linear <- svm(traindat,trainy,kernel='linear',cost=min_error, scale=T)
pre<-predict(svm_linear,testdat)
#calculate the misclassify rate
count<-0
for (i in 1:40){
  if(pre[i]*testy[i]>0){
    count<-count +1}
}
misclassification_rate<-(40-count)/40

#non-linear model

#fixed cost, train gamma
tune.radial<-tune(svm,traindat,trainy, kernel='radial', ranges = 
                 list(cost=c(100),gamma=c(0.0001,0.001,0.002,0.003,0.006,0.01,0.02,0.04,0.05)),scale = T)
performance_radial<-tune.radial$performances
error_radial<- performance_radial[,3]
gamma<- performance_radial[,2]
#graph it
plot(gamma,error_radial)
#we find that gamma=0.003 is the best

#fixed gamma train cost
tune.radial<-tune(svm,traindat,trainy, kernel='radial', ranges = 
                    list(cost=c(0.01,0.1,0.5,0.8,1,1.1,5,10,40,60,80,100),gamma=c(0.003)),scale = T)
performance_radial<-tune.radial$performances
error_radial<- performance_radial[,3]
cost<- performance_radial[,1]
performance_radial
#graph it
plot(cost,error_radial)
# we find that cost=1 is the best

#using test data,with cost=1,gamma=0.003
svm_radial <- svm(traindat,trainy,kernel='radial',cost=1,gamma = 0.003, scale=T)
pre1<-predict(svm_radial,testdat)
#calculate the misclassify rate
count1<-0
for (i in 1:40){
  if(pre1[i]*testy[i]>0){
    count1<-count1 +1}
}
misclassification_rate1<-(40-count1)/40

