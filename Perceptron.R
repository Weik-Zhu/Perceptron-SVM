# question1 write the function: classify(s,z), return label y

classify<- function(S,z){
  n<- nrow(S)
  y<-c()
  for (i in 1:n){
    x<- S[i,]
    if (x%*%z<0){
      y[i]= -1
      }
      else{
        y[i]=1
      }
  }
  return (y)
}

# question2 write the function: perceptrain(S,y), return z and Z_history
perceptrain<-function(S,y){
  n <- nrow(S)
  m <- ncol(S)
  z <- rnorm(m)
  Z_history<- z
  for (k in 1:100) {
    Cp <- 0
    dCp <- 0
    for (i in 1:n) {
      x <- S[i, ]
      temp<-(z %*% x)
      if (temp>0){
        label<-1
      }else{
        label<-(-1)
      }
      if (label != y[i]) {
        Cp <- Cp + abs(temp)
        dCp <- dCp + (-y[i]) * x
      }
    }
    if (Cp == 0) {
        print(3)
      rownames(Z_history)<-c(1:k)
      return(list(z=z, Z_history=Z_history))
      break
    }
    z <- z - (1/k) * dCp
    Z_history<-rbind(Z_history,z)
  }
}

#question3

#generate 3D vectorz
z<- c(1:3)
#run fakedata to get training data
fakedata<-fakedata(z,100)
S<-fakedata$S
y<-fakedata$y
#run perceptrain
per<-perceptrain(S,y)
z1<-perceptrain$z
#re-run fakedata with same z to get test data
fakedata2<-fakedata(z,100)
S<-fakedata$S
y<-fakedata$y
#check it
y1<-classify(S,z1)
#calculate the corrected rate
n<-length(y)
count<-0
for (i in 1:n){
  if (y1[i]==y[i]){
    count = count +1
  }
}
rate <- count/n
# first plot
plot(S,col=(y+3))
k<- -z1[1]/z1[2]
b<- -z1[3]/z1[2]
abline(b,k)
# second plot
plot(S,col=(y+3))
x<- Z_history[,1]
Y<- Z_history[,2]
c<- Z_history[,3]
plot(S,col=(y+3))
for (i in 1:6){
   abline(b[i],k[i])
}
# my Z_history matrix:
> Z_history
[,1]       [,2]      [,3]
[1,]  0.3365569  0.8864816  1.833614
[2,] 12.3003394 10.7040533 -7.166386
[3,]  5.1432314  8.4507385  8.333614
[4,]  3.4720853  6.6966396 10.333614
[5,]  3.5516780  7.0383596 10.083614
[6,]  3.3643554  6.8415381 10.283614