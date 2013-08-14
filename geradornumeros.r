#Middle-square method
storageArray<-vector()
x<-987678

for(i in 1:10){
    newseed = (x / 1000) %% 1000000
    storageArray[i] = newseed
    x = newseed * newseed
    }

storageArray

#Linear congruential generator
m<-100
a<-2
c<-3

x<-vector()
x[1]<-1

for(i in 1:9) {
    x[i+1]<-(a*x[i]+c) %% m
}

x

for(i in 1:99) {
    x[i+1]<-(a*x[i]+c) %% m
}

plot(density(x),frame=F)
acf(ts(x))
x

#Modulo Maior
m<-1000
a<-17
c<-11

x<-vector()
x[1]<-2

for(i in 1:999) {
    x[i+1]<-(a*x[i]+c) %% m
}

plot(density(x),frame=F)
acf(ts(x))
x

#Mersenne Twister
set.seed(1)
x<-runif(100000)

plot(density(x))
acf(x)

?RNG()
