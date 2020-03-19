library(robustbase)
str(CrohnD)
data(CrohnD, package="robustbase")
CrohnD<-data.frame(CrohnD)
CrohnD$ID<-factor(CrohnD$ID,ordered = T)#EINAI H MONADIKH TIMH POU XREIAZETAI NA THN KANOUME KATHGORIKH AN KAI EXEI HDH DIATAKSH ALLA SAN TYPO NUMERIC
freq.tr<-table(CrohnD$treat)
barplot(freq.tr,main="treat",xlab="treat",ylab="frequency",horiz = F,cex.names =0.8,col=rainbow(length(freq.tr)))
freq.sex<-table(CrohnD$sex)
barplot(freq.sex,main="sex",xlab="sex",ylab="frequency",horiz = F,cex.names =0.8,col=rainbow(length(freq.sex)))
freq.co<-table(CrohnD$country)
barplot(freq.co,main="country",xlab="country",ylab="frequency",horiz = F,cex.names =0.8,col=rainbow(length(freq.co)))
pct <- round(freq.tr/sum(freq.tr)*100)
lbls <- c("placebo", "d1", "d2")
lbls <- paste(lbls, pct,"%") # add percents to labels 
lbls
pie(freq.tr,labels=lbls,main="treat",col=rainbow(length(lbls)))
x<- CrohnD$height
h<-hist(x, breaks=10,col="red",xlab="Height",main="Height") 
xfit<-seq(min(x),max(x),((max(x) - min(x))/(length(x) - 1)))
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)   
x<- CrohnD$weight
h<-hist(x, breaks=10,col="green",xlab="Weight",main="Weight") 
x<- CrohnD$age
h<-hist(x, breaks=10,col="yellow",xlab="age",main="age") 
x<- CrohnD$nrAdvE
h<-hist(x, breaks=10,col="blue",xlab="nrAdve",main="nrAdve") 
xfit<-seq(min(x),max(x),((max(x) - min(x))/(length(x) - 1)))
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="pink", lwd=2)   
x<- CrohnD$BMI
h<-hist(x, breaks=10,col="orange",xlab="BMI",main="BMI") 
summary(CrohnD)
ks.test(CrohnD$weight)
