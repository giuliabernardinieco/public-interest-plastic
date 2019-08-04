##Clearing the workspace and setting the working directory
rm(list=ls())
setwd("C:/Users/Giulia/Desktop/Imperial MSc/spring term/miniproject/google search")

##loading packages
require(lattice)
require(dplyr)
#~~~~~~~~~~~~~~~~~~~~~~~~~~PLASTIC POLLUTION~~~~~~~~~~~~~~~~~~~~~~~~~~#
#importing dataset
ppbm<-read.csv("ppbm.csv", header = T, as.is = T)
ppbm$date<-as.Date(ppbm$date)
str(ppbm)

#looking at the distribution of data
par(mfrow=c(2,2))
hist(ppbm$BMLove, breaks = 100, main = "")#right skewed
hist(ppbm$BMLife, breaks = 100, main = "")#right skewed

plot(BMLove~date,data=ppbm, type = "l", lty = 1, col="blue")
plot(BMLife~date,data=ppbm, type = "l", lty = 1, col="red")

#mean and meadian, range and variance
mean(ppbm$BMLove)
mean(ppbm$BMLife)

median(ppbm$BMLove)
median(ppbm$BMLife)

require(modeest)
mlv(ppbm$BMLove, method="shorth")
mlv(ppbm$BMLife, method="shorth")   

range(ppbm$BMLove)
range(ppbm$BMLife)

var(ppbm$BMLove)
var(ppbm$BMLife)

#I'm going to try and log the data for BMLove and BMLife to overcome the problem of not normal distribution
dev.off()
hist(log(ppbm$BMLove), breaks = 50, main = "")#slightly better
hist(log(ppbm$BMLife), breaks = 50, main = "")#slightly better

#shapiro test to test normality
shapiro.test(log(ppbm$BMLove))
shapiro.test(log(ppbm$BMLife))
#significantly different from a normal distribution

##HP: the interest for the word plastic pollution increased trought time
#we test that with a linear regression

#BMLove LM
lmpp<-lm(BMLove~date, data=ppbm)
summary(lmpp) 
#diagnostic plots
par(mfrow=c(2,2))
plot(lmpp) #Residuals vs Fitted,isn't the best
#plotting the model on the data
dev.off()
plot(log(BMLove)~date,data=ppbm, type = "l", lty = 1, col="blue")
abline(lmpp)

#BMLove LM
lmpp2<-lm(log(BMLife)~date, data=ppbm)
summary(lmpp2) 
#diagnostic plots
par(mfrow=c(2,2))
plot(lmpp2) #Residuals vs Fitted,isn't the best
#plotting the model on the data
dev.off()
plot(log(BMLife)~date,data=ppbm, type = "l", lty = 1, col="red")
abline(lmpp2)

#in both models, the rows 174,173,172 are outliers, and correspond to 
#April, May and June 2018. During June 2018 National Geographic published
#an issued dedicated entirely to plastic pollution.


#~~~~~~~~~~~~~~~~~~~~~~~~~~GARBAGE PATCH~~~~~~~~~~~~~~~~~~~~~~~~~~#
#importing dataset
gpbm<-read.csv("gpbm.csv", header = T, as.is = T)
gpbm$date<-as.Date(gpbm$date)
str(gpbm)

#looking at the distribution of data
par(mfrow=c(2,2))
hist(gpbm$BMLove, breaks = 100, main = "")#not normal distribution
hist(gpbm$BMLife, breaks = 100, main = "")#not normal distribution

plot(BMLove~date,data=gpbm, type = "l", lty = 1, col="blue")
plot(BMLife~date,data=gpbm, type = "l", lty = 1, col="red")

#mean and meadian, range and variance
mean(gpbm$BMLove)
mean(gpbm$BMLife)

median(gpbm$BMLove)
median(gpbm$BMLife)

require(modeest)
mlv(gpbm$BMLove, method="shorth")
mlv(gpbm$BMLife, method="shorth")   

range(gpbm$BMLove)
range(gpbm$BMLife)

var(gpbm$BMLove)
var(gpbm$BMLife)

#shapiro test to test normality
shapiro.test(log(gpbm$BMLove))
shapiro.test(log(gpbm$BMLife))
#significantly different from a normal distribution

#I'm going to log the data for BMLove and BMLife to overcome the problem of not normal distribution
dev.off()
hist(log(gpbm$BMLove), breaks = 50, main = "")#still not normal distribution
hist(log(gpbm$BMLife), breaks = 50, main = "")#still not normal distribution

##HP: the interest for this word increased trought time
#we test that with a linear regression

#BMLove LM
lmgp<-lm(BMLove~date, data=gpbm)
summary(lmgp) 
#diagnostic plots
par(mfrow=c(2,2))
plot(lmgp) #Residuals vs Fitted,isn't the best
#plotting the model on the data
dev.off()
plot(BMLove~date,data=gpbm, type = "l", lty = 1, col="blue")
abline(lmgp)

#BMLove LM
lmgp2<-lm(log(BMLife)~date, data=gpbm)
summary(lmgp2) 
#diagnostic plots
par(mfrow=c(2,2))
plot(lmgp2) #Residuals vs Fitted,isn't the best
#plotting the model on the data
dev.off()
plot(log(BMLife)~date,data=gpbm, type = "l", lty = 1, col="red")
abline(lmgp2)

#in both models, the rows 65 and 68 are outliers, and correspond to 
#May and August 2009. During August 2009 the pacific garbage patch was found.


#~~~~~~~~~~~~~~~~~~~~~~~~~~ZERO WASTE~~~~~~~~~~~~~~~~~~~~~~~~~~#
#importing dataset
zwbm<-read.csv("zwbm.csv", header = T, as.is = T)
zwbm$date<-as.Date(zwbm$date)
str(zwbm)

#looking at the distribution of data
par(mfrow=c(2,2))
hist(zwbm$BMLove, breaks = 100, main = "")#right skewed
hist(zwbm$BMLife, breaks = 100, main = "")#right skewed

plot(BMLove~date,data=zwbm, type = "l", lty = 1, col="blue")
plot(BMLife~date,data=zwbm, type = "l", lty = 1, col="red")

#mean and meadian, range and variance
mean(zwbm$BMLove)
mean(zwbm$BMLife)

median(zwbm$BMLove)
median(zwbm$BMLife)

require(modeest)
mlv(zwbm$BMLove, method="shorth")
mlv(zwbm$BMLife, method="shorth")   

range(zwbm$BMLove)
range(zwbm$BMLife)

var(zwbm$BMLove)
var(zwbm$BMLife)

#shapiro test to test normality
shapiro.test(log(zwbm$BMLove))
shapiro.test(log(zwbm$BMLife))
#significantly different from a normal distribution

#I'm going to log the data for BMLove and BMLife to overcome the problem of not normal distribution
dev.off()
hist(log(zwbm$BMLove), breaks = 50, main = "")#slightly better
hist(log(zwbm$BMLife), breaks = 50, main = "")#slightly better

##HP: the interest for this word increased trought time
#we test that with a linear regression

#BMLove LM
lmzw<-lm(log(BMLove)~date, data=zwbm)
summary(lmzw) 
#diagnostic plots
par(mfrow=c(2,2))
plot(lmzw) #Residuals vs Fitted,isn't the best
#plotting the model on the data
dev.off()
plot(log(BMLove)~date,data=zwbm, type = "l", lty = 1, col="blue")
abline(lmzw)

#BMLove LM
lmzw2<-lm(log(BMLife)~date, data=zwbm)
summary(lmzw2) 
#diagnostic plots
par(mfrow=c(2,2))
plot(lmzw2) #Residuals vs Fitted isn't the best
#plotting the model on the data
dev.off()
plot(log(BMLife)~date,data=zwbm, type = "l", lty = 1, col="red")
abline(lmzw2)

