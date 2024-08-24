rm(list=ls())
data=read_excel("ovetime_hr.xlsx")
View(data)

library(ggplot2)

library(skimr)
skim(data)  ##summary of the data frame

attach(data)

##check how charges vary with age when charges<=10000 and also regression 
data1=subset(data,charges<=10000)
ggplot(data1,aes(age,charges))+
  geom_point()

summary(lm(data1$charges~data1$age))
#OR
model=data1%>%lm(formula=charges~age)
model%>%summary()

model$coefficients ##to extract coefficients of regression model
vcov(model) ##obtaining the estimated variance-covariance matrix of parameter estimates in a fitted model

##standard errors of the parameters

sqrt(diag(vcov(model)))

##computing fitted values and plotting fitted value curve over scatterplot

fits=model$fitted.values
fits

data1%>%
  ggplot(aes(age,charges))+
  geom_point(col=2)+
  geom_line(aes(age,fits))

##computing residuals

error=model$residuals
error
plot(error,pch=20,col=2) ##plot of errors

##moving average of errors

library(zoo)
ma=rollmean(error,k=101)
ma
plot(ma,type="l",col=2,pch=20) ##to check E(error)=0. To capture effects to extreme values
abline(h = 0, col = 2, lwd = 1)

plot(model$residuals)
plot(model$residuals^2) ##squaring to visualise better which is outlier
plot(model$residuals^2/var(data1$charges)) ##standardising the values

m=model$residuals^2/var(data1$charges)

out1=which(m>0.5) ##extracting outliers which have value > 0.5
out1 ##vector of indices of outliers

##removing the outlier points from data

y=data1$charges[-out1]
y
x=data1$age[-out1]
x
model1=lm(y~x)
summary(model1)
##repeat entire process with this new data

##finally plot the density of new errors
e=model1$residuals
ggplot(data = NULL, aes(x = e)) + 
  geom_histogram(aes(y=..density..),fill = 2, col = 1)+ 
  geom_density(size = 1, fill = 2, alpha = 0.5)

