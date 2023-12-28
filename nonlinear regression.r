

x=c(250,351,440,262,89.7,96.9,350,85.3,258,140,302,500,350,231,360,400,133.6,351,360,350)
y=c(20.0,18.25,11.2,21.47,34.7,30.4,16.5,36.5,19.7,20.3,17.8,14.39,17.8,23.54,21.47,16.59,23.9,13.9,13.77,16.5)


x*y

df = data.frame(x,y)
df

model = lm(y~x, data=df)
summary(model)

mean(df$x)
mean(df$y)

n=20
sxx = sum(x^2)-(sum(x)^2/n)
sxx
sxy = sum(y*x)-(sum(y)*sum(x)/n)
sxy

b1=sxy/sxx

mean(y)-sxy/sxx*mean(x)

sst = sum(y^2)-sum(y)^2/n
ssres = sst - b1*sxy
ssr=sst-ssres
ssres; ssr

ssr/18
ssres/18
ssr/sst
1-ssres/sst

34.05287-0.04811895*275

qt(p=0.05/2, df=18, lower.tail=FALSE)

18.81946+2.100922*(11.81153*(1/20+(283.025-283.025)^2/sxx))^0.5

18.81946+2.100922*(11.81153*(1/20+(275-283.025)^2/sxx))^0.5



(11.81153*(1/20+(283.025-283.025)^2/sxx))^0.5

(17.20492-20.434)
(17.2012-20.43772)


### mod14
install.packages('readxl')
library(readxl)
data = read_excel('data-prob-12-16.xls')
data = data[,2:5]
data = data.frame(data)
sample.idx = as.numeric(sample(row.names(data), 20)) #  c(1, 26, 12, 16, 17, 20, 19,  3, 22,  8, 18, 11,  4, 21,  7, 15, 24,  5, 25,  2)
sample.idx = c(1,26,12, 16, 17, 20, 19,  3, 22,  8, 18, 11,  4, 21,  7, 15, 24,  5, 25,  2)
data = data[sample.idx,]

par(mfrow=c(3,1))
plot(y~x1, data=data)
plot(y~x2, data=data)
plot(y~x3, data=data)

# transform
data$y = 1/(data$y)

coef(lm(y~., data=data))[2:4]
#          x1          x2          x3
# 0.008395465 0.003392352 0.004454887

nls(y~1/(t1*x1+t1*x2+t3*x3), start=list(t1=9.6035905982, t3=-0.0003493875), data=data)


# model
model = nls(y~1/(t1*x1+t1*x2+t3*x3), start=list(t1=0.008395465, t3=0.004454887), data=data)
model

summary(model)

# adequacy
par(mfrow=c(2,1))
yhat = fitted(model)
e = residuals(model)

plot(yhat, e)
abline(a=NULL, b=NULL, h=0, v=NULL, col='red')
qqnorm(e)
qqline(e, col='red')

AIC(model)
BIC(model)

plot(data$y, data$y)
points(data$y, yhat, col='blue')
lines(data$y, data$y, col='black')
yhat
data$y

### b
data = read.csv('data-prob-12-11.csv')
data = data.frame(data)
sample.idx = as.numeric(sample(row.names(data), 15)) #  16 41 21 22  6  2 24 20 15 17 27 40  8 28 44
sample.idx = c(16, 41, 21, 22,  6,  2, 24, 20, 15, 17, 27, 40,  8, 28, 44)
data = data[sample.idx,]
data = data[order(data$x),]

par(mfrow=c(1,1))
plot(y~x, data=data)

# initialize
lin.data = data
t1 = min(lin.data$y)
lin.data['ystar'] = log(lin.data$y-t1) # -ln(y-t1)=ln(t2)-t3x
lin.data = lin.data[is.finite(lin.data$ystar),]

lm(-ystar~x, data=lin.data)

t2 = coef(lm(-ystar~x, data=lin.data))[1]
t3 = coef(lm(-ystar~x, data=lin.data))[2]

thetas = list(t1=t1, t2=unname(t2), t3=unname(t3))
thetas
model = nls(y~t1-t2*exp(-t3*x), data=data, start=thetas)
yhat = fitted(model)

par(mfrow=c(1,1))
plot(y~x, data=data)
lines(data$x, yhat, col='blue')

summary(model)

# confint(model)
t.val = qt(p=0.05/2, df=12, lower.tail=F)
coef(model)-diag(vcov(model))^0.5*t.val
coef(model)+diag(vcov(model))^0.5*t.val


# adequacy
par(mfrow=c(2,1))
yhat = fitted(model)
e = residuals(model)

plot(yhat, e)
abline(a=NULL, b=NULL, h=0, v=NULL, col='red')
qqnorm(e)
qqline(e, col='red')