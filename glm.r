

### GLM
c(Household, Income, Ownership Status)

data = rbind(
c(1, 38000, 0),
c(2, 51200, 1),
c(3, 39600, 0),
c(4, 43400, 1),
c(5, 47700, 0),
c(6, 53000, 0),
c(7, 41500, 1),
c(8, 40800, 0),
c(9, 45400, 1),
c(10, 52400, 1),
c(11, 38700, 1),
c(12, 40100, 0),
c(13, 49500, 1),
c(14, 38000, 0),
c(15, 42000, 1),
c(16, 54000, 1),
c(17, 51700, 1),
c(18, 39400, 0),
c(19, 40900, 0),
c(20, 52800, 1))
colnames(data) = c('Household', 'Income', 'Ownership Status')

data = data[sample(data[,1], 15),]
data = data.frame(data)

x = data[,2]
y = data[, 3]

anal = glm(y~x, family=binomial(link='logit'), data=data)
summary(anal)

anal$fit
anal$linear.predictors
residuals(anal, c='deviance')
influence.measures(anal)

test = data
test['Income^2'] = test[,2]^2
x = cbind(test[, 2], test[,4])
colnames(x) = c('Income', 'Income^2')

anal = glm(y~x, family=binomial(link='logit'), data=data)
summary(anal)
14.916/12


### 2.
data = rbind(
    c(5, 500, 100),
    c(7, 500, 122),
    c(9, 500, 147),
    c(11, 500, 176),
    c(13, 500, 211),
    c(15, 500, 244),
    c(17, 500, 277),
    c(19, 500, 310),
    c(21, 500, 343),
    c(23, 500, 372),
    c(25, 500, 391))
colnames(data) = c('Discount x', 'Sample Size n', 'Number Redeemed r')
data = data[sample(nrow(data), 8),]
data

y = data[,3]/data[,2]
x = data[, 'Discount x']
data = data.frame(data)

model = glm(y~x, family=binomial(link='logit'), data=data)
summary(model)

4.6425/6



plot(y~x, xlim=c(-10,50), ylim=c(0,1), pch=20)
curve(predict(model, newdata=data.frame(x), type='resp'), col='red', add=TRUE)


x.quad = x^2
model = glm(y~x+x.quad, family=binomial(link='logit'), data=data)
summary(model)

curve.data = data.frame(cbind(x, x.quad))
plot(y~x, xlim=c(-10,50), ylim=c(0,1), pch=20)
curve(predict(model, newdata=data.frame(cbind(x, x.quad)), type='resp'), col='red', add=TRUE)

se = diag(vcov(model))^0.5
coef(model)[1]-1.96*se[1];coef(model)[1]+1.96*se[1]
coef(model)[2]-1.96*se[2];coef(model)[2]+1.96*se[2]
coef(model)[3]-1.96*se[3];coef(model)[3]+1.96*se[3]

diag(vcov(model))^0.5

summary(model)


# bootstrap 1000 runs
bt.b0 = numeric(1000)
bt.b1 = numeric(1000)
bt.b2 = numeric(1000)
for(i in 1:1000){
    bt.sp = data[sample(nrow(data), 8, replace=TRUE),]
    bt.y = bt.sp[,3]/bt.sp[,2]
    bt.x = bt.sp[,1]
    bt.x.quad = bt.x^2
    bt.model = glm(bt.y~bt.x+bt.x.quad, family=binomial(link='logit'), data=bt.sp)
    bt.b0[i] = coef(bt.model)[1]
    bt.b1[i] = coef(bt.model)[2]
    bt.b2[i] = coef(bt.model)[3]
}
quantile(bt.b0, c(0.025, 0.975))
quantile(bt.b1, c(0.025, 0.975))
quantile(bt.b2, c(0.025, 0.975))


### code for 3.
data = rbind(
    c(1, 5, 18),
    c(2, 3, 15),
    c(3, 0, 11),
    c(4, 1, 14),
    c(5, 4, 23),
    c(6, 0, 10),
    c(7, 0, 5 ),
    c(8, 1, 8),
    c(9, 0, 7),
    c(10, 0, 12),
    c(11, 0, 3),
    c(12, 1, 7),
    c(13, 0, 2),
    c(14, 7, 30),
    c(15, 0, 9))
colnames(data) = c('Valve', 'n Failures', 'Months')
data = data[sample(nrow(data), 6),]
df = data.frame(data)
df = df[order(df$Valve),]
rownames(df) = df$Valve
df = df[,2:3]

x=df[,1]
y=df[,2]

model = glm(y~x, family=poisson, data=df)
summary(model)
1.9002/4

plot(y~x, pch=20)
curve(predict(model, newdata=data.frame(x), type='resp'), col='red', add=TRUE)

# bootstrap 1000 runs
bt.b0 = numeric(1000)
bt.b1 = numeric(1000)
for(i in 1:1000){
    bt.sp = df[sample(nrow(df), 6, replace=TRUE),]
    bt.y = bt.sp[,2]
    bt.x = bt.sp[,1]
    bt.x.quad = bt.x^2
    bt.model = glm(bt.y~bt.x, family=poisson, data=bt.sp)
    bt.b0[i] = coef(bt.model)[1]
    bt.b1[i] = coef(bt.model)[2]
}
quantile(bt.b0, c(0.025, 0.975))
quantile(bt.b1, c(0.025, 0.975))
