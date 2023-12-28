
# y : Sale price of the house/1000 
# x1 : Taxes (local, school, county)/1000 
# x2 : Number of baths 
# x3 : Lot size (sq ft × 1000) 
# x4 : Living space (sq ft × 1000) 
# x5 : Number of garage stalls 
# x6 : Number of rooms 
# x7 : Number of bedrooms 
# x8 : Age of the home (years) 
# x9 : Number of fireplaces 
# Source : “ Prediction, Linear R



data = rbind(
    c(25.9, 4.9176, 1.0, 3.4720, 0.9980, 1.0, 7, 4, 42, 0),
    c(29.5, 5.0208, 1.0, 3.5310, 1.5000, 2.0, 7, 4, 62, 0),
    c(27.9, 4.5429, 1.0, 2.2750, 1.1750, 1.0, 6, 3, 40, 0),
    c(25.9, 4.5573, 1.0, 4.0500, 1.2320, 1.0, 6, 3, 54, 0),
    c(29.9, 5.0597, 1.0, 4.4550, 1.1210, 1.0, 6, 3, 42, 0),
    c(29.9, 3.8910, 1.0, 4.4550, 0.9880, 1.0, 6, 3, 56, 0),
    c(30.9, 5.8980, 1.0, 5.8500, 1.2400, 1.0, 7, 3, 51, 1),
    c(28.9, 5.6039, 1.0, 9.5200, 1.5010, 0.0, 6, 3, 32, 0),
    c(35.9, 5.8282, 1.0, 6.4350, 1.2250, 2.0, 6, 3, 32, 0),
    c(31.5, 5.3003, 1.0, 4.9883, 1.5520, 1.0, 6, 3, 30, 0),
    c(31.0, 6.2712, 1.0, 5.5200, 0.9750, 1.0, 5, 2, 30, 0),
    c(30.9, 5.9592, 1.0, 6.6660, 1.1210, 2.0, 6, 3, 32, 0),
    c(30.0, 5.0500, 1.0, 5.0000, 1.0200, 0.0, 5, 2, 46, 1),
    c(36.9, 8.2464, 1.5, 5.1500, 1.6640, 2.0, 8, 4, 50, 0),
    c(41.9, 6.6969, 1.5, 6.9020, 1.4880, 1.5, 7, 3, 22, 1),
    c(40.5, 7.7841, 1.5, 7.1020, 1.3760, 1.0, 6, 3, 17, 0),
    c(43.9, 9.0384, 1.0, 7.8000, 1.5000, 15.0, 7, 3, 23, 0),
    c(37.5, 5.9894, 1.0, 5.5200, 1.2560, 2.0, 6, 3, 40, 1),
    c(37.9, 7.5422, 1.5, 5.0000, 1.6900, 1.0, 6, 3, 22, 0),
    c(44.5, 8.7951, 1.5, 9.8900, 1.8200, 2.0, 8, 4, 50, 1),
    c(37.9, 6.0831, 1.5, 6.7265, 1.6520, 1.0, 6, 3, 44, 0),
    c(38.9, 8.3607, 1.5, 9.1500, 1.7770, 2.0, 8, 4, 48, 1),
    c(36.9, 8.1400, 1.0, 8.0000, 1.5040, 2.0, 7, 3, 3, 0),
    c(45.8, 9.1416, 1.5, 7.3262, 1.8310, 1.5, 8, 4, 31, 0)
    )
colnames(data) = c('y', 'x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7', 'x8', 'x9')
data = data.frame(data)
data


# model selection -> adequacy -> validation

### model selection
library(leaps)
library(cars)

all.variables = regsubsets(y~., data=data)
summary(all.variables)
# 1 subsets of each size up to 8
# Selection Algorithm: exhaustive
#          x1  x2  x3  x4  x5  x6  x7  x8  x9
# 1  ( 1 ) "*" " " " " " " " " " " " " " " " "
# 2  ( 1 ) "*" "*" " " " " " " " " " " " " " "
# 3  ( 1 ) "*" "*" " " " " "*" " " " " " " " "
# 4  ( 1 ) "*" "*" " " " " "*" " " " " " " "*"
# 5  ( 1 ) "*" "*" " " " " "*" " " " " "*" "*"
# 6  ( 1 ) "*" "*" " " "*" "*" " " " " "*" "*"
# 7  ( 1 ) "*" "*" "*" "*" "*" " " " " "*" "*"
# 8  ( 1 ) "*" "*" "*" "*" "*" "*" " " "*" "*"

fm = lm(y~., data=data)
intercept = lm(y~1, data=data)
summary(fm)
summary(intercept)

# search
var.step = step(intercept, direction='both', scope=formula(fm), trace=0)
summary(var.step)
# var.step$anova
var.fwd = step(intercept, direction='forward', scope=formula(fm), trace=0)
summary(var.fwd)
# var.fwd$anova
var.bwd = step(fm, direction='backward', scope=formula(fm), trace=0)
summary(var.bwd)
# var.bwd$anova

### all shows x1 x2 x5 x8, x9, but significance in order of x2 > x5 > x1,x9 > x8
# check models
cp = function(rm, fm, data){
    n = length(data)
    k = length(coef(rm))-1
    sse.rm = anova(rm)['Residuals','Sum Sq'] # rm
    mse.fm = anova(fm)['Residuals','Mean Sq'] # fm
    sse.rm/mse.fm + 2*(k+1) - n
}
r2 = function(model, data){
    press = sum((fitted(model)-data['y'])^2)
    sst = sum(anova(model)['Sum Sq']) # sst
    return(list(r2 = 1-(press/sst), press=press))
}

## all var
m1 = lm(y~x1+x2+x3+x4+x5+x6+x8+x9, data=data)
## x2 x5 x1 x9 x8
m2 = lm(y~x2+x5+x1+x9+x8, data=data)
## x2 x5 x1 x9
m3 = lm(y~x2+x5+x1+x9, data=data)
## x2 x5
m4 = lm(y~x2+x5, data=data)
## x2
m5 = lm(y~x2, data=data)


# compare
vif(m1)
vif(m2) # 4.2
vif(m3) # 3.1
vif(m4) # 1

cp(m1, fm, data)
cp(m2, fm, data) # lowest
cp(m3, fm, data)
cp(m4, fm, data)
cp(m5, fm, data)

cat('R2: ', r2(m1, data)$r2, 'PRESS: ', r2(m1, data)$press, '\n')
cat('R2: ', r2(m2, data)$r2, 'PRESS: ', r2(m2, data)$press, '\n') # R2:  0.8688834 PRESS:  108.7017 
cat('R2: ', r2(m3, data)$r2, 'PRESS: ', r2(m3, data)$press, '\n') # R2:  0.8472443 PRESS:  126.6415
cat('R2: ', r2(m4, data)$r2, 'PRESS: ', r2(m4, data)$press, '\n') # R2:  0.7425546 PRESS:  213.4341
cat('R2: ', r2(m5, data)$r2, 'PRESS: ', r2(m5, data)$press, '\n') # R2:  0.5081351 PRESS:  407.7787 

cat('AIC: ', AIC(m1), 'BIC: ', BIC(m1), '\n')
cat('AIC: ', AIC(m2), 'BIC: ', BIC(m2), '\n') # AIC:  118.3623 BIC:  126.6087
cat('AIC: ', AIC(m3), 'BIC: ', BIC(m3), '\n') # AIC:  120.0284 BIC:  127.0967
cat('AIC: ', AIC(m4), 'BIC: ', BIC(m4), '\n') # AIC:  128.5556 BIC:  133.2679
cat('AIC: ', AIC(m5), 'BIC: ', BIC(m5), '\n') # AIC:  142.0932 BIC:  145.6273

residuals(m1)

par(mfrow=c(5,1))
qqnorm(residuals(m1))
qqline(residuals(m1), col='red')
qqnorm(residuals(m2))
qqline(residuals(m2), col='red')
qqnorm(residuals(m3))
qqline(residuals(m3), col='red')
qqnorm(residuals(m4))
qqline(residuals(m4), col='red')
qqnorm(residuals(m5))
qqline(residuals(m5), col='red')

par(mfrow=c(5,1))
plot(fitted(m1), residuals(m1))
abline(h=0, col='red')
plot(fitted(m2), residuals(m2))
abline(h=0, col='red')
plot(fitted(m3), residuals(m3))
abline(h=0, col='red')
plot(fitted(m4), residuals(m4))
abline(h=0, col='red')
plot(fitted(m5), residuals(m5))
abline(h=0, col='red')

par(mfrow=c(5,1))
plot(m1, which=4) # 17 outlier?
plot(m2, which=4)
plot(m3, which=4)
plot(m4, which=4)
plot(m5, which=4)

par(mfrow=c(5,1))
plot(m1, which=5) # 17 outlier?
plot(m2, which=5)
plot(m3, which=5)
plot(m4, which=5)
plot(m5, which=5)


# remove outlier
reduced.data = data[-17,]

all.variables = regsubsets(y~., data=reduced.data)
summary(all.variables)
# 1 subsets of each size up to 8
# Selection Algorithm: exhaustive
#          x1  x2  x3  x4  x5  x6  x7  x8  x9
# 1  ( 1 ) "*" " " " " " " " " " " " " " " " "
# 2  ( 1 ) "*" "*" " " " " " " " " " " " " " "
# 3  ( 1 ) "*" "*" " " " " " " " " " " " " "*"
# 4  ( 1 ) " " "*" " " " " "*" " " " " "*" "*"
# 5  ( 1 ) " " "*" "*" " " "*" " " " " "*" "*"
# 6  ( 1 ) "*" "*" "*" " " "*" " " " " "*" "*"
# 7  ( 1 ) "*" "*" "*" " " "*" "*" " " "*" "*"
# 8  ( 1 ) "*" "*" "*" "*" "*" "*" " " "*" "*"

fm = lm(y~., data=reduced.data)
intercept = lm(y~1, data=reduced.data)

# search
var.step = step(intercept, direction='both', scope=formula(fm), trace=0)
summary(var.step)
# var.step$anova
var.fwd = step(intercept, direction='forward', scope=formula(fm), trace=0)
summary(var.fwd)
# var.fwd$anova
var.bwd = step(fm, direction='backward', scope=formula(fm), trace=0)
summary(var.bwd)
# var.bwd$anova

## all var
m1 = lm(y~x1+x2+x3+x4+x5+x6+x8+x9, data=reduced.data)
## x2 x5 x1 x9 x8
m2 = lm(y~x2+x5+x1+x9+x8, data=reduced.data)
## x2 x5 x1 x9
m3 = lm(y~x2+x3+x5+x8+x9, data=reduced.data)
## x2 x5 x9 x8 (x3 removed due to p value)
m4 = lm(y~x2+x5+x9+x8, data=reduced.data)

# compare
vif(m1) # 7.12
vif(m2) # 4.3
vif(m3) # 1 oh!!!
vif(m4) # 1

cp(m1, fm, reduced.data) # 21
cp(m2, fm, reduced.data) # 15.29 lowest -> 16.66
cp(m3, fm, reduced.data) # 16.60 -> 16.43
cp(m4, fm, reduced.data) # 21.82 -> 16.21 lowest

cat('R2: ', r2(m1, reduced.data)$r2, 'PRESS: ', r2(m1, reduced.data)$press, '\n') # R2:  0.882367 PRESS:  86.93528
cat('R2: ', r2(m2, reduced.data)$r2, 'PRESS: ', r2(m2, reduced.data)$press, '\n') # R2:  0.8688834 PRESS:  108.7017 -> R2:  0.8673965 PRESS:  97.99906
cat('R2: ', r2(m3, reduced.data)$r2, 'PRESS: ', r2(m3, reduced.data)$press, '\n') # R2:  0.8472443 PRESS:  126.6415 -> R2:  0.8695074 PRESS:  96.43904
cat('R2: ', r2(m4, reduced.data)$r2, 'PRESS: ', r2(m4, reduced.data)$press, '\n') # R2:  0.7425546 PRESS:  213.4341 -> R2:  0.8534053 PRESS:  108.3391 

cat('AIC: ', AIC(m1), 'BIC: ', BIC(m1), '\n') # AIC:  115.8536 BIC:  127.2085
cat('AIC: ', AIC(m2), 'BIC: ', BIC(m2), '\n') # AIC:  118.3623 BIC:  126.6087
cat('AIC: ', AIC(m3), 'BIC: ', BIC(m3), '\n') # AIC:  120.0284 BIC:  127.0967
cat('AIC: ', AIC(m4), 'BIC: ', BIC(m4), '\n') # AIC:  128.5556 BIC:  133.2679

cat('AIC: ', AIC(m1), 'BIC: ', BIC(m1), '\n') # AIC:  115.8536 BIC:  127.2085
cat('AIC: ', AIC(m2), 'BIC: ', BIC(m2), '\n') # AIC:  118.3623 BIC:  126.6087
cat('AIC: ', AIC(m3), 'BIC: ', BIC(m3), '\n') # AIC:  120.0284 BIC:  127.0967
cat('AIC: ', AIC(m4), 'BIC: ', BIC(m4), '\n') # AIC:  128.5556 BIC:  133.2679

par(mfrow=c(4,2))
qqnorm(residuals(m1))
qqline(residuals(m1), col='red')
plot(fitted(m1), residuals(m1))
abline(h=0, col='red')
qqnorm(residuals(m2))
qqline(residuals(m2), col='red')
plot(fitted(m2), residuals(m2))
abline(h=0, col='red')
qqnorm(residuals(m3))
qqline(residuals(m3), col='red')
plot(fitted(m3), residuals(m3))
abline(h=0, col='red')
qqnorm(residuals(m4))
qqline(residuals(m4), col='red')
plot(fitted(m4), residuals(m4))
abline(h=0, col='red')


# # par(mfrow=c(4,1))
# plot(fitted(m1), residuals(m1))
# abline(h=0, col='red')
# plot(fitted(m2), residuals(m2))
# abline(h=0, col='red')
# plot(fitted(m3), residuals(m3))
# abline(h=0, col='red')
# plot(fitted(m4), residuals(m4))
# abline(h=0, col='red')


par(mfrow=c(4,1))
plot(m1, which=5) # 17 outlier?
plot(m2, which=5)
plot(m3, which=5)
plot(m4, which=5)



# significance
summary(m3)

anova(m3)

confint(m3)


reduced.data

nrow(data)

# bootstrap 1000 runs
bt.b0 = numeric(1000)
bt.b1 = numeric(1000)
bt.b2 = numeric(1000)
bt.b3 = numeric(1000)
bt.b4 = numeric(1000)
bt.b5 = numeric(1000)

for(i in 1:1000){
    bt.sp = data[sample(nrow(data), nrow(data), replace=TRUE),]
    bt.model = lm(y~x2+x3+x5+x8+x9, data=bt.sp)
    bt.b0[i] = coef(bt.model)['(Intercept)']
    bt.b1[i] = coef(bt.model)['x2']
    bt.b2[i] = coef(bt.model)['x3']
    bt.b3[i] = coef(bt.model)['x5']
    bt.b4[i] = coef(bt.model)['x8']
    bt.b5[i] = coef(bt.model)['x9']
}

quantile(bt.b0, c(0.025, 0.975))
quantile(bt.b1, c(0.025, 0.975))
quantile(bt.b2, c(0.025, 0.975))
quantile(bt.b3, c(0.025, 0.975))
quantile(bt.b4, c(0.025, 0.975))
quantile(bt.b5[is.finite(bt.b5)], c(0.025, 0.975))

cat('B0:', mean(bt.b0),'B1', mean(bt.b1),'B2', mean(bt.b2),'B3', mean(bt.b3),'B4', mean(bt.b4),'B5', mean(bt.b5[is.finite(bt.b5)]),'\n')


nrow(data)
nrow(reduced.data)
