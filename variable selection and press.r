install.packages("leaps")
library(leaps)

data = data.frame(
    rbind(
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
    c(43.9, 9.0384, 1.0, 7.8000, 1.5000, 15, 7, 3, 23, 0),
    c(37.5, 5.9894, 1.0, 5.5200, 1.2560, 2.0, 6, 3, 40, 1),
    c(37.9, 7.5422, 1.5, 5.0000, 1.6900, 1.0, 6, 3, 22, 0),
    c(44.5, 8.7951, 1.5, 9.8900, 1.8200, 2.0, 8, 4, 50, 1),
    c(37.9, 6.0831, 1.5, 6.7265, 1.6520, 1.0, 6, 3, 44, 0),
    c(38.9, 8.3607, 1.5, 9.1500, 1.7770, 2.0, 8, 4, 48, 1),
    c(36.9, 8.1400, 1.0, 8.0000, 1.5040, 2.0, 7, 3, 3, 0),
    c(45.8, 9.1416, 1.5, 7.3262, 1.8310, 1.5, 8, 4, 31, 0)
    )
)
colnames(data) = c('y', 'x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7', 'x8', 'x9')

sdata = data[sample(nrow(data), 14),] # row
sdata = sdata[, append('y', sample(names(sdata)[2:10], 5))] # col
dim(sdata)

sdata

### a.
fit_all = regsubsets(y~., data=sdata)
summary(fit_all)

### b. stepwise
step.base = lm(y~1, data=sdata)
step.FM = lm(y~., data=sdata)

step.wise = step(step.base, direction='both', scope=formula(step.FM), trace=0)
summary(step.wise)
step.wise$anova
step.wise$coefficients

step.fwd = step(step.base, direction='forward', scope=formula(step.FM), trace=0)
summary(step.fwd)
step.fwd$anova
step.fwd$coefficients

step.bwd = step(step.FM, direction='backward', scope=formula(step.FM), trace=0)
summary(step.bwd)
step.bwd$anova
step.bwd$coefficients

### c. diagnostics
model = lm(y~x1, data=sdata)
summary(model)

par(mfrow = c(2, 1))
plot(model, which=4, main='cook')
plot(model, which=5, main='res vs leverage')

sdata

### d. residuals
par(mfrow = c(3, 1))

plot(model, which=1, add.smooth=FALSE, main='residual vs fit')
plot(model, which=2, main='QQ')
plot(model, which=3, main='studentized residuals')

### e. validation
### press, r^2 pred, vif
# match columns of validation data
val.data = data[-as.numeric(row.names(sdata)),]
val.data = val.data[,c('y','x5','x1','x9','x2','x7')]

model = lm(y~x1, data=sdata)
full.model = lm(y~., data=sdata)

pred.rm = predict(model, newdata=data.frame(sdata), type='resp')
pred.fm = predict(full.model, newdata=data.frame(sdata), type='resp')

# press
press.rm = sum((pred.rm-sdata$y)^2)
press.fm = sum((pred.fm-sdata$y)^2)
cat('PRESS RM: ', press.rm, '\n')
cat('PRESS FM: ', press.fm, '\n')

# R2 prediction
sst.rm = sum(anova(model)[2]) # sst
sst.fm = sum(anova(full.model)[2]) # sst
r2.rm = 1-(press.rm/sst.rm)
r2.fm = 1-(press.fm/sst.fm)
cat('R^2 RM: ', r2.rm, '\n')
cat('R^2 FM: ', r2.fm, '\n')

# VIPS
# install.packages("car")
# library(car)
vif(full.model)
