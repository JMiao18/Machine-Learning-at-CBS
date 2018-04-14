## Homework for Machine Learning
## TASK 1
mlfin = read.csv(file = "M:/A Master of Science in Marketing Sciences/MS Machine Learning/b8142c7257d20c44.csv", header = TRUE)
attach(mlfin)
mlfin$date = as.character(mlfin$date)
mlfin$date <- as.Date(mlfin$date, "%m/%d/%Y")
mlfin$RET = as.character(mlfin$RET)
mlfin$RET = as.numeric(mlfin$RET)

threshold1 = as.Date("12/31/2005","%m/%d/%Y")
threshold2 = as.Date("12/31/2010","%m/%d/%Y")

summary(mlfin)

## TASK 2
train1 = subset(mlfin, date <= threshold1)
train2 = subset(mlfin, date <= threshold2 & date > threshold1)
test = subset(mlfin, date > threshold2)

library(tidyr)
newtrain1 = subset(train1, select = -c(PERMNO))
train1wide = spread(newtrain1, TICKER, RET)
newtrain2 = subset(train2, select = -c(PERMNO))
train2wide = spread(newtrain2, TICKER, RET)
newtest = subset(test, select = -c(PERMNO))
testwide = spread(newtest, TICKER, RET)
rm(train1,train2,test,newtrain1,newtest,newtrain2)

names(train1wide)
names(train2wide)
## "HWP" "SBC" are included in Train1 but not Train2 + Test
names(testwide)
## "ARNC" are added into Test after Train2

train1wide = subset(train1wide, select = -c(HWP,SBC))
testwide = subset(testwide, select = -c(ARNC))

## apply(train1wide,1,mean,na.rm = TRUE)

## For Trial
# tsp <- as.POSIXct(date,format = "%m/%d/%Y",tz ="EST")
# dtsp <- format(tsp,"%y-%m-%d",tz = "EST")
# aggregate(c("return.mean" = RET) ~ dtsp,
#           data = train1wide,
#           FUN = mean)
# apply.daily(train2wide,mean)

    ## Deal With Missing Values
hp_prior = read.csv(file = "M:/A Master of Science in Marketing Sciences/MS Machine Learning/a5f3bc1a54b620ff.csv", header = TRUE)
attach(hp_prior)
hp_prior$date = as.character(hp_prior$date)
hp_prior$date <- as.Date(hp_prior$date, "%m/%d/%Y")
hp_prior$RET = as.character(hp_prior$RET)
hp_prior$RET = as.numeric(hp_prior$RET)
train1wide$"HPQ"[1:585] = hp_prior$RET[1:585]

train1_data = train1wide[,2:length(train1wide[1,])]
sum(is.na(train1wide))

NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

train1_data = replace(train1_data, TRUE, lapply(train1_data, NA2mean))
tr1pca = prcomp(train1_data, scale = FALSE)
summary(tr1pca)
round(tr1pca$rotation,2)

tr1pcaVar = tr1pca$sdev^2
tr1pve = tr1pcaVar/sum(tr1pcaVar)
plot(tr1pve, type = "b", main = "Scree Plot for PCA with Trainset Set 1", ylab = "Proportion of Variance Explained", xlab = "Number of Components")

tr1rot = tr1pca$rotation
tra1loading = tr1rot[,1:3]
round(tra1loading,4)

train2_data = train2wide[,2:length(train2wide[1,])]
train2_daily_predict = as.matrix(train2_data) %*% tra1loading
round(head(train2_daily_predict,20),4)

## Then create a data-frame where the Y variable is the first stock's return at time t + 1 and the X variables are all the lagged Principal Components from time t to time t-30.
lth = length(train2_data[,1])
fultrain = c()
for (k in 2:length(names(train2wide)))
{
  full = c()
  for (i in 31:(lth - 1))
  {
    df = c()
    dat = train2wide[i,1]
    value = train2wide[i + 1,k]
    firm = names(train2wide)[k]
    df = cbind(df, dat, value, firm)
    for (j in (i - 1):(i - 30))
    {
      df = cbind(df, train2_daily_predict[j,1], train2_daily_predict[j,2], train2_daily_predict[j,3])
    }
    full = rbind(full,df)
  }
  fultrain = rbind(fultrain, full)
}

fultrain = as.data.frame(fultrain)
fultrain[,1] = as.Date(as.numeric(fultrain[,1]), origin = "1970-01-01")
fultrain[,2] = as.double(as.character(fultrain[,2]))
fultrain[,3] = as.character(fultrain[,3])

for (a in 4:93)
{
  fultrain[,a] = as.double(as.character(fultrain[,a]))
}

library(dummies)
dful = dummy.data.frame(fultrain)
dim(dful)
summary(dful)

as.Date(as.numeric(train2wide[,1]), origin = "1970-01-01")

head(dful)
x <- model.matrix(value~.,dful)[,-c(1,2)]
y <- dful$value
library(ISLR)
library(glmnet)
grd = seq(0.01, 1, length = 100)
set.seed(1)
train <- sample( 1 : nrow(x),nrow(x)/2)
test <- -train
y.test <- y[test]
y.train <- y[train]

lasso.mod <- glmnet( x[train, ], y[train], alpha = 1, lambda = grd)
plot(lasso.mod)

cv.out <- cv.glmnet(x[train,], y[train], alpha = 1, nfolds = 5)
plot(cv.out)

bestlam = cv.out$lambda.min
trridge.pred <- predict(cv.out, s = bestlam, newx <- x)
mean((trridge.pred - y)^2)

## Here you should take the cross validation estimated lambda parameter (from question 3) and refit the Lasso model to each stock for Train Set 2 (this time with no dummy variable).

## Construct the new Test Set
testwide[is.na(testwide)] = 0

test_data = testwide[,2:length(testwide[1,])]
test_daily_predict = as.matrix(test_data) %*% tra1loading

lth = length(test_data[,1])
ful = c()
for (k in 2:length(names(testwide)))
{
  full = c()
  for (i in 31:(lth - 1))
  {
    df = c()
    dat = testwide[i,1]
    value = testwide[i + 1,k]
    firm = names(testwide)[k]
    df = cbind(df, dat, value, firm)
    for (j in (i - 1):(i - 30))
    {
      df = cbind(df, test_daily_predict[j,1], test_daily_predict[j,2], test_daily_predict[j,3])
    }
    full = rbind(full, df)
  }
  ful = rbind(ful, full)
}

fultest = as.data.frame(ful)
fultest[,1] = as.character(fultest[,1])
fultest[,1] = as.numeric(fultest[,1])
fultest[,1] = as.Date(fultest[,1], origin = "1970-01-01")
fultest[,2] = as.double(as.character(fultest[,2]))
fultest[,3] = as.character(fultest[,3])

for (a in 4:93)
{
  fultest[,a] = as.double(as.character(fultest[,a]))
}

## Step 1: IN FulTrain, we get the Lasso Parameters. use it in Test, we get return predictions
xtr <- fultrain[,-c(1,2,3)]
ytr <- fultrain$value
xte <- fultest[,-c(1,2,3)]
yte <- fultest$value
yte = matrix(yte, 1479, 26)
return_predict = c()
for (m in 0:25)
{
  lasso.mod <- glmnet( as.matrix(xtr)[(1 + 1228 * m) : (1228 * (m + 1)), ], ytr[(1 + 1228 * m):(1228 * (m + 1))], alpha = 1, lambda = bestlam)
  pre = predict(lasso.mod, s = bestlam, newx <- as.matrix(xte)[(1 + 1479 * m):(1479 * (m + 1)), ])
  return_predict = cbind(return_predict, pre)
}

## True Return yte[1,][names(sort(return_predict[1,],decreasing = TRUE)[1:5])]
l = c()
s = c()
lr = c()
sr = c()

for (j in 1:1479)
{
  long = sort(return_predict[j,],decreasing = TRUE)[1:5]
  l = rbind(l, long)
  short = sort(return_predict[j,],decreasing = FALSE)[1:5]
  s = rbind(s, short)
  indexl = order(return_predict[j,],decreasing = TRUE)[1:5]
  indexs = order(return_predict[j,],decreasing = FALSE)[1:5]
  long_return = yte[j,indexl]
  lr = rbind(lr, long_return)
  short_return = yte[j,indexs]
  sr = rbind(sr, short_return)
}

money[1480] = NA
money[1] = 100
for (mj in 1:1479){
  money[mj + 1] = money[mj] + sum(money[mj]/10 * (lr[mj,] - sr[mj,]))
}
plot(money)

(money[1480]/100)^(1/6) -1
