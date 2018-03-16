mlrec = read.csv(file = "M:/A Master of Science in Marketing Sciences/MS Machine Learning/Homework3/BX-Book-Ratings.csv", sep = ";", header = TRUE)

rater = unique(mlrec$User.ID)
count = c(0,length(rater))
for (i in 1:length(rater))
{
   count[i] = sum(mlrec$User.ID == rater[i])
}

select = order(count,decreasing = TRUE)[1:100]
selectrater = rater[select]

active = subset(mlrec, User.ID %in% selectrater)

length(unique(mlrec$ISBN))
length(unique(active$ISBN))

## 2
set.seed(1)
train = sample(nrow(active),100000)
training = active[train, ]
test = active[-train,]

## 3
library(reshape2)
newactive = dcast(active, User.ID ~ ISBN, value.var = "Book.Rating")
newactive = newactive[,-1]
TrainRating = dcast(training, User.ID ~ ISBN, value.var = "Book.Rating")
rownames(TrainRating) = TrainRating[,1]
TrainRating = TrainRating[,-1]
colnames(TrainRating) = as.character(training$ISBN)

TestRating = dcast(test, User.ID ~ ISBN, value.var = "Book.Rating")
rownames(TestRating) = TestRating[,1]
TestRating = TestRating[,-1]

#Task 4
## Step 1: Replace Missing Values with Row Means for Training Set
k <- which(is.na(TrainRating), arr.ind=TRUE)
TrainRating1 = TrainRating
TrainRating1[k] <- rowMeans(TrainRating, na.rm=TRUE)[k[,1]]

## Step 2: Singular Value Decomposition
SVD1 <- svd(TrainRating1)
## With the first 2 factors
ReTrain1 <- SVD1$u[,1:2] %*% diag(SVD1$d)[1:2,1:2] %*% t(SVD1$v[,1:2])
## Change the row and column names so as to match the training set
rownames(ReTrain1) = rownames(TrainRating1)
colnames(ReTrain1) = colnames(TrainRating1)

newReTrain1 = as.data.frame(ReTrain1)
newReTrain1$User.ID = rownames(ReTrain1)
newR1Prediction <- melt(as.matrix(newReTrain1), id.vars = User.ID,  variable.name = "ISBN",   value.name = "Book.Rating")

newR1Prediction$Book.Rating = as.numeric(as.character(newR1Prediction$Book.Rating))
names(newR1Prediction)[1:2] = c("User.ID","ISBN")
test$index = do.call(paste0, test[c("User.ID","ISBN")])
newR1Prediction$index = do.call(paste0, newR1Prediction[c("User.ID","ISBN")])

mjpredict = subset(newR1Prediction, newR1Prediction$index %in% test$index)
mjpredict = mjpredict[order( mjpredict$index ), ]
mjreality = subset(test, test$index %in% mjpredict$index)
mjreality = mjreality[order( mjpredict$index ), ]

diff = mjpredict$Book.Rating - mjreality$Book.Rating
summary(diff)
sum(diff^2)/length(diff)

## Task 5
ReTrain2 =  ReTrain1
## Find the value from the benchmark matrix
trueindex = which(!is.na(TrainRating), arr.ind=TRUE)
ReTrain2[trueindex] = TrainRating[trueindex]

SVD2 = svd(ReTrain2)
ReTrain3 <- SVD2$u[,1:2] %*% diag(SVD2$d)[1:2,1:2] %*% t(SVD2$v[,1:2])
## Change the row and column names so as to match the training set
rownames(ReTrain3) = rownames(TrainRating1)
colnames(ReTrain3) = colnames(TrainRating1)

newReTrain3 = as.data.frame(ReTrain3)
newReTrain3$User.ID = rownames(ReTrain3)
newR2Prediction <- melt(as.matrix(newReTrain3), id.vars = User.ID,  variable.name = "ISBN",   value.name = "Book.Rating")

newR2Prediction$Book.Rating = as.numeric(as.character(newR2Prediction$Book.Rating))
names(newR2Prediction)[1:2] = c("User.ID","ISBN")
newR2Prediction$index = do.call(paste0, newR2Prediction[c("User.ID","ISBN")])

mjpredict2 = subset(newR2Prediction, newR2Prediction$index %in% test$index)
mjpredict2 = mjpredict2[order( mjpredict2$index ), ]
mjreality2 = subset(test, test$index %in% mjpredict2$index)
mjreality2 = mjreality2[order( mjpredict2$index ), ]

diff2 = mjpredict2$Book.Rating - mjreality2$Book.Rating
summary(diff2)
sum(diff2^2)/length(diff2)


ReTrain4 =  ReTrain3
## Find the value from the benchmark matrix
ReTrain4[trueindex] = TrainRating[trueindex]

SVD4 = svd(ReTrain4)
ReTrain5 <- SVD4$u[,1:2] %*% diag(SVD4$d)[1:2,1:2] %*% t(SVD4$v[,1:2])
## Change the row and column names so as to match the training set
rownames(ReTrain5) = rownames(TrainRating1)
colnames(ReTrain5) = colnames(TrainRating1)

newReTrain4 = as.data.frame(ReTrain5)
newReTrain4$User.ID = rownames(ReTrain5)
newR3Prediction <- melt(as.matrix(newReTrain4), id.vars = User.ID,  variable.name = "ISBN",   value.name = "Book.Rating")

newR3Prediction$Book.Rating = as.numeric(as.character(newR3Prediction$Book.Rating))
names(newR3Prediction)[1:2] = c("User.ID","ISBN")
newR3Prediction$index = do.call(paste0, newR3Prediction[c("User.ID","ISBN")])

mjpredict3 = subset(newR3Prediction, newR3Prediction$index %in% test$index)
mjpredict3 = mjpredict3[order( mjpredict3$index ), ]
mjreality3 = subset(test, test$index %in% mjpredict3$index)
mjreality3 = mjreality3[order( mjreality3$index ), ]

diff3 = mjpredict3$Book.Rating - mjreality3$Book.Rating
summary(diff2)
sum(diff3^2)/length(diff3)

ret = ReTrain5
for (i in 1:30)
{
  ret[trueindex] = TrainRating[trueindex]
  SVD = svd(ReTrain4)
  ret <- SVD$u[,1:2] %*% diag(SVD$d)[1:2,1:2] %*% t(SVD$v[,1:2])
}

rownames(ret) = rownames(TrainRating1)
colnames(ret) = colnames(TrainRating1)

newReTrain4 = as.data.frame(ret)
newReTrain4$User.ID = rownames(ret)
library(reshape2)
newR3Prediction <- melt(as.matrix(newReTrain4), id.vars = User.ID,  variable.name = "ISBN",   value.name = "Book.Rating")

newR3Prediction$Book.Rating = as.numeric(as.character(newR3Prediction$Book.Rating))
names(newR3Prediction)[1:2] = c("User.ID","ISBN")
newR3Prediction$index = do.call(paste0, newR3Prediction[c("User.ID","ISBN")])

mjpredict3 = subset(newR3Prediction, newR3Prediction$index %in% test$index)
mjpredict3 = mjpredict3[order( mjpredict3$index ), ]
mjreality3 = subset(test, test$index %in% mjpredict3$index)
mjreality3 = mjreality3[order( mjpredict3$index ), ]

diff3 = mjpredict3$Book.Rating - mjreality3$Book.Rating
sum(diff3^2)/length(diff3)
