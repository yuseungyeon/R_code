setwd("D:/2020_1학기/데이터마이닝입문/데이터 및 조사설계서")
getwd()
Sys.setlocale(category = "LC_CTYPE", locale = "C")
data = read.csv("데이터.csv", quote="")
head(data)
library(dplyr)
data1 <- data%>%
  select(p1004_1, p1003_2, p1003_5, p1003_6, p1003_7,
         p1003_8, p1003_10, p1003_12)
colSums(is.na(data1))
library(caret)
nearZeroVar(data1, saveMetrics=TRUE)
head(data1)
colnames(data1) <- c("believe", "danger", "health",
                     "money", "house", "family", "society",
                     "overall")
colSums(is.na(data1))
data2 <- na.omit(data1)
data2 <- subset(data2, believe>=1 & believe<=2)
write.csv(data2, file="data2.csv", row.names = FALSE)
data2 <- read.csv("data2.csv")

#preProcess#
impute <- preProcess(data1, method=c("knnImpute"))
library(RANN)
imput2 <- predict(impute, data1)
summary(imput2)
idx = apply(data1, 1, function(x) sum(is.na(x)))
as.vector(which(idx==ncol(data1)))
data1 <- data1[-c(2651,2653,2655,2973, 2975,
                  2977,2979,5321,5324,5326,5337,
                  5339, 5341),]
summary(data1)

sum(is.na(data$p1004_8))

imp <- preProcess(data1, method="knnImpute")
demo_imp <- predict(imp, data1)
summary(demo_imp)
#preProcess#

data2[data2$believe==2,"believe"]=0
data2$believe <- factor(data2$believe, levels=c(0,1),
                         labels=c("No","Yes"))
library(ISLR)
library(kknn)
library(caret)
set.seed(100)
idxs <- createDataPartition(y=data2$believe, p=0.75, list=FALSE)
trainData <- data2[idxs,]
testData <- data2[-idxs,]
trainX <- trainData[,names(trainData)!="believe"]
preProValues <- preProcess(x=trainX,method=c("center", "scale"))
preProValues

set.seed(200)
ctrl <- trainControl(method="repeatedcv",repeats = 3)
knnFit <- train(believe~., data=trainData,method="knn",
                trContrl=ctrl, preProcess=c("center","scale"))
knnFit
plot(knnFit)
knnPredict <- predict(knnFit, newdata=testData)
confusionMatrix(knnPredict, testData$conflict)
