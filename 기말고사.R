setwd("D:/2020_1학기/데이터마이닝입문/14")
getwd()
library(haven)
appen <- read_spss("t14appen.sav")
cd <- read_spss("t14cd.sav")
ou <- read_spss("t14ou.sav")
i <- read_spss("t14in.sav")
md <- read_spss("t14md.sav")
md <- md[,c("HHIDWON","MD7")]
cd <- cd[,c("HHIDWON","CD_COUNT")]
i <- i[,c("HHIDWON","INCOUNT")]
ou <- ou[,c("HHIDWON","OUCOUNT")]
appen <- appen[,c("HHIDWON","S2_0", "S5")]
data <- merge(md, cd, by="HHIDWON", all=FALSE)
data <- merge(data, i, by="HHIDWON", all=FALSE)
data <- merge(data, ou, by="HHIDWON", all=FALSE)
data <- merge(data, appen, by="HHIDWON", all=FALSE)
data <- data[-which(duplicated(data$HHIDWON)),]
nrow(data)

hist(data$OUCOUNT)
hist(data$INCOUNT)
hist(data$S2_0)
hist(data$S5)
hist(data$MD7)
boxplot(data$OUCOUNT)
boxplot(data$INCOUNT)

table(data$S2_0)
data$S2_0 <- ifelse(data$S2_0==2, 0, 1)
data = data[data$MD7 <60000,]
sum(is.na(data))
data[data$S5 == -1, "S5"] = 0
nrow(data)
data$S2_0 <- ifelse(data$S2_0==0, "No", "Yes")
data = data[data$INCOUNT<50,]
data = data[data$OUCOUNT<200,]
data = data[,-c(1)]
rownames(data) <- c(1:nrow(data))
head(data)

set.seed(12345)
idx <- sample(x=c("train","test"),size=nrow(data),replace=TRUE,prob=c(8,2))
train <- data[idx=="train",]
test <- data[idx=="test",]
dim(train)
dim(test)

library(kernlab)
svmM <- ksvm(S2_0~.,data=train,kernel='rbfdot',
             C=10,prob.model=T,cross=5)
svmM
pred <- predict(svmM, train)
acc <- table(pred, train$S2_0)
acc
classAgreement(acc)

yhat_test <- predict(svmM, test)
table <- table(real=test$S2_0,predict=yhat_test)
table
classAgreement(table)