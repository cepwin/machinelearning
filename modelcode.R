library(caret)
library(gbm)
library(plyr)
pmltrain<-read.csv("pml-training.csv")
pmltest<-read.csv("pml-testing.csv")
nameset<-names(pmltrain)
cols<-grepl("^pitch|^roll|^yaw|^gyros|^magnet|^accl|classe",nameset)
pmltraincc<-pmltrain[,cols]
pmltestcc<-pmltest[,cols]
set.seed(888)
inTrain <- createDataPartition(y=pmltraincc$classe,
                               p=0.75, list=FALSE)
training <- pmltraincc[inTrain,]
testing <- pmltraincc[-inTrain,]
set.seed(3434)
tc<-trainControl(method="cv",number=3)
model<-train(classe ~ ., method="rf", data=training, trControl = tc,prox = TRUE)
predmod<-predict(model,testing)
sum(predmod == testing$classe)/length(predmod)
set.seed(3434)
modelNTC<-train(classe ~ ., method="rf", data=training, prox = TRUE)
predmod<-predict(model,testing)
sum(predmod == testing$classe)/length(predmod)

result <- rfcv(training[,-37],training[,37], cv.fold=3)
with(result, plot(n.var, error.cv, log="x", type="o", lwd=2))


set.seed(3434)
model<-train(classe ~ ., method="gbm",data=training)
predmod<-predict(model,testing)
sum(predmod == testing$classe)/length(predmod)

set.seed(125)
modRpart <-train(classe ~ ., method="rpart",data=training)
predmod<-predict(modRpart,testing)
sum(predmod == testing$classe)/length(predmod)
fancyRpartPlot(modRpart$finalModel)

set.seed(3434)
modelGlm<-train(classe ~ ., method="glm",data=training)
predmod<-predict(modelGlm,testing)
sum(predmod == testing$classe)/length(predmod)
tc<-trainControl(method="cv")


result <- rfcv(training[,-37],training[,37], cv.fold=3)
with(result, plot(n.var, error.cv, log="x", type="o", lwd=2))
