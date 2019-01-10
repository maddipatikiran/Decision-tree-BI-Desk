
#Reading Data set
ctg<- read.csv("CTG.csv", na.strings = "")

#checking null values
sapply(ctg, function(df)
{
  sum(is.na(df)==T)/length(df)
})

#checking levels y varible
levels(ctg$NSP)
#data type conversion 
ctg$NSP <- as.factor(ctg$NSP)
ctg$Nzeros<-as.factor(ctg$Nzeros)
ctg$Tendency<- as.factor(ctg$Tendency)
levels(ctg$Tendency)

#Data partition 
library(caret)
set.seed(100)
Dpartition<-createDataPartition(ctg$NSP,p=0.7,list = FALSE)
ctg_train<-ctg[Dpartition,]
ctg_test<-ctg[-Dpartition,]
table(ctg_train$NSP)

#Decision tree model
library(rpart)
library(rpart.plot)
dtree<-rpart(ctg_train$NSP ~.,data= ctg_train, method= "class")
dtree
rpart.plot(dtree)
summary(dtree)

#checking critical point
printcp(dtree)
plotcp(dtree)

#Prediction
dpredict<- predict(dtree,ctg_test,type = "class")
dpredict


#confusion matrix
confusionMatrix(ctg_test$NSP,dpredict)

#checking minimum cp
min(dtree$cptable[,"xerror"])
which.min(dtree$cptable[,"xerror"])
cpmin<-dtree$cptable[7,"CP"]
cpmin

#tree prunning
dtree_p<-prune(dtree,cpmin)
rpart.plot(dtree_p)
printcp(dtree_p)

#Accuracy after prunning
dpredict_p<-predict(dtree_p,ctg_test, type = "class")
confusionMatrix(ctg_test$NSP,dpredict_p)

