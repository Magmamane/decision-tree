#Read data file
mydata <- read.csv("C:/Users/magma/Documents/PROGRAMS/Cardiotocographic.csv")
mydata$NSPF <- as.factor(mydata$NSP)

#Partition data into training and validation datasets
set.seed(1234)
pred<-sample(2,nrow(mydata),replace = TRUE,prob = c(0.7,0.3))
train<-mydata[pred==1,]
test<-mydata[pred==2,]


#Decision tree with party(decision trees)
install.packages('party')
library(party)

mytree <- ctree(NSPF~LB+AC+FM, data=train,controls=ctree_control(mincriterion=0.9, minsplit=50))
print(mytree)
plot(mytree)

#Prediction
predict(mytree,train,type="prob")

#Decision tree with party
library(rpart)
mytree1<-rpart(NSPF~LB+AC+FM, data=train)
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(mytree1,,extra = 2)

#Prediction
predict(mytree1,test)

#Misclassification error
misclassification<-table(predict(mytree), train$NSPF)
print(misclassification)
1-sum(diag(misclassification))/sum(misclassification)

#Misclassification error with test set
testmisclas<-predict(mytree,newdata=test)
misclassification<-table(testmisclas,test$NSPF)
print(misclassification)
1-sum(diag(misclassification))/sum(misclassification)

