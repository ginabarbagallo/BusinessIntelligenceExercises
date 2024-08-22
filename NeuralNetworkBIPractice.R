library(mlbench)
library(caret)
install.packages("MLeval")
library(MLeval)
data("PimaIndiansDiabetes")
data = PimaIndiansDiabetes

data$diabetes = relevel(data$diabetes,ref="pos")

index = createDataPartition(data$diabetes,p=0.7,list=F)
train=data[index,]
test=data[-index,]

tree1=train(diabetes~.,data=train,method="mlpML",
           tuneGrid=expand.grid(layer1=seq(1,10,by=1),layer2=0,layer3=0),
           trControl=trainControl(method="cv",number=10))
tree1
tree2=train(diabetes~.,data=train,method="mlpML",
            tuneGrid=expand.grid(layer1=seq(1,10,by=1),layer2=seq(1,10,by=1),layer3=0),
            trControl=trainControl(method="cv",number=10))
tree2

tree_auc1=train(diabetes~.,data=train,method="mlpML",
               tuneGrid=expand.grid(layer1=seq(1,10,by=1),layer2=0,layer3=0),
               trControl=trainControl(method="cv",number=10,
                                      classProbs=TRUE, summaryFunction=twoClassSummary,
                                      savePredictions=TRUE))
tree_auc1
x=evalm(tree_auc1,gnames="tree")
tree_auc2=train(diabetes~.,data=train,method="mlpML",
                tuneGrid=expand.grid(layer1=seq(1,10,by=1),layer2=seq(1,10,by=1),layer3=0),
                trControl=trainControl(method="cv",number=10,
                                       classProbs=TRUE, summaryFunction=twoClassSummary,
                                       savePredictions=TRUE))
tree_auc2
x=evalm(tree_auc2,gnames="tree")

pred_tree_class1=predict(tree_auc1,newdata=test) #predictclass
pred_tree_class1
pred_tree_prob1=predict(tree_auc1,newdata=test,type="prob") #predict probability

confusionMatrix(pred_tree_class1,test$diabetes)
confusionMatrix(pred_tree_class1,test$diabetes,mode="everything")


pred_tree_prob1$obs=test$diabetes #create new variable with actual values coming from the test set
x = evalm(pred_tree_prob1)


pred_tree_class2=predict(tree_auc2,newdata=test) #predictclass
pred_tree_class2
pred_tree_prob2=predict(tree_auc2,newdata=test,type="prob") #predict probability

confusionMatrix(pred_tree_class2,test$diabetes)
confusionMatrix(pred_tree_class2,test$diabetes,mode="everything")


pred_tree_prob2$obs=test$diabetes #create new variable with actual values coming from the test set
x = evalm(pred_tree_prob2)
