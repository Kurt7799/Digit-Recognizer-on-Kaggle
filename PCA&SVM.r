

# input the data
setwd("E://Kaggle//DigitRecognizer")
train<-read.csv("train.csv",header=T,stringsAsFactors=F)
test<-read.csv("test.csv",header=T,stringsAsFactors=F)

#take the variable "label" out of dataset
label<-train$label

#put the two sets together so that we have a bigger set to finish PCA
data<-rbind(train[,-1],test)
dim(data) # 70000 * 784

#PCA
pca<-prcomp(data)

#scores of PCA
scores<-predict(pca)
scores<-scores[,1:43]

#take the scores as the new predictors of label
pca.train<-scores[1:nrow(train),]
pca.test<-scores[42001:70000,]
pca.train<-(label,pca.train)
#
pca.train<-data.frame(pca.train)
pca.test<-data.frame(pca.test)
str(pca.train)
#
pca.train<-within(pca.train,{
			label<-as.factor(label)})

#SVM
library(e1071)

#it would take a lot of time 
fit.svm<-svm(label~.,data=pca.train)

pred<-predict(fit.svm,newdata=pca.test)
write.csv(pred,"result3.csv")

#the final scores on LB is 0.9820 .

#next step maybe K-means + PCA + SVM/RF
