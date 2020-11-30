library(naivebayes)
library(caret)
library(psych)

#Import  data
data<- read.csv("E:/SomervilleHappinessSurvey2015.txt")
View(data)

#Cek adanya missing value
colSums((is.na(data)))

#merubah data ke tipe faktor
for(i in names(data)){
  data[,i]=as.factor(data[,i])
}
str(data)

#Cek korelasi antar variabel
pairs.panels(data)
#drop variabel dengan korelasi tinggi

#Split data, 80% untuk training dan 20% untuk testing
set.seed(1234)
sampelnaiv<-sample(2,nrow(data),replace=TRUE, prob=c(0.8,0.2))
trainingnaiv<-data[sampelnaiv==1,]
testingnaiv<-data[sampelnaiv==2,]


modelnaiv<-naive_bayes(D~.,data=trainingnaiv)
modelnaiv

testingnaiv
prediksinaiv<-predict(modelnaiv,testingnaiv)
prediksinaiv
confusionMatrix(table(prediksinaiv,testingnaiv$D))
