library(randomForest)

df2<-read.csv("/Users/sunnyyueh/Desktop/Fall 2021/Data Mining/Data Mining/final project/data/newdata.csv")
shp<-st_read("/Users/sunnyyueh/Desktop/Fall 2021/Data Mining/Data Mining/final project/data/Data_count_lisa.csv")
shp$meidan_income<-shp$estimate
shp<-shp[-5]


shp$GEOID<-as.double(shp$GEOID)

df<-left_join(df2,shp,by="GEOID")

G=df$LG.li
quad=rep("Uncluster",length(G)) 
quad[G>1.65]="Cluster"
#quad

df$spatial=quad
NY=df
NY2=df
colnames(NY)

NY_select<-NY[,c(13:19,21,25:27,28:29,47,65,82:89,98:102,104,106,)]
NY_select2<-NY2[,c(13:19,21,25:27,28:29,47,65,82:89,98:102,104,106,1,103,104,107)]

NY_geoid<-NY_select2[,c(31,32,34)]
NY<-NY_select2[,1:30]


NY[1:30]<-as.numeric(unlist(NY[1:30]))
NY_select2[1:30]<-as.numeric(unlist(NY_select2[1:30]))
#NY_select2$LG.li<-(NY_select2$LG.li)^(1/2)
NY<-NY_select2%>%drop_na()
NY_GEOID<-NY[31:34]
NY_with_GEOID[1:30]<-as.numeric(unlist(NY[1:30]))
#NY<-NY_select
NY2<-NY[,-c(31:34)]


NY$LG.li<-(NY$LG.li)^(1/2)

set.seed(1000)
num_train <- nrow(NY) * 0.75
train = sample(1:dim(NY)[1],num_train )
test <- -train
NY.train <- NY[train, ]
NY.train <-NY.train[,-c(31:34)]
NY.test <- NY[test, ]
NY.test <-NY.test[,-c(31:34)]

colnames(NY)
NY2[1:30]<-as.numeric(unlist(NY[1:30]))
########################################################################
##                          Forest
########################################################################
bag.NY=randomForest(LG.li~.,data=NY.train ,mtry=29,importance=TRUE) # predict house price #mtry=13: 13 total number of input variable
bag.NY$predicted
mse.oob=mean((bag.NY$predicted-NY.train[,29])^2)

predict.bag = predict(bag.NY,newdata =NY.test) ##prediction (x: predicted value/ y:true value)
plot(predict.bag, NY.test[,29])
abline (0,1)
bag.test.error<-mean((predict.bag - NY.test[,29])^2);bag.test.error #mean square error as test error
importance(bag.NY)
imp.rf<-varImpPlot(bag.NY) 

rf.NY =randomForest(LG.li~.,data=NY.train ,mtry=(29/3), importance =TRUE) #change the value of m (mytry) // important=TRUE, calculate the importance of different variables
predict.bag.rf = predict(rf.NY, newdata=NY.test)
bag.test.error.rf<-mean((predict.bag.rf-NY.test[,29])^2);bag.test.error.rf
importance(rf.NY)
imp.rf<-varImpPlot(rf.NY)

predict.bag.rf = predict(rf.NY, newdata=NY[,-c(31:34)])
predict.bag = predict(bag.NY,newdata =NY[,-c(31:34)])


# this part just creates the data.frame for the plot part
library(dplyr);library(ggplot2) 
imp.rf<-as.data.frame(imp.rf)
imp.rf$varnames <- rownames(imp.rf) # row names to column
#rownames(imp.rf) <- NULL  
imp.rf$`%IncMSE`

ggplot(imp.rf, aes(x=reorder(varnames, IncNodePurity), y=IncNodePurity)) + 
  geom_point() +
  geom_segment(aes(x=varnames,xend=varnames,y=0,yend=IncNodePurity)) +
  ylab("IncNodePurity") +
  xlab("Variable Name") +
  coord_flip()


ggplot(imp.rf, aes(x=reorder(varnames,`%IncMSE`), y=`%IncMSE`)) + 
  geom_point() +
  geom_segment(aes(x=varnames,xend=varnames,y=0,yend=`%IncMSE`)) +
  ylab("%IncMSE") +
  xlab("Variable Name") +
  coord_flip()





