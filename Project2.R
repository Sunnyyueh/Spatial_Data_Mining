library(sf)
library(tree)
library(tidyr);library(dplyr)

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
#row.names(NY)<-NY$GEOID
NY_left<-NY[c(1,2:4,40:46,103:105)]
NY<-NY[-c(1,2:4,40:46,103:105)]

colnames(NY)
NY_select2<-NY[,c(13:19,21,25:27,28:29,47,65,82:89,98:102,104,106,1,103,104,107)]

NY_select2$spatial<-as.factor(NY_select2$spatial)
NY<-NY_select2
NY[1:33]<-as.numeric(unlist(NY[1:33]))
NY<-NY %>% drop_na()

NY<-NY[,-c(29,31:33)]

set.seed(1000)
num_train <- nrow(NY) * 0.75
train = sample(1:dim(df)[1],num_train )
test <- -train
NY.train <- NY[train, ]
NY.test <- NY[test, ]

#####Tree#####
tree.NY=tree(spatial~.,NY.train)
summary(tree.NY)
tree.NY


plot(tree.NY)
text(tree.NY,pretty =0)

tree.pred=predict(tree.NY, NY.test,type="class")
NY.pur<- NY$spatial[-train]
table(tree.pred, NY.pur) #confusiotn matrix 

test.error<- mean(tree.pred != NY.pur)*100;test.error

tree.NY.cv=cv.tree(tree.NY, FUN=prune.misclass, K=10) #using missclassification error as function(10-fold)
# names(cv.NY)
# cv.NY

plot(tree.NY.cv$size ,tree.NY.cv$dev, type="b")

tree.pred=predict(tree.NY.cv, NY.test,type="class")
test.error<- mean(tree.pred != NY.pur)*100;test.error



###
prune.NY =prune.misclass(tree.NY, best =5) 
summary(prune.NY)
plot(prune.NY)
text(prune.NY,pretty =0)


tree.pred.prun=predict (prune.NY, NY.test, type="class")
table(tree.pred.prun,NY.pur)
test.error.prun<- mean(tree.pred.prun != NY.pur)*100;test.error.prun
test.error;test.error.prun

#####LDA#####

library(MASS)
NY.train<-NY.train%>%drop_na()
lda.fit=lda(NY.train[,-93], NY.train[,93])

NY.test<-NY.test%>%drop_na()
# The test error is 
test.pred=predict(lda.fit,NY.test[,-93])$class # predicted class label 
lda.test.error=mean(test.pred!=NY.test[,93])
lda.test.error

#The training error
train.pred=predict(lda.fit,NY.train[,-93])$class # predicted class label 
lda.train.error=mean(train.pred!=NY.train[,93])
lda.train.error


########



#NY<-NY[-c(2:4,40:46)]
tree.pred=predict(tree.NY,NY,type="class")
LDA.pred=predict(lda.fit,NY[,-93])$class
LDA.pred=="Cluster"

NY$tree.hot<-tree.pred
NY$LDA.hot<-LDA.pred

NY_new<-cbind(NY,NY_left)


####
counties <- c("New York County",
              "Kings County",
              "Queens County",
              "Bronx County",
              "Richmond County")
my_vars <- c(median_income = "B19013_001")
new_york <- map(counties,
                ~ get_acs(
                  geography = "tract", 
                  state = "NY",
                  county = .x,
                  geometry = TRUE,
                  variables = my_vars
                ))
new_york <- rbind(new_york[[1]], 
                  new_york[[2]], 
                  new_york[[3]], 
                  new_york[[4]], 
                  new_york[[5]])
new_york%>%class()

NY_new

NY_new$GEOID<- as.character(NY_new$GEOID)
NY_full2<-left_join(x=new_york,y=NY_new,by="GEOID")

NY_full2<-NY_full2%>%drop_na()
mapview::mapview(NY_full2,
                 zcol="LDA.hot")

mapview::mapview(NY_full2,
                 zcol="spatial")

mapview::mapview(NY_full2,
                 zcol="tree.hot")




