library(sf)
library(tree)
library(tidyr);library(dplyr)
library(rgdal)

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
#NY<-NY_select2%>%drop_na()

NY_geoid<-NY_select2[,c(31,32,34)]
NY<-NY_select2[,1:30]
NY<-NY_select2

#NY_add<-NY[,c(1,103,104,107)]
#NY_add2<-NY_add%>%drop_na()
#dim(NY_select)[2]
#NY_select[1:dim(NY_select)[2]]<-as.numeric(unlist(NY_select[1:dim(NY_select)[2]]))
#NY<-NY_select%>%drop_na()
#row.names(NY)<-NY$GEOI
#NY_left<-NY[c(1,2:4,40:46,103,105,107)]
#NY<-NY[-c(1,2:4,40:46,103,105,107)]

#NY$spatial<-as.factor(NY$spatial)
# NY[1:93]<-as.numeric(unlist(NY[1:93]))
# NY<-NY %>% drop_na()

NY_select2[1:30]<-as.numeric(unlist(NY_select2[1:30]))
NY_select2[1:34]<-as.numeric(unlist(NY_select2[1:33]))
NY<-NY_select2%>%drop_na()


#NY<-NY_select

set.seed(1000)
num_train <- nrow(NY) * 0.75
train = sample(1:dim(NY)[1],num_train )
test <- -train
NY.train <- NY[train, ];NY.train <-NY.train[,-c(31:34)]
NY.test <- NY[test, ];NY.test <-NY.test[,-c(31:34)]

colnames(NY)

write.csv(NY,"/Users/sunnyyueh/Desktop/Fall 2021/computer science/Final/CS_final.csv",row.names = FALSE)

CS<-read.csv("/Users/sunnyyueh/Desktop/Fall 2021/computer science/Final/CS_final.csv")

######

########################################################################
##                          PCR
########################################################################
library(pls)
pcr.mod.s = pcr(LG.li~., data=NY.train, method = pls.options()$pcralg,
              scale=T, center=T,validation="CV")
validationplot(pcr.mod.s , val.type = "MSEP")
pcr.pred.cv<-predict(pcr.mod.s, newdata=as.matrix(NY.test[,-29]),ncomp=24)
pcr.mod.s$coefficients[,,24]
pcr.mod.s$loadings
min.prediction.errors.s=mean((pcr.pred.cv-NY.test[,29])^2);min.prediction.errors.s

pcr.pred.cv<-predict(pcr.mod.s, newdata=as.matrix(NY[,-c(29,31:34)]),ncomp=24)
pcr.prediction.errors.s=mean((pcr.pred.cv-NY[,29])^2)
########################################################################
##                          Subset Selection
########################################################################
library(leaps)
#NY.fortesting<-NY[,c(1:20,92)]
regfit.full=regsubsets(LG.li~., data = NY[,-c(31:34)], nvmax = 29, method = "exhaustive")
best.subset<-summary(regfit.full)$outmat[15,]
best.subset<-as.data.frame(best.subset)
  
regfit.fwd <- regsubsets(LG.li~., data = NY[,-c(31:34)], nvmax = 18, method = "forward")
regfit.bwd <- regsubsets(LG.li~., data = NY[,-c(31:34)], nvmax = 18, method = "backward")
regfit.ss <- regsubsets(LG.li~., data = NY[,-c(31:34)], nvmax = 18, method = "seqrep")

best.subset<-summary(regfit.full)$outmat[15,]
fwd.subset<-summary(regfit.fwd)$outmat[15,]
bwd.subset<-summary(regfit.bwd)$outmat[15,]
ss.subset<-summary(regfit.ss)$outmat[15,]


best.subset<-as.data.frame(best.subset)
fwd.subset<-as.data.frame(fwd.subset)
bwd.subset<-as.data.frame(bwd.subset)
ss.subset<-as.data.frame(ss.subset)

best.subset.nb<-which(best.subset$best.subset=="*")
which(fwd.subset$fwd.subset=="*")
bwd.subset.nb<-which(bwd.subset$bwd.subset=="*")
which(ss.subset$ss.subset=="*")


NY.best.subset<-NY[,best.subset.nb]
NY.bwd.subset<-NY[,bwd.subset.nb]

########################################################################
##                            Ridge
########################################################################
library(genridge)
library(glmnet)
ridge.mod = glmnet(as.matrix(NY.train[,-29]), #use the 8 predictors to predict
                   as.vector(NY.train[,29]), # the response variable
                   family="gaussian", # linear regression --> family=gaussian distribution!!
                   alpha=0) # ridge regression --> alpha=0 
ridge.predicted.values=predict(ridge.mod,s=min(ridge.mod$lambda), newx=as.matrix(NY.test[,-29]), 
                         type = "response") 

prediction.errors=rep(NA,length(ridge.mod$lambda))
for(i in 1:length(ridge.mod$lambda)){
  prediction.errors[i]=mean((ridge.predicted.values[,i]-NY.test[,29])^2) 
}
plot(prediction.errors, type="l")

#Finding the best value
#which model has the lowest prediction error
ridge.prediction.error<-mean((ridge.predicted.values-NY.test[,29])^2)
coef(ridge.mod)[,which.min(prediction.errors)]


#Full data ridge
ridge.predicted.values=predict(ridge.mod,s=min(ridge.mod$lambda), newx=as.matrix(NY[,-c(29,31:34)]), 
                               type = "response") 
#ridge.prediction.error<-mean((ridge.predicted.values-NY[,-c(29,31:34)])^2)


########################################################################
##                            Lasso
########################################################################
lasso.mod = glmnet(as.matrix(NY.train[,-29,]),
                   as.vector(NY.train[,29]), 
                   family="gaussian", 
                   alpha=1)

lasso.mod.cv = cv.glmnet(as.matrix(NY.train[,-29,]),
                   as.vector(NY.train[,29]), 
                   family="gaussian", 
                   alpha=1)

plot(ridge.mod, label=T)

predicted.values=predict(lasso.mod, newx=as.matrix(NY.test[,-29]), 
                         type = "response")
prediction.errors=rep(NA,length(lasso.mod$lambda))
for(i in 1:length(lasso.mod$lambda)){
  prediction.errors[i]=mean((predicted.values[,i]-NY.test[,29])^2)
}
plot(prediction.errors, type="l")
####cv####
lasso.mod.cv$lambda.min 
coef(lasso.mod.cv, s = lasso.mod.cv$lambda.min)
lasso.predicted.values.cv=predict(lasso.mod.cv, newx=as.matrix(NY[,-c(29,31:34)]), 
                         type = "response")


#which.min(prediction.errors)
#lasso.prediction.errors<-mean(prediction.errors);lasso.prediction.errors
#lasso.prediction.errors.cv<-mean(prediction.errors.cv);lasso.prediction.errors.cv


lasso.coef.min<-as.data.frame(coef(lasso.mod)[,35])
lasso.coef.min[which((lasso.coef.min$`coef(lasso.mod)[, 35]`)!=0),]

lasso.zero<-colnames(NY[which((lasso.coef.min$`coef(lasso.mod)[, 35]`)==0)])
lasso.variable<-colnames(NY[which((lasso.coef.min$`coef(lasso.mod)[, 35]`)!=0)])

NY.lasso<-NY[which((lasso.coef.min$`coef(lasso.mod)[, 35]`)!=0)]
########################################################################
# using subset data in lasso
########################################################################
NY.best.subset<-NY[,best.subset.nb]
NY.bwd.subset<-NY[,bwd.subset.nb]

set.seed(1000)
num_train <- nrow(NY.best.subset) * 0.75
train = sample(1:dim(NY.best.subset)[1],num_train )
test <- -train
NY.best.subset.train <- NY.best.subset[train, ]
NY.best.subset.test <- NY.best.subset[test, ]


num_train <- nrow(NY.bwd.subset) * 0.75
train = sample(1:dim(NY.bwd.subset)[1],num_train )
test <- -train
NY.bwd.subset.train <- NY.bwd.subset[train, ]
NY.bwd.subset.test <- NY.bwd.subset[test, ]

#### lasso ####
#With best subset
lasso.mod.best.subset = glmnet(as.matrix(NY.best.subset.train [,-15]),
                   as.vector(NY.best.subset.train [,15]), 
                   family="gaussian", 
                   alpha=1)
min(lasso.mod$lambda)
predicted.values=predict(lasso.mod.best.subset, newx=as.matrix(NY.best.subset.test[,-15]), 
                         type = "response")
prediction.errors=rep(NA,length(lasso.mod.best.subset$lambda))
for(i in 1:length(lasso.mod.best.subset$lambda)){
  prediction.errors[i]=mean((predicted.values[,i]-NY.best.subset.test[,15])^2)
}
NY.best.subset.lasso.prediction.errors<-mean(prediction.errors);NY.best.subset.lasso.prediction.errors

###Backward
lasso.mod.bwd.subset = glmnet(as.matrix(NY.bwd.subset.train [,-15]),
                               as.vector(NY.bwd.subset.train [,15]), 
                               family="gaussian", 
                               alpha=1)
min(lasso.mod$lambda)
predicted.values.bwd.subset=predict(lasso.mod.bwd.subset, newx=as.matrix(NY.bwd.subset.train[,-15]), 
                         type = "response")
prediction.errors=rep(NA,length(lasso.mod.bwd.subset$lambda))
for(i in 1:length(lasso.mod.bwd.subset$lambda)){
  prediction.errors[i]=mean((predicted.values[,i]-NY.bwd.subset.test[,15])^2)
}
NY.bwd.subset.lasso.prediction.errors<-mean(prediction.errors);NY.bwd.subset.lasso.prediction.errors


#Without Gini index
lasso.mod.gini = glmnet(as.matrix(NY.train [,-c(28,29)]),
                   as.vector(NY.train [,29]), 
                   family="gaussian", 
                   alpha=1)
plot(lasso.mod.gini , label=T)
predicted.values=predict(lasso.mod.gini, newx=as.matrix(NY.test[,-c(28,29)]), 
                         type = "response")
prediction.errors=rep(NA,length(lasso.mod.gini$lambda))
for(i in 1:length(lasso.mod.gini$lambda)){
  prediction.errors[i]=mean((predicted.values[,i]-NY.test[,29])^2)
}
NY.lasso.prediction.errors.Gini<-mean(prediction.errors);NY.lasso.prediction.errors.Gini



####### using three lasso to predict lisa
predicted.values.lasso=predict(lasso.mod,s=min(lasso.mod$lambda) ,newx=as.matrix(NY[,-c(29,31:34)]), 
                         type = "response")
predicted.values.best.subset=predict(lasso.mod.best.subset, s=min(lasso.mod.best.subset$lambda),newx=as.matrix(NY.best.subset[,-15]), 
                         type = "response")
predicted.values.bwd.subset=predict(lasso.mod.bwd.subset, s=min(lasso.mod.bwd.subset$lambda),newx=as.matrix(NY.bwd.subset[,-15]), 
                                    type = "response")
# predicted.values.best.subset.gini=predict(lasso.mod.gini, s=min(lasso.mod.gini$lambda),newx=as.matrix(NY.best.subset[,-c(14,15)]), 
#                          type = "response")



NY.lasso=NY
NY.lasso=CS

NY.lasso$predicted.values.lasso=as.numeric(predicted.values.lasso)
NY.lasso$predicted.values.best.subset=as.numeric(predicted.values.best.subset)
NY.lasso$predicted.values.bwd.subset=as.numeric(predicted.values.bwd.subset)
NY.lasso$ridge.predicted.values=as.numeric(ridge.predicted.values)
NY.lasso$pcr.pred.cv=as.numeric(pcr.pred.cv)
NY.lasso$lasso.predicted.values.cv=as.numeric(lasso.predicted.values.cv)


NY.lasso$lasso.predicted.values.cv[is.na(NY.lasso$lasso.predicted.values.cv)]<-mean(NY.lasso$lasso.predicted.values.cv,na.rm=TRUE)


colnames(NY.lasso)

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

NY.lasso$GEOID<- as.character(NY.lasso$GEOID)
NY.lasso.poly<-left_join(x=new_york,y=NY.lasso,by="GEOID")

NY.lasso.poly$predicted.values.lasso[is.na(NY.lasso.poly$predicted.values.lasso)]<-mean(NY.lasso.poly$predicted.values.lasso,na.rm=TRUE)
NY.lasso.poly$predicted.values.best.subset[is.na(NY.lasso.poly$predicted.values.best.subset)]<-mean(NY.lasso.poly$predicted.values.best.subset,na.rm=TRUE)
NY.lasso.poly$predicted.values.bwd.subset[is.na(NY.lasso.poly$predicted.values.bwd.subset)]<-mean(NY.lasso.poly$predicted.values.bwd.subset,na.rm=TRUE)

NY.lasso.poly$ridge.predicted.values[is.na(NY.lasso.poly$ridge.predicted.values)]<-mean(NY.lasso.poly$ridge.predicted.values,na.rm=TRUE)
NY.lasso.poly$pcr.pred.cv[is.na(NY.lasso.poly$pcr.pred.cv)]<-mean(NY.lasso.poly$pcr.pred.cv,na.rm=TRUE)

NY.lasso.poly$lasso.predicted.values.cv[is.na(NY.lasso.poly$lasso.predicted.values.cv)]<-mean(NY.lasso.poly$lasso.predicted.values.cv,na.rm=TRUE)


NY.lasso.poly$LG.li[is.na(NY.lasso.poly$LG.li)]<-mean(NY.lasso.poly$LG.li,na.rm=TRUE)


mapview::mapview(NY.lasso.poly,
                 zcol="LG.li")

mapview::mapview(NY.lasso.poly,
                 zcol="spatial")

mapview::mapview(NY.lasso.poly,
                 zcol="predicted.values.lasso")

mapview::mapview(NY.lasso.poly,
                 zcol="predicted.values.lasso.cv")

mapview::mapview(NY.lasso.poly,
                 zcol="predicted.values.best.subset")

mapview::mapview(NY.lasso.poly,
                 zcol="predicted.values.bwd.subset")

mapview::mapview(NY.lasso.poly,
                 zcol="ridge.predicted.values")

mapview::mapview(NY.lasso.poly,
                 zcol="pcr.pred.cv")

ridge.prediction.error
pcr.prediction.errors.s
lasso.prediction.errors
NY.best.subset.lasso.prediction.errors
NY.best.subset.lasso.prediction.errors.Gini

centro_crime <- st_centroid(st_as_sf(NY.lasso.poly))
centro_crime$lon<- st_coordinates(centro_crime)[,1]
centro_crime$lat<- st_coordinates(centro_crime)[,2]

write.csv(centro_crime,"/Users/sunnyyueh/Desktop/Fall 2021/Data Mining/Data Mining/final project/data/1208_cen.csv",row.names = FALSE)

centro_crime%>%class()

mapview::mapview(centro_crime,
                 zcol="counts",
                 #col.regions=brewer.pal(9, "YlGn")
                 col.regions =viridis::"magma",
                 cex=2,
                 alpha=0
)


centro_crime_new <-centro_crime

writeOGR(obj=centro_crime , dsn="centro_crime_new", layer="centro_crime_new", driver="ESRI Shapefile")


library(zoo)
NY.lasso.poly<- NY.lasso.poly[ ! st_is_empty(NY.lasso.poly) , ]
NY.lasso.poly[]<-t(na.aggregate(t(NY.lasso.poly)))


NY.sp<-as_Spatial(NY.lasso.poly)
writeOGR(obj=NY.sp , dsn="NY.sp2", layer="NY.sp2", driver="ESRI Shapefile",encoding="UTF-8")

NY.sp.test<-st_read("/Users/sunnyyueh/Desktop/Fall 2021/Data Mining/Data Mining/final project/Data Mining Final Project/NY.sp/NY.sp.shp")
mapview::mapview(NY.sp.test,
                 zcol="conts")

