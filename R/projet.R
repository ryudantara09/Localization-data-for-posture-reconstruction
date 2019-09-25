library(cluster)
library(corrplot)
library(xlsx)
library(MASS)
library(plotly)
library(quantmod)
library(xts)
library(fpc) 
library(dygraphs)
library(FactoMineR)
library(RPostgreSQL)
library(ggplot2)
library(e1071)
library(rpart)
library(rpart.plot)
library(dplyr)
library(scales)
library(caret)
library(randomForest)
library(mlbench)
library(gdata)
options("scipen" = 999)

summary(ConfLongDemo_JSI)
names(ConfLongDemo_JSI)
table(ConfLongDemo_JSI)

str(ConfLongDemo_JSI)
demande_credit = ConfLongDezmo_JSI
demande.daisy <- "hello"
#demande.daisy <- daisy((demande_credit[1:10000,(5:7)]), metric="gower", stand=FALSE)
demande_ACH <- hclust(demande.daisy,method="ward.D2")
plot(demande_ACH)




fact_tableST<-ConfLongDemo_JSI[which(ConfLongDemo_JSI$X2=="020-000-033-111"),c(1,4,5,6,7,8)]
fact_tableST<-ConfLongDemo_JSI[,c(1,4,5,6,7,8)]

fact_tableST<-ConfLongDemo_JSI[,c(1,2,4,5,6,7,8)]

fact_tableST<-fact_tableST[which(fact_tableST$X1=="A01"),c(2,3,4,5,6)]
fact_tableST<-fact_tableST[which(fact_tableST$X1=="A01"),c(2,3,4,5,6,7)]
# install.packages("RPostgreSQL")
# install.packages("DBI")

#########ST#############
#ankle right
fact_tableST.testingActivity<-fact_tableST[which(fact_tableST$activity=="falling"),]
fact_tableST.testingActivity.meanX<-mean(fact_tableST.testingActivity$x)
fact_tableST.testingActivity.meanY<-mean(fact_tableST.testingActivity$y)
fact_tableST.testingActivity.meanZ<-mean(fact_tableST.testingActivity$z)
plot(x = fact_tableST.testingActivity$date,y = fact_tableST.testingActivity$y ,type = "l")
summary(fact_tableST)

colnames(fact_tableST)<-c("date","x","y","z","activity")
colnames(fact_tableST)<-c("sensor", "date","x","y","z","activity")
fact_tableST$date<-as.xts(fact_tableST$date)
fact_tableST$date <- as.POSIXct(fact_tableST$date, format="%d.%m.%Y %H:%M:%S")
fact_tableST$date <- as.POSIXct(fact_tableST$date, origin="1930-01-01")
fact_tableST<-as.xts(fact_tableST[,-1],order.by = fact_tableST[,1])
minimumX=min(fact_tableST[,"x"])
maximumX=max(fact_tableST[,"x"])
plot(x = fact_tableST$activity,y = fact_tableST$x,xlim = (c(minimumX,maximumX)),xlab = "activity",ylab = "X")
plot(x = fact_tableST$date,y = fact_tableST$x ,type = "l")

plotXBySensor<-ggplot(data = fact_tableST, aes(x=fact_tableST$activity,y=fact_tableST$y,color=fact_tableST$sensor))+geom_line()
#plotXBySensor<-ggplot(data = fact_tableST, aes(x=fact_tableST$date,y=fact_tableST$z,color=fact_tableST$sensor))+geom_line()
plotXBySensor<-ggplot(data = fact_tableST.testingActivity, aes(x=fact_tableST.testingActivity$date))+geom_line(aes(y=fact_tableST.testingActivity$x),color="red")+geom_line(aes(y=fact_tableST.testingActivity$y),color="blue")+geom_line(aes(y=fact_tableST.testingActivity$z),color="green")
plotXBySensor<-ggplot(data = fact_tableST, aes(x=fact_tableST$date))+geom_line(aes(y=fact_tableST$x),color="red")+geom_line(aes(y=fact_tableST$y),color="blue")+geom_line(aes(y=fact_tableST$z),color="green")
plotXBySensorX<-ggplot(data = fact_tableST, aes(x=fact_tableST$activity))+geom_line(aes(y=fact_tableST$x),color="red")
plotXBySensorY<-ggplot(data = fact_tableST, aes(x=fact_tableST$activity))+geom_line(aes(y=fact_tableST$y),color="green")
plotXBySensorZ<-ggplot(data = fact_tableST, aes(x=fact_tableST$activity))+geom_line(aes(y=fact_tableST$z),color="blue")

#plotXBySensor<-ggplot(data = fact_tableST, aes(x=fact_tableST$date,y=fact_tableST$x),color=fact_tableST$activity)+geom_line(aes(y=fact_tableST$y))+geom_line(aes(y=fact_tableST$z))
ggplotly(plotXBySensorX)
#lying /sitting /sittingdown /sitting on the ground 
#standing up from lying/standing up from sitting/standing up from sitting on the ground/walking
#lying Down//on all fours
ggplotly(plotXBySensorY)
#standing up from sitting
ggplotly(plotXBySensorZ)
ggplotly(plotXBySensor)
par(mfrow=c(1,1))








#Data mining############
drv <- dbDriver('PostgreSQL')  
db <- 'DW_validation'  
host_db <- 'localhost'  
db_port <- '5432'  
db_user <- 'postgres'  
db_password <- 'postgres'

conn <- dbConnect(drv, dbname=db, host=host_db, port=db_port, user=db_user, password=db_password)

dbExistsTable(conn, "fact_iot")

fact_table = dbReadTable(conn, 'fact_iot') 
plot(fact_table[1:50,])
df_postgres <- dbGetQuery(conn, "SELECT x_coordinate as x, y_coordinate as y, z_coordinate as z,
                          fk_activities as activity, fk_sensor as sensor, timestemp as time from fact_iot")
df_postgres$activity= as.factor(df_postgres$activity)
df_postgres$sensor= as.factor(df_postgres$sensor)
summary(df_postgres)
df_postgres.ankle_left = df_postgres[which(df_postgres$sensor=='1'),]
df_postgres.chest = df_postgres[which(df_postgres$sensor=='2'),]
df_postgres.belly = df_postgres[which(df_postgres$sensor=='3'),]
df_postgres.ankle_right = df_postgres[which(df_postgres$sensor=='4'),]

df_postgres.chest<-df_postgres.chest[sample(nrow(df_postgres.chest), nrow(df_postgres.chest)), ]
plot(df_postgres.chest[1:35801,6],df_postgres.chest[1:35801,1])

##########KMEANS
classes = fact_tableST
set.seed(123)
fact_tableST.kmeans<-kmeans(fact_tableST[4:6],11)
m <-fact_tableST.kmeans$cluster
fact_tableST$X4 <- as.POSIXct(fact_tableST$X4, format="%d.%m.%Y %H:%M:%S")
cluster<-as.xts(m, order.by = fact_tableST[,1])
table(m,fact_tableST.kmeans$X8)
summary(as.factor(m))
summary(df_postgres.chest$activity)
length(m)
m
#####Model


summary(df_postgres.chest.test$activity)
df_postgres.chest.apprentissage<-df_postgres.chest[1:25060,]
df_postgres.chest.test<-df_postgres.chest[25061:35801,]
df_postgres.chest.apprentissage.tree<-rpart(activity~x+y+z,df_postgres.chest.apprentissage)
plot(df_postgres.chest.apprentissage.tree)
text(df_postgres.chest.apprentissage.tree)
summary(df_postgres.chest.apprentissage.tree)
df_postgres.chest.apprentissage.tree
prediction<-predict(df_postgres.chest.apprentissage.tree,df_postgres.chest.test,type = "class")
db<-cbind(df_postgres.chest.test,prediction)
table(db$prediction,db$activity)
nrow(db[which(db$activity=="1"),])#88%
nrow(db[which(db$activity=="3"),])#88%
nrow(db[which(db$activity=="6"),])#98%
nrow(db[which(db$activity=="9"),])#69%



##########ACTIVITIES#############
#1-walk
#2-sitting down
#3-sitting
#4-standing up from sitting
#5-falling
#6-lying
#7-standing up from lying
#8-lying down
#9-sitting on the ground
#10-standing up from sitting on the ground
#11-on all fours
########################

#######associativité###########
df_postgres.chest.apprentissage<-df_postgres.chest.apprentissagee
df_postgres.chest.apprentissage1<-df_postgres.chest.apprentissage[which(df_postgres.chest.apprentissage[,4]=='1') || which(df_postgres.chest.apprentissage[,4]=='3')||which(df_postgres.chest.apprentissage[,4]=='6')||which(df_postgres.chest.apprentissage[,4]=='7')||which(df_postgres.chest.apprentissage[,4]=='9'),]
modelSVM<-svm(activity~x+y+z ,data=df_postgres.chest.apprentissage)
summary(modelSVM)
df_postgres.chest.apprentissage1<-df_postgres.chest.apprentissage1[sample(nrow(df_postgres.chest), nrow(df_postgres.chest)),]
plot(modelSVM, data=df_postgres.chest.apprentissage,z~x )
df_postgres.chest.apprentissage1 <- dbGetQuery(conn, "SELECT x_coordinate as x, y_coordinate as y, z_coordinate as z,
                          fk_activities as activity, fk_sensor as sensor, timestemp as time from fact_iot where (fk_activities=1 or fk_activities=3 or fk_activities=6 or 
                                                fk_activities=9) and fk_sensor=2")
df_postgres.chest.apprentissage1$activity= as.factor(df_postgres.chest.apprentissage1$activity)
df_postgres.chest.apprentissage1$sensor= as.factor(df_postgres.chest.apprentissage1$sensor)
df_postgres.chest.apprentissage1<-df_postgres.chest.apprentissage1[sample(nrow(df_postgres.chest.apprentissage1), nrow(df_postgres.chest.apprentissage1)),]

summary(df_postgres.chest.test$activity)
df_postgres.chest.apprentissage11<-df_postgres.chest.apprentissage1[1:21942,]
df_postgres.chest.test1<-df_postgres.chest.apprentissage1[21943:31437,]
df_postgres.chest.apprentissage11.tree<-rpart(activity~x+y+z,df_postgres.chest.apprentissage1)
plot(df_postgres.chest.apprentissage11.tree)
text(df_postgres.chest.apprentissage11.tree)
summary(df_postgres.chest.apprentissage11.tree)
df_postgres.chest.apprentissage11.tree
prediction<-predict(df_postgres.chest.apprentissage11.tree,df_postgres.chest.test1,type = "class")
db<-cbind(df_postgres.chest.test1,prediction)
table(db$prediction,db$activity)
confusionMatrix(db$prediction,db$activity)
BIC(model.regressionLogistique)
AIC()
?rms.curv()
confusion(db$prediction,db$activity,1,1)

#regression logistique######
fact_table.falling<-ConfLongDemo_JSI[which(ConfLongDemo_JSI$X2=="020-000-033-111"),c(5,6,7,8)]
colnames(fact_table.falling)<-c("x","y","z","activity")
fact_table.falling[which(fact_table.falling$activity!="falling"),4]<-"0"
fact_table.falling[which(fact_table.falling$activity=="falling"),4]<-"1"
fact_table.falling$activity<-as.numeric(fact_table.falling$activity)
model.regressionLogistique<-glm(activity~.,fact_table.falling,family = "binomial")