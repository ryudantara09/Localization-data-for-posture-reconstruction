#connexion postgreSQL#####
drv <- dbDriver('PostgreSQL')  
db <- 'DW_validation'  
host_db <- 'localhost'  
db_port <- '5432'  
db_user <- 'postgres'  
db_password <- 'postgres'
conn <- dbConnect(drv, dbname=db, host=host_db, port=db_port, user=db_user, password=db_password)
dbExistsTable(conn, "fact_iot")
fact_table = dbReadTable(conn, 'fact_iot') 
dime_date = dbReadTable(conn, 'Dime_date')
plot(fact_table[1:50,])

df_postgres <- dbGetQuery(conn, "SELECT x_coordinate as x, y_coordinate as y, z_coordinate as z,
                          fk_activities as activity, fk_sensor as sensor, timestemp as time from fact_iot")

df_postgres$activity= as.factor(df_postgres$activity)
df_postgres$sensor= as.factor(df_postgres$sensor)
########TREE#######

###########TREE CHEST1369##########
df_postgres.chest = data[which(data$sensor_localisation=='CHEST'),]
df_postgres.chest=df_postgres.chest[which(df_postgres.chest$activities%in%c("walking","sitting","lying","sitting on the ground")),]
#df_postgres.chest<-df_postgres.chest[sample(nrow(df_postgres.chest), nrow(df_postgres.chest)), ]
#levels(df_postgres.chest$activities)
#df_postgres.chest.apprentissage<-df_postgres.chest[1:19228,]
#df_postgres.chest.test<-df_postgres.chest[19229:27469,]

inTrain <- createDataPartition(y=df_postgres.chest$activities, p=0.7, list=FALSE)
df_postgres.chest.apprentissage <- df_postgres.chest[inTrain,]
df_postgres.chest.test <- df_postgres.chest[-inTrain,]

df_postgres.chest.apprentissage.tree<-rpart(activities~x_coordinate+y_coordinate+z_coordinate,df_postgres.chest.apprentissage)
plot(df_postgres.chest.apprentissage.tree)
text(df_postgres.chest.apprentissage.tree)
#summary(df_postgres.chest.apprentissage.tree)
#df_postgres.chest.apprentissage.tree
prediction.chest<-predict(df_postgres.chest.apprentissage.tree,df_postgres.chest.test,type = "class")
#db<-cbind(df_postgres.chest.test,prediction)
#table(db$prediction,db$activities)
#nrow(db[which(db$activity=="1"),])#88%
#nrow(db[which(db$activity=="3"),])#88%
#nrow(db[which(db$activity=="6"),])#98%
#nrow(db[which(db$activity=="9"),])#69%
confusionMatrix(prediction.chest, df_postgres.chest.test$activities)
head(data)
levels(df_postgres.chest.test$activities)
levels(prediction)
names(df_postgres.chest.test$activities)
AIC(df_postgres.chest.apprentissage.tree)
BIC(df_postgres.chest.apprentissage.tree)
#Accuracy : 0.895  
#############################

###########TREE ANKLE_LEFT 1369##########
df_postgres.ankle_left = data[which(data$sensor_localisation=='ANKLE_LEFT'),]
df_postgres.ankle_left=df_postgres.ankle_left[which(df_postgres.ankle_left$activities%in%c("walking","sitting","lying","sitting on the ground")),]
df_postgres.ankle_left<-df_postgres.ankle_left[sample(nrow(df_postgres.ankle_left), nrow(df_postgres.ankle_left)), ]

df_postgres.ankle_left.apprentissage<-df_postgres.ankle_left[1:23290,]
df_postgres.ankle_left.test<-df_postgres.ankle_left[23291:33271,]

#inTrain <- createDataPartition(y=dataPE$activity, p=0.7, list=FALSE)
#df_postgres.ankle_left.apprentissage <- df_postgres.ankle_left[inTrain,]
#df_postgres.ankle_left.test <- df_postgres.ankle_left[-inTrain,]

df_postgres.ankle_left.apprentissage.tree<-rpart(activities~x_coordinate+y_coordinate+z_coordinate,df_postgres.ankle_left.apprentissage)
plot(df_postgres.ankle_left.apprentissage.tree)
text(df_postgres.ankle_left.apprentissage.tree)
#summary(df_postgres.ankle_left.apprentissage.tree)
#df_postgres.ankle_left.apprentissage.tree
prediction.ankle_left<-predict(df_postgres.ankle_left.apprentissage.tree,df_postgres.ankle_left.test,type = "class")
#db<-cbind(df_postgres.ankle_left.test,prediction)
#table(db$prediction,db$activities)
#nrow(db[which(db$activity=="1"),])#88%
#nrow(db[which(db$activity=="3"),])#88%
#nrow(db[which(db$activity=="6"),])#98%
#nrow(db[which(db$activity=="9"),])#69%
confusionMatrix(prediction.ankle_left, df_postgres.ankle_left.test$activities)

head(data)

names(df_postgres.ankle_left.test$activities)
#Accuracy : 0.646  
#############################

###########TREE ANKLE_RIGHT 1369##########
df_postgres.ankle_right = data[which(data$sensor_localisation=='ANKLE_RIGHT'),]
df_postgres.ankle_right=df_postgres.ankle_right[which(df_postgres.ankle_right$activities%in%c("walking","sitting","lying","sitting on the ground")),]
df_postgres.ankle_right<-df_postgres.ankle_right[sample(nrow(df_postgres.ankle_right), nrow(df_postgres.ankle_right)), ]

df_postgres.ankle_right.apprentissage<-df_postgres.ankle_right[1:22597,]
df_postgres.ankle_right.test<-df_postgres.ankle_right[22598:32281,]

#inTrain <- createDataPartition(y=dataPE$activity, p=0.7, list=FALSE)
#df_postgres.ankle_right.apprentissage <- df_postgres.ankle_right[inTrain,]
#df_postgres.ankle_right.test <- df_postgres.ankle_right[-inTrain,]

df_postgres.ankle_right.apprentissage.tree<-rpart(activities~x_coordinate+y_coordinate+z_coordinate,df_postgres.ankle_right.apprentissage)
plot(df_postgres.ankle_right.apprentissage.tree)
text(df_postgres.ankle_right.apprentissage.tree)
#summary(df_postgres.ankle_right.apprentissage.tree)
#df_postgres.ankle_right.apprentissage.tree
prediction.ankle_right<-predict(df_postgres.ankle_right.apprentissage.tree,df_postgres.ankle_right.test,type = "class")
#db<-cbind(df_postgres.ankle_right.test,prediction)
#table(db$prediction,db$activities)
#nrow(db[which(db$activity=="1"),])#88%
#nrow(db[which(db$activity=="3"),])#88%
#nrow(db[which(db$activity=="6"),])#98%
#nrow(db[which(db$activity=="9"),])#69%
confusionMatrix(prediction.ankle_right, df_postgres.ankle_right.test$activities)

head(data)

names(df_postgres.ankle_right.test$activities)
#Accuracy : 0.6371
#############################

###########TREE BELT 1 3 6 9##########
df_postgres.BELT = data[which(data$sensor_localisation=='BELT'),]
df_postgres.BELT=df_postgres.BELT[which(df_postgres.BELT$activities%in%c("walking","sitting","lying","sitting on the ground")),]
df_postgres.BELT<-df_postgres.BELT[sample(nrow(df_postgres.BELT), nrow(df_postgres.BELT)), ]
#levels(df_postgres.BELT$activities)
#df_postgres.BELT.apprentissage<-df_postgres.BELT[1:7195,]
#df_postgres.BELT.test<-df_postgres.BELT[7196:10278,]

inTrain <- createDataPartition(y=df_postgres.BELT$activities, p=0.7, list=FALSE)
df_postgres.BELT.apprentissage <- df_postgres.BELT[inTrain,]
df_postgres.BELT.test <- df_postgres.BELT[-inTrain,]

df_postgres.BELT.apprentissage.tree<-rpart(activities~x_coordinate+y_coordinate+z_coordinate,df_postgres.BELT.apprentissage)
plot(df_postgres.BELT.apprentissage.tree)
text(df_postgres.BELT.apprentissage.tree)
#summary(df_postgres.BELT.apprentissage.tree)
#df_postgres.BELT.apprentissage.tree
prediction.BELT<-predict(df_postgres.BELT.apprentissage.tree,df_postgres.BELT.test,type = "class")
#db<-cbind(df_postgres.BELT.test,prediction)
#table(db$prediction,db$activities)
#nrow(db[which(db$activity=="1"),])#88%
#nrow(db[which(db$activity=="3"),])#88%
#nrow(db[which(db$activity=="6"),])#98%
#nrow(db[which(db$activity=="9"),])#69%
confusionMatrix(prediction.BELT, df_postgres.BELT.test$activities)

levels(prediction.BELT)
names(df_postgres.BELT.test$activities)
#Accuracy : 0.8691 
#############################

###########TREE ANKLE_RIGHTtransition 2 4 5 7 8 10 11##########
df_postgres.ankle_righttransition = data[which(data$sensor_localisation=='ANKLE_RIGHT'),]
df_postgres.ankle_righttransition=df_postgres.ankle_righttransition[which(df_postgres.ankle_righttransition$activities%in%c("falling","lying down","on all fours","sitting down","standing up from lying","standing up from sitting on the ground","standing up from sitting")),]
df_postgres.ankle_righttransition<-df_postgres.ankle_righttransition[sample(nrow(df_postgres.ankle_righttransition), nrow(df_postgres.ankle_righttransition)), ]

df_postgres.ankle_righttransition.apprentissage<-df_postgres.ankle_righttransition[1:7195,]
df_postgres.ankle_righttransition.test<-df_postgres.ankle_righttransition[7196:10278,]

#inTrain <- createDataPartition(y=dataPE$activity, p=0.7, list=FALSE)
#df_postgres.ankle_righttransition.apprentissage <- df_postgres.ankle_righttransition[inTrain,]
#df_postgres.ankle_righttransition.test <- df_postgres.ankle_righttransition[-inTrain,]

df_postgres.ankle_righttransition.apprentissage.tree<-rpart(activities~x_coordinate+y_coordinate+z_coordinate,df_postgres.ankle_righttransition.apprentissage)
plot(df_postgres.ankle_righttransition.apprentissage.tree)
text(df_postgres.ankle_righttransition.apprentissage.tree)
#summary(df_postgres.ankle_righttransition.apprentissage.tree)
#df_postgres.ankle_righttransition.apprentissage.tree
prediction.ankle_righttransition<-predict(df_postgres.ankle_righttransition.apprentissage.tree,df_postgres.ankle_righttransition.test,type = "class")
#db<-cbind(df_postgres.ankle_righttransition.test,prediction)
#table(db$prediction,db$activities)
#nrow(db[which(db$activity=="1"),])#88%
#nrow(db[which(db$activity=="3"),])#88%
#nrow(db[which(db$activity=="6"),])#98%
#nrow(db[which(db$activity=="9"),])#69%
confusionMatrix(prediction.ankle_righttransition, df_postgres.ankle_righttransition.test$activities)


names(df_postgres.ankle_righttransition.test$activities)
#Accuracy : 0.5083
#############################

###########TREE ANKLE_LEFTtransition 2 4 5 7 8 10 11##########
df_postgres.ankle_lefttransition = data[which(data$sensor_localisation=='ANKLE_LEFT'),]
df_postgres.ankle_lefttransition=df_postgres.ankle_lefttransition[which(df_postgres.ankle_lefttransition$activities%in%c("falling","lying down","on all fours","sitting down","standing up from lying","standing up from sitting on the ground","standing up from sitting")),]
df_postgres.ankle_lefttransition<-df_postgres.ankle_lefttransition[sample(nrow(df_postgres.ankle_lefttransition), nrow(df_postgres.ankle_lefttransition)), ]

df_postgres.ankle_lefttransition.apprentissage<-df_postgres.ankle_lefttransition[1:7178,]
df_postgres.ankle_lefttransition.test<-df_postgres.ankle_lefttransition[7179:10255,]

#inTrain <- createDataPartition(y=dataPE$activity, p=0.7, list=FALSE)
#df_postgres.ankle_lefttransition.apprentissage <- df_postgres.ankle_lefttransition[inTrain,]
#df_postgres.ankle_lefttransition.test <- df_postgres.ankle_lefttransition[-inTrain,]

df_postgres.ankle_lefttransition.apprentissage.tree<-rpart(activities~x_coordinate+y_coordinate+z_coordinate,df_postgres.ankle_lefttransition.apprentissage)
plot(df_postgres.ankle_lefttransition.apprentissage.tree)
text(df_postgres.ankle_lefttransition.apprentissage.tree)
#summary(df_postgres.ankle_lefttransition.apprentissage.tree)
#df_postgres.ankle_lefttransition.apprentissage.tree
prediction.ankle_lefttransition<-predict(df_postgres.ankle_lefttransition.apprentissage.tree,df_postgres.ankle_lefttransition.test,type = "class")
#db<-cbind(df_postgres.ankle_lefttransition.test,prediction)
#table(db$prediction,db$activities)
#nrow(db[which(db$activity=="1"),])#88%
#nrow(db[which(db$activity=="3"),])#88%
#nrow(db[which(db$activity=="6"),])#98%
#nrow(db[which(db$activity=="9"),])#69%
confusionMatrix(prediction.ankle_lefttransition, df_postgres.ankle_lefttransition.test$activities)


names(df_postgres.ankle_lefttransition.test$activities)
#Accuracy : 0.4979
#############################

###########TREE CHESTtransition 2 4 5 7 8 10 11##########
df_postgres.CHESTtransition = data[which(data$sensor_localisation=='CHEST'),]
df_postgres.CHESTtransition=df_postgres.CHESTtransition[which(df_postgres.CHESTtransition$activities%in%c("falling","lying down","on all fours","sitting down","standing up from lying","standing up from sitting on the ground","standing up from sitting")),]
df_postgres.CHESTtransition<-df_postgres.CHESTtransition[sample(nrow(df_postgres.CHESTtransition), nrow(df_postgres.CHESTtransition)), ]
levels(df_postgres.CHESTtransition$activities)
#df_postgres.CHESTtransition.apprentissage<-df_postgres.CHESTtransition[1:7195,]
#df_postgres.CHESTtransition.test<-df_postgres.CHESTtransition[7196:10278,]

inTrain <- createDataPartition(y=df_postgres.CHESTtransition$activities, p=0.7, list=FALSE)
df_postgres.CHESTtransition.apprentissage <- df_postgres.CHESTtransition[inTrain,]
df_postgres.CHESTtransition.test <- df_postgres.CHESTtransition[-inTrain,]

df_postgres.CHESTtransition.apprentissage.tree<-rpart(activities~x_coordinate+y_coordinate+z_coordinate,df_postgres.CHESTtransition.apprentissage)
plot(df_postgres.CHESTtransition.apprentissage.tree)
text(df_postgres.CHESTtransition.apprentissage.tree)
#summary(df_postgres.CHESTtransition.apprentissage.tree)
#df_postgres.CHESTtransition.apprentissage.tree
prediction.CHESTtransition<-predict(df_postgres.CHESTtransition.apprentissage.tree,df_postgres.CHESTtransition.test,type = "class")
#db<-cbind(df_postgres.CHESTtransition.test,prediction)
#table(db$prediction,db$activities)
#nrow(db[which(db$activity=="1"),])#88%
#nrow(db[which(db$activity=="3"),])#88%
#nrow(db[which(db$activity=="6"),])#98%
#nrow(db[which(db$activity=="9"),])#69%
confusionMatrix(prediction.CHESTtransition, df_postgres.CHESTtransition.test$activities)

levels(prediction.CHESTtransition)
names(df_postgres.CHESTtransition.test$activities)
#Accuracy : 0.5673
#############################


###########TREE BELTtransition 2 4 5 7 8 10 11##########
df_postgres.BELTtransition = data[which(data$sensor_localisation=='BELT'),]
df_postgres.BELTtransition=df_postgres.BELTtransition[which(df_postgres.BELTtransition$activities%in%c("falling","lying down","on all fours","sitting down","standing up from lying","standing up from sitting on the ground","standing up from sitting")),]
df_postgres.BELTtransition<-df_postgres.BELTtransition[sample(nrow(df_postgres.BELTtransition), nrow(df_postgres.BELTtransition)), ]
levels(df_postgres.BELTtransition$activities)
#df_postgres.BELTtransition.apprentissage<-df_postgres.BELTtransition[1:7195,]
#df_postgres.BELTtransition.test<-df_postgres.BELTtransition[7196:10278,]

inTrain <- createDataPartition(y=df_postgres.BELTtransition$activities, p=0.7, list=FALSE)
df_postgres.BELTtransition.apprentissage <- df_postgres.BELTtransition[inTrain,]
df_postgres.BELTtransition.test <- df_postgres.BELTtransition[-inTrain,]

df_postgres.BELTtransition.apprentissage.tree<-rpart(activities~x_coordinate+y_coordinate+z_coordinate,df_postgres.BELTtransition.apprentissage)
rpart.plot(df_postgres.BELTtransition.apprentissage.tree)
text(df_postgres.BELTtransition.apprentissage.tree)
#summary(df_postgres.BELTtransition.apprentissage.tree)
#df_postgres.BELTtransition.apprentissage.tree
prediction.BELTtransition<-predict(df_postgres.BELTtransition.apprentissage.tree,df_postgres.BELTtransition.test,type = "class")
#db<-cbind(df_postgres.BELTtransition.test,prediction)
#table(db$prediction,db$activities)
#nrow(db[which(db$activity=="1"),])#88%
#nrow(db[which(db$activity=="3"),])#88%
#nrow(db[which(db$activity=="6"),])#98%
#nrow(db[which(db$activity=="9"),])#69%
confusionMatrix(prediction.BELTtransition, df_postgres.BELTtransition.test$activities)

levels(prediction.BELTtransition)
names(df_postgres.BELTtransition.test$activities)
#Accuracy : 0.5367
#############################

######################################

df_postgres.chest<-df_postgres.chest[sample(nrow(df_postgres.chest), nrow(df_postgres.chest)),]

summary(df_postgres.chest.test$activity)
df_postgres.chest.apprentissage11<-df_postgres.chest.apprentissage1[1:21942,]
df_postgres.chest.test1<-df_postgres.chest.apprentissage1[21943:31437,]
df_postgres.chest.apprentissage11.tree<-rpart(activity~x+y+z,df_postgres.chest.apprentissage1)
rpart.plot(df_postgres.chest.apprentissage11.tree)
summary(df_postgres.chest.apprentissage11.tree)
df_postgres.chest.apprentissage11.tree
prediction<-predict(df_postgres.chest.apprentissage11.tree,df_postgres.chest.test1,type = "class")
db<-cbind(df_postgres.chest.test1,prediction)
table(db$prediction,db$activity)
aze<-confusionMatrix(db$prediction,db$activity)
aze$overall
aze$byClass
#########################RANDOM FOREST###############

fit <- randomForest(activities~x_coordinate+y_coordinate+z_coordinate+Accelaration., data = df_postgres.chest.apprentissage, na.action = na.omit)
fit

varImpPlot(fit)

fit$importance
summary(df_postgres.chest.apprentissage$activities)
pred <- predict(fit,df_postgres.chest.test, type="class")
pred

confusionMatrix(pred, df_postgres.chest.test$activities)

############################

install.packages("caret")