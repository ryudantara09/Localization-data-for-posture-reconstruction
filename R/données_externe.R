getwd()
setwd("C:/Users/MBM info/Desktop/ActivityRecognition")
###################################librairie############################
library(mlbench)
library(class) #k-nearest neighbors
library(kknn) #weighted k-nearest neighbors
library(e1071) #SVM
library(caret) #select tuning parameters
library(reshape2) #assist in creating boxplots
library(ggplot2) #create boxplots
library(kernlab) #assist with SVM feature selection
library(pROC)
library(ROCR)
library(pROC)
library(corrplot)
library(doParallel)
library(parallel)
########################################################################
#********************   connexion avec mongoDB*********************************************#

library(RMongo)
library(mongolite)
my_collection = mongo(collection = "things", db = "mydb")

#********************  chargement des données *********************************************#

data_externe<-my_collection$iterate()$page()

#************************************Structure of the Data****************************************************#
#0- Standing 1- Walking 2- Sitting 3- Falling 4- Cramps 5- Running
summary(data_externe)
str(data_externe)
names(data_externe)
table(data_externe$ACTIVITY)
DT::datatable(data_externe[1:100,]) # First 100 observations

library(caret)
#************************************Modify Variable types**********************************#
data_externe$ACTIVITY  <- as.factor(data_externe$ACTIVITY)
data_externe$TIME  <- NULL
nzv = nearZeroVar(data_externe) # diagnose variables with zero or near-zero variability
#filtered_train_clean<- training [, -nzv] # remove Near Zero Variance (nzv) -variables

#****************************************Multivariate Analysis - Dimension(Variable) Reduction using Variable Clustering Approach***********************************#

######################2.2. Selection of Optimal Predictor Variables######################

id_correlated_predictors= findCorrelation(cor(data_externe[,2:7]), cutoff = 0.8, verbose = F)
id_correlated_predictors
preProc <- preProcess(data_externe[,2:7], method=c("BoxCox", "center", "scale", "pca"), thresh = .95)
trainn = predict(preProc,data_externe) # create training dataframe for the 24 Principal Components 
trainn = movetolast(trainPC, c("trainPC")) # set the 'classe' variable as the last column
#######################################important predictors##################################################################

correlations <- cor(training[,2:6])
print(correlations)

highCorrelations <- findCorrelation(correlations, cutoff = .75, verbose = TRUE)

print(highCorrelations)
plot(varImp(model.lm))
train <- training[,-highCorrelations]
model.lm <- train(ACTIVITY ~ .,
                  data = train,
                  method = "lm")

plot(varImp(model.lm))
################################################################################################

############################CAH##########################################


d_externe <- dist(data_externe) # method="man" # is a bit better
hc_externe <- hclust(d_externe, method = "complete")
data_activité <- rev(levels(data_externe[,1]))

library(dendextend)
dend <- as.dendrogram(hc_externe)
# order it the closest we can to the order of the observations:
dend <- rotate(dend, 1:150)

# Color the branches based on the clusters:
dend <- color_branches(dend, k=6) #, groupLabels=iris_species)

# Manually match the labels, as much as possible, to the real classification of the flowers:
library(RColorBrewer)
labels_colors(dend) <-
  rainbow_hcl(6)[sort_levels_values(
    as.numeric(data_externe[,1])[order.dendrogram(dend)]
  )]

# We shall add the flower type to the labels:
labels(dend) <- paste(as.character(data_externe[,1])[order.dendrogram(dend)],
                      "(",labels(dend),")", 
                      sep = "")
# We hang the dendrogram a bit:
dend <- hang.dendrogram(dend,hang_height=0.1)
# reduce the size of the labels:
# dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
dend <- set(dend, "labels_cex", 0.5)
# And plot:
par(mar = c(3,3,3,7))
plot(dend, 
     main = "Clustered Iris data set
     (activity recognetion)", 
     horiz =  TRUE,  nodePar = list(cex = .007))
####################################kmeans############################################
#kmeans clustering clasification, euclidean distance computation and multidimensional scaling
id<-data_externe[,2:7]
km.output <-kmeans(id,6,iter.max = 1000, nstart = 1, algorithm = "MacQueen", trace =F)
km.dis<-dist(id,method = "euclidean")#distance matrix
km.scaled<-cmdscale(km.dis,k=6)## Multidimensional scaling
km.centroid<-km.output$centers;km.clus<-km.output$cluster
#cluster plot
plot(cmdscale(km.dis), col = data_externe[,1])
legend("bottomright", levels(data_externe[,1]), col=1:6, pch=1)
text(km.scaled[,1],km.scaled[,2], km.output$cluster, pos=4)
#eliptical clustered plot
install.packages("clusplot")
library(clusplot)
library("cluster", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
install.packages("cluster")
library(cluster)
clusplot(id, km.clus, color=T, shade=T, labels=0, lines=0, main = "activity Data")
#########################################################################

#****************************************data viz *************************************************************
qplot(data = training, x = ACTIVITY, fill = ACTIVITY)

ggplot(data=training, aes(training$ACTIVITY, fill = training$ACTIVITY)) + 
  geom_bar(colour="#FF9999") + 
  labs(title="Bar Plot for Class Variable") +
  labs(x="Class", y="Count") +
  scale_fill_brewer(palette="Spectral", name = "Class")
library(ggplot2)
p.SL <- ggplot(data_externe, aes(ACTIVITY,SL))
p.SL <- p.SL + geom_boxplot() + ggtitle("SL")
p.EEG <- ggplot(data_externe, aes(ACTIVITY, EEG))
p.EEG <- p.EEG + geom_boxplot() + ggtitle("EEG")
p.BP <- ggplot(data_externe, aes(ACTIVITY, BP))
p.BP <- p.BP + geom_boxplot() + ggtitle("BP")
p.HR <- ggplot(data_externe, aes(ACTIVITY, HR))
p.HR <- p.HR + geom_boxplot() + ggtitle("HR")
p.CIRCLUATION <- ggplot(data_externe, aes(ACTIVITY, CIRCLUATION))
p.CIRCLUATION <- p.CIRCLUATION + geom_boxplot() + ggtitle("CIRCLUATION")

gridExtra::grid.arrange(p.SL,p.EEG, p.BP , p.HR,p.CIRCLUATION,ncol = 3, nrow = 2)
#********************************** Data partition****************************************************************#
data_externe<-data_externe[sample(nrow( data_externe), nrow( data_externe)),]

inTrain <- createDataPartition(y=data_externe$ACTIVITY, p=0.7, list=FALSE)
#select training sample 
train<-data_externe[div_part,] # 70% here
training <- data_externe[inTrain,]
testing <- data_externe[-inTrain,]
dim(training)
dim(testing)

#*********************************Model building with Training Data****************************************************************


library(rpart)
library(rpart)
library(rpart.plot)
library(RCurl)
library(knitr)
library(plyr)
library(ggbiplot)
library(rCharts)
library(qcc)
library(threejs)
library(rgl)
library(pca3d)
library(gridExtra)



#*************************************** LDA ****************************************************************"
library(MASS) #Load package 'MASS' to perform LDA
#fit.LDA = lda( activity ~ x_coordinate + y_coordinate + z_coordinate , train.train)
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
fit.lda_externe <- train(ACTIVITY~., data=training, method="lda", metric=metric, trControl=control)
fit.LDA.C = predict(fit.lda, newdata=test.train)
confusionMatrix(fit.LDA.C, test.train$activity)
table(train.train[,8],fit.LDA.C)

pima.melt = melt(data_externe, id.var="ACTIVITY")
ggplot(data=pima.melt, aes(x=ACTIVITY, y=value)) + geom_boxplot() + facet_wrap(~variable, ncol=2)

# mochkil fiha 

#############################Random Forest.#####################################

rf1 <- randomForest(ACTIVITY~ ., data=training, importance=T )
modelo.rf  <- randomForest( training$ACTIVITY~., data = training, importance=TRUE,ntree=100)
modelo.rf
varImpPlot(modelo.rf)
nuevotest<-testing
rf.predict <- predict(modelo.rf, nuevotest)
rf.cfmx.tb <- table(rf.predict, testing$ACTIVITY)
rf.cfmx.tb
confusionMatrix(rf.predict, testing$ACTIVITY)

df <- as.matrix(rf.cfmx.tb)
colnames(df) = c("Standing", "Walking", "Sitting", "Falling", "Cramps", "Running")
rownames(df) = c("Standing", "Walking", "Sitting", "Falling", "Cramps", "Running")

image(df[,ncol(df):1], axes=FALSE)
axis(2, at = seq(0, 1, length=length(colnames(df))), labels=colnames(df))
heatmap(t(df)[ncol(df):1,], Rowv=NA, Colv=NA, col = rainbow(20, start=.7, end=.1))

#############################Classification tree.#####################################

library(tree)
train.tree <- tree(factor(ACTIVITY)~.,data=training)
par(mfrow=c(1,1))
prediction_tree <- predict(train.tree, testing, type = "class")
confusionMatrix(prediction_tree, testing$ACTIVITY)

plot(train.tree)
text(train.tree, cex=0.6)
summary(train.tree)


#############################Classification Rpart.#####################################


library(rpart)
modFit1 <- rpart(ACTIVITY ~ .,method="class",data=training)
library(rpart.plot)
rpart.plot(modFit1, main="Decision Tree", extra=102, under=TRUE, faclen=0)
prediction1 <- predict(modFit1, testing, type = "class")
confusionMatrix(prediction1, testing$ACTIVITY)
##################################regression logistique polytomique #####################################
library(nnet)
global <- multinom(ACTIVITY~., data=training)
pred <-predict(global, newdata = testing)
confusionMatrix(pred, testing$ACTIVITY)

################################################################################################

