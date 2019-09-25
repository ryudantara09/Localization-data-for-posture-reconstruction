DATANeural<-data[,c("x_coordinate","z_coordinate","y_coordinate","activities","sensor_localisation")]
DATANeural<-DATANeural[which(DATANeural$sensor_localisation=="CHEST"),]
DATANeural$x_coordinate<-(DATANeural$x_coordinate - min(DATANeural$x_coordinate))/(max(DATANeural$èx_coordinate)-min(DATANeural$x_coordinate))
DATANeural$y_coordinate<-(DATANeural$y_coordinate - min(DATANeural$y_coordinate))/(max(DATANeural$y_coordinate)-min(DATANeural$y_coordinate))
DATANeural$z_coordinate<-(DATANeural$z_coordinate - min(DATANeural$z_coordinate))/(max(DATANeural$z_coordinate)-min(DATANeural$z_coordinate))


set.seed(123)
DATANeural<-DATANeural[,-4]
inTrain <- createDataPartition(y=DATANeural$activities, p=0.7, list=FALSE)
DATANeural.apprentissage <- DATANeural[inTrain,]
DATANeural.test <- DATANeural[-inTrain,]
neuralnetwork<-neuralnet(Output~x_coordinate+y_coordinate+z_coordinate,
                          data = DATANeural.apprentissage,2,
                          linear.output = TRUE,stepmax = 10000000,lifesign="full")
neuralnetwork2<-neuralnet(Output~x_coordinate+y_coordinate+z_coordinate,
                         data = DATANeural.apprentissage,1,
                         linear.output = TRUE,stepmax = 10000000,lifesign="full")
str(DATANeural)
levels(DATANeural$activities)
DATANeural[DATANeural$activities=="falling","Output"]<-1
DATANeural[DATANeural$activities=="lying","Output"]<-2
DATANeural[DATANeural$activities=="lying down","Output"]<-3
DATANeural[DATANeural$activities=="on all fours","Output"]<-4
DATANeural[DATANeural$activities=="sitting","Output"]<-5
DATANeural[DATANeural$activities=="sitting down","Output"]<-6
DATANeural[DATANeural$activities=="sitting on the ground","Output"]<-7
DATANeural[DATANeural$activities=="standing up from lying","Output"]<-8
DATANeural[DATANeural$activities=="standing up from sitting","Output"]<-9
DATANeural[DATANeural$activities=="standing up from sitting on the ground","Output"]<-10
DATANeural[DATANeural$activities=="walking","Output"]<-11
#install.packages("caret", dependencies=c("Depends", "Suggests"))
plot(neuralnetwork)

output<-compute(x = neuralnetwork, DATANeural.test[,-4])
neuralnetwork.result<-output$net.result
plot(DATANeural.test$Output, neuralnetwork.result)#, col='blue', pch=16, ylab = "predicted activity NN", xlab = "real activity")
RMSE.NN = (sum((DATANeural.test$Output - neuralnetwork.result)^2) / nrow(DATANeural.test)) ^ 0.5
RMSE.NN
head(neuralnetwork)


output2<-compute(x = neuralnetwork2, DATANeural.test[,-4])
neuralnetwork2.result<-output2$net.result
plot(DATANeural$activities, neuralnetwork2.result, col='blue', pch=16, ylab = "predicted activity NN", xlab = "real activity")
RMSE2.NN = (sum((DATANeural$activities - neuralnetwork2.result)^2) / nrow(DATANeural)) ^ 0.5
RMSE.NN