#visualisation each sensor
#visualisation each sequence/personne
############A01#########
  ##########CHEST##########
  ST_data_A01<-data[which(data$sequence=='A01'),]
  ST_data_A01_CHEST<-ST_data_A01[which(ST_data_A01$sensor_localisation=='CHEST'),]
  ST_data_A01_CHEST<-ST_data_A01_CHEST[,1:4]
  ST_data_A01_CHEST<-ST_data_A01_CHEST[order(ST_data_A01_CHEST$timestemp),]
  plot(x = ST_data_A01_CHEST$timestemp,y = ST_data_A01_CHEST$y ,type = "l")
  #plotXBySensor<-ggplot(data = ST_data_A01_CHEST, aes(x=ST_data_A01_CHEST$timestemp,y=ST_data_A01_CHEST$y_coordinate,ylab="coordinates",color="Y coordonate"))+geom_line()
  #vizualisation off X,Y,Z  ordred by TIME from CHEST sensor of the first Sequeance
  plotXBySensorA01CHEST<-ggplot(data = ST_data_A01_CHEST, aes(x=ST_data_A01_CHEST$timestemp))+geom_line(aes(y=ST_data_A01_CHEST$x_coordinate),color="red")+geom_line(aes(y=ST_data_A01_CHEST$y_coordinate),color="blue")+geom_line(aes(y=ST_data_A01_CHEST$z_coordinate),color="green")
  
  ggplotly(plotXBySensorA01CHEST)

  ##########ANKLE_LEFT##########
  ST_data_A01<-data[which(data$sequence=='A01'),]
  ST_data_A01_ANKLE_LEFT<-ST_data_A01[which(ST_data_A01$sensor_localisation=='ANKLE_LEFT'),]
  ST_data_A01_ANKLE_LEFT<-ST_data_A01_ANKLE_LEFT[,1:4]
  ST_data_A01_ANKLE_LEFT<-ST_data_A01_ANKLE_LEFT[order(ST_data_A01_ANKLE_LEFT$timestemp),]
  plot(x = ST_data_A01_ANKLE_LEFT$timestemp,y = ST_data_A01_ANKLE_LEFT$y ,type = "l")
  #plotXBySensor<-ggplot(data = ST_data_A01_ANKLE_LEFT, aes(x=ST_data_A01_ANKLE_LEFT$timestemp,y=ST_data_A01_ANKLE_LEFT$y_coordinate,ylab="coordinates",color="Y coordonate"))+geom_line()
  #vizualisation off X,Y,Z  ordred by TIME from ANKLE_LEFT sensor of the first Sequeance
  plotXBySensorA01ANKLE_LEFT<-ggplot(data = ST_data_A01_ANKLE_LEFT, aes(x=ST_data_A01_ANKLE_LEFT$timestemp))+geom_line(aes(y=ST_data_A01_ANKLE_LEFT$x_coordinate),color="red")+geom_line(aes(y=ST_data_A01_ANKLE_LEFT$y_coordinate),color="blue")+geom_line(aes(y=ST_data_A01_ANKLE_LEFT$z_coordinate),color="green")
  
  ggplotly(plotXBySensorA01ANKLE_LEFT)
  
  ##########ANKLE_RIGHT##########
  ST_data_A01<-data[which(data$sequence=='A01'),]
  ST_data_A01_ANKLE_RIGHT<-ST_data_A01[which(ST_data_A01$sensor_localisation=='ANKLE_RIGHT'),]
  ST_data_A01_ANKLE_RIGHT<-ST_data_A01_ANKLE_RIGHT[,1:4]
  ST_data_A01_ANKLE_RIGHT<-ST_data_A01_ANKLE_RIGHT[order(ST_data_A01_ANKLE_RIGHT$timestemp),]
  plot(x = ST_data_A01_ANKLE_RIGHT$timestemp,y = ST_data_A01_ANKLE_RIGHT$y ,type = "l")
  #plotXBySensor<-ggplot(data = ST_data_A01_ANKLE_RIGHT, aes(x=ST_data_A01_ANKLE_RIGHT$timestemp,y=ST_data_A01_ANKLE_RIGHT$y_coordinate,ylab="coordinates",color="Y coordonate"))+geom_line()
  #vizualisation off X,Y,Z  ordred by TIME from ANKLE_RIGHT sensor of the first Sequeance
  plotXBySensorA01ANKLE_RIGHT<-ggplot(data = ST_data_A01_ANKLE_RIGHT, aes(x=ST_data_A01_ANKLE_RIGHT$timestemp))+geom_line(aes(y=ST_data_A01_ANKLE_RIGHT$x_coordinate),color="red")+geom_line(aes(y=ST_data_A01_ANKLE_RIGHT$y_coordinate),color="blue")+geom_line(aes(y=ST_data_A01_ANKLE_RIGHT$z_coordinate),color="green")
  
  ggplotly(plotXBySensorA01ANKLE_RIGHT)
  
############A02#########
ST_data_A02<-data[which(data$sequence=='A02'),]
ST_data_A02_CHEST<-ST_data_A02[which(ST_data_A02$sensor_localisation=='CHEST'),]
ST_data_A02_CHEST<-ST_data_A02_CHEST[,1:4]
ST_data_A02_CHEST<-ST_data_A02_CHEST[order(ST_data_A02_CHEST$timestemp),]
#plot(x = ST_data_A02_CHEST$timestemp,y = ST_data_A02_CHEST$y ,type = "l")
#plotXBySensor<-ggplot(data = ST_data_A02_CHEST, aes(x=ST_data_A02_CHEST$timestemp,y=ST_data_A02_CHEST$y_coordinate,ylab="coordinates",color="Y coordonate"))+geom_line()
#vizualisation off X,Y,Z  ordred by TIME from CHEST sensor of the first Sequeance
plotXBySensorA02CHEST<-ggplot(data = ST_data_A02_CHEST, aes(x=ST_data_A02_CHEST$timestemp))+geom_line(aes(y=ST_data_A02_CHEST$x_coordinate),color="red")+geom_line(aes(y=ST_data_A02_CHEST$y_coordinate),color="blue")+geom_line(aes(y=ST_data_A02_CHEST$z_coordinate),color="green")

ggplotly(plotXBySensorA02CHEST)

############A03#########
ST_data_A03<-data[which(data$sequence=='A03'),]
ST_data_A03_CHEST<-ST_data_A03[which(ST_data_A03$sensor_localisation=='CHEST'),]
ST_data_A03_CHEST<-ST_data_A03_CHEST[,1:4]
ST_data_A03_CHEST<-ST_data_A03_CHEST[order(ST_data_A03_CHEST$timestemp),]
#plot(x = ST_data_A03_CHEST$timestemp,y = ST_data_A03_CHEST$y ,type = "l")
#plotXBySensor<-ggplot(data = ST_data_A03_CHEST, aes(x=ST_data_A03_CHEST$timestemp,y=ST_data_A03_CHEST$y_coordinate,ylab="coordinates",color="Y coordonate"))+geom_line()
#vizualisation off X,Y,Z  ordred by TIME from CHEST sensor of the first Sequeance
plotXBySensorA03CHEST<-ggplot(data = ST_data_A03_CHEST, aes(x=ST_data_A03_CHEST$timestemp))+geom_line(aes(y=ST_data_A03_CHEST$x_coordinate),color="red")+geom_line(aes(y=ST_data_A03_CHEST$y_coordinate),color="blue")+geom_line(aes(y=ST_data_A03_CHEST$z_coordinate),color="green")

ggplotly(plotXBySensorA03CHEST)

############A04#########
ST_data_A04<-data[which(data$sequence=='A04'),]
ST_data_A04_CHEST<-ST_data_A04[which(ST_data_A04$sensor_localisation=='CHEST'),]
ST_data_A04_CHEST<-ST_data_A04_CHEST[,1:4]
ST_data_A04_CHEST<-ST_data_A04_CHEST[order(ST_data_A04_CHEST$timestemp),]
#plot(x = ST_data_A04_CHEST$timestemp,y = ST_data_A04_CHEST$y ,type = "l")
#plotXBySensor<-ggplot(data = ST_data_A04_CHEST, aes(x=ST_data_A04_CHEST$timestemp,y=ST_data_A04_CHEST$y_coordinate,ylab="coordinates",color="Y coordonate"))+geom_line()
#vizualisation off X,Y,Z  ordred by TIME from CHEST sensor of the first Sequeance
plotXBySensorA04CHEST<-ggplot(data = ST_data_A04_CHEST, aes(x=ST_data_A04_CHEST$timestemp))+geom_line(aes(y=ST_data_A04_CHEST$x_coordinate),color="red")+geom_line(aes(y=ST_data_A04_CHEST$y_coordinate),color="blue")+geom_line(aes(y=ST_data_A04_CHEST$z_coordinate),color="green")

ggplotly(plotXBySensorA04CHEST)


#########RIEN w TBALBIZ##########
x2<-ST_data_A01_ANKLE_LEFT[1:869,]
X3<-ST_data_A01_CHEST[1:869,]
plotXBySensorRIEN<-ggplot(data = ST_data_A01_CHEST, aes(x=ST_data_A01_CHEST$timestemp))+geom_line(aes(y=ST_data_A01_CHEST$x_coordinate),color="red")+geom_line(aes(y=ST_data_A01_CHEST$y_coordinate),color="blue")+geom_line(aes(y=ST_data_A01_CHEST$z_coordinate),color="green")+geom_line(aes(y=x2$x_coordinate),color="purple")+geom_line(aes(y=x2$y_coordinate),color="black")+geom_line(aes(y=x2$z_coordinate),color="white")
plotXBySensorRIEN<-ggplot(data = ST_data_A01_ANKLE_RIGHT, aes(x=ST_data_A01_ANKLE_RIGHT$timestemp))+geom_line(aes(y=ST_data_A01_ANKLE_RIGHT$y_coordinate),color="blue")+geom_line(aes(y=x2$y_coordinate),color="red")+geom_line(aes(y=x3$y_coordinate),color="purple")
ggplotly(plotXBySensorRIEN)
par(mfrow=c(1,2))

