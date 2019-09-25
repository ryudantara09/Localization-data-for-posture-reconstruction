summary(data)
names(data)
table(data)

str(data)
##############CLUSTERING##################
data.samples.shuffled<-data[sample(nrow(data), nrow(data)), ]
data.classifi.daisy <- daisy((data[1:10000,-7]), metric="gower", stand=FALSE)
data.classifi.daisy_ACH <- hclust(data.classifi.daisy,method="ward.D2")
plot(data.classifi.daisy_ACH)

demande.daisy<-NULL
data.classifi.daisy<-NULL
data.samples.shuffled<-NULL
data.classifi.daisy_ACH<-NULL
demande_credit<-NULL
classification.data<-agnes(data[1:3000,-7],method="ward")
plot(classification.data,xlab="individu",main="")
classif2<-as.hclust(classification.data)
plot(rev(classif2$height), type="h", ylab="hauteurs")
classes<-cutree(classif2,k=11)
plot(classification.data,xlab="individu",main="dendogram")


###########ANALYSE FACTORIELLE#########

dataquanty<-select_if(data, is.numeric)
DATA_FAMD <- PCA(dataquanty, graph=FALSE) 
plot(DATA_FAMD,choix = "var")
barplot(DATA_FAMD$eig[,1], main="Valeurs propres",
        names.arg=paste("dim",1:nrow(DATA_FAMD$eig)))
plot(DATA_FAMD$eig[,1], main="Critère du coude",type="b",
     axes=F,xlab="",yla="valeurs propres")
axis(1, 1:nrow(DATA_FAMD$eig),paste("dim",1:nrow(DATA_FAMD$eig)))
axis(2)

data = na.omit(data)
dataCor = cor(data[1:3000,1:5],method = "pearson" , use = "complete.obs")
highlyCorrelated <- findCorrelation(dataCor, names = TRUE,cutoff=.80)
print(highlyCorrelated)
DATAHigh = data[ , !names(data) %in% highlyCorrelated]
newData = nearZeroVar(DATAHigh, uniqueCut = 1000,saveMetrics = FALSE,
                      names = TRUE)
print(newData)
plot(dataCor)
plot(DATA_FAMD, choix="var",invisible="quali")
#plot(DATA_FAMD, choix="var",invisible="quali",main="correlation des variables quantitatives")

dataNeeded = DATAHigh[ , !names(DATAHigh) %in% newData]
