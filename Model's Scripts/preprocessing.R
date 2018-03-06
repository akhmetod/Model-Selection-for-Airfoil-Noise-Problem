#Installing xlsx package
install.packages("xlsx")
library("xlsx")

#Loading data
Datos <- read.xlsx("X1.xlsx",sheetIndex = 1,header = T)
X2 <- read.xlsx("X2.xlsx",sheetIndex = 1,header = T)

#Univariable analysis of the data
summary(Datos)


#Bivariable analysis of the data
pairs(~Datos$ss+Datos$X.cl.+Datos$X.aa.+Datos$X.fsv.+Datos$X.st.+Datos$X.p.+Datos$X.f.,data=Datos)
cov(Datos$X.p.,Datos$X.f.)/(sqrt(var(Datos$X.p.)*var(Datos$X.f.)))

#Study of outliers
boxplot(scaled[,c(2:9)],main="The Study of Outliers in X2" )
boxplot(scaled_X2,main="The Study of Outliers in X2" )

#Normalization of the data
max = apply(Datos[,c(1:9)],2,max)
min = apply(Datos[,c(1:9)],2,min)
scaled = as.data.frame(scale(Datos[,c(1:9)],center=min,scale = max-min))

#Wrapper algorithms --> Boruta
install.packages("Boruta")

#Loading librarys
library(ranger)
library(Boruta)

#Initializing seed
set.seed(123)


#Applying algorithm
boruta.train <- Boruta(scaled[,c("X.cl.","X.aa.","X.m.","X.st.","X.r.","X.fsv.","X.p.")],scaled$ss,doTrace=2)
print(boruta.train)

#Plotting result
plot(boruta.train,xlab= "",xaxt="n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)


