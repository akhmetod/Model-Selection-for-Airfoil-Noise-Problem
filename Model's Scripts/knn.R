#Installing xlsx package
install.packages("xlsx")
library("xlsx")

#Loading data
Datos <- read.xlsx("X1.xlsx",sheetIndex = 1,header = T)
X2 <- read.xlsx("X2.xlsx",sheetIndex = 1,header = T)

#Normalize the data by min-max criterium
max = apply(Datos[,c(1:9)],2,max)
min = apply(Datos[,c(1:9)],2,min)
scaled = as.data.frame(scale(Datos[,c(1:9)],center=min,scale = max-min))

###K-Nearest Neighbourgh

install.packages("FNN")
library(FNN)

## Cross validation of K-Nearest Neighbourgh

#Setting the data
liminf = 1
limsup = 200
tam = 200
inf=1
top=200
inf=1
contador=0
medias_knn=NULL
medias_kvecinos=NULL
RMSE.knn = NULL
GEN_ERR.knn = NULL
cont=1
cont1=1
contador = 0
aux=0
cont1=1


k=c(4,8,10,20,50,100)

  for(i in 1:5){
    
    scaled_Test <- scaled[c(liminf:limsup),c("ss","X.cl.","X.aa.","X.st.","X.fsv.","X.p.")]
    scaled_validtrain <- scaled[-c(liminf:limsup),]
    
    for(j in 1:6){
      tam_in = 800/k[j]
      inf=1
      top=tam_in
      
      for(n in 1:k[j]){
        #Dividing the data
        index <- c(inf:top)
        trainKnn = scaled_validtrain[-index,c("X.cl.","X.aa.","X.st.","X.fsv.","X.p.")]
        validKnn = scaled_validtrain[index,c("X.cl.","X.aa.","X.st.","X.fsv.","X.p.")]
        targetKnn = scaled_validtrain[index,c("ss")]
        traintKnn = scaled_validtrain[-index,c("ss")]
        
        #Calcule of K-Nearest-Neighbors and the prediction of the model
        pred = knn.reg(train = trainKnn, test = validKnn, traintKnn, k = 1)
        
        #Generalization Error
        GEN_ERR.knn[cont] = (sum((targetKnn - pred$pred)^2) / length(index))
        
        #Updating Variables
        top=top+tam_in
        inf=inf+tam_in
        cont=cont+1
        
      }
      aux=contador+k[j]
      medias_knn[cont1]=mean(GEN_ERR.knn[contador:aux])
      contador=contador+k[j]
      cont1=cont1+1
    }
    limsup = limsup+tam
    liminf = liminf+tam
  }



#Dividing the means of Generalization Errors
medias_4 = NULL
medias_8 = NULL
medias_10 = NULL
medias_20 = NULL
medias_50 = NULL
medias_100 = NULL


j=1
for(i in 1:5){
  
  medias_4[i]=medias_knn[j]
  medias_8[i]=medias_knn[j+1]
  medias_10[i]=medias_knn[j+2]
  medias_20[i]=medias_knn[j+3]
  medias_50[i]=medias_knn[j+4]
  medias_100[i]=medias_knn[j+5]
  j=j+6
  
}

#Plotting the results


plot(c(1:5),medias_4,main = "Result of k-Cross Validation" , xlab = "Variation of sets", ylab = "Mean of Generalization Errors",col = "brown",type="l",ylim = c(0.009,0.012))
lines(c(1:5),medias_8,col = "BLUE")
lines(c(1:5),medias_10,col = "yellow")
lines(c(1:5),medias_20,col = "red")
lines(c(1:5),medias_50,col = "green")
lines(c(1:5),medias_50,col = "Black")

legend(4.2, 0.012, legend=c("Four", "Eight","Ten","Twenty","Fifty","Hundred"),col=c("Brown", "Blue","yellow","red","green","black"),lty = 2:9, cex=0.5, title="Number of k divisions", text.font=4,box.lty = 0)



#Setting variables
liminf = 1
limsup = 200
cont1=1
tam=200
RMSE.knn= NULL

for(i in 1:5){
  #Dividing the data
  scaled_Test <- scaled[c(liminf:limsup),c("ss","X.cl.","X.aa.","X.st.","X.fsv.","X.p.")]
  scaled_validtrain <- scaled[-c(liminf:limsup),]
  
  
  
  #Dividing the data

  trainKnn = scaled_validtrain[,c("X.cl.","X.aa.","X.st.","X.fsv.","X.p.")]
  traintKnn = scaled_validtrain[,c("ss")]
  testeoknn <- scaled_Test[,c("X.cl.","X.aa.","X.st.","X.fsv.","X.p.")]
  targetpostknn = scaled_Test[,c("ss")]
  print(testeoknn)
  print(length(targetpostknn))
  
  #Calcule of K-Nearest-Neighbors and the prediction of the model
  pred = knn.reg(train = trainKnn, test = testeoknn, traintKnn, k = 1)
  
  
  #Calculating Root Mean Squared Error
  RMSE.knn[cont1] = (sum((targetpostknn - pred$pred)^2) / 200) ^ 0.5
  
  #Updating variables
  cont1= cont1+1
  limsup = limsup+tam
  liminf = liminf+tam

}



