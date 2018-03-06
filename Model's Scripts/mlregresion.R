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


###Multilinear regression model

## Cross validation of Multilinear Regression

#Setting variables
liminf = 1
limsup = 200
tam = 200
inf=1
top=200
inf=1
RMSE.ML=NULL
GEN_ERR.ML = NULL
medias_ml=NULL
cont=1
cont1=1
contador=0
aux=0
contador = 0
aux=0
cont1=1
medias_ML_plot=NULL


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
      trainML = scaled_validtrain[-index,c("ss","X.cl.","X.aa.","X.st.","X.fsv.","X.p.")]
      validML = scaled_validtrain[index,c("X.cl.","X.aa.","X.st.","X.fsv.","X.p.")]
      targetML = scaled_validtrain[index,c("ss")]
      
      #Multilinear regression model
      fit <- lm(trainML$ss ~ trainML$X.cl + trainML$X.aa + trainML$X.st + trainML$X.fsv + trainML$X.p. , data=Datos)
      summary(fit)
      
      #Getting the coefficients
      coef<-coefficients(fit)
      
      #Evaluating the prediction
      y <- cbind(length(index))
      t=1

      for(l in 1:tam_in){
        y[t] <- coef[1] + (coef[2] * validML[l,1]) + (coef[3] * validML[l,2]) + (coef[4] * validML[l,3]) + 
          (coef[5] * validML[l,4]) + (coef[6] * validML[l,5])
        t=t+1
        
      }
      
      #Generalization Error
      GEN_ERR.ML[cont] = (sum((targetML - y)^2) / length(index))
      
      #Update variables
      top=top+tam_in
      inf=inf+tam_in
      cont=cont+1
      
    }
    aux=contador+k[j]
    medias_ML_plot[cont1]=mean(GEN_ERR.ML[contador:aux])
    contador=contador+k[j]
    cont1=cont1+1
  }
  
  limsup = limsup+tam
  liminf = liminf+tam

}

#Calculating the mean of each set

medias_4 = NULL
medias_8 = NULL
medias_10 = NULL
medias_20 = NULL
medias_50 = NULL
medias_100 = NULL


j=1
for(i in 1:5){
  
  medias_4[i]=medias_ML_plot[j]
  medias_8[i]=medias_ML_plot[j+1]
  medias_10[i]=medias_ML_plot[j+2]
  medias_20[i]=medias_ML_plot[j+3]
  medias_50[i]=medias_ML_plot[j+4]
  medias_100[i]=medias_ML_plot[j+5]
  j=j+6
  
}

#Plotting the graph

plot(c(1:5),medias_4,main = "Result of k-Cross Validation" , xlab = "Variation of sets", ylab = "Mean of Generalization Errors",col = "brown",type="l")
lines(c(1:5),medias_8,col = "BLUE")
lines(c(1:5),medias_10,col = "yellow")
lines(c(1:5),medias_20,col = "red")
lines(c(1:5),medias_50,col = "green")
lines(c(1:5),medias_50,col = "Black")

legend(0.9, 0.0218, legend=c("Four", "Eight","Ten","Twenty","Fifty","Hundred"),col=c("Brown", "Blue","yellow","red","green","black"),lty = 2:9, cex=0.5, title="Number of k divisions", text.font=4,box.lty = 0)


#Testing error ~ RMSE

#Setting variables

liminf = 1
limsup = 200
cont1=1
tam=200
RMSE.ML= NULL

for(i in 1:5){
  #Dividing the data 
  scaled_Test <- scaled[c(liminf:limsup),c("ss","X.cl.","X.aa.","X.st.","X.fsv.","X.p.")]
  scaled_validtrain <- scaled[-c(liminf:limsup),]
  
  trainML = scaled_validtrain[,c("ss","X.cl.","X.aa.","X.st.","X.fsv.","X.p.")]
  
  #Multilinear regression model
  fit <- lm(trainML$ss ~ trainML$X.cl + trainML$X.aa + trainML$X.st + trainML$X.fsv + trainML$X.p. , data=Datos)
  summary(fit)
  
  #Getting the coefficients
  coef<-coefficients(fit)
  b = 1:200
  predic <- cbind(length(b))
  
  
  for(l in 1:200){
    predic[l] <- coef[1] + (coef[2] * scaled_Test[l,2]) + (coef[3] * scaled_Test[l,3]) + (coef[4] * scaled_Test[l,4]) + 
      (coef[5] * scaled_Test[l,5]) + (coef[6] * scaled_Test[l,6])
  }
  
  #Calculating Root Mean Squared Error
  RMSE.ML[cont1] = (sum((scaled_Test$ss - predic)^2) / 200) ^ 0.5
  
  cont1= cont1+1
  
  limsup = limsup+tam
  liminf = liminf+tam
}