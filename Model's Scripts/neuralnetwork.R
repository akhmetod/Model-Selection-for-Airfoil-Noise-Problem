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

#Multilayer Perceptron

install.packages("neuralnet")
library("neuralnet")


### Cross validation of neural network model

#Installing relevant libraries
install.packages("boot")
install.packages("plyr")

# Load libraries
library(boot)
library(plyr)

#Setting variable  

liminf = 1
limsup = 200
tam = 200
inf=1
top=200
inf=1
medias_NN=NULL
medias_kneurons=NULL
medias_kneuronsSegundavez=NULL
medias_klaiers=NULL
RMSE.NN = NULL
GEN_ERR.NN = NULL
cont=1
cont1=1
contador=0
aux=0
cont1=1
medias_NN_plot=NULL

k=c(4,8,10,20)

for(i in 1:5){
    
    scaled_Test <- scaled[c(liminf:limsup),c("ss","X.cl.","X.aa.","X.st.","X.fsv.","X.p.")]
    scaled_validtrain <- scaled[-c(liminf:limsup),]
    
    for(j in 1:4){
      tam_in = 800/k[j]
      inf=1
      top=tam_in
      
      for(n in 1:k[j]){
        #Dividing the data 
        index <- c(inf:top)
        trainNN = scaled_validtrain[-index,c("ss","X.cl.","X.aa.","X.st.","X.fsv.","X.p.")]
        validNN = scaled_validtrain[index,c("X.cl.","X.aa.","X.st.","X.fsv.","X.p.")]
        targetNN = scaled_validtrain[index,c("ss")]
        
        #Calcule the neural network
        NN = neuralnet(trainNN$ss ~ trainNN$X.cl + trainNN$X.aa + trainNN$X.st + trainNN$X.fsv + trainNN$X.p. , trainNN, hidden = c(15) ,stepmax = 300000, linear.output = T )
        #Predict of neural network
        predict_testNN = compute(NN,validNN)
        predict_testNN = (predict_testNN$net.result * (max(targetNN) - min(targetNN))) + min(targetNN)
        
        #Generalization error
        GEN_ERR.NN[cont]<- (sum((targetNN - predict_testNN)^2) / length(index))
        
        #Updating variables
        top=top+tam_in
        inf=inf+tam_in
        cont=cont+1
      }
      aux=contador+k[j]
      medias_NN_plot[cont1]=mean(GEN_ERR.NN[contador:aux])
      contador=contador+k[j]
      cont1=cont1+1
 
    }
    limsup = limsup+tam
    liminf = liminf+tam
  }


#Setting variables
liminf = 1
limsup = 200
cont1=1
tam=200
RMSE.NN= NULL



  for(i in 1:5){
    #Dividing the data
    scaled_Test <- scaled[c(liminf:limsup),c("ss","X.cl.","X.aa.","X.st.","X.fsv.","X.p.")]
    scaled_validtrain <- scaled[-c(liminf:limsup),]
    trainNN = scaled_validtrain[,c("ss","X.cl.","X.aa.","X.st.","X.fsv.","X.p.")]
    
    #Calcule of neural network
    NN = neuralnet(trainNN$ss ~ trainNN$X.cl + trainNN$X.aa + trainNN$X.st + trainNN$X.fsv + trainNN$X.p. , trainNN, hidden = c(5,5),stepmax = 300000, linear.output = T )
    
    
    testeoNN <- scaled_Test[,c("X.cl.","X.aa.","X.st.","X.fsv.","X.p.")]
    targetpostNN = scaled_Test[,c("ss")]
    
    #Prediction of neural network
    predict_testNN = compute(NN,testeoNN)
    predict_testNN = (predict_testNN$net.result * (max(targetpostNN) - min(targetpostNN))) + min(targetpostNN)
  
    #Calcule of Root Mean Square Error
    RMSE.NN[cont1] = (sum((targetpostNN - predict_testNN)^2) / 200) ^ 0.5
    
    cont1= cont1+1
    limsup = limsup+tam
    liminf = liminf+tam
  }
  

#Means of Generalization Error

medias_4_2 = NULL
medias_8_2 = NULL
medias_10_2 = NULL
medias_20_2 = NULL

j=1
for(i in 1:4){
  
  medias_4_2[i]= medias_NN_plot[j]
  medias_8_2[i]= medias_NN_plot[j+1]
  medias_10_2[i]= medias_NN_plot[j+2]
  medias_20_2[i]= medias_NN_plot[j+3]
  
  j=j+4
}

gen_f_2= NULL



gen_f_2[1] = mean(medias_4_2)
gen_f_2[2] = mean(medias_8_2)
gen_f_2[3] = mean(medias_10_2)
gen_f_2[4] = mean(medias_20_2)


#Plotting the graph to study variation of number of neurons

plot(c(4,8,10,20),gen_f_1,col = "gray",main = "Generalization Error to study the number of neurons",xlab = "Values of k=(4,8,10,20)",ylab = "Generalization Error",ylim = c(0.005,0.017))

lines(c(4,8,10,20),gen_f_2,col = "pink")
lines(c(4,8,10,20),gen_f_3,col = "yellow")
lines(c(4,8,10,20),gen_f_4,col = "green")
lines(c(4,8,10,20),gen_f_5,col = "black")
lines(c(4,8,10,20),gen_f_6,col = "brown")
lines(c(4,8,10,20),gen_f_7,col = "orange")
lines(c(4,8,10,20),gen_f_8,col = "red")
lines(c(4,8,10,20),gen_f_9,col = "blue")


legend(16, 0.016, legend=c("Three", "Four","Five"),col=c("red", "green","blue"),lty = 2:9, cex=0.5, title="Number Neurons first layer", text.font=4,box.lty = 0)


#To do predictions of X2

X2 <- read.xlsx("X2.xlsx",sheetIndex = 1,header = T)

#Normalize the data by min-max criterium
max = apply(X2[,c(1:8)],2,max)
min = apply(X2[,c(1:8)],2,min)
scaled_X2 = as.data.frame(scale(X2[,c(1:8)],center=min,scale = max-min))


#Dividing the data

trainNN = scaled[,c("ss","X.cl.","X.aa.","X.st.","X.fsv.","X.p.")]

#Calcule of neural network
NN = neuralnet(trainNN$ss ~ trainNN$X.cl + trainNN$X.aa + trainNN$X.st + trainNN$X.fsv + trainNN$X.p. , trainNN, hidden = c(15),stepmax = 300000, linear.output = T )

#Predict of neural network
predict_testNN = compute(NN,X2[,c("X.cl.","X.aa.","X.st.","X.fsv.","X.p.")])
#predict_testNN = (predict_testNN$net.result * (max(target_X2) - min(targer_X2))) + min(target_X2)
#RMSE.NN = (sum((target_X2 - predict_testNN)^2) / 200) ^ 0.5