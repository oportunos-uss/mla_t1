###########################################################################
# TALLER 1 : Arboles de decisión
# fecha : 30/09/2021
# Machine Learning Avanzado.
#
###########################################################################

#instalar paquete xgboost, luego cargar libreria-----------------------------
install.packages("xgboost")
library(xgboost)

#instalar paquete caret, luego cargar libreria-----------------------------
install.packages("caret")
library(caret)

#carga de los dataset
csv1 = "https://raw.githubusercontent.com/mchandia-uss/data/main/T1M1.csv"
csv2 = "https://raw.githubusercontent.com/mchandia-uss/data/main/T1M2.csv"
matriz1 <- read.csv(csv1,sep=";")
matriz2 <- read.csv(csv2,sep=";")

#trabajo con matriz 1
#generar particion aleatoria-----------------------------------------------
set.seed(2021)
sub <- sample(nrow(matriz1), floor(nrow(matriz1) * 0.7))
train<-matriz1[sub, ] #conjunto de entrenamiento 70 %
test<-matriz1[-sub, ] #conjunto de prueba 30%

#cambiar la salida a 0 (no) y 1(si) ---------------------------------------
salidatrain<-ifelse(train$Compra=="si",1,0)
salidatest<-ifelse(test$Compra=="si",1,0)

trainxgb<-train
testxgb<-test

trainxgb$Compra<-salidatrain
testxgb$Compra<-salidatest
