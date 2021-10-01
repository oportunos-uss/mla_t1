###########################################################################
# TALLER 1 : Arboles de decisión
# fecha : 30/09/2021
# Machine Learning Avanzado.
# Grupo 1.
###########################################################################

#instalar paquete caret, luego cargar libreria-----------------------------
install.packages("caret")
library(caret)

#instalar paquete rpart, luego cargar libreria-----------------------------
install.packages("rpart")
library(rpart)

#instalar paquete rpart, luego cargar libreria-----------------------------
install.packages("e1071")
library(e1071)

install.packages('rattle')
library(rattle)
install.packages('rpart.plot')
library(rpart.plot)

#carga de los dataset
csv1 = "https://raw.githubusercontent.com/mchandia-uss/data/main/T1M1.csv"
csv2 = "https://raw.githubusercontent.com/mchandia-uss/data/main/T1M2.csv"
matriz1 <- read.csv(csv1,header = TRUE,sep=";")
matriz2 <- read.csv(csv2,header = TRUE,sep=";")

##################################### #####################################
########################### trabajo con matriz 1 ##########################
##################################### #####################################

################# Métrica de entropía #####################################
#generar particion aleatoria-----------------------------------------------
set.seed(2021)
sub <- sample(nrow(matriz1), floor(nrow(matriz1) * 0.7))
train<-matriz1[sub, ] #conjunto de entrenamiento 70 %
test<-matriz1[-sub, ] #conjunto de prueba 30%

arbol2 <- rpart(Compra ~ ., data=train,control =rpart.control(minsplit =1, cp=0),
                parms = list(split = "information"))
plot(arbol2,uniform=T,margin=0.2)
text (arbol2, use.n = T, pretty = TRUE)
title("Training Set's Classification Compra")

fancyRpartPlot(arbol2)

#evaluar en conjunto de prueba---------------------------------------------
predictions2 <- predict(arbol2, test, type="class")

confusionMatrix(table(predictions2,test$Compra)) 

#Poda (probar con cp=0.01, cp=0.03)----------------------------------------
prune.arbol2 <- prune(arbol2, cp=0.01)
plot(prune.arbol2,uniform=T,margin=0.2)
text(prune.arbol2, use.n = T, pretty = TRUE)

fancyRpartPlot(prune.arbol2)

################# Random forests ##########################################
#instalar paquete randomForest, luego cargar libreria----------------------
install.packages("randomForest")
library(randomForest)

str(train)

# Nota: Se cambio el atribuito de salida "char" a "Factor"
train$Compra <- as.factor(train$Compra)
str(train)

forest <- randomForest(Compra ~ ., data=train, importance=TRUE)
predictionsrf <- predict(forest, newdata = test)
confusionMatrix(table(predictionsrf, test$Compra))

# Plot: Variable importance
varImpPlot(forest,type=2) 

##################################### #####################################
########################### trabajo con matriz 2 ##########################
##################################### #####################################

matriz2<-matriz2[,-c(1)]
matriz2

################# Métrica de entropía #####################################
#generar particion aleatoria-----------------------------------------------
set.seed(2021)
sub2 <- sample(nrow(matriz2), floor(nrow(matriz2) * 0.7))
train2<-matriz2[sub2, ] #conjunto de entrenamiento 70 %
test2<-matriz2[-sub2, ] #conjunto de prueba 30%

arbol3 <- rpart(PlayTennis ~ ., data=train2,control =rpart.control(minsplit =1, cp=0),
                parms = list(split = "information"))
plot(arbol3,uniform=T,margin=0.2)
text (arbol3, use.n = T, pretty = TRUE)
title("Training Set's Classification PlayTennis")

fancyRpartPlot(arbol3)

#evaluar en conjunto de prueba---------------------------------------------
predictions3 <- predict(arbol3, test2, type="class")

confusionMatrix(table(predictions3,test2$PlayTennis)) 

#Poda (probar con cp=0.01, cp=0.03)----------------------------------------
prune.arbol3 <- prune(arbol3, cp=0.01)
plot(prune.arbol3,uniform=T,margin=0.2)
text(prune.arbol3, use.n = T, pretty = TRUE)

fancyRpartPlot(prune.arbol3)

################# Random forests ##########################################

str(train2)

# Nota: Se cambio el atribuito de salida "char" a "Factor"
train2$PlayTennis <- as.factor(train2$PlayTennis)
str(train2)

forest2 <- randomForest(PlayTennis ~ ., data=train2, importance=TRUE)
predictionsrf2 <- predict(forest2, newdata = test2)
confusionMatrix(table(predictionsrf2, test2$PlayTennis))

# Plot: Variable importance
varImpPlot(forest2,type=2) 

