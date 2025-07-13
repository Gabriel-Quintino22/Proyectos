#Proyecto paso a pass Regresiones

# R con su librería caret nos ofrecen muchos modelos de machine learning.
# Lo genial de caret es que es sencilla de usar y poderosa a la vez.
# No olvidar: primero entrenamos el modelo y luego con los datos de testeo se prueba.





# 1. Regresiones ----


# Cargamos los datos
library(readxl)
library(stargazer)
datos <- read_excel("D:/Documentos/Metodos Estadisticos 3/IPS-DATAX/Base de datos R y Python/Datos_Modelo.xlsx")

datos <- read_excel("D:/Documentos/Metodos Estadisticos 3/IPS-DATAX/Base de datos R y Python/Datos_Modelo.xlsx",
                    sheet = "Datos")


# Revisamos los datos
head(datos)



# Modelo 1 Salario ~ Experiencia
modelo_1 <- lm(Salario ~ Experiencia, data = datos)
summary(modelo_1)
stargazer(modelo_1, title = "Regresion simple" , type = "text" )








# Modelo 2 Salario ~ Experiencia + Años_Educación
modelo_2 <- lm(Salario ~ Experiencia + Años_Educación, data = datos)
summary(modelo_2)
stargazer(modelo_2, title = "Regresion Multiple", type = "text")


# Modelo 3 Salario ~ Experiencia + Años_Educación + Tamaño_Empresa
modelo_3 <- lm(Salario ~ Experiencia + Años_Educación + Tamaño_Empresa, data = datos)
summary(modelo_3)





# 2. Contenidos ----


# Instalamos los paquetes (si no están instalados)
install.packages("tidyverse") # Procesamiento de datos
install.packages("caret") # Machine learning
library(tidyverse) 
library(caret) 



# Transformamos Tamaño_Empresa a factor
datos$Tamaño_Empresa
class(datos$Tamaño_Empresa)
as.factor(datos$Tamaño_Empresa)
class(as.factor(datos$Tamaño_Empresa))
datos$Tamaño_Empresa <- as.factor(datos$Tamaño_Empresa)
class(datos$Tamaño_Empresa)


# Creamos los índices de entrenamiento al azar
set.seed(123)
createDataPartition(datos$Salario, p = 0.8, list = FALSE)
trainIndex <- createDataPartition(datos$Salario, p = 0.8, list = FALSE)



# Creamos los datos de entrenamiento
datos[trainIndex,]
trainData <- datos[trainIndex, ]



# Creamos los datos de testeo
datos[-trainIndex, ]
testData <- datos[-trainIndex, ]



# Entrenamos los modelos con los modelos de machine learning
modelo_rl <- train(Salario ~ ., data = trainData, method = "lm")       # Regresión Lineal
modelo_dt <- train(Salario ~ ., data = trainData, method = "rpart")    # Árbol de decisión
modelo_rf <- train(Salario ~ ., data = trainData, method = "rf")       # Random Forest
modelo_knn <- train(Salario ~ ., data = trainData, method = "knn")     # K-Nearest Neighbors



# Guardamos todos los modelos entrenados en una lista
modelos <- list(
  "Regresión Lineal" = modelo_rl,
  "Árbol de Decisión" = modelo_dt,
  "Random Forest" = modelo_rf,
  "KNN" = modelo_knn
)



# Revisamos algunos valores de la lista
modelos
names(modelos)
modelos$`Regresión Lineal`
modelos[["Regresión Lineal"]] # Este se suele utilizar en los ciclos



# Almacenamos el modelo para un futuro uso
modelos[["Random Forest"]]
modelo <- modelos[["Random Forest"]]



# Realizamos predicciones con la data de testeo y el modelo indicado
predict(modelo, newdata = testData)
pred <- predict(modelo, newdata = testData)



# Calculamos las métricas con las predicciones de la data de testeo
R2(pred, testData$Salario)
r2 <- R2(pred, testData$Salario)

MAE(pred, testData$Salario)
mae <- MAE(pred, testData$Salario)

RMSE(pred, testData$Salario)
rmse <- RMSE(pred, testData$Salario)



# Creamos un data.frame vacío para guardar los resultados
data_vacia <- data.frame(
  Modelo = character(), 
  R2 = double(), 
  MAE = double(), 
  RMSE = double()
)



# Unimos dataframes verticalmente
data_ejemplo1 <- data.frame(Modelo = 1, R2 = 2, MAE = 3, RMSE = 4)
data_ejemplo1

data_ejemplo2 <- data.frame(Modelo = 100, R2 = 200, MAE = 300, RMSE = 400)
data_ejemplo2

rbind(data_ejemplo1, data_ejemplo2)





# 3. Proyecto ----

# Creamos un dataframe vacío, para almacenar los resultados
resultados <- data.frame(Modelo = character(), R2 = double(), MAE = double(), RMSE = double())

names(modelos)

# Evaluamos los modelos ya entrenados y capturamos sus métricas
for (nombre in names(modelos)) {
  
  # Extraemos el modelo desde la lista
  modelo <- modelos[[nombre]]  
  
  # Realizamos una predicción con el modelo extraído
  pred <- predict(modelo, newdata = testData)
  
  # Calculamos las métricas
  r2 <- R2(pred, testData$Salario)
  mae <- MAE(pred, testData$Salario)
  rmse <- RMSE(pred, testData$Salario)
  
  # Unimos los resultados verticalmente en un dataframe
  resultados <- rbind(resultados, data.frame(Modelo = nombre, R2 = r2, MAE = mae, RMSE = rmse))
  
}



# Mostramos los resultados
print(resultados)
