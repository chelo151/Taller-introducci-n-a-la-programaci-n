# Taller-introduccion-a-la-programacion

## Cargar y Preprocesar los Datos

rm(list=ls())         #Borrar el entorno de trabajo

library("readr")      #Cargar libreria readr para abrir archivos csv

library("dplyr")      #Cargar libreria dplyr 

data <- read.csv("mubio02_act3_alimentos_nutrientes_4900.csv") #Cargar la base de datos

head(data)            #Mostrar las primeras filas del data frame data

data$id <- as.factor(data$id) #Convertir la columna id a factor


#### Verificar si hay valores NA y eliminarlos
colSums(is.na(data)) 
data <- data[complete.cases(data),]
any(is.na(data))


#### Estandarizar todas las columnas numéricas 
data_scale <- data %>%
  mutate(across(where(is.numeric), scale))

head(data_scale) #Mostrar las primeras filas del data frame data_scale

## Comprovar la normalidad de los datos 

library("nortest") # Cargar la libreria para poder utilizar la función ad.test

pvalor <- matrix(NA, nrow=ncol(data_scale), ncol=1) # Crear una matriz para registrar los p values

#### Utilizar un bucle para calcular la normalidad en cada columna
for (i in 2:ncol(data_scale)) {
  resultado_anderson <- ad.test(data_scale[[i]]) # Aplicar el test de Anderson-Darling a cada columna
  pvalor[i, ] <- resultado_anderson$p.value #Guardar el valor de p-value en la matriz
}

rownames(pvalor) <- colnames(data_scale) # Asignar el nombres a las filas de la matriz

pvalor # Mostrar la matriz con los p-value
