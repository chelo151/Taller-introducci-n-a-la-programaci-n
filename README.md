# Taller-introduccion-a-la-programacion


## Cargar y Preprocesar los Datos

rm(list=ls()) #Borrar el entorno de trabajo
library("readr") #Cargar libreria readr para abrir archivos csv
library("dplyr") #Cargar libreria dplyr 

data <- read.csv("mubio02_act3_alimentos_nutrientes_4900.csv") #Cargar la base de datos

head(data)  #Mostrar las primeras filas del data frame data

data$id <- as.factor(data$id) # Convertir la columna id a factor

#### Verificar si hay valores NA y eliminarlos
colSums(is.na(data)) 
data <- data[complete.cases(data),]
any(is.na(data))

##### Estandarizar todas las columnas numéricas 
data_scale <- data %>%
  mutate(across(where(is.numeric), scale))

head(data_scale) #Mostrar las primeras filas del data frame data_scale
