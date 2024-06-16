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


## Realizar el PCA de alimentos y nurtientes

library(factoextra) # Librería para realizar PCA
library(ggplot2) # Librería para crear gráficos

#### Realizar PCA seleccionando todas las columnas a partir de la número 21
pca <- prcomp(data_scale[, 21:ncol(data_scale)], center = TRUE, scale. = TRUE)

summary(pca) # Resumen del PCA

#### Extraer los valores propios/varianzas de los componentes principales

eigenvalues<- get_eigenvalue(pca)

head(eigenvalues)

#### Generar un scree plot para la elección del número de componentes principales

fviz_eig(pca, addlabels = TRUE)

#### Extraer los resultados de las variables 
var <- get_pca_var(pca)

#### Calcular el valo de Rcuadrado para cada componente

pca_sdev <- as.data.frame(pca$sdev)
pca_sdev

#### Mostrar las cargas para cada variable 
pca_rotation <- as.data.frame(pca$rotation)
pca_rotation

## Crear gráficos de los componentes principales

#### Gráfico para visualizar las dos primeras componentes principales
fviz_pca_ind(pca, geom.ind = "point", 
             col.ind = "blue", 
             axes = c(1, 2), 
             pointsize = 1.5) 

#### Visualizar el gráfico de correlación de las variable 
fviz_pca_var(pca, col.var = "cos2", 
             geom.var = "arrow", 
             labelsize = 2, 
             repel = FALSE)
