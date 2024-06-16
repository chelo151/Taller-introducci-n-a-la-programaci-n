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

##Crear una tabla descriptiva

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

## Crear tablas descriptivas

### Realizar PCA solo de las variabls sociodemográficas
pca2 <- prcomp(data_scale[,2:21], center = TRUE, scale. = TRUE)

#### Extraer los componentes principales en un data frame
pca_data <- as.data.frame(pca2$x)

#### Calcular los terciles del componente1 y componente2

##### Cargar las librerías necesarias
library(gtsummary) #Cargar libreria gtsummary para crear tablas resumen
library(gt)

#### Calcular los terciles para PC1
terciles <- quantile(pca_data$PC1, probs = c(0.33, 0.66))

#### Calcular los terciles para PC2
terciles2 <- quantile(pca_data$PC2, probs = c(0.33, 0.66))

#### Crear la nueva variable (Tercil_PC1) categórica basada en los terciles calculados
datos <- data %>%
  mutate(Tercil_PC1 = case_when(
    pca_data$PC1 <= terciles[1] ~ 1,
    pca_data$PC1 <= terciles[2] ~ 2,
    TRUE ~ 3
  ))

#### Crear la nueva variable (Tercil_PC2) categórica basada en los terciles calculados
datos2 <- datos %>%
  mutate(Tercil_PC2 = case_when(
    pca_data$PC2 <= terciles[1] ~ 1,
    pca_data$PC2 <= terciles[2] ~ 2,
    TRUE ~ 3
  ))


#### Convertir Tercil_PC1 a factor
datos$Tercil_PC1 <- as.factor(datos$Tercil_PC1)

#### Convertir Tercil_PC2 a factor
datos2$Tercil_PC2 <- as.factor(datos2$Tercil_PC2)


#### Crear un vector con las columnas que se quieren añadir al nuevo data frame
columnas <- c("edad", "altura", "peso", "IMC", "sexo", "Tercil_PC1")

#### Crear un data frame con las columnas anteriores
datos_PC1 <- datos %>% select(all_of(columnas))
datos_PC2 <- datos2 %>% select(all_of(columnas))


### Crear una tabla descriptiva del componente1

#### Crear una tabla resumen usando gtsummary solo con las variables numéricas y Tercil_PC1
tabla <- datos_PC1 %>%
  tbl_summary(
    by = Tercil_PC1, # Crear tabla según terciles
    type = all_continuous() ~ "continuous",
    statistic = all_continuous() ~ "{median} ({p25} - {p75})", # Calcular mediana y rangos intercuartílicos para todas las variables continuas
    missing_text = "(missing)"
  ) %>%
  add_p(
    test = all_continuous() ~ "kruskal.test", # Buscar el valor p usando la prueba de Kruskal-Wallis
    pvalue_fun = ~ style_pvalue(.x, digits = 3) # Valor p con 3 decimales
  ) %>%
  as_gt() # Convertir la tabla de gtsummary a un objeto gt
tabla

gt::gtsave(tabla, "~/Desktop/BIOINFORMÁTICA/Estadistica y R para ciencias de la salu//tabla.docx") # Guardar la tabla en un archivo .docx


### Crear una tabla descriptiva del componente2

#### Crear una tabla resumen usando gtsummary solo con las variables numéricas y Tercil_PC1
tabla <- datos_PC2 %>%
  tbl_summary(
    by = Tercil_PC1, # Crear tabla según terciles
    type = all_continuous() ~ "continuous",
    statistic = all_continuous() ~ "{median} ({p25} - {p75})", # Calcular mediana y rangos intercuartílicos para todas las variables continuas
    missing_text = "(missing)"
  ) %>%
  add_p(
    test = all_continuous() ~ "kruskal.test", # Buscar el valor p usando la prueba de Kruskal-Wallis
    pvalue_fun = ~ style_pvalue(.x, digits = 3) # Valor p con 3 decimales
  ) %>%
  as_gt() # Convertir la tabla de gtsummary a un objeto gt
tabla

gt::gtsave(tabla, "~/Desktop/BIOINFORMÁTICA/Estadistica y R para ciencias de la salu//tabla.docx") # Guardar la tabla en un archivo .docx


## Implementar un modelo de regresión logística

#### Verificar la clase de la variable Tercil_PC1
class(datos2$Tercil_PC1)

#### Convertir Tercil_PC1 y Tercil_PC2 a factores
datos2$Tercil_PC1 <- as.factor(datos2$Tercil_PC1)
datos2$Tercil_PC2 <- as.factor(datos2$Tercil_PC2)

#### Renombrar las columnas para evitar conflictos con nombres
datos2 <- datos2 %>%
  rename(
    Tercil_PC1_ = Tercil_PC1,
    Tercil_PC2_ = Tercil_PC2
  )

### Aplicar el modelo de regresión logística
modelo_logistica <- glm(diab_prev ~ Tercil_PC1_ + Tercil_PC2_, data = datos2, family = "binomial")

#### Mostrar un resumen del modelo de regresión logística
summary(modelo_logistica)

#### Calcular los intervalos de confianza para los coeficientes del modelo
confint(modelo_logistica) # Intervalo de confianza

#### Calcular las Odds Ratios (OR) a partir de los coeficientes del modelo
exp(modelo_logistica$coefficients) # Las OR
exp(coef(modelo_logistica)) # Las OR

#### Calcular los intervalos de confianza para las Odds Ratios
exp(confint(modelo_logistica)) # Intervalo de confianza de las OR
