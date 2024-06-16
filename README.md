# Taller-introduccion-a-la-programacion

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
