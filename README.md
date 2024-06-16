# Taller-introduccion-a-la-programacion

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
