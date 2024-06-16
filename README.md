# Taller-introduccion-a-la-programacion


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
