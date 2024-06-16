# Taller-introduccion-a-la-programacion

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

