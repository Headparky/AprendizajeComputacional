library(ggplot2)

credit <- read.csv("crx.data", header = FALSE, na.strings = "?")

names(credit) <- c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", 
                   "A9", "A10", "A11", "A12", "A13", "A14", "A15", "class")

credit.trainIdx<-readRDS("credit.trainIdx.rds")
credit.Datos.Train<-credit[credit.trainIdx,]
credit.Datos.Test<-credit[-credit.trainIdx,]

nrow(credit.Datos.Train)
nrow(credit.Datos.Test)

summary(credit)

# Distribucion de variable de clase (probablemente sea el sexo)
table(credit$class)

colSums(is.na(credit))

# Ver tipo de cada variable
sapply(credit, class)

numeric_vars <- names(credit)[sapply(credit, is.numeric)]
categorical_vars <- names(credit)[sapply(credit, is.factor) | sapply(credit, is.character)]

cat("\nAnalisis de la variable: A2\n")
print(summary(credit[["A2"]]))
#Representamos histograma de longitud del sepalo
myhist = ggplot(data=credit,aes(A2)) +
  geom_histogram(col="orange",fill="orange",alpha=0.3,
                 breaks=seq(12, 81, by=0.5), na.rm = TRUE) + 
  labs(title="Histograma A2") 
#Marca el valor de la media con una línea azul vertical
myhist = myhist + geom_vline(xintercept = mean(credit$A2, na.rm = TRUE),
                             col="blue")
#Marca el valor de la mediana con una línea roja
myhist = myhist + geom_vline(xintercept = median(credit$A2, na.rm = TRUE),
                             col="red")
myhist

summary(credit$A2)

for(var in numeric_vars) {
  cat("\n\nAnálisis de", var, ":\n")
  print(summary(credit[[var]]))
  # Desvío estándar
  cat("Desviación estándar:", sd(credit[[var]], na.rm=TRUE), "\n")
  # Coeficiente de variación
  cat("Coef. de variación:", 100*sd(credit[[var]], na.rm=TRUE)/mean(credit[[var]], na.rm=TRUE), "\n")
}

# Histogramas de todas las variables numéricas
par(mfrow=c(2,3))
for(var in numeric_vars) {
  hist(credit[[var]], main=paste("Histograma de", var), xlab=var)
}

# Boxplots de todas las variables numéricas
par(mfrow=c(2,3))
for(var in numeric_vars) {
  boxplot(credit[[var]], main=paste("Boxplot de", var))
}
# 5. ANÁLISIS DE VARIABLES CATEGÓRICAS
# Frecuencias y proporciones
for(var in categorical_vars) {
  cat("\n\nAnálisis de", var, ":\n")
  # Frecuencias absolutas
  print(table(credit[[var]]))
  # Proporciones
  print(prop.table(table(credit[[var]])))
}

# Gráficos de barras para variables categóricas
par(mfrow=c(3,3))
for(var in categorical_vars) {
  barplot(table(credit[[var]]), main=paste("Distribución de", var))
}

# 6. RELACIÓN CON LA VARIABLE DE CLASE (OBJETIVO) 
# Para variables numéricas
par(mfrow=c(2,3))
for(var in numeric_vars) {
  boxplot(credit[[var]] ~ credit$class, 
          main=paste(var, "vs Class"),
          xlab="Class", ylab=var)
}

# Para variables categóricas
for(var in categorical_vars) {
  cat("\n\nTabla de contingencia para", var, "vs class:\n")
  print(table(credit[[var]], credit$class))
  # Test Chi-cuadrado
  print(chisq.test(table(credit[[var]], credit$class)))
}

# 7. CORRELACIONES ENTRE VARIABLES NUMÉRICAS
# Matriz de correlación
numeric_data <- credit[, numeric_vars]
correlation_matrix <- cor(numeric_data, use="complete.obs")
print(correlation_matrix)

# Visualización de correlaciones
library(corrplot)
corrplot(correlation_matrix, method="color", 
         type="upper", order="hclust",
         addCoef.col = "black",
         tl.col="black", tl.srt=45,
         title="Matriz de Correlaciones")

# 8. ANÁLISIS DE OUTLIERS EN VARIABLES NUMÉRICAS
for(var in numeric_vars) {
  # Calcular Q1, Q3 e IQR
  Q1 <- quantile(credit[[var]], 0.25, na.rm=TRUE)
  Q3 <- quantile(credit[[var]], 0.75, na.rm=TRUE)
  IQR <- Q3 - Q1
  
  # Identificar outliers
  outliers <- credit[[var]][credit[[var]] < (Q1 - 1.5*IQR) | 
                              credit[[var]] > (Q3 + 1.5*IQR)]
  
  cat("\nOutliers en", var, ":\n")
  cat("Número de outliers:", length(outliers), "\n")
  cat("Porcentaje de outliers:", 
      round(length(outliers)/length(credit[[var]])*100, 2), "%\n")
}

# 9. ANÁLISIS DE DISTRIBUCIÓN
library(moments)
for(var in numeric_vars) {
  cat("\nAnálisis de distribución para", var, ":\n")
  # Asimetría
  cat("Asimetría:", skewness(credit[[var]], na.rm=TRUE), "\n")
  # Curtosis
  cat("Curtosis:", kurtosis(credit[[var]], na.rm=TRUE), "\n")
  # Test de normalidad
  print(shapiro.test(credit[[var]]))
}

# 10. RESUMEN VISUAL COMPLETO CON GGPLOT2
library(ggplot2)
library(gridExtra)

# Para variables numéricas
plots_list <- list()
for(var in numeric_vars) {
  p1 <- ggplot(credit, aes_string(x=var)) +
    geom_histogram(fill="skyblue", bins=30) +
    theme_minimal() +
    ggtitle(paste("Distribución de", var))
  
  p2 <- ggplot(credit, aes_string(x="class", y=var)) +
    geom_boxplot(fill="skyblue") +
    theme_minimal() +
    ggtitle(paste(var, "por Clase"))
  
  plots_list[[length(plots_list) + 1]] <- p1
  plots_list[[length(plots_list) + 1]] <- p2
}

do.call(grid.arrange, c(plots_list, ncol=2))






