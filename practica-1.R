# Cargar librerías necesarias
library(caret)

# Cargar los datos
credit <- read.table("crx.data", 
                     sep=",", 
                     na.strings="?")

# Cargar los índices proporcionados para train/test
credit.trainIdx <- readRDS("credit.trainIdx.rds")

# Dividir en train y test
credit.Datos.Train <- credit[credit.trainIdx,]
credit.Datos.Test <- credit[-credit.trainIdx,]

# Verificar dimensiones
nrow(credit.Datos.Train)  # Debe ser 553
nrow(credit.Datos.Test)   # Debe ser 137

# Estructura de los datos
str(credit)

# Resumen estadístico
summary(credit)

# Verificar valores faltantes
na_count <- colSums(is.na(credit))
print("Número de valores faltantes por columna:")
print(na_count)

# Ver las primeras filas
head(credit)

# Modificamos el comando de carga ya que los datos tienen una estructura específica
credit <- read.csv(unz("credit+approval.zip", "crx.data"), 
                   header=FALSE, 
                   sep=",", 
                   na.strings="?")

# Nombramos las columnas de manera descriptiva
names(credit) <- c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", 
                   "A9", "A10", "A11", "A12", "A13", "A14", "A15", "Class")

na_count <- colSums(is.na(credit))
print("Valores faltantes por columna:")
print(na_count)

# Convertir las variables categóricas a factores
cat_vars <- c("A1","A4","A5","A6","A7","A9","A10","A12","A13","Class")
credit[cat_vars] <- lapply(credit[cat_vars], FUN=as.factor)

# Análisis básico de las variables categóricas
for(var in cat_vars) {
  print(paste("Distribución de", var))
  print(table(credit[[var]], useNA="ifany"))
}

# Análisis básico de las variables numéricas
num_vars <- setdiff(names(credit), cat_vars)
summary(credit[num_vars])

# Visualización inicial de las variables numéricas
library(ggplot2)

# Histogramas para variables numéricas
for(var in num_vars) {
  p <- ggplot(credit, aes_string(x=var)) +
    geom_histogram(bins=30, fill="blue", alpha=0.5) +
    theme_minimal() +
    ggtitle(paste("Distribución de", var))
  print(p)
}

# Boxplots para ver relación con la clase
for(var in num_vars) {
  p <- ggplot(credit, aes_string(x="Class", y=var)) +
    geom_boxplot() +
    theme_minimal() +
    ggtitle(paste(var, "por Clase"))
  print(p)
}

# 1. Análisis detallado de valores faltantes
missing_analysis <- data.frame(
  n_missing = colSums(is.na(credit)),
  pct_missing = round(colSums(is.na(credit))/nrow(credit)*100, 2)
)
print("Análisis de valores faltantes:")
print(missing_analysis[missing_analysis$n_missing > 0,])

# 2. Identificar variables numéricas y categóricas
num_vars <- sapply(credit, is.numeric)
cat_vars <- sapply(credit, is.factor)

# 3. Revisar desbalanceo en variables categóricas
cat_balance <- lapply(credit[,cat_vars], table)
print("Distribución de variables categóricas:")
print(cat_balance)

# 4. Identificar valores extremos en variables numéricas
boxplot_stats <- lapply(credit[,num_vars], boxplot.stats)
outliers_count <- sapply(boxplot_stats, function(x) length(x$out))
print("Número de outliers por variable numérica:")
print(outliers_count)

# 1. Tratamiento de valores faltantes
# Para variables numéricas: mediana
# Para variables categóricas: moda
preproc_na <- function(x) {
  if(is.numeric(x)) {
    x[is.na(x)] <- median(x, na.rm = TRUE)
  } else if(is.factor(x)) {
    mode_val <- names(sort(table(x), decreasing = TRUE))[1]
    x[is.na(x)] <- mode_val
  }
  return(x)
}

credit_clean <- as.data.frame(lapply(credit, preproc_na))

# 2. Tratamiento de outliers para variables numéricas
# Usaremos el método IQR para identificar y tratar outliers
handle_outliers <- function(x) {
  if(!is.numeric(x)) return(x)
  
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  
  x[x < lower_bound] <- lower_bound
  x[x > upper_bound] <- upper_bound
  return(x)
}

credit_clean[,num_vars] <- lapply(credit_clean[,num_vars], handle_outliers)

# 3. Normalización de variables numéricas
# Usamos preProcess de caret para normalizar
preproc_num <- preProcess(credit_clean[,num_vars], method = c("center", "scale"))
credit_clean[,num_vars] <- predict(preproc_num, credit_clean[,num_vars])

# 4. Codificación de variables categóricas
# Usamos dummyVars de caret para crear variables dummy
dummy <- dummyVars(" ~ .", data = credit_clean[,cat_vars])
credit_dummy <- predict(dummy, credit_clean[,cat_vars])

# 5. Combinar todo en un dataset final
credit_final <- cbind(credit_clean[,num_vars], credit_dummy)

# 6. Dividir en train y test usando los índices proporcionados
credit.Datos.Train <- credit_final[credit.trainIdx,]
credit.Datos.Test <- credit_final[-credit.trainIdx,]

# Verificar que no hay valores faltantes
print("Valores faltantes después del preprocesado:")
print(colSums(is.na(credit.Datos.Train)))

# Verificar la normalización de variables numéricas
print("Resumen de variables numéricas después de la normalización:")
print(summary(credit.Datos.Train[,num_vars]))

# Verificar las dimensiones finales
print("Dimensiones del conjunto de entrenamiento:")
print(dim(credit.Datos.Train))
print("Dimensiones del conjunto de test:")
print(dim(credit.Datos.Test))

library(corrplot)
library(caret)

# 1. Matriz de correlación para variables numéricas
cor_matrix <- cor(credit_clean[,num_vars], use = "complete.obs")
print("Matriz de correlación de variables numéricas:")
print(round(cor_matrix, 2))

# Visualización de la matriz de correlación
corrplot(cor_matrix, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         title = "Correlaciones entre variables numéricas")

# 2. Identificar correlaciones altas
high_cor <- findCorrelation(cor_matrix, cutoff = 0.7)
if(length(high_cor) > 0) {
  print("Variables altamente correlacionadas que podrían eliminarse:")
  print(names(credit_clean[,num_vars])[high_cor])
}

# 3. Para variables categóricas, usamos el V de Cramer
# Función para calcular V de Cramer
cramer_v <- function(x, y) {
  confusion_matrix <- table(x, y)
  chi_square <- chisq.test(confusion_matrix)$statistic
  n <- sum(confusion_matrix)
  min_dim <- min(nrow(confusion_matrix), ncol(confusion_matrix)) - 1
  sqrt(chi_square / (n * min_dim))
}

# Calcular V de Cramer para todas las combinaciones de variables categóricas
cat_vars_names <- names(credit_clean)[cat_vars]
n_cat <- length(cat_vars_names)
cramer_matrix <- matrix(NA, n_cat, n_cat)
rownames(cramer_matrix) <- cat_vars_names
colnames(cramer_matrix) <- cat_vars_names

for(i in 1:n_cat) {
  for(j in 1:n_cat) {
    if(i != j) {
      cramer_matrix[i,j] <- cramer_v(credit_clean[[cat_vars_names[i]]], 
                                     credit_clean[[cat_vars_names[j]]])
    }
  }
}

print("Matriz de V de Cramer para variables categóricas:")
print(round(cramer_matrix, 2))

# Visualizar correlaciones entre variables categóricas
corrplot(cramer_matrix, method = "color", type = "upper", 
         title = "V de Cramer entre variables categóricas")

# 4. Análisis de la relación con la variable objetivo
# Para variables numéricas
print("Correlación con la variable objetivo (para variables numéricas):")
for(var in names(credit_clean[,num_vars])) {
  correlation <- cor.test(as.numeric(credit_clean$Class == "+"), 
                          credit_clean[[var]], 
                          use = "complete.obs")
  print(paste(var, ":", round(correlation$estimate, 3), 
              "p-value:", round(correlation$p.value, 4)))
}

# Para variables categóricas
print("V de Cramer con la variable objetivo (para variables categóricas):")
for(var in cat_vars_names[cat_vars_names != "Class"]) {
  v <- cramer_v(credit_clean[[var]], credit_clean$Class)
  print(paste(var, ":", round(v, 3)))
}
# Para variables numéricas
num_vars <- sapply(credit, is.numeric)
cor_matrix <- cor(credit[,num_vars], use = "complete.obs")
print("Matriz de correlación:")
print(cor_matrix)

# Para valores faltantes
missing_prop <- colMeans(is.na(credit))
print("Proporción de valores faltantes:")
print(missing_prop)

# Para valores únicos
unique_prop <- sapply(credit, function(x) length(unique(x))/length(x))
print("Proporción de valores únicos:")
print(unique_prop)

# 1. Para variables numéricas: correlación con Class
num_vars <- c("A2", "A3", "A8", "A11", "A14", "A15")
class_numeric <- as.numeric(credit$Class == "+")

# Correlación con la clase
cor_with_class <- sapply(num_vars, function(var) {
  cor(credit[[var]], class_numeric, use = "complete.obs")
})
print("Correlación de variables numéricas con Class:")
print(cor_with_class)

# 2. Para variables categóricas: V de Cramer
cat_vars <- c("A1", "A4", "A5", "A6", "A7", "A9", "A10", "A12", "A13")

# Función corregida para calcular V de Cramer
cramer_v <- function(x, y) {
  tbl <- table(x, y)
  chi2 <- chisq.test(tbl)
  n <- sum(tbl)
  min_dim <- min(nrow(tbl), ncol(tbl)) - 1
  v <- sqrt(chi2$statistic / (n * min_dim))
  return(c(v = v, p_value = chi2$p.value))
}

# Calcular V de Cramer para cada variable categórica con Class
cramer_results <- sapply(cat_vars, function(var) {
  cramer_v(credit[[var]], credit$Class)
})

print("V de Cramer de variables categóricas con Class y p-valores:")
print(cramer_results)


# Variables a eliminar
vars_to_remove <- c("A1", "A12", "A13", "A14")

# Crear nuevo dataset sin estas variables
credit_reduced <- credit[, !names(credit) %in% vars_to_remove]

# ----------------------------------------------------------------------

# # # 2. Identificar tipos de variables
# # num_vars <- c("A2", "A3", "A8", "A11", "A15")
# # cat_vars <- setdiff(names(credit_reduced), c(num_vars, "Class"))

# # # 3. Tratar valores faltantes
# # # Para variables numéricas: mediana
# # # Para variables categóricas: moda
# # for(var in num_vars) {
# #   if(any(is.na(credit_reduced[[var]]))) {
# #     credit_reduced[[var]][is.na(credit_reduced[[var]])] <- 
# #       median(credit_reduced[[var]], na.rm = TRUE)
# #   }
# # }

# for(var in cat_vars) {
#   if(any(is.na(credit_reduced[[var]]))) {
#     mode_val <- names(sort(table(credit_reduced[[var]]), decreasing = TRUE))[1]
#     credit_reduced[[var]][is.na(credit_reduced[[var]])] <- mode_val
#   }
# }

# # 4. Normalizar variables numéricas usando preProcess de caret
# preproc_num <- preProcess(credit_reduced[num_vars], method = c("center", "scale"))
# credit_reduced[num_vars] <- predict(preproc_num, credit_reduced[num_vars])

# # 5. Codificar variables categóricas usando dummyVars
# # Primero aseguramos que las variables categóricas sean factores
# credit_reduced[cat_vars] <- lapply(credit_reduced[cat_vars], as.factor)

# dummy <- dummyVars(" ~ .", data = credit_reduced[c(cat_vars)])
# credit_dummy <- predict(dummy, credit_reduced[c(cat_vars)])

# 6. Combinar variables numéricas normalizadas con las dummy
credit_final2 <- cbind(credit_reduced[num_vars],
                      credit_dummy,
                      Class = credit_reduced$Class)

# 7. Dividir en train y test usando los índices proporcionados
credit.Datos.Train <- credit_final2[credit.trainIdx,]
credit.Datos.Test <- credit_final2[-credit.trainIdx,]

# Verificar el resultado final
print("Dimensiones del conjunto de entrenamiento:")
print(dim(credit.Datos.Train))
print("Dimensiones del conjunto de test:")
print(dim(credit.Datos.Test))

# Verificar que no hay valores faltantes
print("Valores faltantes en conjunto de entrenamiento:")
print(sum(is.na(credit.Datos.Train)))
print("Valores faltantes en conjunto de test:")
print(sum(is.na(credit.Datos.Test)))

# Verificar la estructura de los datos
print("Estructura del conjunto de entrenamiento:")
str(credit.Datos.Train)

#-----------------------------------------------------------

library(ggplot2)
library(gridExtra)

# 1. Análisis de variables numéricas normalizadas
# Crear histogramas y densidades para cada variable numérica
num_plots <- list()
for(var in num_vars) {
  p <- ggplot(credit.Datos.Train, aes_string(x = var, fill = "Class")) +
    geom_density(alpha = 0.5) +
    theme_minimal() +
    ggtitle(paste("Distribución de", var, "por Clase")) +
    theme(plot.title = element_text(size = 10))
  num_plots[[var]] <- p
}

# Mostrar todos los gráficos de variables numéricas
grid.arrange(grobs = num_plots, ncol = 2)

# 2. Boxplots para ver la separación de clases en variables numéricas
num_boxplots <- list()
for(var in num_vars) {
  p <- ggplot(credit.Datos.Train, aes_string(x = "Class", y = var)) +
    geom_boxplot(aes(fill = Class)) +
    theme_minimal() +
    ggtitle(paste("Boxplot de", var)) +
    theme(plot.title = element_text(size = 10))
  num_boxplots[[var]] <- p
}

grid.arrange(grobs = num_boxplots, ncol = 2)

# 3. Análisis de variables dummy (categóricas transformadas)
dummy_vars <- setdiff(names(credit.Datos.Train), c(num_vars, "Class"))

# Proporción de cada categoría por clase
prop_plots <- list()
for(var in dummy_vars) {
  prop_table <- prop.table(table(credit.Datos.Train[[var]], credit.Datos.Train$Class), 1)
  prop_data <- as.data.frame(prop_table)
  names(prop_data) <- c("Value", "Class", "Proportion")
  
  p <- ggplot(prop_data, aes(x = Value, y = Proportion, fill = Class)) +
    geom_bar(stat = "identity", position = "fill") +
    theme_minimal() +
    ggtitle(paste("Proporción de clases en", var)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 10))
  prop_plots[[var]] <- p
}

# Mostrar algunos gráficos de variables dummy (ajustar ncol según el número de variables)
grid.arrange(grobs = head(prop_plots, 25), ncol = 5)

# 4. Análisis de correlaciones entre variables numéricas transformadas
cor_matrix <- cor(credit.Datos.Train[num_vars])
library(corrplot)
corrplot(cor_matrix, method = "color", 
         type = "upper", 
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         title = "Correlaciones entre variables numéricas normalizadas")

# 5. Resumen estadístico de las variables numéricas transformadas
summary_stats <- summary(credit.Datos.Train[num_vars])
print("Resumen estadístico de variables numéricas normalizadas:")
print(summary_stats)

# Renombrar los niveles de la clase en los conjuntos de entrenamiento y test
credit.Datos.Train$Class <- factor(credit.Datos.Train$Class, 
                                   levels = c("+", "-"),
                                   labels = c("positive", "negative"))

credit.Datos.Test$Class <- factor(credit.Datos.Test$Class, 
                                  levels = c("+", "-"),
                                  labels = c("positive", "negative"))

# Verificar los nuevos niveles
print("Nuevos niveles de la variable Class:")
print(levels(credit.Datos.Train$Class))

# Ahora podemos proceder con el entrenamiento
set.seed(123)  # Para reproducibilidad
# Configuración del control de entrenamiento
ctrl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  verboseIter = TRUE
)

# 1. Modelo LDA (simple)
set.seed(123)
model_lda <- train(
  Class ~ .,
  data = credit.Datos.Train,
  method = "lda",
  metric = "ROC",
  trControl = ctrl
)

# 2. Random Forest con grid search
rf_grid <- expand.grid(
  mtry = seq(2, ncol(credit.Datos.Train)-1, by = 2)
)

set.seed(123)
model_rf <- train(
  Class ~ .,
  data = credit.Datos.Train,
  method = "rf",
  metric = "ROC",
  tuneGrid = rf_grid,
  trControl = ctrl
)

# 3. SVM Radial
set.seed(123)
model_svm <- train(
  Class ~ .,
  data = credit.Datos.Train,
  method = "svmRadial",
  metric = "ROC",
  trControl = ctrl
)

# 4. Red Neuronal
set.seed(123)
model_nnet <- train(
  Class ~ .,
  data = credit.Datos.Train,
  method = "nnet",
  metric = "ROC",
  trControl = ctrl,
  trace = FALSE,
  maxit = 1000
)

# Comparar resultados
models_list <- list(
  LDA = model_lda,
  RF = model_rf,
  SVM = model_svm,
  NNET = model_nnet
)

# Comparación de modelos
results <- resamples(models_list)
summary(results)

# Visualización de comparación
bwplot(results)
dotplot(results)

# Imprimir los mejores parámetros de cada modelo
print("Mejores parámetros para Random Forest:")
print(model_rf$bestTune)
print("Mejores parámetros para SVM:")
print(model_svm$bestTune)
print("Mejores parámetros para Red Neuronal:")
print(model_nnet$bestTune)

# Afinar más el grid de RF
rf_grid_fine <- expand.grid(
  mtry = seq(6, 10, by = 1)  # Búsqueda más fina alrededor del óptimo (8)
)

model_rf_fine <- train(
  Class ~ .,
  data = credit.Datos.Train,
  method = "rf",
  metric = "ROC",
  tuneGrid = rf_grid_fine,
  trControl = ctrl
)

# Predicciones con el mejor modelo (RF)
pred_test <- predict(model_rf, credit.Datos.Test)
pred_test_prob <- predict(model_rf, credit.Datos.Test, type = "prob")

# Matriz de confusión
confusionMatrix(pred_test, credit.Datos.Test$Class)

# Curva ROC
library(pROC)
roc_obj <- roc(credit.Datos.Test$Class, pred_test_prob$positive)
plot(roc_obj)
auc(roc_obj)

# Importancia de variables
var_imp <- varImp(model_rf)
plot(var_imp)

# Identificar casos mal clasificados
wrong_classifications <- pred_test != credit.Datos.Test$Class
misclassified_cases <- credit.Datos.Test[wrong_classifications,]

# Calcular probabilidades
probs <- predict(model_rf, credit.Datos.Test, type = "prob")
roc_curve <- roc(credit.Datos.Test$Class, probs$positive)
plot(roc_curve)
auc(roc_curve)
