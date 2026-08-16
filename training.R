library(caret)

# Configuración común para todos los modelos
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