# Cargar el dataset
setwd(".")
bike_data <- read.csv("C:/Users/kiril/Documentos/CienciaDatos3/EDM/XAI3/day.csv")

# Ver las primeras filas para entender la estructura
head(bike_data)

# Asegurarte de que las variables de tiempo estén en formato adecuado
bike_data$yr <- as.factor(bike_data$yr)
bike_data$holiday <- as.factor(bike_data$holiday)

# Crear el modelo de Random Forest
library(randomForest)
rf_model <- randomForest(cnt ~ temp + humidity + windspeed + weekday + season, data = bike_data, ntree = 100)

library(pdp)

# Crear un PDP para la variable 'temp' (temperatura)
pdp_temp <- partial(rf_model, pred.var = "temp", plot = TRUE, chull = TRUE)

set.seed(42)  # Para reproducibilidad
sample_data <- bike_data[sample(nrow(bike_data), 1000), ]  # Extraer 1000 muestras aleatorias

# Crear un PDP bidimensional
pdp_2d <- partial(rf_model, pred.var = c("temp", "humidity"), train = sample_data, plot = TRUE, grid.resolution = 50)

# Cargar el dataset de casas
house_data <- read.csv("C:/Users/kiril/Documentos/CienciaDatos3/EDM/XAI3/kc_house_data.csv")

# Crear el modelo Random Forest
rf_model_house <- randomForest(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + yr_built, data = house_data, ntree = 100)

# PDP para una sola variable, por ejemplo 'sqft_living'
pdp_sqft <- partial(rf_model_house, pred.var = "sqft_living", plot = TRUE)

# PDP bidimensional entre 'sqft_living' y 'bedrooms'
pdp_2d_house <- partial(rf_model_house, pred.var = c("sqft_living", "bedrooms"), train = house_data, plot = TRUE, grid.resolution = 50)