get.metricas <- function(y, y_hat){
  n <- length(y)
  metricas <- list()
  dif <- y-y_hat
  scalingFactor <- sum(abs(y[2:n] - y[1:(n-1)])) / (n-2)
  
  metricas$RMSE <- round(sqrt(mean(dif^2)), 4)
  metricas$MAE <- round(mean(abs(dif)), 4)
  metricas$MPE <- sum(dif/(y))/n
  metricas$MAPE <- mean(abs(dif/y))
  metricas$MASE <- round(mean(abs(dif/scalingFactor)), 6)
  metricas$ME <- round(mean(dif), 6)
  metricas <- do.call("cbind", metricas)
  return(metricas)
}


customGraph <- function(base, zoom.date = NULL){
  objetos <- list()
  
  colors <- c("Training set" = "black", 
              "Test set" = "darkred",
              "SARIMAX" = "#3D7DDA",
              "SARIMAX Lagged" = "#03367c",
              "SARIMA" = "orange")
  
  if(!is.null(zoom.date)){
    base <-base[base$Periodo >=date(zoom.date),] 
  }
  
  p <- 
    ggplot(base[base$Periodo >=date(zoom.date),])+
  geom_path(aes(x = Periodo, y =Stock.BChile,
                color = "Training set"),
            show.legend = T,
            data = base[base$nobreaks == 1, ])+
  geom_path(aes(x = Periodo, y =Stock.BChile,
                color = "Test set"), 
            show.legend = T,
            data = base[base$split.base == "Test" &
                          base$Periodo >=date(zoom.date), ])+
  # geom_path(aes(x = Periodo, y =Modelo_sarimax.log,
  #               color = "SARIMAX"),
  #           show.legend = T,
  #           alpha = al.value)+
  geom_path(aes(x = Periodo, 
                y =Modelo_sarimax.lagged,
                color = "SARIMAX Lagged"),
            show.legend = F,
            alpha = al.value)+
  geom_path(aes(x = Periodo, 
                y =Modelo_sarima,
                color = "SARIMA"),
            show.legend = F,
            alpha = al.value)+
  geom_vline(xintercept = base[base$split.base == "Test","Periodo"][[1]][1],
             colour = "darkgrey",
             linetype = "dashed")+
  theme_bw()+
  theme(legend.position = "top")+
  scale_colour_manual(values = colors)+
  labs(x = "Period", 
       y = "Normal Stock: Billions of CLP",
       caption = "Series: Banco de Chile's Normal Consumer stock",
       color = "")+
  scale_y_continuous(
    labels = scales::label_currency(accuracy = 0.02,
                                    scale = 1/1000000, prefix = ""))+
  scale_x_date(date_breaks = "year",
               date_labels= "%Y")
  
  objetos$data <- base
  objetos$grafico <- p
  return(objetos)
  }

rollingModels <- function(base, fecha.corte, prediccion.futuro = 4){
  max.date <- as_date(fecha.corte) %m+% months(prediccion.futuro)
  base <- base[base$Periodo < max.date,] |> 
    mutate(split.base = if_else(Periodo < date(fecha.corte), "Train", 
                         "Test")
           )
  train <- base|> filter(Periodo < date(fecha.corte))
  test <- base |> filter(Periodo >= date(fecha.corte))
  
  X_train.Lagged <- 
    model.matrix(
      Stock.BChile ~ I(log(lagged.Consumo))+ I(log(lagged.imacec)) , data = train)
  
  X_test.Lagged <- model.matrix(
    Stock.BChile  ~ I(log(lagged.Consumo))+ I(log(lagged.imacec)) , data = test)
  
  X_train.log <- 
    model.matrix(
      Stock.BChile ~ +I(log(imacec))+ I(log(Consumo)) , data = train)
  
  X_test.log <- model.matrix(
    Stock.BChile ~ +I(log(imacec))+ I(log(Consumo)) , data = test)
  
  Y <- ts(train$Stock.BChile, frequency = 3)
  
  m.sarimax.lagged <- Arima(Y, order = c(3,1,1), 
                            seasonal=list(order=c(3,0,1), period=4),
                            xreg = X_train.Lagged[,-1],
                            lambda = 0)
  
  m.sarimax.log <- Arima(Y, order = c(3,1,1), 
                         seasonal=list(order=c(3,0,1), period=4),
                         xreg = X_train.log[,-1],
                         lambda = 0)
  
  
  m.sarima <- Arima(Y, order = c(3,1,1), 
                    seasonal=list(order=c(3,0,1), period=4),
                    lambda = 0)
  
  predicciones.sarimax.log <- forecast(m.sarimax.log, h = prediccion.futuro, xreg = X_test.log[,-1])
  predicciones.sarimax.lagged <- forecast(m.sarimax.lagged, h = prediccion.futuro, xreg = X_test.Lagged[,-1])
  predicciones.sarima <- forecast(m.sarima, h = prediccion.futuro)
  
  
  base$Modelo_sarima <- c(predicciones.sarima$fitted, predicciones.sarima$mean)
  base$Modelo_sarimax.log <- c(predicciones.sarimax.log$fitted, predicciones.sarimax.log$mean)
  base$Modelo_sarimax.lagged <- c(predicciones.sarimax.lagged$fitted, predicciones.sarimax.lagged$mean)
  
  base <- 
    base |> mutate(
      nobreaks = if_else(Periodo <= date(fecha.corte), 1, 
                         0))
  
  base$labels <- ifelse(base$split.base == "Test", 
                        "Serie de prueba", 
                        "Serie de ajuste")
  return(base)
  
}

