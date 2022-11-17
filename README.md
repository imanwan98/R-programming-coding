library('ggplot2')
library('forecast')
library('tseries')
library(e1071)
library('Metrics')
library('extrafont')
loadfonts(device = "win")
library(survminer)
library(survival)
library(hydroGOF)
library(lmtest)
library(readxl)

data <- read_excel("file.nama_data") 
View(data)
plot(data,type)

data$Date <- as.Date(data$Date)

train <- data.frame(data) 
test <- data.frame(data) 

graph <- ggplot(data = data) c(data)) +
  ggtitle("file name ") +
  theme(text _text(size,  family))+
  geom_line(aes(Date)) + scale_x_date('time') 

ggsave(
  filename = "file name",
  plot = graph,
  width,
  height 
)
plot(graph)

ts_data <- ts(data[, c('data')])
adf.test(ts_data,"stationary")
ts_stationary<-diff(ts_data)
plot(ts_stationary)
adf.test(ts_stationary)
ts_data %>% diff() %>% ggtsdisplay(main) # plot diff(),acf,pacf

arima_model <- auto.arima(ts_data)
arima_model_pred <- fitted(model)

res <-residuals(object, “ “)
res_plot <- autoplot(res) + ggtitle("file name") +
 geom_point( ) + xlab("Day") + ylab("") +
 theme(text _text(family, size))
res_histogram <- gghistogram(res) + ggtitle("name") +
 xlab("residuals") + theme(text _text(family, size))
res_acf <- ggAcf(res) + ggtitle("file name") +
 theme(text _text(family, size))

coeftest(arima_model)
Box.test(res,lag, fitdf, type " ")

svm_tune <- tune.svm(
  ts_data ~ data$Date,
  data = data,
  gamma,
  cost, 
  epsilon,
  kernel = " "
)

svm_model <- svm_tune$best.model
svm_pred <- fitted(svm_model)

arima_residuals = residuals(arima_seasonal)
plot(arima_residuals)

r=residuals(arima_seasonal)

autoplot(r)+ggtitle("file name")+geom_point()+xlab(" ")+ylab("")+ theme(text _text(family, size))
gghistogram(r)+ggtitle("file name")+xlab(" ")+theme(text _text(family, size))
ggAcf(r)+ggtitle("file name")+theme(text (family, size))

arima_res_svm_tune <- tune.svm(
  arima_residuals ~ arima_seasonal_pred,
  data = data,
  gamma = values
  cost = values
  epsilon = seq(_),
  kernel = ""
)

arima_res_svm = ( )
arima_res_svm_pred <- predict( )

hybrid_pred <- ts_data + file
graph <- ggplot() +
  geom_point(aes(x, y , type, colour )) +
  geom_point(aes(x, y, colour ))

ggsave(
  filename = "file name",
  plot = graph,
  width,
  height 
)
plot(graph)
plot()


label1 <- "data"
label9 <- "ARIMA model"
label3 <- "SVMs"
label14 <- "ARIMA-SVMs"

graph <- ggplot(data) +
  ggtitle() +
  theme(text _text(size,  family))+
  geom_line(aes(x, y, colour), size) +
  geom_line(aes(x, y, colour), linetype) +
  geom_point(aes(x, y, colour), size) +
  geom_line(aes(x, y, colour), linetype) +
  geom_point(aes(x, y, colour), size) +
  geom_line(aes(x, y, colour), linetype) +
  geom_point(aes(x, y, colour), size) +

  scale_x_date("Date") +
  ylab("data")
graph$labels$colour <- "Legend"


ggsave(
  filename = "file name",
  plot = graph,
  width,
  height 
)

plot(graph)

arima_mae <- mae(data$data, name)
arima_mape <- mape(data$data,, name)
arima_mse <- mse(data$data,, name)
arima_rmse <- rmse(data$data,, name)

svm_mae <- mae(data$data,, name)
svm_mape <- mape(data$data,, name)
svm_mse <- mse(data$data,, name)
svm_rmse <- rmse(data$data,, name)

hybrid_mae <- mae(data$data,, name)
hybrid_mape <- mape(data$data,, name)
hybrid_mse <- mse(data$data,, name)
hybrid_rmse <- rmse(data$data,, name)

indices <- data.frame(
  "MAE" = c( ),
  "MAPE" = c( ),
  "MSE" = c(),
  "RMSE" = c( )
)

print(indices, class = TRUE)
