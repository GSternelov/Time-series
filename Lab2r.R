library(TSA)
# 1
AR2sim <- function(T, phi1, phi2){
  y <- rnorm(T)
  j <- y
  y[2] <-  phi1*y[1] + j[2]
  for (i in 3:T){
    y[i] <- phi1*y[i-1]+phi2*y[i-2] + j[i]
  }
  # plot(y, type= "l")
  #acf(y)
  #var(y)
  return(y)
}

AR2LS <- function(Y){
  ylag2 <- zlag(zlag(y))
  ylag1 <- zlag(y)
  
  model <- lm(y~ ylag1+ ylag2)
  
  phi <- model$coef[2:3]
  
  ylag1[1] <- 0
  ylag2[1:2] <- 0
  
  stderror <- sqrt(sum(y-phi[1]*ylag1-phi[2]*ylag2)^2/(28-2))
  func_vals <- c(phi, stderror)
  return(func_vals)
}

phi1_100 <-0
phi2_100 <- 0
stderror_100 <- 0
for (i in 1:1000){
  y <- AR2sim(100, 0.3, 0.2)
  phis_100 <- AR2LS(y)
  phi1_100[i] <- phis_100[1]
  phi2_100[i] <- phis_100[2]
  stderror_100[i] <- phis_100[3]
}

phi1_30 <-0
phi2_30 <- 0
stderror_30 <- 0
for (i in 1:1000){
  y <- AR2sim(30, 0.3, 0.2)
  phis_30 <- AR2LS(y)
  phi1_30[i] <- phis_30[1]
  phi2_30[i] <- phis_30[2]
  stderror_30[i] <- phis_30[3]
}

par(mfrow=c(2,1))
# hist(phi1_30)
# hist(phi2_30)
par(mfrow=c(1,1))

par(mfrow=c(2,1))
# hist(phi1_100)
# hist(phi2_100)
par(mfrow=c(1,1))

#d
phi1_ml <-0
phi2_ml <- 0
stderror_ml <- 0
for (i in 1:1000){
  y <- AR2sim(30, 0.3, 0.2)
  phis_ml <- arima(y, order=c(2,0,0), include.mean=FALSE, method="ML")
  phi1_ml[i] <- phis_ml$coef[1]
  phi2_ml[i] <- phis_ml$coef[2]
  stderror_ml[i] <- sqrt(phis_ml$sigma2)
}

# hist(phi1_ml)
# sd(stderror_ml)


# 2
# Fix med datamaterial
elec_c <- read.csv("/Users/Kevin/Documents/Skola/732A34 - Time series/Lab2/electricity_consumption.csv", sep=";")
model_elec <- elec_c[1:150,]
valid_elec <- elec_c[151:300,]
elec_c <- ts(elec_c, freq = 12, start = 1990)
valid_elec <- ts(model_elec, freq = 12, start = 2002.5)
model_elec <- ts(model_elec, freq = 12, start = 1990)

# plot(elec_c[,2])
# points(elec_c[,2],
#        pch=as.vector(season(elec_c[,2])))
# lm(elec_c[,2] ~ time(elec_c[,2]))
# abline(lm(elec_c[,2] ~ time(elec_c[,2])), col = "red")

log_elec <- log(model_elec[,2])
# plot(log_elec, type="l")


diff_season <- diff(model_elec[,2], lag = 12)
diff_sea_regu <- diff(diff_season)

par(mfrow = c(2,1))
# acf(diff_sea_regu, lag.max=36)
# pacf(diff_sea_regu, lag.max=36)
par(mfrow = c(1,1))

# eacf(diff_sea_regu, ar.max=7, ma.max=10) 

m1 = arima(model_elec[,2],order=c(0,1,0),seasonal=list(order = c(1,1,1)))
m3 = arima(model_elec[,2],order=c(0,1,3),seasonal=list(order = c(1,1,1)))

resid_M1 <- rstandard(m1)
resid_M3 <- rstandard(m3)

# plot(resid_M1)
# plot(resid_M3)

# acf(resid_M1,lag.max=36)
# acf(resid_M3,lag.max=36)

# qqnorm(resid_M1)
# qqline(resid_M1)
# qqnorm(resid_M3)
# qqline(resid_M3)

# 3
par(mfrow=c(1,1))
pred <- predict(m3, n.ahead=150)
predts <- ts(pred$pred, start=c(2002.5,1), freq=12)

predup <- predts + 1.96* pred$se
predlow <-predts - 1.96* pred$se

#plot(elec_c[,2], type="l", xlim=c(2002, 2015), ylim=c(8000, 20000))
#lines(predts, col="red")
#lines(predup, col="purple")
#lines(predlow, col="purple")

# c)
library(forecast)

par(mfrow=c(1,1))
m3_forecast = Arima(valid_elec[,2],order=c(0,1,3),seasonal=list(order = c(1,1,1)))

resid <- m3_forecast$residuals
#plot(resid)
#acf(resid)
#qqnorm(resid); qqline(resid)








