 ### Lab 2 - Time Series ###
library(TSA)


# b)
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
  return(phi)
}

phi1 <-0
phi2 <- 0
for (i in 1:1000){
  y <- AR2sim(100, 0.3, 0.2)
  phis <- AR2LS(y)
  phi1[i] <- phis[1]
  phi2[i] <- phis[2]
  
}


# d)
y <- AR2sim(30, 0.3, 0.2)
arima(y, order=c(2,0,0), include.mean=FALSE, method="ML")

# Enligt snabbkoll ganska så lika standardfel

# 2)

# Fix med datamaterial
elec_c <- read.csv("C:/Users/Gustav/Documents/Time-series/electricity_consumption.csv", sep=";")
model_elec <- elec_c[1:150,]
valid_elec <- elec_c[151:300,]
elec_c <- ts(elec_c, freq = 12, start = 1990)
valid_elec <- ts(model_elec, freq = 12, start = 2002.5)
model_elec <- ts(model_elec, freq = 12, start = 1990)


# Testar att plotta träningsdata med points och abline
plot(model_elec[,2])
points(model_elec[,2], pch=as.vector(season(model_elec[,2])))
lm(model_elec[,2] ~ time(model_elec[,2]))
abline(lm(model_elec[,2] ~ time(model_elec[,2])))


# Avsnitt för transfomering och diffar
# Testar att transformera med avseende på variansen
log_elec <- log(model_elec[,2])
plot(log_elec, type="l")
# Ser ut att ej behövas, blir ingen direkt skillnad


# Diffar, testar med både säsong och vanlig som första
diff_season <- diff(model_elec[,2], lag = 12) #Säsongsdiff
diff_regu <- diff(model_elec[,2])  #Diff för år

diff_sea_regu <- diff(diff_season) #säsongsdiff diffas
diff_regu_sea <- diff(diff_regu, lag = 12) #Årsdiff säsongsdiffas (samma som föregående)

# ACF för respektive diff och transformation
acf(model_elec[,2], lag.max=36) #icketransformerat data
acf(log_elec, lag.max=36) #för loggad data
acf(diff_season, lag.max=36) #säsongsdiffad data
acf(diff_regu, lag.max=36) #årsdiffad data
# Den bästa:
acf(diff_sea_regu, lag.max=36) # säsong + årsdiff
# V?ljer ut en f?r s?song, endast den som ?r v?ldigt tydligt signifikant.
# F?r stora modellen tas ?ven en "vanlig" MA-parameter med d? den ?r signifikant ocks?

# PACF
pacf(diff_season, lag.max=36) #säsongsdiffad
pacf(diff_regu, lag.max=36) # Årsdiffad
# Den b?sta:
pacf(diff_sea_regu, lag.max=36) #Kombon
# V?ljer ut en f?r s?song, endast den som ?r v?ldigt tydligt signifikant.
# F?r stora modellen tas ?ven en "vanlig" AR-parameter med d? den ?r signifikant ocks?

# EACF
eacf(diff_sea_regu, ar.max=7, ma.max=10) # Kombodiffar
# Hittar en MA(3)


# Testar att anpassa de två modellerna från SAC och SPAC
# Tar M1, den har minst antal parametrar
m1 = arima(model_elec[,2],order=c(0,1,0),seasonal=list(order = c(1,1,1)))
#m2 = arima(model_elec[,2],order=c(1,1,1),seasonal=list(order = c(1,1,1)))
#m2

# EAFC-modellen (Den som blev bäst)
m3 = arima(model_elec[,2],order=c(0,1,3),seasonal=list(order = c(1,1,1)))


# c)
# Kolla på AIC, signifikans, residualer(plot, acf, qq)
# Tar fram residualerna
resid_M1 <- rstandard(m1)
resid_M3 <- rstandard(m3)

par(mfrow=c(2,1))
# Plottar residualer
plot(resid_M1)
points(resid_M1, pch=as.vector(season(model_elec[,2])))
plot(resid_M3)
# Ser ingen exakt trend, bra variation g?r runt noll. 

# ACF
acf(resid_M1,lag.max=36)
acf(resid_M3,lag.max=36)
# Ser ut som att s?songsp?verkan finns kvar

# QQ-plottar
qqnorm(resid_M1); qqline(resid_M1)
qqnorm(resid_M3); qqline(resid_M3)

# Har kvar årligt samband i residualerna; F?r?ndra modell f?r att f? bort det. 

# Testar lite fler modeller, enligt instruktion för c). 
m4 = arima(model_elec[,2],order=c(1,1,0),seasonal=list(order = c(2,1,0)))
m4

# Testar en massa men hittar ingen bättre modell




# 3
# b)
# Tror mest på M1 först, men m3 är bäst
par(mfrow=c(1,1))
pred <- predict(m3, n.ahead=150)
predts <- ts(pred$pred, start=c(2002.5,1), freq=12)
 
predup <- predts + 1.96* pred$se
predlow <-predts - 1.96* pred$se

plot(elec_c[,2], type="l", xlim=c(2002, 2015), ylim=c(8000, 20000))
lines(predts, col="red")
lines(predup, col="purple")
lines(predlow, col="purple")

# c)
library(forecast)

par(mfrow=c(1,1))
m3_forecast = Arima(valid_elec[,2],order=c(0,1,3),seasonal=list(order = c(1,1,1)))

resid <- m3_forecast$residuals
plot(resid)
acf(resid)
qqnorm(resid); qqline(resid)







