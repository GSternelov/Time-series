 ### Lab 2 - Time Series ###
library(TSA)

# a)
t <- 3
sos_func <- sum(Y[t:length(Y)] - mean(Y) *  Y[(t-1):length(Y)] - mean(Y))



# b)
AR2LS <- function(Y){

  ylag2
  ylag1
  
  lm(ylag2 ~ )
  
  
}



# 2)

elec_c <- read.csv("C:/Users/Gustav/Documents/Time-series/electricity_consumption.csv", sep=";")

model_elec <- elec_c[1:150,]
valid_elec <- elec_c[151:300,]

elec_c <- ts(elec_c, freq = 1, start = 1)
valid_elec <- ts(model_elec, freq = 12, start = 2002.5)

model_elec <- ts(model_elec, freq = 12, start = 1990)
plot(model_elec[,2])
points(model_elec[,2], pch=as.vector(season(model_elec[,2])))
lm(model_elec[,2] ~ time(model_elec[,2]))
abline(lm(model_elec[,2] ~ time(model_elec[,2])))

# Testar att transformera med avseende p? variansen
log_elec <- log(model_elec[,2])
plot(log_elec, type="l")

# Diffar, testar med b?de s?song och vanlig som f?rsta
diff_season <- diff(model_elec[,2], lag = 12)
diff_regu <- diff(model_elec[,2])

diff_sea_regu <- diff(diff_season)
diff_regu_sea <- diff(diff_regu, lag = 12)



acf(model_elec[,2], lag.max=36)
acf(log_elec, lag.max=36)
acf(diff_season, lag.max=36)
acf(diff_regu, lag.max=36)
# Den b?sta:
acf(diff_sea_regu, lag.max=36)
# V?ljer ut en f?r s?song, endast den som ?r v?ldigt tydligt signifikant.
# F?r stora modellen tas ?ven en "vanlig" MA-parameter med d? den ?r signifikant ocks?


pacf(diff_season, lag.max=36)
pacf(diff_regu, lag.max=36)
# Den b?sta:
pacf(diff_sea_regu, lag.max=36)
# V?ljer ut en f?r s?song, endast den som ?r v?ldigt tydligt signifikant.
# F?r stora modellen tas ?ven en "vanlig" AR-parameter med d? den ?r signifikant ocks?

eacf(diff_sea_regu, ar.max=7, ma.max=10)
eacf(diff_sea_regu, ar.max=7, ma.max=10)


# Testar att anpassa de tv? modellerna
# Tar M1, den har minst antal parametrar
m1 = arima(model_elec[,2],order=c(0,1,0),seasonal=list(order = c(1,1,1)))
#m2 = arima(model_elec[,2],order=c(1,1,1),seasonal=list(order = c(1,1,1)))
#m2

# EAFC-modellen
m3 = arima(model_elec[,2],order=c(0,1,3),seasonal=list(order = c(1,1,1)))


# c)
# Kolla p? AIC, signifikans, residualer(plot, acf, qq)
resid_M1 <- rstandard(m1)

resid_M3 <- rstandard(m3)

par(mfrow=c(2,1))
plot(resid_M1)
points(resid_M1, pch=as.vector(season(model_elec[,2])))
plot(resid_M3)
# Ser ingen exakt trend, bra variation g?r runt noll. 

acf(resid_M1,lag.max=36)
acf(resid_M3,lag.max=36)
# Ser ut som att s?songsp?verkan finns kvar

qqnorm(resid_M1); qqline(resid_M1)
qqnorm(resid_M3); qqline(resid_M3)

# Har kvar ?rligt samband i residualerna; F?r?ndra modell f?r att f? bort det. 


m4 = arima(model_elec[,2],order=c(1,1,0),seasonal=list(order = c(2,1,0)))
m4

# Testar en massa men hittar ingen b?ttre modell




# 3
# b)
# Tror mest på M1
par(mfrow=c(1,1))
pred <- predict(m1, n.ahead=150)
predts <- ts(pred$pred, start=c(151,1), freq=1)
predup <- predts + 1.96* pred$se
predlow <-predts - 1.96* pred$se

plot(elec_c[,2], type="l")
lines(predts, col="red")





