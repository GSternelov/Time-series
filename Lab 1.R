rhine <- read.csv2("/Users/Kevin/Documents/Skola/732A34 - Time series/Lab 1/Rhine.csv")
library(TSA)
rhine <- ts(rhine, freq = 12, start = 1)

# 1. a)
# Plot data with months marked on the time series.
# plot(time(rhine), rhine[, 4], type = "l",ylab = "Nitrogen", main = "Monthly concentrations of total nitrogen in the Rhine river")
# points(rhine[,4], pch=as.vector(season(rhine)))

# 1.b)
# Estimate linear model and name it model_b
b_model <- lm(rhine[,4]~time(rhine))
# summary(b_model)

# Plot the ACF
# acf(rstudent(b_model))

# c)
# Plottar data med regression.
# plot(time(rhine), rhine[, 4], type = "l",ylab = "Nitrogen", main = "Monthly concentrations of total nitrogen in the Rhine river")
# points(rhine[,4], pch=as.vector(season(rhine)))
# abline(b_model, col = "red")

# Skapar en modell med dummyvariabler för varje månad.
b_month <- season(rhine[, 4])
b_model_month <- lm(rhine[, 4] ~ b_month + time(rhine))
# summary(b_model_month)

# Plottar residualerna för b_model_month_resid
b_model_month_resid <- rstudent(b_model_month)
# plot(b_model_month_resid, type="l")
# points(b_model_month_resid, pch=as.vector(season(rhine)))

# Plottar ACF för b_model_month
# acf(rstudent(b_model_month))

# Plottar QQ plott
# qqnorm(b_model_month_resid)
# qqline(b_model_month_resid)


# 1.d.a
data(retail)
# plot(time(retail), retail, type = "l",ylab = "Sales", main = "Monthly total UK retail sales in billions of pounds")
# points(retail, pch=as.vector(season(retail)))

# 1.d.b
d_b_model <- lm(retail~time(retail))
# summary(d_b_model)

# plottar ACF
# acf(rstudent(d_b_model))

# 1.d.c
# Skapar plot
# plot(time(retail), retail, type = "l",ylab = "Sales", main = "Monthly total UK retail sales in billions of pounds")
# points(retail, pch=as.vector(season(retail)))
# abline(d_b_model, col = "red")

# Skapar model med dummy
d_b_month <- season(retail)
d_b_model2 <- lm(retail ~ d_b_month + time(retail))
# summary(d_b_model2)

# Plottar residualerna
d_b_model2_resid <- rstudent(d_b_model2)
# plot(d_b_model2_resid, type="l")
# points(d_b_model2_resid, pch=as.vector(season(retail)))

# ACF for the new model
# acf(rstudent(d_b_model2))

# qqPlot
# qqnorm(rstudent(d_b_model2)); qqline(rstudent(d_b_model2))


# Uppgift 2
AR2sim <- function(T, phi1, phi2){
  y <- rnorm(T)
  j <- y
  y[2] <-  phi1*y[1] + j[2]
  for (i in 3:T){
    y[i] <- phi1*y[i-1]+phi2*y[i-2] + j[i]
  }
  # plot(y, type= "l")
  acf(y)
  #var(y)
}

# AR2sim(T=100,0.8,0)
# AR2sim(T=100,-0.5,0.5)
# AR2sim(T=100, 0, -0.64)

#d)
#AR2sim(10000, 0.8, 0.2) # Exempel p? en outside stationary region


# e)
ma1 <- arima.sim(list(ma = 1), 1000)
acf(ma1)

ma2 <- arima.sim(list(ma = c(1, -0.5)), 1000)
acf(ma2)

ma1.1 <- arima.sim(list(ma = 1, ar = -0.5), 1000)
acf(ma1.1)

# 3)
silver <- read.csv("C:/Users/Gustav/Desktop/time series analysis/silver.csv", sep=';', stringsAsFactors=F, header=T)

silver <- ts(silver, freq = 52, start = 1)

# a
#plot(y = silver[, 2], x = time(silver), type = "l")
#points(y= silver[, 2], x = time(silver), pch = as.vector(season(silver)))


# b
Log_silv <- log(silver[, 2])
#plot(y = Log_silv, x = time(Log_silv), type = "l")
#points(y= Log_silv, x = time(Log_silv), pch = as.vector(season(silver)))


lm_logsilv <- lm(Log_silv~time(Log_silv))
#acf(rstudent(lm_logsilv))
#qqnorm(rstudent(lm_logsilv)); qqline(rstudent(lm_logsilv))


# c)
minBoxCox <- BoxCox.ar(silver[,2])

#par(mfrow=c(2,1))
#plot((silver[, 2]^-0.2 - 1) / -0.2)
#plot(Log_silv)
#par(mfrow=c(1,1))

box_silver <- (silver[, 2]^-0.2 - 1) / -0.2


box_silver_diff <- diff(box_silver, differences = 2)
log_silver_diff <- diff(Log_silv, differences = 2)

lm_box <- lm(box_silver_diff~time(box_silver_diff))
lm_log <- lm(log_silver_diff~time(log_silver_diff))


#plot(rstudent(lm_box), type="l")
#plot(rstudent(lm_log), type="l")
#acf(rstudent(lm_box))
#acf(rstudent(lm_log))

# d)
data(bluebird)
#plot(bluebird[, 2])


#plot(log(bluebird[, 2]))
lambd <- c(-2:12)
#blueBox <- BoxCox.ar(bluebird[, 2], lambda=lambd)


bluebird_box <- (bluebird[, 2]^3 - 1) / 3
#plot(diff(bluebird_box,differences = 1))
lm_blue_box <- lm(bluebird_box~time(bluebird_box))

#acf(rstudent(lm_blue_box))
#plot(rstudent(lm_blue_box), type="l")