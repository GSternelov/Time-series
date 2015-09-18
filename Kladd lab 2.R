Sc <- sum(y-phi*ylag1-phi2*ylag2)^2



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

y <- AR2sim(30, 0.3, 0.2)

AR2LS <- function(Y){
  ylag2 <- zlag(zlag(y))
  ylag1 <- zlag(y)
  
  model <- lm(y~ ylag1+ ylag2)
  
  phi <- model$coef[2:3]
  
  ylag1[1] <- 0
  ylag2[1:2] <- 0
  
  stderror <- sqrt(sum(y-phi[1]*ylag1-phi[2]*ylag2)^2/(28-2))
  stderror
  return(phi)
}

y <- AR2sim(30, 0.3, 0.2)
AR2LS(y)

phi1 <-0
phi2 <- 0
for (i in 1:1000){
  y <- AR2sim(100, 0.3, 0.2)
  phis <- AR2LS(y)
  phi1[i] <- phis[1]
  phi2[i] <- phis[2]
  
}

hist(phi1)
hist(phi2)
