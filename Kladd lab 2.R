

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
  
  
  
}


