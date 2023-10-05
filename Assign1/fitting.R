get_confidence <- function(b, std_x, std_e, alpha){
  z <- qnorm(1-alpha/2)
  x <- rnorm(N,0,std_x)
  x.mean <- mean(x)
  e <- rnorm(N,0,std_e)
  y <-  b * x+ e
  y.mean <- mean(y)
  b_hat <- sum((x-x.mean) %*% (y-y.mean))/sum((x-x.mean)^2)
  error <- z * sqrt(var(e))/sqrt(sum((x-x.mean)^2))
  return(c(b_hat,error))
}

beta <- seq(-1000, 1000, by=0.1) 
N <- 100
s_x <- 2
s_error <- 1
alpha <- 0.05

total <- 0
for(i in beta){
  c <- get_confidence(i, s_x, s_error, alpha)
  if(i > c[1]-c[2] & i < c[1]+c[2])
    total <- total + 1
}
print(paste("Percentage of cases where beta landed in the interval:",  
            (100*total/length(beta))))

