if(!dir.exists('~/Analise Series Temporais/Chap8_Modelo Vetorial Autorregressivo')) {
  
  dir.create('~/Analise Series Temporais/Chap8_Modelo Vetorial Autorregressivo')
}

setwd('~/Analise Series Temporais/Chap8_Modelo Vetorial Autorregressivo')

n <- 500
r <- 0.7
set.seed(1)
Z1 <- rnorm(n)
Z2 <- rnorm(n)

E1 <- Z1
E2 <- r*Z1 + sqrt(1-r^2)*Z2

A <- matrix(c(.7,.2,.4,.3),2,2)

y_1 = y_2 <- rep(0,n)
for(t in 2:n){
  y_1[t] <- A[1,1]*y_1[t-1] + A[1,2]*y_2[t-1] + E1[t]
  y_2[t] <- A[2,1]*y_1[t-1] + A[2,2]*y_2[t-1] + E2[t]
}

# Figura 8.1
plot(y_1, type = "l", lty = 1, ylim = c(-6,6), ann = FALSE)
lines(y_2,lty = 3, col = "darkgrey")

# A outra  matriz "A" abaixo gera o processo do sistema (8.14).
 A = matrix(c(1,0,0,1),2,2)
 y_1 = y_2 <- rep(0,n)
 for(t in 2:n){
  y_1[t] <- A[1,1]*y_1[t-1] + A[1,2]*y_2[t-1] + E1[t]
   y_2[t] <- A[2,1]*y_1[t-1] + A[2,2]*y_2[t-1] + E2[t]
 }
 
# # Figura 8.2
 plot(y_1, type = "l", lty = 1, ylim = c(-15,20), ann = FALSE)
 lines(y_2,lty = 3, col = "darkgrey")
 