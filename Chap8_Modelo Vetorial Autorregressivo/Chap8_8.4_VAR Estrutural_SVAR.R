if(!dir.exists('~/Analise Series Temporais/Chap8_Modelo Vetorial Autorregressivo')) {
  
  dir.create('~/Analise Series Temporais/Chap8_Modelo Vetorial Autorregressivo')
}

setwd('~/Analise Series Temporais/Chap8_Modelo Vetorial Autorregressivo')
install.packages('dse')

library(vars)
library(urca)
library(dse)

# criação do polinômio de defasagem

Apoly <- array(c(1.0,-0.5,0.3,0.8,
                 0.2,0.1,-0.7,-0.2,
                 0.7,1,0.5,-0.3),
               dim = c(3,2,2))

B <- diag(2)

# estruturação do modelo de acordo com A e B

svarA <- ARMA(A = Apoly, B = B)

# simulação das observações

svarsim <- simulate(svarA, sampleT = 500,
                    rng = list(seed = c(123456)))

# reformatando observações em matriz

svardat <- matrix(svarsim$output, nrow = 500, ncol = 2)
colnames(svardat) <- c("y1","y2")

## estimando VAR para observações criadas

varest <- VAR(svardat, p = 2, type = "none")

# definindo matriz de restrições da especificação do VAR Estrutural
Amat <- diag(2)
Amat[2,1] <- NA
Amat[1,2] <- NA

# estimando VAR Estrutural

svar.A <- SVAR(varest, estmethod = "direct",
               Amat = Amat, hessian = TRUE)


