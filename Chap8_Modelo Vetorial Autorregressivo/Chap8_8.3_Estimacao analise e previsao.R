if(!dir.exists('~/Analise Series Temporais/Chap8_Modelo Vetorial Autorregressivo')) {
  
  dir.create('~/Analise Series Temporais/Chap8_Modelo Vetorial Autorregressivo')
}

setwd('~/Analise Series Temporais/Chap8_Modelo Vetorial Autorregressivo')

#Estimação usando os dados simulados
# Estruturação dados com como um objeto "ts" e ajuste em um modelo VAR.

data <- as.ts(cbind(y_1,y_2))

# subseção 8.3.1 - estimação 
# Estimação da número de lags e do processo

install.packages('vars')
library(vars)

VARselect(data, lag.max = 6, type = "none")
(model1 <- VAR(data, p = 1, type = "none"))

#diagnóstico

serial.test(model1)
arch.test(model1)
vars::roots(model1)

#Função impulso-resposta
# resposta do choque em y1

model1.irf <- irf(model1, impulse = "y_1", n.ahead = 40, boot = TRUE)

# resposta do choque em y2

model2.irf <- irf(model1, impulse = "y_2", n.ahead = 40, boot = TRUE)

par(mfcol=c(2,2), mar=c(0,4,0,0), oma=c(5,3,5,3))

plot.ts(model1.irf$irf$y_1[,1], axes=F, oma.multi = c(0,0,5,0),
        ylab='y_1', ylim=c(-0,1))
lines(model1.irf$Lower$y_1[,1], lty=2, col='red')
lines(model1.irf$Upper$y_1[,1], lty=2, col='red')
axis(side=2, las=2, ylab='')
abline(h=0, col='red')
box()
mtext("Response from y_1", line = 2)

plot.ts(model1.irf$irf$y_1[,2], axes=F, oma.multi = c(0,0,5,0),
        ylab='y_2', ylim=c(-0,1))
lines(model1.irf$Lower$y_1[,2], lty=2, col='red')
lines(model1.irf$Upper$y_1[,2], lty=2, col='red')
axis(side=1, las=1)
axis(side=2, las=2)
abline(h=0, col='red')
box()

plot.ts(model2.irf$irf$y_2[,1], axes=F, oma.multi = c(0,0,5,0),
        ylab='', ylim=c(-0,1))
lines(model2.irf$Lower$y_2[,1], lty=2, col='red')
lines(model2.irf$Upper$y_2[,1], lty=2, col='red')
axis(side=2, las=2, ylab='')
abline(h=0, col='red')
box()
mtext("Response from y_2", line = 2)

plot.ts(model2.irf$irf$y_2[,2], axes=F, oma.multi = c(0,0,5,0),
        ylab='', ylim=c(-0,1))
lines(model2.irf$Lower$y_2[,2], lty=2, col='red')
lines(model2.irf$Upper$y_2[,2], lty=2, col='red')
axis(side=1, las=1)
axis(side=2, las=2)
abline(h=0, col='red')
box()

#Efeito acumulado

# resposta do choque em y1

model1.irf <- irf(model1, impulse = "y_1", n.ahead = 40, boot = TRUE, cumulative = T)

# resposta do choque em y2

model2.irf <- irf(model1, impulse = "y_2", n.ahead = 40, boot = TRUE, cumulative = T)

par(mfcol=c(2,2), mar=c(0,4,0,0), oma=c(5,3,5,3))

plot.ts(model1.irf$irf$y_1[,1], axes=F, oma.multi = c(0,0,5,0),
        ylab='y_1', ylim=c(-0,8))
lines(model1.irf$Lower$y_1[,1], lty=2, col='red')
lines(model1.irf$Upper$y_1[,1], lty=2, col='red')
axis(side=2, las=2, ylab='')
abline(h=0, col='red')
box()
mtext("Cumulative Response from y_1", line = 2)

plot.ts(model1.irf$irf$y_1[,2], axes=F, oma.multi = c(0,0,5,0),
        ylab='y_2', ylim=c(-0,8))
lines(model1.irf$Lower$y_1[,2], lty=2, col='red')
lines(model1.irf$Upper$y_1[,2], lty=2, col='red')
axis(side=1, las=1)
axis(side=2, las=2)
abline(h=0, col='red')
box()

plot.ts(model2.irf$irf$y_2[,1], axes=F, oma.multi = c(0,0,5,0),
        ylab='', ylim=c(-0,8))
lines(model2.irf$Lower$y_2[,1], lty=2, col='red')
lines(model2.irf$Upper$y_2[,1], lty=2, col='red')
axis(side=2, las=2, ylab='')
abline(h=0, col='red')
box()
mtext("Cumulative Response from y_2", line = 2)

plot.ts(model2.irf$irf$y_2[,2], axes=F, oma.multi = c(0,0,5,0),
        ylab='', ylim=c(-0,8))
lines(model2.irf$Lower$y_2[,2], lty=2, col='red')
lines(model2.irf$Upper$y_2[,2], lty=2, col='red')
axis(side=1, las=1)
axis(side=2, las=2)
abline(h=0, col='red')
box()

#Decomposição da variância

fevd.model1 <- fevd(model1, n.ahead = 7)
plot(fevd.model1, main="")

#Previsões a partir da função fanchart() (produz os intervalos como áreas sombreadas)

model1.y_1 <- predict(model1, n.ahead = 10, ci = 0.95)
fanchart(model1.y_1, xlim = c(450,505), main = c("Fanchart de y_1", "Fanchart de y_2"))

#Causalidade de Granger

causality(model1)$Granger










