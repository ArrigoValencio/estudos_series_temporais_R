if(!dir.exists('~/Analise Series Temporais/Chap8_Modelo Vetorial Autorregressivo')) {
  
  dir.create('~/Analise Series Temporais/Chap8_Modelo Vetorial Autorregressivo')
}

setwd('~/Analise Series Temporais/Chap8_Modelo Vetorial Autorregressivo')

set.seed(9)

e1 <- rnorm(250,0,0.5) 
e2 <- rnorm(250,0,0.5) 
e3 <- rnorm(250,0,0.5) 

u1.ar1 <- arima.sim(model = list(ar=0.75), innov = e1, n = 250)
u2.ar1 <- arima.sim(model = list(ar=0.3), innov = e2, n = 250)

y1 <- 0.8*y3 + u1.ar1
y2 <- -0.3*y3 + u2.ar1
y3 <- cumsum(e3)

y.mat <- data.frame(y1,y2,y3)

# Figura 8.8 
plot.ts(y1, lty = 1, ylim = c(-3,7), ylab = "")
lines(y2, lty = 2)
lines(y3, lty = 4)

vecm <- ca.jo(y.mat, type = c("trace"), ecdet = "const")

jo.results <- summary(vecm)
jo.results

vecm.r2 <- cajorls(vecm, r = 2)
vecm.r2

#Exemplo com as sérires: Indicador de incerteza da economia brasileira, taxa selic,
#IPCA e PIB

library(vars)
library(urca)

data <- read.csv2("https://raw.githubusercontent.com/pedrocostaferreira/timeseries/master/cap8-modelo-vetorial-autorregressivo/data_exemplo8.5.5.csv")

data <- ts(data[,-1], start = c(2003,12), freq = 12)
plot(data)

# estimação VEC
#selecinando parâmetros

VARselect(data, lag.max = 12)

#estimando e testando

vec <- ca.jo(data, type = "trace", ecdet = "const", K = 2, spec = "transitory")
vec2var <- vec2var(vec,r=1)
serial.test(vec2var)
arch.test(vec2var)

#Estimação SVEC

SR <- matrix(NA, nrow = 4, ncol = 4)
rownames(SR)<- colnames(data)
colnames(SR)<- colnames(data)
SR[2,3] <- 0
SR[4,3] <- 0
SR[4,3] <- 0

LR <- matrix(NA, nrow = 4, ncol = 4)
rownames(LR)<- colnames(data)
colnames(LR)<- colnames(data)
LR[2,2:4] <- 0
LR[4,2] <- 0

SR;LR

svec <- SVEC(vec, r = 1, LR = LR, SR = SR,lrtest = F)
summary(svec)

#Análise funçoes impulso-resposta

svecm.irf_IIEBR <- irf(svec, cumulative = F, runs = 100, n.ahead = 50, ci = .9,
                       impulse = "IIEBR", response = list("IIEBR", "SELIC"))
par(mfcol=c(2,1), mar=c(0,4,0,0), oma=c(5,3,5,3))
plot.ts(svecm.irf_IIEBR$irf$IIEBR[,1], axes=F, oma.multi = c(0,0,5,0),
        ylab='IIEBR', ylim=c(-4,6))
lines(svecm.irf_IIEBR$Lower$IIEBR[,1], lty=2, col='red')
lines(svecm.irf_IIEBR$Upper$IIEBR[,1], lty=2, col='red')
axis(side=2, las=2, ylab='')
abline(h=0, col='red')
box()

plot.ts(svecm.irf_IIEBR$irf$IIEBR[,2], axes=F, oma.multi = c(0,0,5,0),
        ylab='SELIC', ylim=c(-2,1))
lines(svecm.irf_IIEBR$Lower$IIEBR[,2], lty=2, col='red')
lines(svecm.irf_IIEBR$Upper$IIEBR[,2], lty=2, col='red')
axis(side=1, las=1)
axis(side=2, las=2)
abline(h=0, col='red')
box()

svecm.irf_SELIC<- irf(svec, cumulative = F, runs = 100, n.ahead = 50, ci = .9, 
                      impulse = "SELIC", response = list("IIEBR", "PIB"))
par(mfcol=c(2,1), mar=c(0,4,0,0), oma=c(5,3,5,3))
plot.ts(svecm.irf_SELIC$irf$SELIC[,1], axes=F, oma.multi = c(0,0,5,0),
        ylab='IIEBR', ylim=c(-1,3))
lines(svecm.irf_SELIC$Lower$SELIC[,1], lty=2, col='red')
lines(svecm.irf_SELIC$Upper$SELIC[,1], lty=2, col='red')
axis(side=2, las=2, ylab='')
abline(h=0, col='red')
box()

plot.ts(svecm.irf_SELIC$irf$SELIC[,2], axes=F, oma.multi = c(0,0,5,0),
        ylab='PIB', ylim=c(-1,1))
lines(svecm.irf_SELIC$Lower$SELIC[,2], lty=2, col='red')
lines(svecm.irf_SELIC$Upper$SELIC[,2], lty=2, col='red')
axis(side=1, las=1)
axis(side=2, las=2)
abline(h=0, col='red')
box()

#impondo restrição sobre a taxa de juros na incerteza como exercício

LR[1,2]<-0
svec <- update(svec, LR = LR, SR=SR, r=1, lrtest=T)
summary(svec)
svec$LRover

#plotando função impulso-resposta

svecm.irf_SELIC<- irf(svec, cumulative = F, runs = 100, n.ahead = 50, ci = .9, 
                      impulse = "SELIC", response = list("IIEBR"))

par(mfcol=c(1,1), mar=c(0,0,0,0), oma=c(5,3,5,3))
plot.ts(svecm.irf_SELIC$irf$SELIC[,1], axes=F, oma.multi = c(0,0,5,0),
        ylab='IIEBR', ylim=c(-1,3))
lines(svecm.irf_SELIC$Lower$SELIC[,1], lty=2, col='red')
lines(svecm.irf_SELIC$Upper$SELIC[,1], lty=2, col='red')
axis(side=2, las=2, ylab='')
abline(h=0, col='red')
axis(side=1, las=1)
axis(side=2, las=2)
abline(h=0, col='red')
box()










