if(!dir.exists('~/Analise Series Temporais/Chap8_Modelo Vetorial Autorregressivo')) {
  
  dir.create('~/Analise Series Temporais/Chap8_Modelo Vetorial Autorregressivo')
}

setwd('~/Analise Series Temporais/Chap8_Modelo Vetorial Autorregressivo')

#Uso da metodologia no contexto de investimento e confiança industrial

library(mFilter)

#download e leitura de dados

data <- read.csv2("https://raw.githubusercontent.com/pedrocostaferreira/timeseries/master/cap8-modelo-vetorial-autorregressivo/data_exemplo8.6.csv")

# criando proxy para hiato do produto através do filtro HP

data$hiato <- hpfilter(data$ibc.br, type = "lambda", freq = 14400)$cycle

#finalizando a configuração dos dados

data <- ts(cbind(data$bk, data$ici, data$nuci, data$hiato, data$iiebr), start = c(2005,9), freq = 12)
colnames(data) <- c("bk","ici","nuci", "hiato", "iiebr")
plot(data)

# identificando ordem de defasagem do var

VARselect(data, lag.max = 12)

# estimando o modelo var

var_inv <- VAR(data, p = 3, type = c("const"))
summary(var_inv)

# guardar autovalores do processo

autoval <- vars::roots(var_inv)

# plotando

x <- seq(-1,1,length = nrow(data))
y1 <- sqrt(1-x^2)
y2 <- -sqrt(1-x^2)
plot(c(x,x),c(y1,y2),xlab='Parte Real',ylab='Parte Complexa',type='l',main='Círculo Unitário',ylim=c(-2,2),xlim=c(-2,2))
abline(h = 0)
abline(v = 0)
points(autoval, Im(autoval),pch=19)
legend(-2.0,-1.5,legend="Autovalores",pch=19)

# diagnóstico

arch.test(var_inv)
serial.test(var_inv, type = "PT.adjusted")

# plot dos processos de flutuacao 

plot(vars::stability(var_inv))


# função resposta impulso pontual e acumulada, descomente a desejada.
# resposta pontual 

ri_inv <- irf(var_inv, cumulative = FALSE, ci = 0.90, boot = TRUE, n.ahead = 30)

# resposta acumulada
# ri_inv <- irf(var_inv, cumulative = TRUE, ci = 0.90, boot = TRUE, n.ahead = 30)

# plot das funções impulso-resposta para BK e ICI
#respostas da produção de bens de capital

par(mfrow=c(5,1), mar=c(0,4,0,0), oma=c(5,3,5,3))

plot.ts(ri_inv$irf$ici[,1], axes=F, oma.multi = c(0,0,5,0),
        ylab='from ici', ylim=c(-2,3))
#
lines(ri_inv$Lower$ici[,1], lty=2, col='red')
lines(ri_inv$Upper$ici[,1], lty=2, col='red')
axis(side=2, las=2)
abline(h=0, col='red')
box()
grid(ny=0)         
mtext("VAR Impulse Response in BK", line = 2)

plot.ts(ri_inv$irf$bk[,1], axes=F, oma.multi = c(0,0,5,0),
        ylab='from bk', ylim=c(-3,4))
#
lines(ri_inv$Lower$bk[,1], lty=2, col='red')
lines(ri_inv$Upper$bk[,1], lty=2, col='red')
axis(side=2, las=2)
abline(h=0, col='red')
box()
grid(ny=0)   

plot.ts(ri_inv$irf$nuci[,1], axes=F, oma.multi = c(0,0,5,0),
        ylab='from nuci', ylim=c(-2,3))
#
lines(ri_inv$Lower$nuci[,1], lty=2, col='red')
lines(ri_inv$Upper$nuci[,1], lty=2, col='red')
axis(side=2, las=2)
abline(h=0, col='red')
box()
grid(ny=0)

plot.ts(ri_inv$irf$hiato[,1], axes=F, oma.multi = c(0,0,5,0),
        ylab='from hiato', ylim=c(-7,2))
#
lines(ri_inv$Lower$hiato[,1], lty=2, col='red')
lines(ri_inv$Upper$hiato[,1], lty=2, col='red')
axis(side=2, las=2)
abline(h=0, col='red')
box()
grid(ny=0)

plot.ts(ri_inv$irf$iiebr[,1], axes=F, oma.multi = c(0,0,5,0),
        ylab='from iiebr', ylim=c(-2,3))
#
lines(ri_inv$Lower$iiebr[,1], lty=2, col='red')
lines(ri_inv$Upper$iiebr[,1], lty=2, col='red')
axis(side=1, las=1)
axis(side=2, las=2)
abline(h=0, col='red')
box()
grid(ny=0)

#respostas do índice de confiança industrial

par(mfrow=c(5,1), mar=c(0,4,0,0), oma=c(5,3,5,3))

plot.ts(ri_inv$irf$ici[,2], axes=F, oma.multi = c(0,0,5,0),
        ylab='from ici', ylim=c(-2,3))
#
lines(ri_inv$Lower$ici[,2], lty=2, col='red')
lines(ri_inv$Upper$ici[,2], lty=2, col='red')
axis(side=2, las=2)
abline(h=0, col='red')
box()
grid(ny=0)         
mtext("VAR Impulse Response in ICI", line = 2)

plot.ts(ri_inv$irf$bk[,2], axes=F, oma.multi = c(0,0,5,0),
        ylab='from bk', ylim=c(-3,2))
#
lines(ri_inv$Lower$bk[,2], lty=2, col='red')
lines(ri_inv$Upper$bk[,2], lty=2, col='red')
axis(side=2, las=2)
abline(h=0, col='red')
box()
grid(ny=0)        

plot.ts(ri_inv$irf$nuci[,2], axes=F, oma.multi = c(0,0,5,0),
        ylab='from nuci', ylim=c(-2,3))
#
lines(ri_inv$Lower$nuci[,2], lty=2, col='red')
lines(ri_inv$Upper$nuci[,2], lty=2, col='red')
axis(side=2, las=2)
abline(h=0, col='red')
box()
grid(ny=0) 

plot.ts(ri_inv$irf$hiato[,2], axes=F, oma.multi = c(0,0,5,0),
        ylab='from hiato', ylim=c(-7,2))
#
lines(ri_inv$Lower$hiato[,2], lty=2, col='red')
lines(ri_inv$Upper$hiato[,2], lty=2, col='red')
axis(side=2, las=2)
abline(h=0, col='red')
box()
grid(ny=0) 

plot.ts(ri_inv$irf$iiebr[,2], axes=F, oma.multi = c(0,0,5,0),
        ylab='from iiebr', ylim=c(-2,3))
#
lines(ri_inv$Lower$iiebr[,2], lty=2, col='red')
lines(ri_inv$Upper$iiebr[,2], lty=2, col='red')
axis(side=1, las=1)
axis(side=2, las=2)
abline(h=0, col='red')
box()
grid(ny=0)








