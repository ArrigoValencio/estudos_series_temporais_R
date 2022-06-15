
if(!dir.exists('~/Analise Series Temporais/Chap2_Modelos de Suavizacao Exponencial')) {
 
   dir.create('~/Analise Series Temporais/Chap2_Modelos de Suavizacao Exponencial')
}

setwd('~/Analise Series Temporais/Chap2_Modelos de Suavizacao Exponencial')

#download de série para aplicacão do modelo de suavização exponencial sazonal de Holt-Winters
#É o mais indicado em séries que apresentam comportamento sazonal

url_2.4 <- 'https://raw.githubusercontent.com/pedrocostaferreira/timeseries/master/cap2-HW/taxa_desemprego_regiao_metropolitana_sp.csv'
tx_desemprego <- read.csv2(url_2.4, sep = '')[ , 2]
serie_desemprego <- ts(tx_desemprego, start = c(1985, 1), frequency = 12)
plot(serie_desemprego, xlab = 'tempo', ylab = 'taxa de desemprego',main = '')

#modelo aditivo

ajuste_sazon <- HoltWinters(serie_desemprego)

plot(serie_desemprego, xlab = 'tempo', ylab = 'taxa de desemprego', main = '')
lines(fitted(ajuste_sazon)[ , 1], lwd = 2, col = 'red')
legend(1985, 15, c('Taxa de Desemprego', 'Ajuste HW'), lwd = c(1, 2),
       col = c('black', 'red'), bty = 'n')

#previsão

library(forecast)
prev_hw <- forecast(ajuste_sazon, h = 12, level = 95)
plot(prev_hw, xlab = 'tempo', ylab = 'Taxa de Desemprego', main = '')

#modelo multiplicativo

ajuste_sazon_mult <- HoltWinters(serie_desemprego, seasonal = 'multiplicative')

#comparativo gráfico aditivo x multiplicativo

plot(serie_desemprego, xlab = 'tempo', ylab = 'taxa de desemprego', main = '',
     ylim = c(4, 20))
lines(fitted(ajuste_sazon)[ , 1], lwd = 2, col = 'red')
lines(fitted(ajuste_sazon_mult)[ , 1], lwd = 2, col = 'blue')
legend(1985, 20, c('Taxa de Desemprego', 'Ajuste HW Aditivo',
      'Ajuste HW Multiplicativo'), lwd = c(1, 2, 2), col = c('black', 'red', 'blue'),
bty = 'n')







