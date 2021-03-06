
if(!dir.exists('~/Analise Series Temporais/Chap2_Modelos de Suavizacao Exponencial')) {
 
   dir.create('~/Analise Series Temporais/Chap2_Modelos de Suavizacao Exponencial')
}

setwd('~/Analise Series Temporais/Chap2_Modelos de Suavizacao Exponencial')

#download de s�rie para ajuste e previs�o com Suaviza��o Exponencial de Holt (SEH)
#Permite realizar previs�es em s�ries que apresentem o efeito de tend�ncia linear

url_2.3 <- 'https://raw.githubusercontent.com/pedrocostaferreira/timeseries/master/cap2-HW/consumo_energia_eletrica_regiao_sudeste.csv'
dados <- read.csv2(url_2.3)[-457, ]
consumo <- ts(dados$Consumo, start = c(1979, 1), frequency = 12)
plot(consumo, xlab = 'tempo', ylab = 'Consumo de Energia El�trica (Gwh)', main = '')

#ajuste via fun��o HoltWinters ()
ajuste_holt <- HoltWinters(consumo, gamma = FALSE)

plot(consumo, xlab = 'tempo', ylab = 'Valores Observados/Ajustados', main = '')
lines(fitted(ajuste_holt)[ , 1], lwd = 2, col = 'red')
legend(1980, 20000, c('Consumo Energia El�trica', 'Ajuste SEH'), lwd = c(1, 2),
       col = c('black', 'red'), bty = 'n')

#previs�o
library(forecast)
prev_holt <- forecast(ajuste_holt,h = 12, level = 95)

plot(prev_holt, xlab =  'tempo', ylab = 'valores observados/previstos', main = '')
