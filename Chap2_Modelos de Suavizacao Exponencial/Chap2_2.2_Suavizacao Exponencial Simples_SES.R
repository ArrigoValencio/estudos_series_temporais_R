
if(!dir.exists('~/Analise Series Temporais/Chap2_Modelos de Suavizacao Exponencial')) {
 
   dir.create('~/Analise Series Temporais/Chap2_Modelos de Suavizacao Exponencial')
}

setwd('~/Analise Series Temporais/Chap2_Modelos de Suavizacao Exponencial')

#simulando série para suavização exponencial simples (SES)

set.seed(1234)
serie <- ts(runif(100, 10, 15), start = c(1915, 1), frequency = 1)
plot(serie)

#ajuste e previsão com método SES com função HoltWinters ()
#para séries temporais livres de de tendência e sazonalidade (séries etacionárias)

ajuste <- HoltWinters(serie, beta = FALSE, gamma = FALSE)

plot(ajuste, xlab = 'tempo', ylab = 'valores observados/ajustados', main = '')

#previsão

library(forecast)
ajuste_prev <- forecast(ajuste, h = 10, level = 95)

plot(ajuste_prev, main = '', xlab =  'tempo', ylab = 'Dados')

