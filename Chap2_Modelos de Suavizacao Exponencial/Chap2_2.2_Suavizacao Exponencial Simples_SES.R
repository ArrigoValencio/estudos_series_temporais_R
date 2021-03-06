
if(!dir.exists('~/Analise Series Temporais/Chap2_Modelos de Suavizacao Exponencial')) {
 
   dir.create('~/Analise Series Temporais/Chap2_Modelos de Suavizacao Exponencial')
}

setwd('~/Analise Series Temporais/Chap2_Modelos de Suavizacao Exponencial')

#simulando s�rie para suaviza��o exponencial simples (SES)

set.seed(1234)
serie <- ts(runif(100, 10, 15), start = c(1915, 1), frequency = 1)
plot(serie)

#ajuste e previs�o com m�todo SES com fun��o HoltWinters ()
#para s�ries temporais livres de de tend�ncia e sazonalidade (s�ries etacion�rias)

ajuste <- HoltWinters(serie, beta = FALSE, gamma = FALSE)

plot(ajuste, xlab = 'tempo', ylab = 'valores observados/ajustados', main = '')

#previs�o

library(forecast)
ajuste_prev <- forecast(ajuste, h = 10, level = 95)

plot(ajuste_prev, main = '', xlab =  'tempo', ylab = 'Dados')

