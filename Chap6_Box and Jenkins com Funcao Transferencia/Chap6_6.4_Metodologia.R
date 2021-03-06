if(!dir.exists('~/Analise Series Temporais/Chap6_Box and Jenkins com Funcao Transferencia')) {
  
  dir.create('~/Analise Series Temporais/Chap6_Box and Jenkins com Funcao Transferencia')
}

setwd('~/Analise Series Temporais/Chap6_Box and Jenkins com Funcao Transferencia')


#downloading de dados e visualiza��o

gas <- ts(read.csv('https://raw.githubusercontent.com/pedrocostaferreira/timeseries/master/cap6-BJFT/gas.csv'))

plot(gas, main = '')

#Metodologia
#dividida em quatro passos:
#calcular a fun��o de correla��o cruzada entre Yt e Xt
#identificar r, s e b
#estimar o modelo com fun��o de transfer�ncia
#verificar se o modelo � adequado

#Calcular a fun��o de correla��o cruzada (CCF)
#m�todo pr�-branqueamento para autocorrela��o/n�o estacionariedade

#etapas:
#ajustar um modelo ARIMA para a s�rie independente Xt
#filtrar Yt pelo modelo encontrado no passo anterior/modelo de Yt � o mesmo de Xt
#salvar res�duos dos dois modelos
#calcular a CCF entre os res�duos obtidos no passo anterior

#ajuste de modelo ARIMA para Xt

#fun��es FAC e FACP para identifica��o da ordem do modelo (sugere-se (3,0,0))

library(BETS)
library(forecast)
corrgram(gas[ , 'InputGasRate'], lag.max = 36)

corrgram(gas[ , 'InputGasRate'], lag.max = 36, type = 'partial')

modelo_x <- Arima(gas[ , 'InputGasRate'], order = c(3, 0, 0), include.mean = FALSE)

#filtrar Yt pelo modelo encontrado

modelo_y <- Arima(gas[ , 'CO2'], model = modelo_x)

#salvar res�duos

alpha <- resid(modelo_x)
beta <- resid(modelo_y)

#calcular a CCF com os res�duos

ccf(beta, alpha, xlim = c(0, 20))

#Identificar r, s e b

#b: refere-se ao primeiro lag significativo. Portanto, b = 3
#s: refere-se ao n�mero de lags crescentes depois de b. Portanto, s = 2
#r: por haver queda exponencial ap�s os lags crescentes, r = 1

#Estima��o BJFT

library(TSA)
#identifica��o do modelo pela FAC e FACP (sugere-se (2,0,0))

corrgram(gas[ , 'CO2'], lag.max = 36)
corrgram(gas[ , 'CO2'], lag.max = 36, type = 'partial')

#defasando Xt

x_novo <- lag(gas[ , 'InputGasRate'], k = -3)

#eliminando tr�s primeiros valores de Yt

gas_novo <- na.omit(cbind(x_novo, gas[ , 'CO2']))
colnames(gas_novo) <- c('InputGasRate', 'CO2')

modelo_ft <- arimax(x = gas_novo[ , 'CO2'], order = c(2,0,0), 
                    xtransf = gas_novo[ , 'InputGasRate'], transfer = list(c(1, 2)))

#verifica��o da adequa��o do modelo

#Duas an�lises:
#calcular autocorrea��o dos res�duos
#CCF entre res�duos e a vari�vel auxiliar Xt pr�-branqueada

residuos <- resid(modelo_ft)
corrgram(residuos, lag.max = 36)
ccf(residuos, alpha, na.action = na.omit)

Box.test(residuos, type = 'Ljung-Box', lag = 24)

#Gr�fico observado x ajustado

modelo_y <- Arima(gas[ , 'CO2'], order = c(2, 0, 0), include.mean = TRUE)
ajustados <- modelo_y$fitted
ajustados_ft <- fitted(modelo_ft)

ts.plot(gas[ , 'CO2'], ajustados, ajustados_ft, lty = c(1, 3, 2), lwd = c(1, 3, 2),
        col = c(1, 'orangered', 'dodgerblue'))
legend('bottomright', col = c(1, 'dodgerblue', 'orangered'),
       legend = c('Observados', 'Ajustados c/ FT', 'Ajustados s/ FT'), lty = c(1, 2, 3),
                  lwd = c(1, 2, 2), cex = 0.7)










