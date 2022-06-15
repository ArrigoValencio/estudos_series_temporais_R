if(!dir.exists('~/Analise Series Temporais/Chap6_Box and Jenkins com Funcao Transferencia')) {
  
  dir.create('~/Analise Series Temporais/Chap6_Box and Jenkins com Funcao Transferencia')
}

setwd('~/Analise Series Temporais/Chap6_Box and Jenkins com Funcao Transferencia')


#downloading de dados e visualização

gas <- ts(read.csv('https://raw.githubusercontent.com/pedrocostaferreira/timeseries/master/cap6-BJFT/gas.csv'))

plot(gas, main = '')

#Metodologia
#dividida em quatro passos:
#calcular a função de correlação cruzada entre Yt e Xt
#identificar r, s e b
#estimar o modelo com função de transferência
#verificar se o modelo é adequado

#Calcular a função de correlação cruzada (CCF)
#método pré-branqueamento para autocorrelação/não estacionariedade

#etapas:
#ajustar um modelo ARIMA para a série independente Xt
#filtrar Yt pelo modelo encontrado no passo anterior/modelo de Yt é o mesmo de Xt
#salvar resíduos dos dois modelos
#calcular a CCF entre os resíduos obtidos no passo anterior

#ajuste de modelo ARIMA para Xt

#funções FAC e FACP para identificação da ordem do modelo (sugere-se (3,0,0))

library(BETS)
library(forecast)
corrgram(gas[ , 'InputGasRate'], lag.max = 36)

corrgram(gas[ , 'InputGasRate'], lag.max = 36, type = 'partial')

modelo_x <- Arima(gas[ , 'InputGasRate'], order = c(3, 0, 0), include.mean = FALSE)

#filtrar Yt pelo modelo encontrado

modelo_y <- Arima(gas[ , 'CO2'], model = modelo_x)

#salvar resíduos

alpha <- resid(modelo_x)
beta <- resid(modelo_y)

#calcular a CCF com os resíduos

ccf(beta, alpha, xlim = c(0, 20))

#Identificar r, s e b

#b: refere-se ao primeiro lag significativo. Portanto, b = 3
#s: refere-se ao número de lags crescentes depois de b. Portanto, s = 2
#r: por haver queda exponencial após os lags crescentes, r = 1

#Estimação BJFT

library(TSA)
#identificação do modelo pela FAC e FACP (sugere-se (2,0,0))

corrgram(gas[ , 'CO2'], lag.max = 36)
corrgram(gas[ , 'CO2'], lag.max = 36, type = 'partial')

#defasando Xt

x_novo <- lag(gas[ , 'InputGasRate'], k = -3)

#eliminando três primeiros valores de Yt

gas_novo <- na.omit(cbind(x_novo, gas[ , 'CO2']))
colnames(gas_novo) <- c('InputGasRate', 'CO2')

modelo_ft <- arimax(x = gas_novo[ , 'CO2'], order = c(2,0,0), 
                    xtransf = gas_novo[ , 'InputGasRate'], transfer = list(c(1, 2)))

#verificação da adequação do modelo

#Duas análises:
#calcular autocorreação dos resíduos
#CCF entre resíduos e a variável auxiliar Xt pré-branqueada

residuos <- resid(modelo_ft)
corrgram(residuos, lag.max = 36)
ccf(residuos, alpha, na.action = na.omit)

Box.test(residuos, type = 'Ljung-Box', lag = 24)

#Gráfico observado x ajustado

modelo_y <- Arima(gas[ , 'CO2'], order = c(2, 0, 0), include.mean = TRUE)
ajustados <- modelo_y$fitted
ajustados_ft <- fitted(modelo_ft)

ts.plot(gas[ , 'CO2'], ajustados, ajustados_ft, lty = c(1, 3, 2), lwd = c(1, 3, 2),
        col = c(1, 'orangered', 'dodgerblue'))
legend('bottomright', col = c(1, 'dodgerblue', 'orangered'),
       legend = c('Observados', 'Ajustados c/ FT', 'Ajustados s/ FT'), lty = c(1, 2, 3),
                  lwd = c(1, 2, 2), cex = 0.7)










