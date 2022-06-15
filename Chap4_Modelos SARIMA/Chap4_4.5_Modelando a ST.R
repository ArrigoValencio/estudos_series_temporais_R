if(!dir.exists('~/Analise Series Temporais/Chap4_Modelos SARIMA')) {
  
  dir.create('~/Analise Series Temporais/Chap4_Modelos SARIMA')
}

setwd('~/Analise Series Temporais/Chap4_Modelos SARIMA')

#Metodologia Box and Jenkins para séries temporais estacionárias e construção
#dos modelos ARIMA segue um ciclo iterativo composto por cinco partes:
#Especificação; identificação; estimação; diagnóstico; modelo definitivo.

#Identificação
#obsevando-se FAC e FACP (Função Autocorrelação Parcial)
require(BETS)

corrgram(diff(diff(log(AirPassengers), lag = 1, differences = 1), lag = 12,
              differences = 1), lag.max = 48)
corrgram(diff(diff(log(AirPassengers), lag = 1, differences = 1), lag = 12,
              differences = 1), type = 'partial', lag.max = 48)

#Autores pensam nos seguintes modelos:
#SARIMA(1,1,1)(1,1,1); SARIMA(0,1,1)(0,1,1)

#Estimação

library(forecast)
fit.air <- Arima(AirPassengers, order = c(1, 1, 1), seasonal = c(1, 1, 1),
                 method = 'ML', lambda = 0)

t_test(fit.air) #teste de significância para o modelo

#parâmetros de ar não significativos. Reestima-se modelo

fit.air <- Arima(AirPassengers, order = c(0, 1, 1), seasonal = c(0, 1, 1), 
                 method = 'ML', lambda = 0)

t_test(fit.air)

#Diagnóstico
#verificar resíduos: ausência de autocorrelação serial; 
#ausência de heterocedasticidade condicional; normalidade.

diag_res <- tsdiag(fit.air, gof.lag = 20)

#testes específicos
#teste de Ljung and Box para autocorelação linear

Box.test(x = fit.air$residuals, lag = 24, type = 'Ljung-Box', fitdf = 2)

#teste do Multiplicador de Lagrange para heterocedasticidade condicional regressiva

require(FinTS)
ArchTest(fit.air$residuals, lags = 12)

#teste Jarque and Bera para normalidade dos resíduos

require(normtest)

jb.norm.test(fit.air$residuals, nrepl = 2000)

#Feito diagnósticos nos resíduos, próximo passo é fazer previsões.

#Previsão

require(forecast)

plot(forecast(object = fit.air, h = 12, level = 0.95))

#Analisando as métricas da previsão pra julgar se boa previsão:

accuracy(fit.air) #grande diferença em relação ao livro (PQ??)

#Exportando previsão
#em .csv

write.csv2(x = data.frame(forecast(object = fit.air, h = 12, level = 0.95)), 
           file = 'previsao.csv')

#em xlx

writexl::write_xlsx(data.frame(forecast(object = fit.air, h = 12, level = 0.95)),
                   path = 'previsao.xlsx' )

read.csv('previsao.csv')

readxl::read_xlsx('previsao.xlsx')







