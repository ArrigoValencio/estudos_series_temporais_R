if(!dir.exists('~/Analise Series Temporais/Chap4_Modelos SARIMA')) {
  
  dir.create('~/Analise Series Temporais/Chap4_Modelos SARIMA')
}

setwd('~/Analise Series Temporais/Chap4_Modelos SARIMA')

#Metodologia Box and Jenkins para s�ries temporais estacion�rias e constru��o
#dos modelos ARIMA segue um ciclo iterativo composto por cinco partes:
#Especifica��o; identifica��o; estima��o; diagn�stico; modelo definitivo.

#Identifica��o
#obsevando-se FAC e FACP (Fun��o Autocorrela��o Parcial)
require(BETS)

corrgram(diff(diff(log(AirPassengers), lag = 1, differences = 1), lag = 12,
              differences = 1), lag.max = 48)
corrgram(diff(diff(log(AirPassengers), lag = 1, differences = 1), lag = 12,
              differences = 1), type = 'partial', lag.max = 48)

#Autores pensam nos seguintes modelos:
#SARIMA(1,1,1)(1,1,1); SARIMA(0,1,1)(0,1,1)

#Estima��o

library(forecast)
fit.air <- Arima(AirPassengers, order = c(1, 1, 1), seasonal = c(1, 1, 1),
                 method = 'ML', lambda = 0)

t_test(fit.air) #teste de signific�ncia para o modelo

#par�metros de ar n�o significativos. Reestima-se modelo

fit.air <- Arima(AirPassengers, order = c(0, 1, 1), seasonal = c(0, 1, 1), 
                 method = 'ML', lambda = 0)

t_test(fit.air)

#Diagn�stico
#verificar res�duos: aus�ncia de autocorrela��o serial; 
#aus�ncia de heterocedasticidade condicional; normalidade.

diag_res <- tsdiag(fit.air, gof.lag = 20)

#testes espec�ficos
#teste de Ljung and Box para autocorela��o linear

Box.test(x = fit.air$residuals, lag = 24, type = 'Ljung-Box', fitdf = 2)

#teste do Multiplicador de Lagrange para heterocedasticidade condicional regressiva

require(FinTS)
ArchTest(fit.air$residuals, lags = 12)

#teste Jarque and Bera para normalidade dos res�duos

require(normtest)

jb.norm.test(fit.air$residuals, nrepl = 2000)

#Feito diagn�sticos nos res�duos, pr�ximo passo � fazer previs�es.

#Previs�o

require(forecast)

plot(forecast(object = fit.air, h = 12, level = 0.95))

#Analisando as m�tricas da previs�o pra julgar se boa previs�o:

accuracy(fit.air) #grande diferen�a em rela��o ao livro (PQ??)

#Exportando previs�o
#em .csv

write.csv2(x = data.frame(forecast(object = fit.air, h = 12, level = 0.95)), 
           file = 'previsao.csv')

#em xlx

writexl::write_xlsx(data.frame(forecast(object = fit.air, h = 12, level = 0.95)),
                   path = 'previsao.xlsx' )

read.csv('previsao.csv')

readxl::read_xlsx('previsao.xlsx')







