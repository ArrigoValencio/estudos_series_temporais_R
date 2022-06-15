if(!dir.exists('~/Analise Series Temporais/Chap4_Modelos SARIMA')) {
  
  dir.create('~/Analise Series Temporais/Chap4_Modelos SARIMA')
}

setwd('~/Analise Series Temporais/Chap4_Modelos SARIMA')

#Conhecendo a ST antes da modelagem

#Teste estacionariedade - parte não sazonal

#Quatro maneiras de investigar se a ST é ou não estacinonária
#análise gráfica; comparação da média e variância para diferentes períodos de tempo;
#observação da FAC (Função de Autocorrelação); testes de raiz unitária.

#gráfico FAC

install.packages('BETS')
library(BETS)
corrgram(AirPassengers, lag.max = 36)

#Teste Raíz Unitária - Teste Augmented Dickey Fuller (ADF)

#H0: Há raíz unitária - não estacionária
#H1: não há raíz unitária - estacionária

#valor estatística de teste > valor crítico, então não se rejeita H0; 
#valor estatística de teste < valor crítico, então rejeita-se H0.

#várias especificações de não estacionariedade; importante identificar a forma
#de estacionariedade a ser testada.Opções:
#RU + constante + tend determinística (type = 'trend')
#RU + constante (type = 'drift)
#RU (type = 'none')

require(urca)
adf.drift <- ur.df(y = AirPassengers, type = 'drift', lags = 24, selectlags = 'AIC')

corrgram(adf.drift@res, lag.max = 36)

adf.drift@teststat #estatística de teste é tau2

adf.drift@cval

#Após identificação de não estacionariedade, realiza-se exercício para descobrir
#o número de diferenciações para a estacionariedade

ts.plot(diff(AirPassengers, lag = 1, differences = 1))
corrgram(diff(AirPassengers, lag = 1, differences = 1), lag.max = 36)

ts.plot(diff(log(AirPassengers), lag = 1, differences = 1))
corrgram(diff(log(AirPassengers), lag = 1, differences = 1), lag.max = 48)

#A FAC anterior é adequada para identificação da estrutura do modelo SARIMA?
#Ela indica que a ST é não estacionária na parte sazonal. Para correção,
#diferencia-se a parte sazonal, estabelecendo lag 12:

corrgram(diff(diff(log(AirPassengers), lag = 1, differences = 1), lag = 12,
              differences = 1), lag.max = 48)

#refazer teste para confirmar estacionariedade apóes transformações

adf.drift2 <- ur.df(y = diff(diff(log(AirPassengers), lag = 1, differences = 1), 
                         lag = 12,
                         differences = 1), type = 'drift', lags = 24,
                    selectlags = 'AIC')
  

adf.drift2@teststat

adf.drift2@cval

corrgram(adf.drift2@res, lag.max = 36)

#A FAC do livro está vinculada a essa especificação do teste:
#ur.df(y = diff(log(AirPassengers)), type = c("drift"), lags = 21)

#O resultado confirma a validade do teste e a modelagem pode começar.
















