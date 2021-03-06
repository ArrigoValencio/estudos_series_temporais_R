if(!dir.exists('~/Analise Series Temporais/Chap4_Modelos SARIMA')) {
  
  dir.create('~/Analise Series Temporais/Chap4_Modelos SARIMA')
}

setwd('~/Analise Series Temporais/Chap4_Modelos SARIMA')

#Conhecendo a ST antes da modelagem

#Teste estacionariedade - parte n�o sazonal

#Quatro maneiras de investigar se a ST � ou n�o estacinon�ria
#an�lise gr�fica; compara��o da m�dia e vari�ncia para diferentes per�odos de tempo;
#observa��o da FAC (Fun��o de Autocorrela��o); testes de raiz unit�ria.

#gr�fico FAC

install.packages('BETS')
library(BETS)
corrgram(AirPassengers, lag.max = 36)

#Teste Ra�z Unit�ria - Teste Augmented Dickey Fuller (ADF)

#H0: H� ra�z unit�ria - n�o estacion�ria
#H1: n�o h� ra�z unit�ria - estacion�ria

#valor estat�stica de teste > valor cr�tico, ent�o n�o se rejeita H0; 
#valor estat�stica de teste < valor cr�tico, ent�o rejeita-se H0.

#v�rias especifica��es de n�o estacionariedade; importante identificar a forma
#de estacionariedade a ser testada.Op��es:
#RU + constante + tend determin�stica (type = 'trend')
#RU + constante (type = 'drift)
#RU (type = 'none')

require(urca)
adf.drift <- ur.df(y = AirPassengers, type = 'drift', lags = 24, selectlags = 'AIC')

corrgram(adf.drift@res, lag.max = 36)

adf.drift@teststat #estat�stica de teste � tau2

adf.drift@cval

#Ap�s identifica��o de n�o estacionariedade, realiza-se exerc�cio para descobrir
#o n�mero de diferencia��es para a estacionariedade

ts.plot(diff(AirPassengers, lag = 1, differences = 1))
corrgram(diff(AirPassengers, lag = 1, differences = 1), lag.max = 36)

ts.plot(diff(log(AirPassengers), lag = 1, differences = 1))
corrgram(diff(log(AirPassengers), lag = 1, differences = 1), lag.max = 48)

#A FAC anterior � adequada para identifica��o da estrutura do modelo SARIMA?
#Ela indica que a ST � n�o estacion�ria na parte sazonal. Para corre��o,
#diferencia-se a parte sazonal, estabelecendo lag 12:

corrgram(diff(diff(log(AirPassengers), lag = 1, differences = 1), lag = 12,
              differences = 1), lag.max = 48)

#refazer teste para confirmar estacionariedade ap�es transforma��es

adf.drift2 <- ur.df(y = diff(diff(log(AirPassengers), lag = 1, differences = 1), 
                         lag = 12,
                         differences = 1), type = 'drift', lags = 24,
                    selectlags = 'AIC')
  

adf.drift2@teststat

adf.drift2@cval

corrgram(adf.drift2@res, lag.max = 36)

#A FAC do livro est� vinculada a essa especifica��o do teste:
#ur.df(y = diff(log(AirPassengers)), type = c("drift"), lags = 21)

#O resultado confirma a validade do teste e a modelagem pode come�ar.
















