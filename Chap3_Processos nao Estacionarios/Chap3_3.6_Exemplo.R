if(!dir.exists('~/Analise Series Temporais/Chap3_Processos nao Estacionarios')) {
  
  dir.create('~/Analise Series Temporais/Chap3_Processos nao Estacionarios')
}

setwd('~/Analise Series Temporais/Chap3_Processos nao Estacionarios')

#3.6 Exemplo

url_3.6 <- 'https://raw.githubusercontent.com/pedrocostaferreira/timeseries/master/cap3-processos-nao-estacionarios/STI-20160413130025067.csv'
ibc <- read.csv2(url_3.6, header = FALSE, dec = '.')
ibc_ts <- ts(ibc[ , 2], start = c(2003, 1), frequency = 12)

library(urca)

#Teste ADF

adf_ibc <- ur.df(ibc_ts, type =  'trend', lags = 6, selectlags = 'AIC')

summary(adf_ibc)@teststat
summary(adf_ibc)@cval

#Teste KPSS

kpss_ibc <- ur.kpss(ibc_ts, type = 'tau', lags = 'short')

summary(kpss_ibc)@teststat
summary(kpss_ibc)@cval

#Teste Phillips-Perron

pp_ibc <- ur.pp(ibc_ts, type = 'Z-tau', lags = 'short')

summary(pp_ibc)@teststat
summary(pp_ibc)@cval

#Teste DF-GLS

gls_ibc <- ur.ers(ibc_ts, type = 'DF-GLS', model = 'trend', lag.max = 6)

summary(gls_ibc)@teststat
summary(gls_ibc)@cval

#Test de Zivot-Andrews
za_ibc <- ur.za(ibc_ts, model = 'both', lag = 6)
summary(za_ibc)






