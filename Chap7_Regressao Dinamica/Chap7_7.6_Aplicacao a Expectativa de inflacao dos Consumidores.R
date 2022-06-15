if(!dir.exists('~/Analise Series Temporais/Chap7_Regressao Dinamica')) {
  
  dir.create('~/Analise Series Temporais/Chap7_Regressao Dinamica')
}

setwd('~/Analise Series Temporais/Chap7_Regressao Dinamica')

data <- read.csv2('https://raw.githubusercontent.com/pedrocostaferreira/timeseries/master/cap7-regressao-dinamica/cap7-regressao-dinamica%20-%20dados.csv')

expinf_cons <- ts(data = data[ , -1], start = c(year(as.Date(data[1, 1])), month(as.Date(data[1, 1]))), frequency = 12)

require(urca)
require(lubridate)
require(BETS)

#teste para a expectativa de inflação

adf_expinf <- ur.df(expinf_cons[, 'Exp_Cons'], type = 'none', lags = 13, selectlags = 'AIC')

summary(adf_expinf)

corrgram(adf_expinf@res, lag.max = 15)

#IPCA

adf_ipca <- ur.df(expinf_cons[ ,'IPCA'], type = 'none', lags = 12, selectlags = 'AIC')

summary(adf_ipca)  

corrgram(adf_ipca@res, lag.max = 15)  

#regressão clássica Exp_Cons ~ IPCA

ajuste_coin1 <- lm(expinf_cons[ , 'Exp_Cons'] ~ expinf_cons[ , 'IPCA'] - 1)

summary(ajuste_coin1)

adf_coin1 <- ur.df(ajuste_coin1$residuals, type = 'none', lags = 12,
                   selectlags = 'AIC')

#estatísticas levam à rejeição da hipótese nula de não estacionariedade

summary(adf_coin1)

corrgram(adf_coin1@res, lag.max = 15) #no livro está acf(adf_coin1@res, main = ''), ou seja, diferente do arquivo online

#comparando os modelos autorregressivos com defasagens distribuídas (ADL) com os de correção de erros (ECM)
#Estimação do modelo

install.packages('dynlm')
require(dynlm)

#Procedimento em duas etapas

reg1 <- lm(expinf_cons[ , 'Exp_Cons'] ~ expinf_cons[ , 'IPCA'] - 1)

res <- ts(reg1$residuals, start = c(2005, 09), frequency = 12)

reg2 <- dynlm(d(expinf_cons[ , 'Exp_Cons'], 1) ~ d(expinf_cons[ , 'IPCA'], 1) + L(res, 1) - 1)

#Procedimento em uma etapa

reg3 <- dynlm(d(expinf_cons[ , 'Exp_Cons'], 1) ~ d(expinf_cons[ , 'IPCA'], 1) +
                L(expinf_cons[ , 'Exp_Cons'], 1) + L(expinf_cons[ , 'IPCA'], 1) - 1)
  
  
  
  
  

