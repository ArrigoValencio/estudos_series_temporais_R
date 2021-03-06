if(!dir.exists('~/Analise Series Temporais/Chap5_Ajuste Sazonal')) {
  
   dir.create('~/Analise Series Temporais/Chap5_Ajuste Sazonal')
}
setwd('~/Analise Series Temporais/Chap5_Ajuste Sazonal')


#Aplica��o do ajuste sazonal (X-13-ARIMA-SEATS) no �ndice de Produ��o Industrial

library(seasonal)

#Etapas de um ajuste sazonal segundo a literatura:
#An�lise gr�fica;
#Execu��o do X13-ARIMA-SEATS no modo autom�tico;
#Avalia��o do ajuste sazonal realizado no passo anterior;
#Corre��o do ajuste se necess�rio.

#download de dados
pim <- read.csv2('https://raw.githubusercontent.com/pedrocostaferreira/timeseries/master/cap5-ajuste-sazonal/pimpf.csv')
pim_ts <- ts(data = pim, start = c(2002, 1), frequency = 12)

#An�lise gr�fica

plot(pim_ts)

monthplot(pim_ts, col.base = 2, lty.base = 2)
legend('topleft', legend = c('pim', 'm�dia'), lty = c(1, 2), col = c(1, 2), bty = 'n')

#Execu��o no modo autom�tico

pim_auto_ajuste <- seas(pim_ts)

#Avalia��o do ajuste feito acima

summary(pim_auto_ajuste)

#gr�fico espectral ajuda na avalia��o

library(ggplot2)

spec.orig <- data.frame(series(pim_auto_ajuste, 'sp0'))

ggplot(data = spec.orig, aes(x = 0:60, y = X10.Log.Spectrum_AdjOri.), colour = 'black') +
 geom_line() +
  geom_vline(colour = 'red', xintercept = c(10, 20, 30, 40, 50), linetype = 5) +
  geom_vline(colour = 'blue', xintercept = c(42, 52), linetype = 3) + 
  ylab('') + xlab('') + theme_bw() + 
  ggtitle('Spectral plot of the first-differenced original series') + 
  theme(plot.title = element_text(lineheight = 2, face = 'bold', size = 16))

#estat�stica QS ajuda na avalia��o

qs(pim_auto_ajuste)

#gr�fico SI ratio ajuda na avalia��o

monthplot(pim_auto_ajuste, col.base = 1, lty.base = 2, lwd.base = 2)
legend('topleft', legend = c('SI', 'Fator sazonal', 'M�dia Fator Sazonal FS'), 
       cex = 0.7, lty = c(1, 1, 2), col = c(4, 2, 1), lwd = c(1, 2, 2), bty = 'n')

#Corre��o do ajuste autom�tico
#por raz�es espec�ficas, � necess�rio criar nova vari�vel 'carnaval', para corrigir o modelo

dates <- c("02/12/2002","03/04/2003","02/24/2004","02/08/2005",
           "02/28/2006","02/20/2007","02/05/2008","02/24/2009",
           "02/16/2010","03/08/2011","02/21/2012","02/12/2013",
           "03/04/2014","02/17/2015","02/09/2016","02/28/2017")
carnaval.date <- as.Date(dates,  "%m/%d/%Y")
carnaval <- genhol(carnaval.date, start = -3, end = 1, frequency = 12)


pim_ajuste <- seas(pim_ts, transform.function = 'none', xreg = carnaval, 
                   regression.variables = 'td1coef')

summary(pim_ajuste)

qs(pim_ajuste)

plot(pim_ajuste, main = '')
legend('topleft', legend = c('Observada', 'Com ajuste sazonal'), lty = 1, col = c(1, 2),
       lwd = c(1, 2), bty = 'n')






