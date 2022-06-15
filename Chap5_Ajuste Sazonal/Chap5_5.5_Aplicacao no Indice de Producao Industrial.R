if(!dir.exists('~/Analise Series Temporais/Chap5_Ajuste Sazonal')) {
  
   dir.create('~/Analise Series Temporais/Chap5_Ajuste Sazonal')
}
setwd('~/Analise Series Temporais/Chap5_Ajuste Sazonal')


#Aplicação do ajuste sazonal (X-13-ARIMA-SEATS) no Índice de Produção Industrial

library(seasonal)

#Etapas de um ajuste sazonal segundo a literatura:
#Análise gráfica;
#Execução do X13-ARIMA-SEATS no modo automático;
#Avaliação do ajuste sazonal realizado no passo anterior;
#Correção do ajuste se necessário.

#download de dados
pim <- read.csv2('https://raw.githubusercontent.com/pedrocostaferreira/timeseries/master/cap5-ajuste-sazonal/pimpf.csv')
pim_ts <- ts(data = pim, start = c(2002, 1), frequency = 12)

#Análise gráfica

plot(pim_ts)

monthplot(pim_ts, col.base = 2, lty.base = 2)
legend('topleft', legend = c('pim', 'média'), lty = c(1, 2), col = c(1, 2), bty = 'n')

#Execução no modo automático

pim_auto_ajuste <- seas(pim_ts)

#Avaliação do ajuste feito acima

summary(pim_auto_ajuste)

#gráfico espectral ajuda na avaliação

library(ggplot2)

spec.orig <- data.frame(series(pim_auto_ajuste, 'sp0'))

ggplot(data = spec.orig, aes(x = 0:60, y = X10.Log.Spectrum_AdjOri.), colour = 'black') +
 geom_line() +
  geom_vline(colour = 'red', xintercept = c(10, 20, 30, 40, 50), linetype = 5) +
  geom_vline(colour = 'blue', xintercept = c(42, 52), linetype = 3) + 
  ylab('') + xlab('') + theme_bw() + 
  ggtitle('Spectral plot of the first-differenced original series') + 
  theme(plot.title = element_text(lineheight = 2, face = 'bold', size = 16))

#estatística QS ajuda na avaliação

qs(pim_auto_ajuste)

#gráfico SI ratio ajuda na avaliação

monthplot(pim_auto_ajuste, col.base = 1, lty.base = 2, lwd.base = 2)
legend('topleft', legend = c('SI', 'Fator sazonal', 'Média Fator Sazonal FS'), 
       cex = 0.7, lty = c(1, 1, 2), col = c(4, 2, 1), lwd = c(1, 2, 2), bty = 'n')

#Correção do ajuste automático
#por razões específicas, é necessário criar nova variável 'carnaval', para corrigir o modelo

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






