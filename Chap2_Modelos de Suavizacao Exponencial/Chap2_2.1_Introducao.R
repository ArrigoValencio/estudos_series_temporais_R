
if(!dir.exists('~/Analise Series Temporais/Chap2_Modelos de Suavizacao Exponencial')) {
 
   dir.create('~/Analise Series Temporais/Chap2_Modelos de Suavizacao Exponencial')
}

setwd('~/Analise Series Temporais/Chap2_Modelos de Suavizacao Exponencial')

#2.1 Introdu��o
#decomposi��o da s�rie elecequip (�ndice de novos pedidos de fabrica��o de eletr�nicos, 16 pa�ses Europa)

library(fpp)
data(elecequip)
plot(elecequip, xlab = 'Tempo', ylab= '�ndice de novas ordens')

decomp <- decompose(elecequip, type = 'additive')
plot(decomp)

ajuste_sazonal <- elecequip - decomp$seasonal
plot(ajuste_sazonal)

#decomposi��o da s�rie cafe (despesas trimestrais de caf� em restaurantes e servi�os de comida na Austr�lia)
#decomposi��o pelo filtro Hodrick-Prescott

data(cafe)
library(mFilter)
filtro_hp <- hpfilter(cafe, type = 'lambda')

par(mfrow = c(1, 2))

plot(cafe, ylab = 'Despesas Trimestrais', xlab = 'tempo')
lines(filtro_hp$trend, col = 'red', lwd = 2)
legend(1985, 8000, c('S�rie Original', 'Tend�ncia - Filtro HP'), col = c('black', 'red'), lwd = c(1, 2), bty = 'n')

plot(filtro_hp$cycle, ylab = 'Componente C�clica', xlab = 'tempo')









