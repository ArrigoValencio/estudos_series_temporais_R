
if(!dir.exists('~/Analise Series Temporais/Chap2_Modelos de Suavizacao Exponencial')) {
 
   dir.create('~/Analise Series Temporais/Chap2_Modelos de Suavizacao Exponencial')
}

setwd('~/Analise Series Temporais/Chap2_Modelos de Suavizacao Exponencial')

#2.1 Introdução
#decomposição da série elecequip (índice de novos pedidos de fabricação de eletrônicos, 16 países Europa)

library(fpp)
data(elecequip)
plot(elecequip, xlab = 'Tempo', ylab= 'Índice de novas ordens')

decomp <- decompose(elecequip, type = 'additive')
plot(decomp)

ajuste_sazonal <- elecequip - decomp$seasonal
plot(ajuste_sazonal)

#decomposição da série cafe (despesas trimestrais de café em restaurantes e serviços de comida na Austrália)
#decomposição pelo filtro Hodrick-Prescott

data(cafe)
library(mFilter)
filtro_hp <- hpfilter(cafe, type = 'lambda')

par(mfrow = c(1, 2))

plot(cafe, ylab = 'Despesas Trimestrais', xlab = 'tempo')
lines(filtro_hp$trend, col = 'red', lwd = 2)
legend(1985, 8000, c('Série Original', 'Tendência - Filtro HP'), col = c('black', 'red'), lwd = c(1, 2), bty = 'n')

plot(filtro_hp$cycle, ylab = 'Componente Cíclica', xlab = 'tempo')









