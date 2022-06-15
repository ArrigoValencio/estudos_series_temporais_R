if(!dir.exists('~/Analise Series Temporais/Chap3_Processos nao Estacionarios')) {
  
  dir.create('~/Analise Series Temporais/Chap3_Processos nao Estacionarios')
}

setwd('~/Analise Series Temporais/Chap3_Processos nao Estacionarios')

#Análise exploratória da ST de vendas de passagens aéreas

data("AirPassengers")

ts.plot(AirPassengers, ylab = 'Vendas de Passagens Aéreas', xlab = 'Anos')

#Análise mais profunda da sazonalidade

monthplot(AirPassengers, ylab = 'Vendas de Passagens Aéreas', xlab = 'Meses')

#Decomposição da série
#tendência + ciclo; sazonalidade; erro (componente irregular ou inovação)

plot(decompose(AirPassengers))



