if(!dir.exists('~/Analise Series Temporais/Chap3_Processos nao Estacionarios')) {
  
  dir.create('~/Analise Series Temporais/Chap3_Processos nao Estacionarios')
}

setwd('~/Analise Series Temporais/Chap3_Processos nao Estacionarios')

#An�lise explorat�ria da ST de vendas de passagens a�reas

data("AirPassengers")

ts.plot(AirPassengers, ylab = 'Vendas de Passagens A�reas', xlab = 'Anos')

#An�lise mais profunda da sazonalidade

monthplot(AirPassengers, ylab = 'Vendas de Passagens A�reas', xlab = 'Meses')

#Decomposi��o da s�rie
#tend�ncia + ciclo; sazonalidade; erro (componente irregular ou inova��o)

plot(decompose(AirPassengers))



