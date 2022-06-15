if(!dir.exists('~/Analise Series Temporais/Chap7_Regressao Dinamica')) {
  
  dir.create('~/Analise Series Temporais/Chap7_Regressao Dinamica')
}

setwd('~/Analise Series Temporais/Chap7_Regressao Dinamica')

# Inserindo dados artificiais

price<-c( 0.27, 0.28, 0.28, 0.28, 0.27, 0.26, 0.28 ,0.27 ,0.26 ,0.28, 0.28 ,0.27, 0.27, 0.29, 0.28,0.29, 0.28, 0.28, 0.28, 0.28, 0.29, 0.29, 0.28, 0.28, 0.28, 0.26, 0.26 ,0.26, 0.27 ,0.26)
cons<-c(0.39, 0.37, 0.39, 0.42, 0.41, 0.34, 0.33, 0.29, 0.27, 0.26, 0.29 ,0.30, 0.33, 0.32, 0.38,0.38, 0.47, 0.44, 0.39, 0.34, 0.32, 0.31, 0.28, 0.33, 0.31, 0.36, 0.38 ,0.42, 0.44 ,0.55)
income<-c(78, 79, 81, 80 ,76 ,78, 82, 79, 76, 79, 82, 85, 86, 83, 84, 82, 80, 78, 84, 86, 85, 87, 94, 92, 95, 96, 94, 96, 91, 90)
temp<-c(41,56,63,68,69,65,61,47,32,24,28,26,32,40,55,63,72,72,67,60,44,40,32,27,28,33,41,52,64,71)

# Agrupando os dados em um data.frame
dados = data.frame(cons, price, income, temp)

head(dados)

#estimando regressão linear clássica

reg <- lm(cons ~ price + income + temp, data = dados)

#testando correlação serial primeira ordem

require(lmtest)

#Durbin-Watson

dw_reg <- dwtest(cons ~ price + income + temp, data = dados)

dw_reg

#Breusch-Godfrey

bg_reg <- bgtest(cons ~ price + income + temp, data = dados)

bg_reg

#corrigindo correlação serial

#estimação do modelo com estrutura no erro
#cochrane-orcutt

install.packages('orcutt')

require(orcutt)

co_reg <- cochrane.orcutt(reg)

#Prais-Winsten

install.packages('prais')

require(prais)

pw_reg <- prais_winsten(cons ~ price + income + temp, data = dados, index = c("cons", "price", "income", "temp"))









