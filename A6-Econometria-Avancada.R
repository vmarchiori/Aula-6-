# Econometria Avançada A6

install.packages("readxl") #instalando o pacote readxl
library(readxl) #rodando o pacote readxl
install.packages("ggplot2") #instalando o pacote ggplot2
library(ggplot2) #rodando o pácote ggplot2
IPCA <- read_excel("C:/Econometria/IPCA.xls") #Mostra o arquivo excel IPCA salvo na Rede
IPCA <- IPCA[,-1] #exclui a primeira coluna da tabela
Inflação <- ts(IPCA$IPCA, start = 2008-01, frequency = 12) #cria uma série temporal Inflação
View(Inflação) # Visualizar os dados sobre a inflação
write.csv(Inflação,file = "Inflação.csv")
autoplot(Inflação, main="Índice de Preços ao Consumidor Amplo") #novo comando para criar gráficos
plot(Inflação, main="Índice de Preços ao Consumidor Amplo") #cria uma gráfico comum quando o autoplot não funciona

# Informações Sobre o Resumo Estatístico

Resumo_Estatístico <- summary(Inflação) #cria um resumo estatístico para Inflação
Resumo_Estatístico #vizualiza o resumo estatístico

# Criando Analises FAC e FACP, com os dados informadis

acf(IPCA) #cria um gráfico FAC para o IPCA
pacf(IPCA) #cria uma gráfico FACP para o IPCA

# Modelos Autoregressivos e de Médias Móveis

AR1 <- arima(Inflacao, order = c(1,0,0)) #modelo auto regressivo de ordem 1
AR1
MA3 <- arima(Inflacao,order=c(0,0,3)) #modelo de médias móveis de ordem 3
MA3
ARMA13 <- arima(Inflacao,order = c(1,0,3)) #junção da AR com a MA
ARMA13
ARMA13$residuals #gera os resíduos da ARMA

# Teste de Ljung-Box - Teste de Autocorrelação de Erros

TesteJB13 <- Box.test(ARMA13$residuals,lag = 3, type = "Ljung") #teste de Ljung-Box para autocrrelação de erros do modelo ARMA auto regressivo de médias móveis
TesteJB13

MA1 <- arima(Inflação,order = c(0,0,1)) #modelo de médias móveis de ordem 1
TesteJB1 <- Box.test(MA1$residuals,lag = 3, type = "Ljung") #teste de Ljung-Box para a MA1
TesteJB1

MA2 <- arima(Inflação,order = c(0,0,2)) #modelo de médias móveis de ordem 2
TesteJB2 <- Box.test(MA2$residuals,lag = 3, type = "Ljung") #teste de Ljung-Box para a MA2
TesteJB2

MA3 <- arima(Inflação,order = c(0,0,3)) #modelo de médias móveis de ordem 3
TesteJB3 <- Box.test(MA3$residuals,lag = 3, type = "Ljung") #teste de Ljung-Box para a MA3
TesteJB3

# Tabela de Dados com o Resultado dos Teste

P_Valores <- c(TesteJB1$p.value,TesteJB2$p.value,TesteJB3$p.value) #cria um vetor com os p valores dos testes
Modelos <- c("MA1","MA2","MA3") #cria um vetor com os nomes dos modelos
Resultados <- data.frame(Modelos,P_Valores) #cria uma tabela com os dois vetores criados
View(Resultados)
write.csv(Resultados,file = "Resultados.csv")

#Salvar CNVAZQUEZ