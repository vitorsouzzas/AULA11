#Aula 11
remove.packages(c("ggplot2", "data.table"))
install.packages('Rcpp', dependencies = TRUE)
install.packages('ggplot2', dependencies = TRUE)
install.packages('data.table', dependencies = TRUE)

install.packages("Hmisc")      #Instala Pacote Hmisc  
install.packages("forecast")
library(Hmisc)
library(forecast)
library(readxl)               #Carrega os Pacotes


IPCA.df<-read_excel("c:/Econometria/IPCA.xls")                       #Carrega o arquivo
plot(IPCA.df$IPCA, type = "l")                                       #Cria um gráfico do arquivo
MM <- data.frame(na.omit(ma(IPCA.df$IPCA,order = 12, centre = T)))   #Cria  uma serie de médias móveis tradicionais omitindo oa NAS de lag(N)=12    

a <- (127-nrow(MM))+1                                                #Define um parâmetro a para ponderar as perdas de dados para ponderação da média
IPCA.DF <- as.data.frame(IPCA.df$IPCA[a:127])                        #Define IPCA.DF como uma vetor do mesmo tamanho que o vetor das médias móveis MM
Tabela1 <- cbind(IPCA.DF,MM)                                         #Cria a Tabela de Dados Tabela1
colnames(Tabela1) <- c("IPCA","Média Móvel")
View(Tabela1)

Grafico <- ts(Tabela1, start = 2008, frequency = 12)                 #Cria a Serie Temporal "Grafico" mensal iniciando em 2008

plot(Grafico, plot.type= "single", col=c("Black","Blue"))            #Cria o gráfico da série de dados e de médias móveis conjuntamente.
z <- lm(IPCA.df$IPCA~IPCA.df$Ano.Mês)                                #Regride os dados em relação ao tempo e verifica a tendência
abline(z, col="Green")                                               #Coloca a linha de regressão de tendência no gráfico
summary(z)

tabela2 <- as.data.frame(Tabela1$IPCA/Tabela1$`Média Móvel`)
plot(tabela2)

Inflacao <- ts(IPCA.df$IPCA, start = 2008, frequency = 12)

decomposicao <- decompose(Inflacao)                                          #Decompõe a série em Ciclo, Tendência e Sazonalidade

plot(decompose(Inflacao))  

Tendencia <- decomposicao$trend

Sazonalidade <- decomposicao$seasonal

Ciclo <- decomposicao$random

Tab_Dados1 <- data.frame(IPCA.df$IPCA, Ciclo)

View(Tab_Dados1)

plot(Sazonalidade, type="l")

Serie_Tempo1 <- ts(Tab_Dados1, start = 2008, frequency = 12)
plot(Serie_Tempo1, plot.type = "single", col= c("Blue", "Red"))

Tab_Dados2 <- data.frame(IPCA.df$IPCA, Tendencia)
Serie_Tempo2 <- ts(Tab_Dados2, start = 2008, frequency = 12)
plot(Serie_Tempo2, plot.type = "single", col= c("Blue", "Red"))
