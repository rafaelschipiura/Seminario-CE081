setwd("~/GitHub/Seminario-CE081")
library(ggplot2)
dados <- read.csv("~/GitHub/Seminario-CE081/Dados Seminario.csv",
                  sep = ";", dec = ",", header = T)

# deletado: referencia.do.trabalho
dados <- dados[,2:13]
# categoricos: bioma, estacao, clima, solofao, textura
dados$Estacao <- as.factor(dados$Estacao)
dados$Clima <- as.factor(dados$Clima)
dados$Bioma <- as.factor(dados$Bioma)
dados$Solofao <- as.factor(dados$Solofao)
dados$Textura <- as.factor(dados$Textura)
dados$pH <- cut(dados$pH, breaks=c(3,4,5,6,7,8))
dados$pH <- as.factor(dados$pH)
# variavel resposta "Dens"
summary(dados)

# box-plot Solofao
png(filename="boxplot_Solofao.png")
ggplot(dados, aes(x = Solofao, y = Dens)) + geom_boxplot() +
theme(axis.text.x = element_text(angle=45, vjust=.5, hjust=1))
png(filename="boxplot_Solofao_log.png")
ggplot(dados, aes(x = Solofao, y = log(Dens))) + geom_boxplot()

# box-plot Estacao
png(filename="boxplot_Estacao.png")
ggplot(dados, aes(x = Estacao, y = Dens)) + geom_boxplot()
png(filename="boxplot_Estacao_log.png")
ggplot(dados, aes(x = Estacao, y = log(Dens))) + geom_boxplot()

# box-plot Bioma
png(filename="boxplot_Bioma.png")
ggplot(dados, aes(x = Bioma, y = Dens)) + geom_boxplot()
png(filename="boxplot_Bioma_log.png")
ggplot(dados, aes(x = Bioma, y = log(Dens))) + geom_boxplot()

# box-plot Textura
png(filename="boxplot_Textura.png")
ggplot(dados, aes(x = Textura, y = Dens)) + geom_boxplot() +
theme(axis.text.x = element_text(angle=45, vjust=.5, hjust=.5))
png(filename="boxplot_Textura_log.png")
ggplot(dados, aes(x = Textura, y = log(Dens))) + geom_boxplot() +
theme(axis.text.x = element_text(angle=45, vjust=.5, hjust=.5))

# box-plot Ph
png(filename="boxplot_PH.png")
ggplot(dados, aes(x = pH, y = Dens)) + geom_boxplot()
png(filename="boxplot_PH_log.png")
ggplot(dados, aes(x = pH, y = log(Dens))) + geom_boxplot()

