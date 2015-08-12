setwd("PODI2014")

source(file = "GeraInd.R",local = FALSE)
source(file = "Selecao.R",local = FALSE)
source(file = "Mutacao.R",local = FALSE)
source(file = "Avalia.R",local = FALSE)
source(file = "CrossingOver.R",local = FALSE)


#Setando dados iniciais
individuos <- 100
peso_a <- 1.00
peso_b <- 1.00
peso_c <- 1.00

#Carregar dados do Neissan de 2014: Esses dados ja foram multiplicados pelos pesos
raw_database <- read.csv(file = "database_IDP.csv",sep =";")

#relavancia
relevancia <- raw_database$Relevância

#Tratamento da database
database <- cbind.data.frame(raw_database$DC,raw_database$DV,raw_database$DO,raw_database$custo)
colnames(database) <- c("DC","DV","DO","custo")

#Obras selecionadas no PODI2014 - Sera o ponto de partida de comparacao
PODI2014 <- as.logical(raw_database$PODI2014)

#Quantidade de obras selecionadas no PODI 2014
#sum(PODI2014)
#print (paste("Foram selecionadas",sum(PODI2014),"obras no PODI2014"))


#Calculo da restricao orcamentaria a partir do PODI2014 (benchmark)
#restricao <- sum(database$custo[PODI2014])
#print(paste("O custo total dessas obras",restricao,"reais"))

#Hipotecicamente SEM LIMITADOR, o valor maximo que DC, DV e Do podem atingir. Isto seria se todas as obras fossem realizadas
max_sem_limites_DC <- sum(database$DC)
max_sem_limites_DV <- sum(database$DV)
max_sem_limites_DO <- sum(database$DO)
max_custo <- sum(database$custo)

#Tratamento da database para saturar os desempenhos em 120

# Casos de saturacao
sum(database[,1:3] > 120)

# Aplicacao da saturacao na database
database$DC[database$DC > 120] <- 120
database$DV[database$DV > 120] <- 120
database$DO[database$DO > 120] <- 120

# Aplicacao da relevancia
database$DC <- database$DC*relevancia
database$DV <- database$DV*relevancia
database$DO <- database$DO*relevancia


#Hipotecicamente COM LIMITADOR, o valor maximo que DC, DV e Do podem atingir. Isto seria se todas as obras fossem realizadas
max_sem_limites_DC <- sum(database$DC)
max_sem_limites_DV <- sum(database$DV)
max_sem_limites_DO <- sum(database$DO)

#calculos do DC, DV e DO das obras selecionadas
DC_PODI2014 <- sum(database$DC[PODI2014])
DV_PODI2014 <- sum(database$DV[PODI2014])
DO_PODI2014 <- sum(database$DO[PODI2014])
custo_PODI2014 <- sum(database$custo[PODI2014])
IDP_PODI2014 <- DC_PODI2014*peso_a+DV_PODI2014*peso_b+max_sem_limites_DO*peso_c

teste_DC <- c(max_sem_limites_DC,DC_PODI2014)
teste_DV <- c(max_sem_limites_DV,DV_PODI2014)
teste_DO <- c(max_sem_limites_DO,DO_PODI2014)

#hist(database$custo, breaks = 5, main = "Histograma dos Custos")

#summary(database$custo)

#print (paste("quantidade media de obras selecionadas",restricao/summary(database$custo)[4]))

#hist(database$DC)
#hist(database$DV)
#hist(database$DO)

#Gerar a populacao
cromossomos <- nrow(database)
restricao <- custo_PODI2014
#restricao <- 20*1e6

populacao <- read.csv(file = "outfile.csv",sep =";")

#populacao <- numeric()
#pop <- 1
#while (pop <= individuos)
#{
#        Ind <- GeraInd(cromossomos)
#        custo <- sum(database*Ind)
#        
#        if (custo <= restricao)
#        {
#                populacao=rbind(populacao,Ind)
#                pop=pop+1
#        }
#        print(pop)
#        
#}

#Calculo do IDP das obras
IDP_obra <- as.numeric(database$DC*peso_a + database$DV*peso_b + database$DO*peso_c)

#Criacao da Matrix indicadores, IDP e Custos
Matrix_IDP_Custo <- cbind.data.frame(database$DC,database$DV,database$DO,database$custo,IDP_obra)

#Atribuicao de nomes nas colunas da Matrix
colnames(Matrix_IDP_Custo) <- c("DC","DV","DO","custo","IDP")


#Selecao de individuos
populacao_selecionada <- selecao_apriori(populacao,Matrix_IDP_Custo)

#Mutacao dos individuos
populacao_selecionada_mutada <- Mutacao(populacao_selecionada[[1]],nrow(database))

#Avalicao dos individuos mutados
lista_selecionada <- AvaliaPop(database,populacao_selecionada_mutada,nrow(populacao_selecionada_mutada))

#Cruzamento dos individuos
populacao_selecionada_v2 <- CrossingOver(restricao,lista_selecionada,populacao_selecionada_mutada,individuos)

#Avaliacao dos individuos cruzados
lista <- AvaliaPop(database,populacao_selecionada_v2,individuos)
#plot(lista[[2]],lista[[6]])

for (i in 1:100)
        {
        populacao_selecionada <- selecao_apriori(lista[[9]],Matrix_IDP_Custo)
        populacao_selecionada_mutada <- Mutacao(populacao_selecionada[[1]],nrow(populacao_selecionada[[1]]))
        lista_selecionada <- AvaliaPop(database,populacao_selecionada_mutada,nrow(populacao_selecionada_mutada))
        populacao_selecionada_v2 <- CrossingOver(restricao,lista_selecionada,populacao_selecionada_mutada,individuos)
        lista <- AvaliaPop(database,populacao_selecionada_v2,individuos)
        vetor_selecao <- max(lista[[2]]*peso_a+lista[[4]]*peso_b+lista[[6]]*peso_c) == lista[[2]]*peso_a+lista[[4]]*peso_b+lista[[6]]*peso_c
        print(paste(lista[[2]][vetor_selecao],lista[[4]][vetor_selecao],lista[[6]][vetor_selecao]))
        }
