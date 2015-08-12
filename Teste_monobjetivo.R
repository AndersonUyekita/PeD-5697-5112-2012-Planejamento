source(file = "GeraInd_2.0.R",local = FALSE)
source(file = "GeraPop_2.0.R",local = FALSE)
source(file = "selecao_apriori.R",local = FALSE)
source(file = "Mutacao.R",local = FALSE)
source(file = "AvaliaPop.R",local = FALSE)
source(file = "CrossingOver_apriori.R",local = FALSE)

# Inicializacao das variaveis e restricoes
restricao <- 10000
individuos <- 800
tam_pop <- 400
## Pesos
a <- .50
b <- .50
IDP <- numeric()

#Carregar dados aleatorios
database <- read.csv(file = "TESTE.csv",sep =";")

#Gerar a populacao
populacao <- GeraPop(database,restricao,tam_pop,individuos)

#Calculo do IDP da obra com pesos iguais para F1 e F2
IDP_obra <- as.numeric((database[[1]])*a+(database[[2]])*b)

#Criacao da Matrix IDP_0.5 e Custos
Matrix_IDP_Custo <- cbind(database,IDP_obra)

#Atribuicao de nomes nas colunas da Matrix
colnames(Matrix_IDP_Custo) <- c("F1","F2","Custo","IDP")


################# Primeira Selecao ##############################

#Selecao de individuos
populacao_selecionada <- selecao_apriori(populacao,Matrix_IDP_Custo)

plot(c(populacao_selecionada[[2]]),c(populacao_selecionada[[3]]),xlab = "F1", ylab = "F2", main = "1a Populacao")
plot(c(130.6757,populacao_selecionada[[2]]),c(128.9162,populacao_selecionada[[3]]),xlab = "F1", ylab = "F2", main = "1a Populacao")
points(130.6757,128.9162,col="blue",pch=16,cex=1)

populacao_selecionada_mutada <- Mutacao(populacao_selecionada[[1]])

lista_selecionada <- AvaliaPop(database,populacao_selecionada_mutada,nrow(populacao_selecionada_mutada))

#plot(lista_selecionada[[2]],lista_selecionada[[4]])

populacao_selecionada_v2 <- CrossingOver(restricao,lista_selecionada,populacao_selecionada_mutada,individuos)

lista <- AvaliaPop(database,populacao_selecionada_v2,individuos)

#plot(lista[[2]],lista[[4]])



################# Segunda Selecao ##############################
for (i in 1:999)
        {
        populacao_selecionada <- selecao_apriori(lista[[7]],Matrix_IDP_Custo)
        
        plot(c(130.6757,populacao_selecionada[[2]]),c(128.9162,populacao_selecionada[[3]]),xlab = "F1", ylab = "F2", main = paste(i+1,"a Populacao"))
        points(130.6757,128.9162,col="blue",pch=16,cex=1)
        
        populacao_selecionada_mutada <- Mutacao(populacao_selecionada[[1]])
        
        lista_selecionada <- AvaliaPop(database,populacao_selecionada_mutada,nrow(populacao_selecionada_mutada))
        
        #plot(lista_selecionada[[2]],lista_selecionada[[4]])
        
        populacao_selecionada_v2 <- CrossingOver(restricao,lista_selecionada,populacao_selecionada_mutada,individuos)
        
        lista <- AvaliaPop(database,populacao_selecionada_v2,individuos)

}




#plot(x = lista[[2]],y = lista[[4]],xlim = c(80,150),ylim=c(75,150),main = "1a Cruzamento")

selecao_final <- lista[[7]][lista[[6]] <= 10000,TRUE]

lista <- AvaliaPop(database,selecao_final,nrow(selecao_final))
solucao_otimizada <- (max((lista[[2]]+lista[[4]])*0.5) == (lista[[2]]+lista[[4]])*0.5)

## lista[[2]][solucao_otimizada]
## lista[[4]][solucao_otimizada]

#Resultados do algoritmo mono-objetivo
plot(c(lista[[2]],lista[[2]][solucao_otimizada]),c(lista[[4]],lista[[4]][solucao_otimizada]),xlab = "F1", ylab = "F2", main = "Comparativo")

# Comparativo com o resultado da metodologia atual
plot(c(lista[[2]],lista[[2]][solucao_otimizada],130.6757),c(lista[[4]],lista[[4]][solucao_otimizada],128.9162),xlab = "F1", ylab = "F2", main = "Comparativo")
points(lista[[2]][solucao_otimizada],lista[[4]][solucao_otimizada],col="red",pch=3,cex=.6)
points(130.6757,128.9162,col="blue",pch=16,cex=.6)


