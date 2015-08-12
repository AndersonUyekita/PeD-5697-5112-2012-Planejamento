source(file = "GeraInd_2.0.R",local = FALSE)
source(file = "GeraPop_2.0.R",local = FALSE)
source(file = "AvaliaPop.R",local = FALSE)
source(file = "Selecao_3.0.R",local = FALSE)
source(file = "CrossingOver_3.0.R",local = FALSE)
source(file = "Dominancia.R",local = FALSE)
source(file = "Geracao.R",local = FALSE)
source(file = "Mutacao.R",local = FALSE)
source(file = "torneio.R",local = FALSE)


restricao <- 10000
individuos <- 800
tam_pop <- 400



database<-read.csv(file = "TESTE.csv",sep =";")

populacao <- GeraPop(database,restricao,tam_pop,individuos)

lista <- AvaliaPop(database,populacao,individuos)

############################################## 1a Geracao ########################################

plot(x = lista[[2]],y = lista[[4]],xlim = c(80,115),ylim=c(75,150),main = "1a Geracao")

populacao_selecionada <- Selecao(restricao,lista,individuos)

teste <- AvaliaPop(database,populacao_selecionada,nrow(populacao_selecionada))

plot(x = teste[[2]],y = teste[[4]],xlim = c(80,115),ylim=c(75,150),main = "1a Selecao")


populacao_selecionada_mutada <- Mutacao(populacao_selecionada)

lista_selecionada <- AvaliaPop(database,populacao_selecionada_mutada,nrow(populacao_selecionada_mutada))

plot(x = lista_selecionada[[2]],y = lista_selecionada[[4]],xlim = c(80,150),ylim=c(75,150),main = "1a Mutacao")

populacao_selecionada_v2 <- CrossingOver(restricao,lista_selecionada,populacao_selecionada_mutada,individuos)

lista <- AvaliaPop(database,populacao_selecionada_v2,individuos)

plot(x = lista[[2]],y = lista[[4]],xlim = c(80,150),ylim=c(75,150),main = "1a Cruzamento")


############################################## 2a Geracao ########################################
for (i in 1:100)
        {
        populacao_selecionada <- Selecao(restricao,lista,individuos)
        
        teste <- AvaliaPop(database,populacao_selecionada,nrow(populacao_selecionada))
        
        #plot(x = teste[[2]],y = teste[[4]],xlim = c(80,150),ylim=c(75,150),main = paste(i,"° Populacao",sep =""))
        #plot(x = teste[[2]],y = teste[[4]],xlim = c(140,155),ylim=c(140,155),main = paste(i,"° Populacao",sep =""))
        plot(x = teste[[2]],y = teste[[4]],main = paste(i,"° Populacao",sep =""))
        
        
        
        
        populacao_selecionada_mutada <- Mutacao(populacao_selecionada)
        
        lista_selecionada <- AvaliaPop(database,populacao_selecionada_mutada,nrow(populacao_selecionada_mutada))
        
        #plot(x = lista_selecionada[[2]],y = lista_selecionada[[4]],xlim = c(80,115),ylim=c(75,110),main = "2a Mutacao")
        
        populacao_selecionada_v2 <- CrossingOver(restricao,lista_selecionada,populacao_selecionada_mutada,individuos)
        
        lista <- AvaliaPop(database,populacao_selecionada_v2,individuos)
        
        #plot(x = lista[[2]],y = lista[[4]],xlim = c(80,115),ylim=c(75,110),main = "2o Cruzamento")
        }

############################################## Resultados ########################################

populacao_selecionada <- Selecao(restricao,lista,individuos)
teste <- AvaliaPop(database,populacao_selecionada,nrow(populacao_selecionada))

dominancia_teste <- Dominancia(teste,nrow(teste[[7]]))


resultados <- matrix()
resultados <- cbind(teste[[2]],teste[[4]],teste[[6]])




plot(teste[[2]],teste[[4]], cex = 1, pch = 1, ylab = "F2", xlab = "F1", main = "Fronteira de Pareto")



points(teste[[2]][dominancia_teste==0],teste[[4]][dominancia_teste==0],col="red",pch=3,cex=.6)
points(teste[[2]][dominancia_teste==1],teste[[4]][dominancia_teste==1],col="blue",pch=5,cex=.6)

plot(c(130.6757,144.6357,teste[[2]]),c(128.9162,138.7942,teste[[4]]), cex = 1, pch = 1, ylab = "F2", xlab = "F1", main = "Fronteira de Pareto")
points(130.6757,128.9162,col="blue",pch=16,cex=.6)
points(144.6357,138.7942,col="blue",pch=17,cex=.6)





resultados
