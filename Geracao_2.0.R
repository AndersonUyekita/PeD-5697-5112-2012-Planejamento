Geracao <- function(restricao,lista,iteracao,individuos)
{
        
        for (i in 1:iteracao) 
        {
                populacao_selecionada <- Selecao(lista,individuos)
                
                populacao_selecionada_mutada <- Mutacao(populacao_selecionada)
                
                lista_selecionada <- AvaliaPop(database,populacao_selecionada_mutada,nrow(populacao_selecionada_mutada))
                
                populacao_selecionada_v2 <- CrossingOver(restricao,lista_selecionada,populacao_selecionada_mutada,individuos)
                
                lista <- AvaliaPop(database,populacao_selecionada_v2,individuos)
                
                plot(x = lista[[2]],y = lista[[4]],xlim = c(80,115),ylim=c(75,110))
                #print(head(lista[[6]]))
                }
        lista
}