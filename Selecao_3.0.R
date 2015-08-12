Selecao <- function(restricao,lista,individuos)
{
        f1 <- lista[[1]]
        f2 <- lista[[3]]
        Pop_sel <- lista[[7]]
        
        #Elimina duplicatas
        hahahahahah <- !duplicated.array(Pop_sel)
        Pop_sel <- Pop_sel[hahahahahah,TRUE]
        #print(paste("individuos unicos:",nrow(Pop_sel)))
        
        #Elimina acima do custo
        lista <- AvaliaPop(database,Pop_sel,nrow(Pop_sel))        
        custo_grande <- numeric()
        custo_grande <- lista[[6]] < restricao
        Pop_sel <- lista[[7]][custo_grande,TRUE]

        if (nrow(Pop_sel)<individuos) 
                {
                complemento_individuos <- GeraPop(database,restricao,400,(individuos-nrow(Pop_sel)))
                Pop_sel <- rbind(Pop_sel,complemento_individuos)
                }
        #print(paste("individuos complementados:",nrow(Pop_sel)))
        lista <- AvaliaPop(database,Pop_sel,nrow(Pop_sel))        
        
        
        #Vetor Dominancia
        dd <- vector()
        dd <- Dominancia(lista,nrow(Pop_sel))

        #metodo de selecao arcaica
        #escolhidos <- dd < summary(dd)[[3]]

        escolhidos <- torneio(lista,dd,0.5)
        
        populacao_selecionada <- Pop_sel[escolhidos,TRUE]
        populacao_selecionada #Populacao elitista
}
