Corpo <- function(seletivo,geracoes)
{
        pop_inic <- numeric()
        restricao <- 15000
        cromossomos <- 400
        individuos <- 100
        seletivo <- seletivo
        dist <- numeric()
        historico<- numeric()
        #Primeira Geracao
        pop_inic <- GeraPop(pop_inic,restricao,cromossomos,individuos)
        
        T_f1 <- pop_inic[[2]]
        T_f2 <- pop_inic[[3]]
        
        dist<-sqrt((max(T_f1)-T_f1)*(max(T_f1)-T_f1)+(max(T_f2)-T_f2)*(max(T_f2)-T_f2))
        historico[1]<-sum(dist)
        
        #Selecao da Primeira Geracao
        pop_sel <- Selecao(pop_inic,individuos,seletivo)
        
        #Cruzamento entre a população remanescente
        pop_sel <- CrossingOver(pop_sel)
        
        
        i=2
        while (i < geracoes)
        {
                #Completa Populacao = Segunda Geracao
                pop_sel2 <- GeraPop(pop_sel,restricao,cromossomos,individuos)
                
                T_f1 <- pop_sel2[[2]]
                T_f2 <- pop_sel2[[3]]
                dist[i]<-min(sqrt((max(T_f1)-T_f1)*(max(T_f1)-T_f1)+(max(T_f2)-T_f2)*(max(T_f2)-T_f2)))
                historico[i]<-sum(dist)       
        
                #Selecao da Segunda Geracao
                pop_sel <- Selecao(pop_sel2,individuos,seletivo)
        
                i=i+1        
        }
        list(pop_sel,pop_sel2)
}