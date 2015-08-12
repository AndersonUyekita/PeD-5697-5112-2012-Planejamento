GeraPop <- function(database,restricao,cromossomos,individuos)
{
        pop_inic <- numeric()

        if (length(pop_inic)==0)
        {
                Populacao <- numeric()
                T_custo <- numeric()
        }
        
        else
        {
                Populacao <- pop_inic[[1]]
                T_f1 <- pop_inic[[2]]
                T_f2 <- pop_inic[[3]]
                T_custo <- pop_inic[[4]]        
        }
        
        pop <- 1
        while (pop <= individuos)
        {
                Ind <- GeraInd(cromossomos)
                custo <- sum(database[,3]*Ind)
                
                if (custo <= restricao)
                {
                        Populacao=rbind(Populacao,Ind)
                        pop=pop+1
                }
                
        }
        
        Populacao
}

