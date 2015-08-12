GeraPop <- function(pop_inic,restricao,cromossomos,individuos)
{
        database<-read.csv(file = "TESTE.csv",sep =";" )
        pop<-1
        i<-1
        if (length(pop_inic)==0)
                {
                Populacao <- numeric()
                T_f1 <- numeric()
                T_f2 <- numeric()
                T_custo <- numeric()
                }
        else
                {
                Populacao <- pop_inic[[1]]
                T_f1 <- pop_inic[[2]]
                T_f2 <- pop_inic[[3]]
                T_custo <- pop_inic[[4]]        
                }
        while (pop <= individuos)
        {
                Ind<- GeraInd(cromossomos)
                f1 <- sum(database[,1]*Ind)
                f2 <- sum(database[,2]*Ind)
                custo <- sum(database[,3]*Ind)
                
                if (custo < restricao)
                {
                        Populacao=cbind(Populacao,Ind)
                        T_f1 <- cbind(T_f1,f1)
                        T_f2 <- cbind(T_f2,f2)
                        T_custo <- cbind(T_custo,custo)
                        pop=pop+1
                        i=i+1
                }
                
        }
        list(Populacao,T_f1,T_f2,T_custo)
}

