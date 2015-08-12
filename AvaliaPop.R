AvaliaPop <- function(database,Populacao,individuos)
{
        t_Populacao=t(Populacao)
        
        f1 <-  database[,1]*t_Populacao
        f2 <-  database[,2]*t_Populacao
        custo <- database[,3]*t_Populacao
        
        f1_sum <- vector()
        f2_sum <- vector()
        custo_sum <- vector()
        for (i in 1:individuos)
                {
                f1_sum[i] <- sum(f1[,i])
                f2_sum[i] <- sum(f2[,i])
                custo_sum[i] <- sum(custo[,i])
                }
        
        #plot(f1_sum,f2_sum)
        #hist(custo_sum)
        list(f1,f1_sum,f2,f2_sum,custo,custo_sum,Populacao)
}