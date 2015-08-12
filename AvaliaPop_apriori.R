AvaliaPop <- function(database,Populacao,individuos)
{
        t_Populacao=t(Populacao)
        
        DC <-  database$DC*t_Populacao
        DV <-  database$DV*t_Populacao
        DO <-  database$DO*t_Populacao
        custo <- database$custo*t_Populacao
        
        DC_sum <- vector()
        DV_sum <- vector()
        DO_sum <- vector()
        custo_sum <- vector()
        for (i in 1:individuos)
                {
                DC_sum[i] <- sum(DC[,i])
                DV_sum[i] <- sum(DV[,i])
                DO_sum[i] <- sum(DO[,i])
                custo_sum[i] <- sum(custo[,i])
                }
        
        #plot(f1_sum,f2_sum)
        #hist(custo_sum)
        list(DC,DC_sum,DV,DV_sum,DO,DO_sum,custo,custo_sum,Populacao)
}