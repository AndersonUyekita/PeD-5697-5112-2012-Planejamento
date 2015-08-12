Selecao <- function(pop_inic,individuos,seletivo)
{
        T_f1_new <- numeric()
        T_f2_new <- numeric()
        T_custo_new <- numeric()
        Populacao_new <- numeric()
        Populacao <- pop_inic[[1]]
        T_f1 <- pop_inic[[2]]
        T_f2 <- pop_inic[[3]]
        custo <- pop_inic[[4]]
        dist <- numeric()
        
        dist <- sqrt((max(T_f1) - T_f1)^2+(max(T_f2) - T_f2)^2)

        teste <- order(dist) <= seletivo
        j <- 1
        for (i in 1:individuos)
        {
                
                if (teste[i] == TRUE)
                        {
                        Populacao_new <- cbind(Populacao_new,Populacao[,i])
                        T_f1_new <- cbind(T_f1_new,T_f1[i])
                        T_f2_new <- cbind(T_f2_new,T_f2[i])
                        T_custo_new <- cbind(T_custo_new,custo[i])
                        }
        }
        list(Populacao_new,T_f1_new,T_f2_new,T_custo_new)
}