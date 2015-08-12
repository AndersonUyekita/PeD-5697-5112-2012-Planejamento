selecao_apriori <- function(populacao,Matrix_IDP_Custo)
{
        
        #Elimina duplicatas
        hahahahahah <- !duplicated.array(populacao)
        populacao <- populacao[hahahahahah,TRUE]
        
        IDP_individuo <- numeric()
        DC_individuo <- numeric()
        DV_individuo <- numeric()
        DO_individuo <- numeric()
        custo_individuo <- numeric()
        # Calculo do IDP de cada individuo
        for (i in 1:nrow(populacao))
                {
                IDP_individuo[i] <- sum(populacao[i,]*Matrix_IDP_Custo$IDP)
                DC_individuo[i] <- sum(populacao[i,]*Matrix_IDP_Custo$DC)
                DV_individuo[i] <- sum(populacao[i,]*Matrix_IDP_Custo$DV)
                DO_individuo[i] <- sum(populacao[i,]*Matrix_IDP_Custo$DO)
                custo_individuo[i] <- sum(populacao[i,]*Matrix_IDP_Custo$custo)
                }
        
        # Vetor de selecao de individuos
        vetor_selecao_individuo <- IDP_individuo > median(IDP_individuo)
        
        # Selecao dos individuos que passaram pelo crivo de serem maiores que a mediana
        t_populacao <- as.data.frame(populacao)
        
        # Geracao do banco de dados apos selecao
        IDP_selecionado <- IDP_individuo[vetor_selecao_individuo]
        populacao_selecionada <- t_populacao[vetor_selecao_individuo,TRUE]
        DC_individuo <- DC_individuo[vetor_selecao_individuo]
        DV_individuo <- DV_individuo[vetor_selecao_individuo]
        DO_individuo <- DO_individuo[vetor_selecao_individuo]
        custo_selecionado <- custo_individuo[vetor_selecao_individuo]
        list(populacao_selecionada,DC_individuo,DV_individuo,DO_individuo,custo_selecionado)
}