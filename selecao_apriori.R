selecao_apriori <- function(populacao,Matrix_IDP_Custo)
{
        
        #Elimina duplicatas
        hahahahahah <- !duplicated.array(populacao)
        populacao <- populacao[hahahahahah,TRUE]
        
        IDP_individuo <- numeric()
        F1_individuo <- numeric()
        F2_individuo <- numeric()
        custo_individuo <- numeric()
        # Calculo do IDP de cada individuo
        for (i in 1:nrow(populacao))
                {
                IDP_individuo[i] <- sum(populacao[i,]*Matrix_IDP_Custo[,4])
                F1_individuo[i] <- sum(populacao[i,]*Matrix_IDP_Custo[,1])
                F2_individuo[i] <- sum(populacao[i,]*Matrix_IDP_Custo[,2])
                custo_individuo[i] <- sum(populacao[i,]*Matrix_IDP_Custo[,3])
                }
        
        # Vetor de selecao de individuos
        vetor_selecao_individuo <- IDP_individuo > median(IDP_individuo)
        
        # Selecao dos individuos que passaram pelo crivo de serem maiores que a mediana
        t_populacao <- as.data.frame(populacao)
        
        # Geracao do banco de dados apos selecao
        IDP_selecionado <- IDP_individuo[vetor_selecao_individuo]
        populacao_selecionada <- t_populacao[vetor_selecao_individuo,TRUE]
        F1_selecionado <- F1_individuo[vetor_selecao_individuo]
        F2_selecionado <- F2_individuo[vetor_selecao_individuo]
        custo_selecionado <- custo_individuo[vetor_selecao_individuo]
        list(populacao_selecionada,F1_selecionado,F2_selecionado,custo_selecionado)
}