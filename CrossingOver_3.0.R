CrossingOver <- function(restrição,lista_selecionada,populacao_selecionada,individuos)
{
        f1_selecionado <- lista_selecionada[[2]]
        f2_selecionado <- lista_selecionada[[4]]
        tamanho <- length(f1_selecionado)
        d <- vector()
        probabi<- vector()

        d <- Dominancia(lista_selecionada,tamanho)

        probabi <- (individuos-d)/sum(individuos-d)

        limite <- ncol(lista_selecionada[[1]])
        offspring <- 1
        populacao_selecionada_v2 <- t(populacao_selecionada)

        while (offspring <= (individuos-limite))
        {
                #Escolha aleatoria dos pais

                pai <- sample(1:limite,1,replace = FALSE, prob = NULL)
                mae <- sample(1:limite,1,replace = FALSE, prob = NULL)
                
                #Crossing Over
                local <- sample(2:399,1,replace = FALSE, prob = NULL)
                
                cromossomo1 <- populacao_selecionada_v2[1:local,pai]

                cromossomo2 <- populacao_selecionada_v2[(local+1):400,mae]
                
                
                cromossomofilho <- c(cromossomo1,cromossomo2)
                
                custo_filho <- database[,3]*cromossomofilho
                
                custo_filho <- sum(custo_filho)
                
                
                cbind(cromossomofilho,custo_filho)
                
                if (custo_filho < restrição)
                {
                        populacao_selecionada_v2=cbind(populacao_selecionada_v2,cromossomofilho)
                        offspring=offspring+1
                }
        }
        t(populacao_selecionada_v2)
}