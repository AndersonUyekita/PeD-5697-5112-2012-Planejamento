CrossingOver <- function(lista_selecionada,populacao_selecionada)
{
        f1_selecionado <- lista_selecionada[[2]]
        order(f1_selecionado)
        f2_selecionado <- lista_selecionada[[4]]
        order(f2_selecionado)
        
        
        bbb <- sqrt((max(f1_selecionado)-f1_selecionado)^2 + (max(f2_selecionado)-f2_selecionado)^2)
        
        cccc <- order(bbb)
        
        probabi <- bbb/sum(bbb)
        
        #probabi
        
        #plot(probabi)
        
        #hist(probabi)
        
        #cbind(bbb,cccc)
        
        
        limite <- ncol(lista_selecionada[[1]])
        offspring <- 1
        populacao_selecionada_v2 <- t(populacao_selecionada)
        
        while (offspring <= (100-limite))
        {
                #Escolha aleatoria dos pais
                pai <- sample(1:limite,1,replace = FALSE, prob = probabi)
                mae <- sample(1:limite,1,replace = FALSE, prob = probabi)
                
                #Crossing Over
                local <- sample(1:400,1,replace = FALSE, prob = NULL)
                
                cromossomo1 <- populacao_selecionada_v2[1:local,pai]
                cromossomo2 <- populacao_selecionada_v2[(local+1):400,mae]
                
                
                cromossomofilho <- c(cromossomo1,cromossomo2)
                
                custo_filho <- database[,3]*cromossomofilho
                
                custo_filho <- sum(custo_filho)
                
                
                cbind(cromossomofilho,custo_filho)
                
                if (custo_filho < 15000)
                {
                        populacao_selecionada_v2=cbind(populacao_selecionada_v2,cromossomofilho)
                        offspring=offspring+1
                }
        }
        t(populacao_selecionada_v2)
}