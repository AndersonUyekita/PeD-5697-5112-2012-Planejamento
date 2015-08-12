Selecao <- function(lista)
{
        f1 <- lista[[1]]
        f2 <- lista[[3]]
        #plot(lista[[2]],lista[[4]])
        f1_max <- max(lista[[2]])
        f2_max <- max(lista[[4]])
        custo_max <- max(lista[[6]])
        
        dist <- numeric()
        
        
        dist <- sqrt( (f1_max-lista[[2]])^2 +(f2_max-lista[[4]])^2 )
        #hist(dist)
        
        escolhidos <- order(dist) <= 30
        
        
        f1 <- t(f1)
        f2 <- t(f2)
        custo <- t(lista[[5]])
        
        
        f1 <- f1[escolhidos,TRUE]
        f1 <- t(f1)
        f2 <- f2[escolhidos,TRUE]
        f2 <- t(f2)
        custo <- custo[escolhidos,TRUE]
        custo <- t(custo)
        
        populacao[escolhidos,TRUE]

}



