Dominancia <- function(lista,tamanho)
{
        f1_vector <- lista[[2]]
        f2_vector <- lista[[4]]
        #plot(f1_vector,f2_vector)
        F22 <- logical()
        d <- vector()
        

        for (i in 1:tamanho)
        {

                F2 <- f2_vector[i] < f2_vector
                F1 <- f1_vector[i] < f1_vector
                F1ouF2 <- as.logical(F1) & as.logical(F2)
                d[i] <- sum(F1ouF2)
        }
        d
}