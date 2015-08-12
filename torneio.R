torneio <- function(lista,dd,raio)
{
        pop_original <- lista[[7]]
        ID_pop <- 1:nrow(pop_original)
        escolhidos <- dd < summary(dd)[[3]]
        rownames(pop_original) <- ID_pop
        dentro_raio <- logical()
        dominancia <- dd
        names(dominancia) <- 1:length(as.numeric(names(dominancia)))
        #Todas as fronteiras inclusive as que possuem 1 incidencia
        fronteiras <- as.numeric(names(table(dd)))
        
        #fronteira so com mais de uma incidencia
        fronteiras <- fronteiras[table(dd) > 1]
        
        
        
        
        for (i in 1:(round(length(fronteiras)*0.5)))
                {
                dominancia_0 <- dd == fronteiras[i]
                
                ind_0 <- sum(dominancia_0 <- dd == fronteiras[i])
                
                pop_dom_0 <- pop_original[dominancia_0,TRUE]
                
                lista_dom_0 <- AvaliaPop(database,pop_dom_0,ind_0)
                
                #plot(lista_dom_0[[2]],lista_dom_0[[4]],xlim = c(80,115),ylim=c(75,150),main = paste("Fronteira:",fronteiras[i]))
                
                lista_ind_domo <- as.numeric(rownames(pop_dom_0))
                
                        for (k in 2:length(lista_dom_0[[2]]))
                                {
                                x <- lista_dom_0[[2]][1]
                                y <- lista_dom_0[[4]][1]
                                x0 <- lista_dom_0[[2]][k]
                                y0 <- lista_dom_0[[4]][k]
                                dentro_raio <- (x-x0)^2+(y-y0)^2 < raio^2 #Se a igualdade for verdadeira teremos um ponto dentro do raio do dominante
                                        if (dentro_raio == TRUE)
                                                {
                                                escolhidos[lista_ind_domo[k]] <- !escolhidos[lista_ind_domo[k]]
                                                }
                                }
                }

        
        
  
        escolhidos        
}