Mutacao <- function(populacao_selecionada)
{
        populacao_selecionada2 <- populacao_selecionada
        
        #Sorteio aleatorio de quantos individuos serao mutados
        num_ind_mutantes <- sample(10:30,1)
        #Sorteio de quantidade de genes mutados de cada individuo 
        ind_mutantes <- sample(1:nrow(populacao_selecionada),num_ind_mutantes)
        selecionar_os_mutantes <- vector(mode = "logical",length = nrow(populacao_selecionada))
        
        
        for (i in 1:num_ind_mutantes)
        {
                1:nrow(populacao_selecionada) == ind_mutantes[i]
                selecionar_os_mutantes <- selecionar_os_mutantes | (1:nrow(populacao_selecionada) == ind_mutantes[i])
        }
        populacao_selecionada2 <- populacao_selecionada2[selecionar_os_mutantes,TRUE]
        
        
        for (i in 1:length(ind_mutantes))
        {
                #Sorteio do Gene mutante
                
                gene_mutante <- sample(2:147,1)
                populacao_selecionada2[i,gene_mutante] <- !populacao_selecionada2[i,gene_mutante]
        }
        
        rbind(populacao_selecionada,populacao_selecionada2)
}