#Carregar dados aleatorios
database<-read.csv(file = "TESTE.csv",sep =";")

#Calculo do IDP com pesos iguais para F1 e F2
IDP_05_peso <- as.numeric((database[[2]]+database[[1]])*0.5)

#Criacao da Matrix IDP_0.5 e Custos
Matrix_IDP_Custo <- cbind(IDP_05_peso[order(IDP_05_peso,decreasing = TRUE)],database[,3][order(IDP_05_peso,decreasing = TRUE)],database[,1][order(IDP_05_peso,decreasing = TRUE)],database[,2][order(IDP_05_peso,decreasing = TRUE)])

#Atribuicao de nomes nas colunas da Matrix
colnames(Matrix_IDP_Custo) <- c("IDP","Custo","F1","F2")

#Selecao das obras ateh atingir a restricao
soma <- 0
Matrix_IDP_Custo_selecao <- c(FALSE)
for (i in 1:400)
        {
        if (soma < 10000.0)
                {
                soma <- soma + as.numeric(Matrix_IDP_Custo[i,2])
                Matrix_IDP_Custo_selecao[i] <- TRUE
                }
        else
                Matrix_IDP_Custo_selecao[i] <- FALSE
        
        
}

# Quantidade de obras selecionadas
quantidade_obras_selecionadas <- sum(Matrix_IDP_Custo_selecao)
F1_metodologia_atual <- sum(Matrix_IDP_Custo[,3][Matrix_IDP_Custo_selecao])
F2_metodologia_atual <- sum(Matrix_IDP_Custo[,4][Matrix_IDP_Custo_selecao])

# Plotagem de F1 e F2 no grafico.
plot(F1_metodologia_atual,F2_metodologia_atual, xlab = "F1", ylab = "F2", main = "Metodologia Atual")

# devo fazer isso para comparar as abordagens
#plot(c(150,F1),c(145,F2))