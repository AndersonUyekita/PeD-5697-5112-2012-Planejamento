database<-read.csv(file = "TESTE.csv",sep =";" )
relevancia <-sample(10,size = 400,replace = TRUE,prob = NULL)*0.05+0.5

#Base atual com Relevancia
database2 <- cbind(database,relevancia)

#Separacao das variaveis
severidade <- database2[,1]
atratividade <- database2[,2]
relevancia <- database2[,4]
custo <- database2[,3]

#Calculo dos pesos
a <- (1.31)/(1.31+1.16)
b <- (1.16)/(1.31+1.16)

#Calculo do IDP sem relevancia
IDP <- vector()
IDP <- a*severidade + b*atratividade

#Base com IDP e Dinheiro
IDP_dindin <- data.frame(IDP,custo)

#Ordenacao por IDP
ordem <- order(IDP_dindin[,1], decreasing = TRUE)

IDP_dindin_matrix <- as.matrix(IDP_dindin)
IDP_dindin_matrix
data.frame(IDP_dindin_matrix[,1][ordem],IDP_dindin_matrix[,2][ordem])
IDP_dindin_ordenado <- as.matrix(data.frame(IDP_dindin_matrix[,1][ordem],IDP_dindin_matrix[,2][ordem]))

custo_obras_todas <-sum(IDP_dindin_ordenado[,2])
IDP_todas_obras <- sum(IDP_dindin_ordenado[,1])
custo_obras_todas
IDP_todas_obras

soma <- 0
i<-1
while (soma < 15000)
        {
        soma = soma + as.numeric(IDP_dindin_ordenado[i,2])
        i=i+1
        }
print(i)

IDP_modelo_atual <- sum(IDP_dindin_ordenado[1:289,1])+sum(IDP_dindin_ordenado[291,1])

IDP_modelo_atual

