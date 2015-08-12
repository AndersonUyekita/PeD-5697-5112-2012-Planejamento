source(file = "GeraInd_2.0.R",local = FALSE)
source(file = "GeraPop_2.0.R",local = FALSE)
source(file = "AvaliaPop.R",local = FALSE)
source(file = "Selecao_3.0.R",local = FALSE)
source(file = "CrossingOver_3.0.R",local = FALSE)
source(file = "Dominancia.R",local = FALSE)
source(file = "Geracao.R",local = FALSE)
source(file = "Mutacao.R",local = FALSE)


database<-read.csv(file = "TESTE.csv",sep =";")

populacao <- GeraPop(database,10000,400,200)

lista <- AvaliaPop(database,populacao,200)

fronteira <- Geracao(10000,lista,300,200)
