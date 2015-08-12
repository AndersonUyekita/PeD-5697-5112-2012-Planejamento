source(file = "GeraInd_2.0.R",local = FALSE)
source(file = "GeraPop_2.0.R",local = FALSE)
source(file = "AvaliaPop.R",local = FALSE)
source(file = "Selecao_3.0.R",local = FALSE)
source(file = "CrossingOver_3.0.R",local = FALSE)
source(file = "Dominancia.R",local = FALSE)
source(file = "Geracao.R",local = FALSE)
source(file = "Mutacao.R",local = FALSE)
source(file = "Corpo_2.0.R",local = FALSE)


database<-read.csv(file = "TESTE.csv",sep =";")

resultado <- AG(restricao = 10000,database = database,populacao = 400,individudos = 400,iteracao = 500)

