CrossingOver <- function(pop_sel)
{
database<-read.csv(file = "TESTE.csv",sep =";" )
offspring <- 1
Populacao <- pop_sel[[1]]
a<-pop_sel[[2]] # Funcao 1
b<-pop_sel[[3]] # Funcao 2
c<-pop_sel[[4]] # Custos

dist<-sqrt((max(a)-a)*(max(a)-a)+(max(b)-b)*(max(b)-b)) # Distancia

pu <- dist/max(dist) # Calculo do PU

limite <- ncol(pop_sel[[1]])
probabi <- pu/sum(pu)

while (offspring <= 30)
{
        #Escolha aleatoria dos pais
        pai <- sample(1:limite,1,replace = FALSE, prob = probabi)
        mae <- sample(1:limite,1,replace = FALSE, prob = probabi)
        
        #Crossing Over
        local <- sample(1:400,1,replace = FALSE, prob = NULL)
        
        cromossomo1 <- pop_sel[[1]][1:local,pai]
        cromossomo2 <- pop_sel[[1]][(local+1):400,mae]
        
        
        cromossomofilho <- c(cromossomo1,cromossomo2)
        
        f1_filho <- sum(database[,1]*cromossomofilho)
        f2_filho <- sum(database[,2]*cromossomofilho)
        custo_filho <- sum(database[,3]*cromossomofilho)

        
        if (custo_filho < 15000)
        {
                Populacao=cbind(Populacao,cromossomofilho)
                a <- cbind(a,f1_filho)
                b <- cbind(b,f2_filho)
                c <- cbind(c,custo_filho)
                offspring=offspring+1
        }

}
list(Populacao,a,b,c)
}

