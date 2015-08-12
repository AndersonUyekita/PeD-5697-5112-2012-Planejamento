T_f1 <- numeric()
T_f2 <- numeric()
pop1 <- numeric()
cus1 <- numeric()
pop120 <- numeric()

resultado<-Corpo(30,20)
T_f1 <- resultado[[1]][[2]]
T_f2 <- resultado[[1]][[3]]
pop1 <- resultado[[1]][[1]]
cus1 <- resultado[[1]][[4]]

resultado<-Corpo(30,20)
T_f11 <- resultado[[1]][[2]]
T_f22 <- resultado[[1]][[3]]
pop11 <- resultado[[1]][[1]]
cus11 <- resultado[[1]][[4]]

resultado<-Corpo(30,20)
T_f111 <- resultado[[1]][[2]]
T_f222 <- resultado[[1]][[3]]
pop111 <- resultado[[1]][[1]]
cus111 <- resultado[[1]][[4]]

resultado<-Corpo(30,20)
T_f1111 <- resultado[[1]][[2]]
T_f2222 <- resultado[[1]][[3]]
pop1111 <- resultado[[1]][[1]]
cus1111 <- resultado[[1]][[4]]

T_F1 <- c(T_f1,T_f11,T_f111,T_f1111)
T_F2 <- c(T_f2,T_f22,T_f222,T_f2222)
Populacao <- cbind(pop1,pop11,pop111,pop1111)
Custos <- cbind(cus1,cus11,cus111,cus1111)

#pop120 <- list(Populacao,T_F1,T_F2,Custos)

#pop120 <- CrossingOver(pop120)

#T_F1 <- pop120[2]
#T_F2 <- pop120[3]
#Populacao <- pop120[1]

Populacao_new <- numeric()
T_f1_new <- numeric()
T_f2_new <- numeric()
T_custo_new <- numeric()

dist <- sqrt((max(T_F1) - T_F1)^2+(max(T_F2) - T_F2)^2)

teste <- order(dist) <= 100
j <- 1
for (i in 1:length(T_F1))
{
        
        if (teste[i] == TRUE)
        {
                Populacao_new <- cbind(Populacao_new,Populacao[,i])
                T_f1_new <- cbind(T_f1_new,T_F1[i])
                T_f2_new <- cbind(T_f2_new,T_F2[i])
        }
}
#list(Populacao_new,T_f1_new,T_f2_new)
plot(T_f1_new,T_f2_new)
