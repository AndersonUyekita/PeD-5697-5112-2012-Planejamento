GeraInd <- function(cromossomo)
{       
        teste <- vector()
        for (i in 1:cromossomo) {
                teste[i] <-  sample(c(0,1), size = 1, replace = TRUE)
        }
        teste
}