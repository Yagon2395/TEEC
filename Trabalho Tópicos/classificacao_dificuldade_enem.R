library("RSNNS")
#usada para gerar matrix de confusao
library(SDMTools)
dados <- read.csv(file = "classificado3.csv",sep = ",")
N <- dim(dados)[1]
indElementosEmbaralhados <- sample(N)
x <- data.frame(dados[,1],dados[,2],dados[,3],dados[,4],dados[5])
for(i in 1:N){
  x[i,] <- dados[indElementosEmbaralhados[i],]
}
treinamentoX <- x[1:81,2:4]#60% da amostra
treinamentoY <- x[1:81,5]#60% da amostra

primeiraClassificacaoX <- x[82:135,2:4]
primeiraClassificacaoY <- x[82:135,5]
#configuracoes da MLP
nNeuronios = 10
maxEpocas <- 20000

#treinamento da MLP com 60% da amostra
redeCA <- NULL

#Backpropagation
print("treinando a rede na serie ajustada...")

redeCA<-mlp(treinamentoX, treinamentoY, size=nNeuronios, maxit=maxEpocas, initFunc="Randomize_Weights",
            initFuncParams=c(-0.3, 0.3), learnFunc="Std_Backpropagation",
            learnFuncParams=c(0.1), updateFunc="Topological_Order",
            updateFuncParams=c(0), hiddenActFunc="Act_Logistic",
            shufflePatterns=F, linOut=TRUE)
#PLOT DO ERRO
plot(redeCA$IterativeFitError,type="l",main="Erro da MLP CA")

#primeira classificação com 20% da amostra
yhat <- predict(redeCA,primeiraClassificacaoX)

padroniza_em_grupos <- function(vet){
  for(i in 1:length(vet)){
    if(vet[i] <= 1 || (vet[i] >= 1 && vet[i] < 1.5)){
      vet[i] <- 1
    }else if(((vet[i] >= 1.5) && (vet[i] <= 2)) || ((vet[i] >= 2) && (vet[i] <= 2.5))){
      vet[i] <- 2
    }else{
      vet[i] <- 3
    }
  }
  return(vet)
}

yhatPadronizado <- padroniza_em_grupos(yhat)

conta_grupos <- function(vet){
  
  qtd1 <- 0
  qtd2 <- 0
  qtd3 <- 0
  
  for(i in 1:length(vet)){
    if(vet[i] == 1){
      qtd1 <- qtd1 + 1
    }else if(vet[i] == 2){
      qtd2 <- qtd2 + 1
    }else{
      qtd3 <- qtd3 + 1
    }
  }
  
  return(c(qtd1,qtd2,qtd3))
}

teste1 <- conta_grupos(primeiraClassificacaoY)

barplot(teste1, main="Classificação de grupos para Y", 
        xlab="Grupos", names.arg=c("Grupo 1", "Grupo 2", "Grupo 3"),
        ylab = "Quantidade", col = c("red","green","blue"))
teste2 <- conta_grupos(yhatPadronizado)
barplot(teste2, main="Classificação de grupos para Yhat", 
        xlab="Grupos", names.arg=c("Grupo 1", "Grupo 2", "Grupo 3"),
        ylab = "Quantidade", col = c("red","green","blue"))
