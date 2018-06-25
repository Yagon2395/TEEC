#YAGO SOARES CAMARGO NUNES
#GUSTAVO HENRIQUE NUNES
#IFMG - CAMPUS BAMBUI
#TOPICOS ESPECIAIS EM ENGENHARIA DE COMPUTACAO: COMPUTACAO NATURAL
#CLASSIFICAÇÃO DE QUESTÕES DO NEM EM GRUPOS DE DIFICULDADE
#-------------------------------------------------------

#limpar workspace
rm(list=ls())

#limpar tela
cat('\014')

#bibliotecas
library("RSNNS")
library("SDMTools")
library(plotly)

#funções
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

dados <- read.csv(file = "classificado_SOMENTE_MT.csv",sep = ",")
N <- dim(dados)[1]
#embaralhando os dados
indElementosEmbaralhados <- sample(N)
x <- data.frame(dados[,1],dados[,2],dados[,3],dados[,4],dados[5])
for(i in 1:N){
  x[i,] <- dados[indElementosEmbaralhados[i],]
}

#separando amostras de testes
treinamentoX <- x[1:81,2:4]#60% da amostra
treinamentoY <- x[1:81,5]#60% da amostra

primeiraClassificacaoX <- x[82:108,2:4]
primeiraClassificacaoY <- x[82:108,5]

segundaClassificacaoX <- x[109:135,2:4]
segundaClassificacaoY <- x[109:135,5]

#configuracoes da MLP
nNeuronios = 6
maxEpocas <- 20000

#treinamento da MLP com 60% da amostra
redeCA <- NULL

#Backpropagation
print("treinando a rede na serie ajustada...")
#redeCA<-mlp(treinamentoX, treinamentoY, size=nNeuronios, maxit=maxEpocas, initFunc="Randomize_Weights",
 #          initFuncParams=c(-0.25, 0.25), learnFunc="Std_Backpropagation",
  #       learnFuncParams=c(0.1), updateFunc="Topological_Order",
   #     updateFuncParams=c(0), hiddenActFunc="Act_Logistic",
    #   shufflePatterns=F, linOut=TRUE)
#save(redeCA,file="melhorResultado.Rdata")
load("melhorResultado.Rdata")
#PLOT DO ERRO
plot(redeCA$IterativeFitError,type="l",main="Erro da rede",xlab="Número de épocas",ylab = "Erro iterativo")

#primeira classificação com 20% da amostra
yhat <- predict(redeCA,primeiraClassificacaoX)
yhatPadronizado <- padroniza_em_grupos(yhat)

primeira_classfY <- conta_grupos(primeiraClassificacaoY)
barplot(primeira_classfY, main="Classificação de grupos para Y", 
        xlab="Grupos", names.arg=c("Grupo 1", "Grupo 2", "Grupo 3"),
        ylab = "Quantidade", col = c("red","green","blue"))
primeira_classfYHAT <- conta_grupos(yhatPadronizado)
barplot(primeira_classfYHAT, main="Classificação de grupos para Yhat", 
        xlab="Grupos", names.arg=c("Grupo 1", "Grupo 2", "Grupo 3"),
        ylab = "Quantidade", col = c("red","green","blue"))

#segunda classificação com os outros 20% da amostra
yhat2 <- predict(redeCA,segundaClassificacaoX)
yhat2Padronizado <- padroniza_em_grupos(yhat2)

segunda_classfY <- conta_grupos(segundaClassificacaoY)

barplot(segunda_classfY, main="Classificação de grupos para Y", 
        xlab="Grupos", names.arg=c("Grupo 1", "Grupo 2", "Grupo 3"),
        ylab = "Quantidade", col = c("red","green","blue"))
segunda_classfYHAT <- conta_grupos(yhat2Padronizado)
barplot(segunda_classfYHAT, main="Classificação de grupos para Yhat", 
        xlab="Grupos", names.arg=c("Grupo 1", "Grupo 2", "Grupo 3"),
        ylab = "Quantidade", col = c("red","green","blue"))

#matrizes de confusao
matrizPrimeiraClassificacao <- confusionMatrix(primeiraClassificacaoY, yhatPadronizado)
matrizPrimeiraClassificacao

matrizSegundaClassificacao <- confusionMatrix(segundaClassificacaoY, yhat2Padronizado)
matrizSegundaClassificacao
#m <- matrix(rnorm(9), nrow = 3, ncol = 3)
f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#000000"
)
eix <- list(
  title = "Valor observado",
  titlefont = f
)
eiy <- list(
  title = "Valor esperado",
  titlefont = f
)
p <- plot_ly(
  x = c("1", "2", "3"), y = c("1", "2", "3"),
  z = matrizSegundaClassificacao, type = "heatmap",
  colors = colorRamp(c("#607D8B", "#FFEB3B","#00E676"))
) %>%
  layout(xaxis = eix, yaxis = eiy)

p

