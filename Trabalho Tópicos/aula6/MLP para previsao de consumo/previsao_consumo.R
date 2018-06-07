#AULA 6 - PROFESSOR CINIRO NAMETALA
#MLP APLICACOES - REGRESSOES

#limpar workspace
rm(list=ls())

#limpar tela
cat('\014')

#bibliotecas
library("RSNNS")

#---------------------------------------
#FUNCOES
#funcao para converter horario em valor
previsor <- function(horario,rede)
{
  hora <- paste("2007-02-01 ",horario,":00 BRST",sep = "")
  indice <- which(consumo$Time == hora)
  x <- padroniza(seq(1:length(consumo$Time)))
  momento <- x[indice]
  valor <- predict(rede,momento)
  return(paste("As ",horario," o consumo sera de ",round(valor,2)," KW.",sep=""))
}

#funcao de ativacao utilizada
sech2<-function(u) 
{
  ((2/(exp(u)+exp(-u)))*(2/(exp(u)+exp(-u))))
}

#funcao que ajusta outliers de qualquer amostra
ajustaOutliers <- function(x, na.rm = TRUE, ...) 
{
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  
  for(i in 1:length(y)) 
  {
    #caso o primeiro valor seja NA procura o proximo valor nao NA e coloca
    #no lugar do NA
    if (is.na(y[1]) == TRUE)
    {
      encontrou = FALSE
      cont = 1
      posterior = NA
      #procura o primeiro numero POSTERIOR ao valor atual que nao seja NA
      while (encontrou == FALSE)
      {
        if (is.na(y[1+cont]) == TRUE)
        {
          cont <- cont + 1
        }
        else
        {
          posterior <- y[1+cont];
          encontrou <- TRUE
        }
      }
      
      y[1] <- posterior
    }
    
    #caso o ultimo valor seja NA procura o primeiro valor anterior que nao NA e coloca
    #no lugar do NA
    if (is.na(y[length(y)]) == TRUE)
    {
      encontrou <- FALSE
      cont <- 1
      anterior <- NA
      
      #procura o primeiro numero ANTERIOR ao valor atual que nao seja NA
      while (encontrou == FALSE)
      {
        if (is.na(y[length(y)-cont]) == TRUE)
        {
          cont <- cont + 1
        }
        else
        {
          anterior <- y[length(y)-cont];
          encontrou <- TRUE
        }
      }
      
      y[length(y)] <- anterior
    }
    
    
    
    if (is.na(y[i])==TRUE)
    {
      encontrou <- FALSE
      cont <- 1
      anterior <- NA
      
      #procura o primeiro numero ANTERIOR ao valor atual que nao seja NA
      while (encontrou == FALSE)
      {
        if (is.na(y[i-cont]) == TRUE)
        {
          cont <- cont + 1
        }
        else
        {
          anterior <- y[i-cont];
          encontrou <- TRUE
        }
      }
      
      encontrou = FALSE
      cont = 1
      posterior = NA
      
      #procura o primeiro numero POSTERIOR ao valor atual que nao seja NA
      while (encontrou == FALSE)
      {
        if (is.na(y[i+cont]) == TRUE)
        {
          cont <- cont + 1
        }
        else
        {
          posterior <- y[i+cont];
          encontrou <- TRUE
        }
      }
      
      #executa uma media entre o anterior e posterior valor valido na serie e insere no lugar do outlier
      y[i] <- (anterior+posterior)/2
    }
  }
  
  return(y)
}

#Coloca amostra nos intervalos proporcionais entre 0 e 1
padroniza <- function(s)
{
  retorno <- (s - min(s))/(max(s)-min(s))
  return(retorno)
}

#concatena tipo date e tipo time
date_time <- function(date, time) 
{
  return (strptime(paste(date, time), "%d/%m/%Y %H:%M:%S"))
}

#carrega, trata e separa apenas uma parte do dataset
carrega_subset <- function(arquivo,inicio,fim) 
{
  dados <- read.table(arquivo,
                   header=TRUE,
                   sep=";",
                   colClasses=c("character", "character", rep("numeric",7)),
                   na="?")
  
  # converte para datetime
  dados$Time <- strptime(paste(dados$Date, dados$Time), "%d/%m/%Y %H:%M:%S")
  dados$Date <- as.Date(dados$Date, "%d/%m/%Y")
  
  # separa o subset que vai de <inicio> a <fim>
  dates <- as.Date(c(inicio, fim), "%Y-%m-%d")
  dados <- subset(dados, Date %in% dates)
  
  return(dados)
}
#---------------------------------------

#CARREGAMENTO
#carrega a base de dados
consumo <- carrega_subset("consumo.txt","2007-02-01","2007-02-01")

#ANALISE DE DADOS
#analise da serie de consumo global
plot(consumo$Time, consumo$Global_active_power,
     type="l",
     xlab="",
     ylab="Consumo global (kilowatts)")

#analise das subcomponentes da serie de consumo global
#cozinha
plot(consumo$Time, consumo$Sub_metering_1, type="l", col="black",
     xlab="", ylab="Consumo do subcomponente")
#lavanderia
lines(consumo$Time, consumo$Sub_metering_2, col="red")
#aquecedor e ar condicionado
lines(consumo$Time, consumo$Sub_metering_3, col="blue")
legend("topright",
       col=c("black", "red", "blue"),
       c("Cozinhas", "Lavanderias", "Climatizadores"),
       lty=1)

#analise da serie de consumo global com remocao de outliers
consumoajustado <- ajustaOutliers(consumo$Global_active_power)
plot(consumo$Time, consumoajustado,
     type="l",
     xlab="",
     ylab="Consumo global (kilowatts) - Sem Outliers")

#IMPLEMENTACAO DA REDE NEURAL NOS DADOS AJUSTADOS
x<-padroniza(seq(1:length(consumo$Time)))
y<-padroniza(consumoajustado)

#configuracoes da MLP
nNeuronios = 10
maxEpocas <- 20000

#treinamento da MLP
redeCA <- NULL

#Backpropagation
print("treinando a rede na serie ajustada...")

redeCA<-mlp(x, y, size=nNeuronios, maxit=maxEpocas, initFunc="Randomize_Weights",
          initFuncParams=c(-0.3, 0.3), learnFunc="Std_Backpropagation",
          learnFuncParams=c(0.1), updateFunc="Topological_Order",
          updateFuncParams=c(0), hiddenActFunc="Act_Logistic",
          shufflePatterns=F, linOut=TRUE)

plot(redeCA$IterativeFitError,type="l",main="Erro da MLP CA")

#COMANDOS PARA SALVAR E CARREGAR UM MODELO TREINADO-------------------
#
# save(nome_do_objeto,file="nome_a_ser_dado_para_o_arquivo.Rdata")
#
# load('nome_do_arquivo_com_o_modelo_salvo.Rdata')
#
#---------------------------------------------------------------------

#EXECUTANDO A PREVISOES COM O MODELO TREINADO
yhat = vector()
for (i in 1:length(x))
{
  print(i)
  yhat[i] = predict(redeCA,x[i])
}

#plot da aproximacao da funcao
plot(consumo$Time,consumoajustado,type="l",col="blue",xlab="",ylab="")
par(new=T)
plot(x,yhat,col="red",type="l",xlab="",ylab="",xaxt='n',yaxt='n')

#Aplicacao e analise de Lowess
consumoregredido <- lowess(consumoajustado,f=0.1)
ylow<-padroniza(consumoregredido$y)

par(new=T)
plot(x,ylow,col="green",type="l",xlab="",ylab="",xaxt='n',yaxt='n')

#IMPLEMENTACAO DA REDE NEURAL NO LOWESS
#treinamento da MLP
redeLOW <- NULL

#Backpropagation Levenberg-Marquardt
print("treinando a rede no lowess...")

redeLOW<-mlp(x, ylow, size=nNeuronios, maxit=maxEpocas, initFunc="Randomize_Weights",
          initFuncParams=c(-0.3, 0.3), learnFunc="Std_Backpropagation",
          learnFuncParams=c(0.1), updateFunc="Topological_Order",
          updateFuncParams=c(0), hiddenActFunc="Act_Logistic",
          shufflePatterns=F, linOut=TRUE)

#EXECUTANDO A PREVISOES COM O MODELO TREINADO COM LOWESS
yhatlow = vector()
for (i in 1:length(x))
{
  print(i)
  yhatlow[i] = predict(redeLOW,x[i])
}

par(new=T)
plot(x,yhatlow,col="black",type="l",xlab="",ylab="",xaxt='n',yaxt='n')

legend("topleft",
       col=c("blue","red","green","black"),
       c("Consumo (KW)","MLP CA","Lowess","MLP LOW"),
       lty=1, cex=0.6)

#CALCULO DO ERRO
erromlpca <- mean(sqrt((y-yhat)^2))
errolowess <- mean(sqrt((y-ylow)^2))
erromlplow <- mean(sqrt((y-yhatlow)^2))

print(paste("ERRO MLP CA:",erromlpca))
print(paste("ERRO LOWESS:",errolowess))
print(paste("ERRO MLP LOWESS:",erromlplow))

#EXECUTANDO UMA PREVISAO PARA QUALQUER HORARIO
print(previsor("20:00",redeCA))
