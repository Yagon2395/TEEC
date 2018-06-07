rm(list=ls())

#FUNCOES---
sech2<-function(u) 
{
  ((2/(exp(u)+exp(-u)))*(2/(exp(u)+exp(-u))))
}
#----------
#POLINOMIAL
poli <- function(xx,p)
{
  y <- xx^p
  return(y)
}

#QUARTO GRAU
poli4 <- function(xx)
{
  y <- xx^4 + xx^3 + xx^2 + xx
  return(y)
}

#TANGENTE HIPERBOLICA
tangentehiper <- function(xx)
{
  y <- tanh(xx)
  return(y)
}

#RASTRIGIN
rastr <- function(xx)
{
  d <- length(xx)
  sum <- sum(xx^2 - 10*cos(2*pi*xx))
  y <- 10*d + sum
  return(y)
}

#------------------------------------

x<-seq(0,2*pi,0.3)
#y<-sin(x)
#y <- tangentehiper(x)
#y <- poli(x,2)
#y <- poli4(x)

#para funcao de rastringin
y<-rastr(x)
y = vector()
for (i in 1:21)
{
  y[i] =  rastr(x[i])
}


#geracao da entrada limiar
i1 = 1

#sorteio dos pesos
#neuronio 3 - tanh
w31<-runif(1)-0.5
w32<-runif(1)-0.5
#neuronio 4 - tanh
w41<-runif(1)-0.5
w42<-runif(1)-0.5
#neuronio 5 - tanh
w51<-runif(1)-0.5
w52<-runif(1)-0.5
#neuronio 7 - linear
w71<-runif(1)-0.5
w73<-runif(1)-0.5
w74<-runif(1)-0.5
w75<-runif(1)-0.5

#seta os valores importantes para o treinamento do MLC
nepocas <- 0
tol <- 0 #regra delta
eepoca <- tol+1
n <- length(x)
maxepocas=1500
eta <- 0.001

#TREINAMENTO MLP--------------------------------------------
#vetor que armazenara os erros em cada uma das epocas
evec<-matrix(nrow=1,ncol=maxepocas)
#aleatoriza as observacoes a serem coletadas na amostra
xseq <- sample(n)

while ((nepocas < maxepocas) && (eepoca>tol))
{
  print(nepocas)
  erro<-0
  
  for (i in 1:n)
  {
    #gera as duas entradas e duas respectivas saidas esperadas
    i2<-x[xseq[i]]
    y7<-y[xseq[i]]
    
    #----------------------------------inicio execucao do MLP
    #-------->FORWARD----------
    #camada escondida
    i3 <- tanh(i1*w31+i2*w32)
    i4 <- tanh(i1*w41+i2*w42)
    i5 <- tanh(i1*w51+i2*w52)
    #camada de saida
    i7 <- i1*w71+i3*w73+i4*w74+i5*w75
    
    #<-------BACKPROPAGATION---
    #calculo dos deltas da camada de saida
    d7 <- y7-i7
    
    #calculo dos deltas da camada escondida
    d3 <- sech2(i1*w31+i2*w32)*(d7*w73)
    d4 <- sech2(i1*w41+i2*w42)*(d7*w74)
    d5 <- sech2(i1*w51+i2*w52)*(d7*w75)
    
    #atualizacao dos peso
    #camada escondida
    w31 <- w31 + (eta*d3*i1)
    w32 <- w32 + (eta*d3*i2)
    
    w41 <- w41 + (eta*d4*i1)
    w42 <- w42 + (eta*d4*i2)
    
    w51 <- w51 + (eta*d5*i1)
    w52 <- w52 + (eta*d5*i2)
    
    #camada de saida
    w71 <- w71 + (eta*d7*i1)
    w73 <- w73 + (eta*d7*i3)
    w74 <- w74 + (eta*d7*i4)
    w75 <- w75 + (eta*d7*i5)
    
    #---------------------------------------fim execucao do MLP
    
    #calculo do erro quadratico (acumulacao do erro de todas as amostras usadas no treinamento)
    erro<-erro+((y7-i7)^2)
  }
  #contador de epocas
  nepocas<-nepocas+1
  #armazena o erro quadratico M?DIO para todas as epocas
  evec[nepocas]<-erro/n
  #valor de referencia para o erro tolerado
  eepoca<-erro/n
}

#plot do erro medio quadratico por epoca
plot(1:length(evec),evec,type="l",xlab="epocas",ylab="EQM")

#OPERACAO MLP--------------------------------------------

yhat = vector()

for (i in 1:length(x))
{
  #entrada
  i2 <- x[i]
  #camada escondida
  i3 <- tanh(i1*w31+i2*w32)
  i4 <- tanh(i1*w41+i2*w42)
  i5 <- tanh(i1*w51+i2*w52)
  #camada de saida
  i7 <- i1*w71+i3*w73+i4*w74+i5*w75
  yhat[i] = i7
}

#plot da aproximacao da funcao
plot(x,y,type="l",col="blue",xlab="",ylab="")
par(new=T)
plot(x,yhat,col="red",type="l",xlab="Entradas X",ylab="Y real X Y estimado")
