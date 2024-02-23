#preparacao do ambiente
rm(list = ls())
cat("\014")  # clear console
dir_path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir_path)
#------------------------------------------------------------------------------------
#Bibliotecas de interesse
library(mclust)
library(car)
library(lmtest)

#------------------------------------------------------------------------------------
#FUNCOES
#funcao que ajusta outliers de qualquer amostra e substitui o outlier
#limite inferior = q1 - 1.5(q3 - q1)
#q1 = (n+1)/4
#q2 = (n+1)/2 (mediana - valor do meio)
#q3 = 3(n+1)/4
#limite superior = q3 + 1.5(q3 - q1)
#------------------------------------------------------------------------------------
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
#------------------------------------------------------------------------------------


#função normaliza de dados
normalizeData <- function(s){
  s<- ((s- mean(s))/sd(s))
}
#------------------------------------------------------------------------------------
#Coloca amostra nos intervalos proporcionais entre 0 e 1
padroniza <- function(s)
{
  retorno <- (s - min(s))/(max(s)-min(s))
  return(retorno)
}
#------------------------------------------------------------------------------------

#funcao degrau
degrau <- function(u)
{
  if (u>=0)
    1
  else
    0
}
#------------------------------------------------------------------------------------
#Neurônio
#Implementacao basica do neuronio
neuronio <- function(dados,theta,w1,w2,w3)
{
  #pega a quantidade de itens na amostra
  tam <- length(dados[,1])
  #aloca um vetor para essa quantidade de itens
  yhat <- rep(0,tam)
  
  #para cada amostra
  for (i in 1:tam)
  {
    #calcula o potencial de ativacao
    u <- (dados[i,1]*w1)+(dados[i,2]*w2)+(theta*w3)
    #calcula a saida
    yhat[i] <- degrau(u)
  }
  
  #retorna os grupos associados aos elementos da amostra
  return(yhat)
}

#------------------------------------------------------------------------------------
#Exercicio 1
serie <- read.table("bbas3.csv",header=T,sep=";")
fechamento <- serie$fec
plot(fechamento, type='l', main ="Fechamento")

#1 a outliers aqui
boxplot(fechamento, main ="Boxplot sem tratamento")
hist(fechamento,20, main ="Histograma sem tratamento")
qqPlot(fechamento,
       pch=16,
       cex=1.5,
       las=1, main ="Sem tratamento")

# #1 B
fechamento<- ajustaOutliers(fechamento)
boxplot(fechamento, main =" ajusta outliers aplicado")
hist(fechamento,20, main =" ajusta outliers aplicado")
qqPlot(fechamento,
       pch=16,
       cex=1.5,
       las=1, main =" ajusta outliers aplicado")
# #1 c
fechamento<- normalizeData(fechamento)
boxplot(fechamento, main =" Normaliza aplicado")
hist(fechamento,20, main =" Normaliza aplicado")
qqPlot(fechamento,
       pch=16,
       cex=1.5,
       las=1, main =" Normaliza aplicado")
# #1 D
# #Padronizando
fechamento <- padroniza(fechamento)
boxplot(fechamento, main =" Padroniza aplicado")
hist(fechamento,20, main =" Padroniza aplicado")
qqPlot(fechamento,
       pch=16,
       cex=1.5,
       las=1, main =" Padroniza aplicado")

#------------------------------------------------------------------------------------
#Exercicio 2
dados <- read.table("amostra.csv",header=T,sep=";")
x1 <- as.numeric(round(dados[,1],2))
x2 <- as.numeric(round(dados[,2],2))
dados <- cbind(x1,x2)
plot(dados,main ="Antes de ajustar outliers")
boxplot(c(dados [,1],dados [,2]))
hist(dados,20)
qqPlot(dados,
       pch=16,
       cex=1.5,
       las=1)

#2 A
 dados <- ajustaOutliers(dados)
 boxplot(c(dados [,1],dados [,2]))  #concatenando dados
   # hist(dados,20)
   # qqPlot(dados,
   #        pch=16,
   #        cex=1.5,
   #        las=1)
 #normaliza
 dados <- normalizeData(dados)
   hist(dados,20)
   qqPlot(dados,
          pch=16,
          cex=1.5,
          las=1)

 #padroniza
 dados <- padroniza(dados)
   hist(dados,20)
   qqPlot(dados,
          pch=16,
          cex=1.5,
          las=1)

 plot(dados,main ="Depois de ajustar outliers") #como era antes de aplicar o kmeans

 #------------------------------------------------------------------------------------
 #2 B

 quantKs <- 2
 fit = kmeans(dados, quantKs)                      #aplica o kmeans na amostra gerando o fit

 if (fit$cluster[1] != 1) {
   yhatKmeans <- (fit$cluster-2)*-1             #este if está ajustando a classificação do kmeans
 } else {
   yhatKmeans <- fit$cluster-1
 }
 #plotando novamente a separacao com Kmeans para analise
 plot(dados, col =yhatKmeans+2, main="Separacao com Kmeans")

 #2 C
 resultadoNeuronio <- neuronio(dados,1,-0.12,1.5,-0.9)
 plot(dados,col =resultadoNeuronio+2, main= "Neuronio" )

