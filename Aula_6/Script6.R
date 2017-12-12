####### Hugo Andrade 07/12/2017 ######### página 1################

media <-function(x)
{
  soma=sum(x)
  nobs=length(x)  #cria uma funçao para o objeto chamado media, onde devemos saber o valor de soma, comprimento dos nobs e a media, usando a funçao "return da media" em seguida
  media=soma/nobs
  return(media)
}
??rnorm
rnorm(20,2,1)

ls()  #lista o nome dos objetos
media  #é o objeto que contem o valor da funçao "media"
media() #dar a media de um dado objeto, caso nao defina isso entre os parentes ele dara a informaçao a seguuir "Error in media() : argument "x" is missing, with no default"
dados=rnorm(20,2,1) #pega os dados da distribuiçao normal e joga para o objeto "dados", sendo 20 o valor de n, 2 a media e 1 o desvio padrao
media(dados) #o valor da media do objeto "dados" = 2.066385
dados1=rnorm(200,2,1) #pega os dados da distribuiçao normal e joga para o objeto "dados1", sendo 200 o valor de n, 2 a media e 1 o desvio padrao
media(dados1) #mostra o valor da media  "dados1" = 1.855705
dados2=(rnorm(10000,2,1)) #pega os dados da distribuiçao normal e joga para o objeto "dados2", sendo 1000 o valor de n, 2 a media e 1 o desvio padrao
media(dados2) ##mostra o valor da media  "dados2" = 1.999456
sd(dados) #retorna o desvio padrao do objeto "dados"
dados3=rnorm(20,2,0.01) #pega os dados da distribuiçao normal e joga para o objeto "dados3", sendo 20 o valor de n, 2 a media e 0.1 o desvio padrao
media(dados3)#mostra o valor da media  "dados3" = 2.001801
dados4=rnorm(200,2,0.01) #pega os dados da distribuiçao normal e joga para o objeto "dados4", sendo 200 o valor de n, 2 a media e 0.01 o desvio padrao
media(dados4)#mostra o valor da media  "dados4" = 2.000037
dados[2]<-NA #torna o segundo valor do objeto dados igual a NA
dados #mostra os valores do objeto dados com o NA na segunda linha
media(dados) #ele nao mostra o valor da media, devido a presenca de NA nos dados



media<-function(x,rmNA=TRUE)  #calcula a funcao de X com remoçao de NA
{
  if(rmNA==TRUE) #se a remocao de Na ocorrer
  {
    dados=(na.omit(x)) #dos valores de x vai omitir os NA e mandar para o objeto dados
    n.NA=length(x)-length(dados) #diminuir o n do objeto "dados" pelo n original de x que é igual ao numero de NAs existentes
    cat("\t", n.NA," valores NA excluídos\n") #nos fornerá o número de NA "n.NA"
  }
  else #se nao
  {
    dados=x #os valores de x sao mandados para o objeto dados
  }
  soma=sum(dados) #somar os valores de dados e mandar para o objeto "soma"
  nobs=length(dados) #dar o n de dados e mandar para o objeto "nobs"
  media=soma/nobs # pega a soma dos dados e divide pela  n de dados, enviando-o para o objeto media
  return(media) #retorna o valor da funçao para o objeto media
}


media(dados) #calculou a media do objeto "dados" apos a exclusao do NAs feito na funcao acima


var.curso<-function(x) #calcula a funcao para o objeto "var.curso"
{
  media=media(x) #media do dado X
  dados=na.omit(x)	#omitir os NA x contidos no objeto "dados"
  disvquad=(dados-media)^2 #diminuira o valor de media (com NA) pelo valor de dados (sem NA) à segunda potencia, os valores obtidos irao para o objeto "disvquad"
  var.curso=sum(disvquad)/(length(dados)-1) #soma do valor de disvquad dividido pelo n encontrado em dados (sem NAs) menos um. Enviando-o em seguida para o objeto "var.curso"
  return(var.curso) #retorna o valor da funçao para o objeto media
}

help("var")

var.curso(dados) #variancia dos dados X eh fornceida, eh possbilitida por ter feito a funcao de "var.curso"
var(dados)### dica: veja o help dessa função "help(var)"
var(dados,na.rm=TRUE) #dar a variancia com os NA sendo removidos 
var(dados,na.rm=FALSE) #mostra apenas o NA, porque nao tem como dar a variancia sem ter sido removida os NA



ID.curso<-function(x) #ID.curso é o objeto da funçao(x)
{
  id=var.curso(x)/media(x) #o valor de "var.curso(x) dividido pela media de X
  return(id) #a funçao executada retorna os dados para o objeto id
}


aleat=rpois(200,2) #a funcao variada de multi-poisson, dando o n=200 e lambda=2
aleat #mostra os valores gerado no console



unif=runif(200,0,4) #a distribuiçao norma entre o minimo e maximo, sendo o n=200, valor min=0 e valor max= 4.
unif #fornece os valore no console
unif=round(unif,0) #unif eh igual aos 200 valores da funçao acima citada e 0 eh o valor padrao do default 
unif


agreg=round(c(runif(100,0,1),runif(100,5,10))) #agrupa 100 valores com o primeiro objeto minimo 0 e maximo 1 e o segundo objeto de runif com minimo 5 e maximo 10
agreg #mostra no console os resultados obtidos




ID.curso(aleat) #mostra a aleatoriedade do vetor

ID.curso(unif)#mostra a uniformiedade do vetor

ID.curso(agreg) #mostra a agregacao do vetor




test.ID <- function(x, nsim=1000)
{ 
  ID.curso=function(x){var(x)/mean(x)}# essa função precisa das funcoes media e ID.curso
  dados=na.omit(x)
  ndados=length(dados)
  med=mean(dados)
  id=var(dados)/med
  simula.aleat=rpois(length(dados)*nsim, lambda=med)
  sim.dados=matrix(simula.aleat,ncol= ndados)
  sim.ID=apply(sim.dados,1,ID.curso)
  quant.ID=quantile(sim.ID, probs=c(0.025,0.975))
  if(id>=quant.ID[1] & id<=quant.ID[2])
  { 
    cat("\n\n\n\t distribuição aleatória para alfa=0.05 \n\t ID= ",id,"\n\n\n")
  }
  if(id < quant.ID[1]) 
  { 
    cat("\n\n\n\t distribuição uniforme, p<0.025 \n\t ID= ",id,"\n\n\n")
  }
  if(id>quant.ID[2])
  { 
    cat("\n\n\n\t distribuição agregado, p>0.975 \n\t ID= ",id,"\n\n\n")
  }
  resulta=c(id,quant.ID)
  names(resulta)<-c("Indice de Dispersão", "critico 0.025", "critico 0.975")
  return(resulta)
}



test.ID(aleat)
test.ID(agreg)
test.ID(unif)


eda.shape <- function(x)
{
  x11() 
  par(mfrow = c(2,2))	## muda o dispositivo gráfico para 2x2
  hist(x)                 ## produz histograma de x
  boxplot(x)
  iqd <- summary(x)[5] -	summary(x)[2]     ## faz a diferença entre o quinto elemento x e o segundo
  plot(density(x,width=2*iqd),xlab="x",ylab="",type="l")
  qqnorm(x)
  qqline(x)
  par(mfrow=c(1,1))
  
}



set.seed(22) ## estabelece uma semente aleatória 
dados.pois20<-rpois(20,lambda=6) ## sorteia dados aleatórios (de 20 individuos) de uma função poisson com média 6
sum(dados.pois20) ## a somatória aqui sempre dará 131, somente porque a semente é a mesma
set.seed(22)
dados.norm20<-rnorm(20,mean=6, sd=2) ## sorteia 20 dados de uma função normal com média 6 e dp = 1
sum (dados.norm20)               ### aqui o resultado dará sempre 130.48

###aplicar eda.shape para dados.dens

eda.shape(dados.pois20)

eda.shape(dados.norm20)

###aumentando a amostra

eda.shape(rpois(500,6))

eda.shape(rnorm(500,6))



eda.shape1 <- function(x)
{
  x11()
  par(mfrow = c(2,2))
  hist(x,main="Histograma de x") #dara um titulo "Histograma de x"
  boxplot(x, main="BoxPlot de x") #dara um titulo "BoxPlot de x"
  iqd <- summary(x)[5] -	summary(x)[2]
  plot(density(x,width=2*iqd),xlab="x",ylab="",type="l", main="Distribuição de densidade de x")
  qqnorm(x,col="red",main="Gráfico Quantil x Quantil",xlab="Quantil Teórico",ylab="Quantil da Amostra")
  qqline(x)
  par(mfrow=c(1,1))
  
}

eda.shape1(rnorm(500,6))

x1=rpois(20,1)
x2=rpois(20,2)
x3=rpois(20,3)
x4=rpois(20,1)
sp.oc=matrix(c(x1,x2,x3,x4), ncol=4)
colnames(sp.oc)<-c("plot A", "plot B", "plot C", "plot D") #as 4 colunas terão os nomes que estçao contatenados "plot..."
rownames(sp.oc)<-paste("sp", c(1:20)) #da 1ª à 20ª linha o nome será "sp"
str(sp.oc)
dim(sp.oc)
head(sp.oc)



n.spp<-function(dados)
{
  nplot=dim(dados)[2]
  resultados=rep(0,nplot)
  names(resultados)<-paste("n.spp", c(1:nplot))
  dados[dados>0]=1
  for(i in 1:(dim(dados)[2]))
  {
    cont.sp=sum(dados[,i])
    resultados[i]=cont.sp
  }
  return(resultados)
}


##### Aplicando a função 

n.spp(sp.oc)


??qqline

######## página 2 ########## Hugo Andrade 07/12/2017 ################

analise.e= function(x, y)
{
  if(length(x) != length(y)) {message("diferentes objetos")}
  x11()
  par(mfrow = c(2,2))
  boxplot(x,main="variavel x") #dara um titulo "variavel x"
  boxplot(y, main="variavel y") #dara um titulo "variavel y"
  iqd <- summary(x)[5] -	summary(x)[2]
  plot(density(x,width=2*iqd),xlab="x",ylab="",type="l", main="Distribuição de densidade de x")
  qqnorm(x,col="blue",main="Analise explanatoria",xlab="Dados x",ylab="Dados y")
  qqline(x)
  par(mfrow=c(1,1)) 
  return(list(cor(x,y), summary(x,y)))
}


