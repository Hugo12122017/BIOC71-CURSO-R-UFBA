#### Aluno: Hugo Andrade

aves.c <- read.table("aves_cerrado.csv", row.names=1, header=T, sep=";", dec=",", as.is=T) #Abre em um objeto os dados do arquivo
aves.c <- read.csv2("aves_cerrado.csv", row.names=1, as.is=T) #Outra forma de carregar arquivo em formato csv

# Diferentes formas de Verificação inicial do data frame.
head(aves.c)
tail(aves.c)
str(aves.c)
summary(aves.c)

aves.c[aves.c$urubu==NA,] #não executa, pois O teste lógico para NA é feito pela função is.na:
is.na(aves.c) #testa a presença de NA no objeto aves.c.
is.na(aves.c$urubu) #testa a presença de NA na coluna urubu

aves.c[is.na(aves.c$urubu),] #Mostra os demais valores das colunas para a linha que tem o NA em "urubu".
aves.c[is.na(aves.c$urubu)|is.na(aves.c$carcara)|is.na(aves.c$seriema),] #mostra os valores de NA das colunas de uma vez só.

temp1 <- aves.c[is.na(aves.c$urubu)|is.na(aves.c$carcara)|is.na(aves.c$seriema),] #Cria um objeto temporário
aves.c$urubu[is.na(aves.c$urubu)] <- 0 #Corrige o valor de NA por 0 na coluna urubu.
aves.c[is.na(aves.c$urubu),2] <- 0 #corrigr o valor de NA por 0, a partir da indicação da posição da coluna.
aves.c[is.na(aves.c[,2]), 2] <- 0 #corrigr o valor de NA por 0, a partir da indicação da posição da coluna.
aves.c$carcara[is.na(aves.c$carcara)] <- 0 #Corrige o valor de NA por 0 na coluna carcara.
aves.c$seriema[is.na(aves.c$seriema)] <- 0 #Corrige o valor de NA por 0 na coluna seriema.

aves.c[aves.c$urubu==0|aves.c$carcara==0|aves.c$seriema==0,]
temp1 #Resgata o objeto temp1, o que mostra que o valor "0" só foi substituída para a coluna urubu.

table(aves.c$fisionomia) #tabula os dados da coluna fisionomia.
aves.c$fisionomia[aves.c$fisionomia=="ce"] <- "Ce" #corrige o erro, trocando "ce" por "Ce"
table(aves.c$fisionomia) #tabula os dados da coluna fisionomia com as correções feitas para Ce.

aves.c$fisionomia <- factor(aves.c$fisionomia, levels=c("CL","CC","Ce")) #converte a coluna fisionomia para um fator organizado por níveis do mais aberto para menos.
str(aves.c) #Resumo dos valores.
summary(aves.c) #Resumo dos valores.

#Média, Mediana e Quantis
mean(aves.c[,2:4]) #calcula a média  da coluna 2 a 4.
sapply(aves.c[,2:4],mean) #calcula a média  da coluna 2 a 4 usando a função sapply.
sapply(aves.c[,2:4],median) #calcula a mediana  da coluna 2 a 4 usando a função sapply.

apply(aves.c[,2:4],2,median) #calcula a mediana  da coluna 2 a 4 usando a função apply, especificando a margem 2 que corresponde a todas as colunas.
apply(aves.c[,2:4], 2, mean, trim=0.1) #Calcula a média truncada

quantile(aves.c$urubu) ## O mesmo que o retornado pelo summary, mas não executa, pois deveria remover os NA's.
summary(aves.c$urubu)

quantile(aves.c$urubu, probs= seq(from=0,to=1,by=0.1)) #Não executa, pois deveria remover os NA's antes.
summary(aves.c[,2:4])

##Exploração de uma Variável Categórica
caixeta <- read.csv("caixeta.csv", as.is=T) #Abre em um objeto os dados do arquivo caixeta.csv
names(caixeta) #Títulos das colunas.
table(caixeta$especie) #Tabula os valores coluna especie com o valor de quantas vezes aparece na tabela.

sort(table(caixeta$especie), decreasing=T) #Organiza os valores do maior para o menor

barplot(sort(table(caixeta$especie), decreasing=T)) #Plota os valores das especies do maior para o menor.
barplot(table(caixeta$local)) #Plota a coluna local em um grafico de barras.

#Gráficos para uma Variável
par(mfrow=c(2,2)) #Define a posição dos gráficos, neste caso: duas linhas e duas colunas.
boxplot(aves.c$urubu) #Boxplot da coluna urubu
hist(aves.c$urubu) #Histograma da coluna urubu
plot(density(aves.c$urubu)) #Plot da densidade de urubus
stripchart(aves.c$urubu, method="stack")
par(mfrow=c(1,1)) #Define a posição dos gráficos, neste caso: uma linha e uma coluna.

#Variações do Histograma
## Histograma com os valores (funcao rug)
hist(aves.c$urubu) #Histograma da coluna urubu
rug(jitter(aves.c$urubu))
rug(aves.c$urubu)
?rug

hist(aves.c$urubu, prob=T) #Histograma da coluna urubu
lines( density(aves.c$urubu),col="blue" ) #Insere uma linha de ajuste não paramétrico de densidade probabilística

hist(aves.c$urubu, prob=T) #Histograma da coluna urubu
curve(expr = dnorm(x,mean=mean(aves.c$urubu),sd=sd(aves.c$urubu)),add=T, col="red") #Insere a curva normal no histograma.

plot(density(aves.c$urubu),col="blue", ylim=c(0,0.08)) #Plota a curva de densidade
curve(expr = dnorm(x,mean=mean(aves.c$urubu),sd=sd(aves.c$urubu)),add=T, col="red") #Plota a curva normal, baseada na media e desvio padrão.


#table e aggregate
table(caixeta$especie,caixeta$local) #Tabula a variavel especie e local (com suas frequencias)
caixeta.alt <- aggregate(caixeta$h, by=list(local=caixeta$local,especie=caixeta$especie),
                         FUN=mean) #Obtem do objeto caixeta um data frame com a altura média dos fustes de cada espécie de árvore por local. 
caixeta.alt

#xtabs
Titanic.df <- read.csv("titanic.csv", as.is=T) #Carrega o objeto Titanic.df com arquivo Titanic.csv
Titanic.df 
xtabs(Freq~Sex+Survived, data=Titanic.df) #Cria uma tabela de contigência, nesse caso Freq~Sex+Survived
?xtab
prop.table(xtabs(Freq~Sex+Survived, data=Titanic.df), margin=1) #Resultado da tabela de contigência em proporção.
?prop.table
xtabs(Freq~Class+Survived, data=Titanic.df) #Cria uma tabela de contigência, nesse caso Freq~Class+Survived
prop.table(xtabs(Freq~Class+Survived, data=Titanic.df), margin=1) #Resultado da tabela de contigência em proporção.

table(Titanic.df$Sex,Titanic.df$Survived)

#Fórmula Estatística em Gráficos
boxplot(urubu~fisionomia, data=aves.c) #Boxplot da coluna urubu em função da fisionomia.
plot(seriema~urubu, data=aves.c, subset=fisionomia=="Ce") #Plot da variável seriema em função de urubu

plot(seriema~urubu, data=aves.c, subset=fisionomia=="CC") #Plot da variável seriema em função de urubu, quando a fisionomia for CC.
plot(seriema~urubu, data=aves.c, subset=fisionomia!="CL") #Plot da variável seriema em função de urubu, quando a fisionomia for CL.

library(lattice)
?lattice
xyplot(seriema~urubu|fisionomia, data= aves.c) #Plota a variavel seriema em função de urubu, sob a condicionante fisionomia.

#O quarteto de Anscombe
data(anscombe)#carrega para a area de trabalho
ls() #agora o objeto está no workspace

names(anscombe) ##carrega para a area de trabalho

apply(anscombe[1:4], MARGIN=2, FUN=mean) #Cálculo da média
apply(anscombe[5:8], 2, mean) #Cálculo da média

apply(anscombe[1:4], 2, var)   #Cálculo da variância
apply(anscombe[5:8], 2, var) #Cálculo da variância

with(anscombe,cor(x1,y1)) #Testa a relação entre as variáveis.
with(anscombe,cor(x2,y2)) #Testa a relação entre as variáveis.
with(anscombe,cor(x3,y3)) #Testa a relação entre as variáveis.
with(anscombe,cor(x4,y4)) #Testa a relação entre as variáveis.

par(mfrow=c(2,2)) # 4 graficos em uma janela
plot(y1~x1, data=anscombe) #Plota Y1 em função de X1
plot(y2~x2, data=anscombe) #Plota Y2 em função de X2
plot(y3~x3, data=anscombe) #Plota Y3 em função de X3
plot(y4~x4, data=anscombe) #Plota Y4 em função de X4
par(mfrow=c(1,1)) #

par(mfrow=c(2,2)) # 4 graficos em uma janela
plot(y1~x1, data=anscombe,
     xlim=range(anscombe[,1:4]),ylim=range(anscombe[,5:8]))
abline(lm(y1~x1, data=anscombe))
plot(y2~x2, data=anscombe,
     xlim=range(anscombe[,1:4]),ylim=range(anscombe[,5:8]))
abline(lm(y2~x2, data=anscombe))
plot(y3~x3, data=anscombe,
     xlim=range(anscombe[,1:4]),ylim=range(anscombe[,5:8]))
abline(lm(y3~x3, data=anscombe))
plot(y4~x4, data=anscombe,
     xlim=range(anscombe[,1:4]),ylim=range(anscombe[,5:8]))
abline(lm(y4~x4, data=anscombe))
par(mfrow=c(1,1))



