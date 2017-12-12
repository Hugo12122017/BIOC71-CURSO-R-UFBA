plot(x= iris$Sepal.Length, y=iris$Sepal.Width,col= c("red", "purple", "olivedrab"))
colors()
plot(x= iris$Sepal.Length, y=iris$Sepal.Width,col= rainbow(3))
plot(iris$Pepal.Length ~ iris$Sepal.Length,col= rainbow(3))
help(plot)
plot(x= iris$Sepal.Length, y=iris$Sepal.Width, col= rainbow(3), col.axis= "black" xlab= "Eixo x", ylab = "Eixo y")
??legend
legend(3,3.5,4, legend = c(unique(iris$Species)))

plot(iris$Petal.Length ~ iris$Sepal.Length
     ,col= rainbow(3)
     ,type= "p"
     ,xlab= "Eixo x"
     ,ylab = "Eixo y"
     ,main= "Título do gráfico"
     ,family= "serif"
     ,cex= .8 #modifica o tamanho das bolas no plote do gráfico
     ,lwd= 2  #modifica a espessura das bolas no plote do gráfico
     ,lty= "dotdash"
     ,xlim= range(iris$Sepal.Length)
     ,ylim= range(iris$Petal.Length)
     , las= 2 #gira os valores/nomes dos eixos
     ,bty= "l" #mostra o formato dos eixos com ou sem caixa
     )


x11()
par(mfrow=c(2,2))      #linhas, colunas

plot(x=iris$Sepal.Length, y=iris$Petal.Length,        #primeiro gráfico
     col = iris$Species,                          
     main = "Sepal.Length x Petal.Length")     

plot(x=iris$Sepal.Length, y=iris$Sepal.Width,         #segundo gráfico
     col = iris$Species,                          
     main = "Sepal.Length x Sepal.Width in Iris")      


abline(h=mean(iris$Sepal.Length),col= "green") #adiciona uma reta horizontal
abline(
  v=mean(iris$Sepal.Length) #adiciona uma reta vertical
  ,col= "purple"
)
abline(a=3, b=3, col="green") #a=intercepto e b=inclinação
mod=lm(iris$Petal.Length~iris$Sepal.Length) #regressão linear
abline(mod) #reta de regressão
points(x=2, y=2)
arrows(x0=2, y0=2, x1=1, y1=1, angle = 90)
segments(x0= 2, y0= 8, x1= 4, y1= 8) #segmento indo de x0, y0 para x1 e y1
text(x=5, y=6, "chuchu", col = "purple") #texto ma coordenada X, y.



