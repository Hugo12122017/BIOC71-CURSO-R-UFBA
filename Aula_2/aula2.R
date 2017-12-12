vacas = c(121.72355, 103.79754, 130.15442, 98.29305, 103.43365, 102.44998, 125.49993, 
          111.07215, 113.74047, 103.16081, 80.87149, 98.66692, 65.09549, 155.74965, 
          88.30168, 147.4361, 114.60806, 109.87433, 149.54772, 83.54137)
fazendeiros = c(77.91352, 78.07251, 81.95604, 75.64862, 78.45213, 79.11058, 
                79.98952, 79.18127, 840.1635, 74.8686, 82.01886, 78.26936, 77.94691, 78.75372, 
                77.64901, 77.64097, 77.19803, 72.48175, 83.45336, 78.99681)
virgens = c(127.9366, 201.7158, 136.1366, 136.588, 131.7213, 118.1486, 125.7534, 
            139.6544, 163.589, 139.7455, NA, 141.445, 110.7311, 157.5921, 176.8437, 
            102.8659, 121.8286, 134.7097, 157.1392, 166.7133)
aventureiros = c(191.3721, 216.1671, 165.438, 196.273, 172.6565, 178.2955, 193.6687, 
                 189.7674, 160.2968, 208.44, 204.0934, 208.1798, 186.638, 193.9446, 197.365, 
                 198.6853, 213.8838, 210.1881, 209.9109, 210.9228)
alimento = c(rep("vacas", 20), rep("fazendeiros", 20), rep("virgens", 20), rep("aventureiros", 
                                                                               20))
dragoes = data.frame(x = alimento, y = c(vacas, fazendeiros, virgens, aventureiros))
head(dragoes)
summary(dragoes)
x11()
plot(dragoes)
summary(lm(formula = dragoes$y ~ dragoes$x))
max(dragoes[, 2])
max(dragoes[, 2], na.rm = T)

dragoes[dragoes == 840.1635] = 84.01635
max(dragoes[, 2], na.rm = TRUE)
x11()
plot(dragoes)


getwd()
nomesComErro = read.table("caixetaComErro.csv", header = T, sep = ",")
head(nomesComErro)
unique(nomesComErro$especie)
sort(unique(nomesComErro$especie))

nomesComErro$especie[nomesComErro$especie == "Tabebuia casssinoides"] = "Tabebuia cassinoides"
sort(unique(nomesComErro$especie))
nomesComErro$especie[nomesComErro$especie == "Tabebuia casinoides"] = "Tabebuia cassinoides"
sort(unique(nomesComErro$especie))
nomesComErro$especie == "Tabebuia casinoides"
nomesComErro$especie[nomesComErro$especie == "Tabebuia casinoides"]
nomesComErro$especie == "Tabebuia casinoides"
nomesComErro$especie[nomesComErro$especie == "Tabebuia casinoides"] = "Tabebuia cassinoides"
sort(unique(nomesComErro$especie))

pi
3.141593 == pi


i = 0.1
i = i + 0.05
all.equal(i, 0.15)
i == 0.15


teste = c(1, 2, 2, 3, 4, 5, 6, 7)
(teste[1] == 1) & (teste[2]/2 == 1)


teste = c(1, 2, 2, 3, 4, 5, 6, 7)
(teste[1] == 1) & (teste[2]/2 == 1.5)

nomesComErro[nomesComErro$especie == "Tabebuia cassinoides" & nomesComErro$local =="chauas",] 
head(nomesComErro)


nomesComErro[nomesComErro$especie == "Tabebuia cassinoides" | nomesComErro$local =="chauas",] 
head(nomesComErro)


dragoes[dragoes$y >= 100 & dragoes$y <= 150, ]

dragoes$x=factor(dragoes$x)

teste.de.normalidade = shapiro.test(dragoes$y)
if (teste.de.normalidade$p.value >= 0.05) {
  fit = lm(dragoes$y ~ dragoes$x)
  plot(dragoes, main="normal")
  summary(fit)
} else {kruskal.test(dragoes)
plot(dragoes, main= "kruskal")}


for (i in 1:10) {
  a = i^2
  message(a)
}

for (i in 10:1) #coloque aqi uma ssequencia com quantas vezes vc quer 
  #repetir seu codigo abaixo

{message(listfile)} 