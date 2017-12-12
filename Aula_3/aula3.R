getwd()
source("https://raw.githubusercontent.com/vrios/Intro-Linguagem-R/master/arquivos/scriptOnline.R")
caixeta = read.csv("https://raw.githubusercontent.com/vrios/Intro-Linguagem-R/master/arquivos/caixeta.csv")

#save.image salva todo o script

list.files()
aulas = list.files(pattern = "aula") # arquivos que possuam "aula"" em qualquer lugar do nome
aulas

scripts = list.files(pattern = "\\.R$") # "\\.R$" significa arquivos terminados em .R
scripts # objeto contendo os nomes dos arquivos, não os conteúdos
#pode trocar o ".R" para substituir por qualquer outra extensão no diretorio


source("..//..//arquivos/toroidal.distance.R") # o nº de pontos entre contra barra (..//..//) significa que o arquivo
#a ser procurado não está a um caminho anterior


arquivo = read.csv(file.choose()) #serve para dar o comando de procurara o arquivo no diretorio onde vc o deixou, porém vc não pode escolher
#se quer que todas as virgulas sejam lidas como casa decimal ou colunas.

arquivolido = read.csv(file = "diretorio do arquivo"
                       ,header = TRUE #interpreta a primeira linha como sendo o nome das colunas
                       ,as.is = TRUE  # não altera interpreta o tipo de dado em cada coluna, não altera seus dados. Se for FALSE, texto é interpretado como factor
                       ,sep = "," # define o caractere que deve ser o separador de colunas
                       ,dec = "." #define o caractere que deve ser o spearador de decimal
)


write.csv(x= caixeta, 
          file= caixeta.csv,
          sep= ",",
          dec= ".",
)


install.packages("writexl", dep= T)
library(writexl)
library(readxl)
write_xlsx(mtcars ,mtcars.xlsx")
out <- read_xlsx("../../arquivos/mtcars.xlsx")


load(caixeta.csv)

