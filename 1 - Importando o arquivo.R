################################################
#
# Teste de Analytics da Semantix
#
# Candidato: Bruno Cesar Pasquini
#
#  1 - Importacao da Base
#  2 - Analise Exploratoria
#  3 - Respondendo as perguntas do teste
#
################################################


### - Setando o diretorio com os arquivos...

setwd("D:/Documentos/Documentos/Bruno/Teste de Analytics para a Semantix/")

getwd()


### - Importando base dados em csv

Base <- read.table(file = "bank-full.csv",
                   header = TRUE, #primeira linha traz os nomes das vari�veis
                   sep = ";",     #delimitador de colunas
                   dec = ".",     #decimal, se "." ou ","
                   quote = "\""   #delimitador de variaveis string
                  )  


### - Verificando o consumo de RAM

memory.size() # Informa a quantidade de mem�ria RAM alocada (em Mb)


### - Dados preliminares do arquivo lido...

dim(Base)   # Dimens�o da base 45211 x 17

names(Base) # Fun��o q retorna os nomes das variaveis

summary(Base) # Resumo da base.


### - Salvando o arquivo no formato proprio do R

save(Base, file = "Base.RData") # Salva o objeto como R


### - Apagando o arquivo da mem�ria

rm(Base)
