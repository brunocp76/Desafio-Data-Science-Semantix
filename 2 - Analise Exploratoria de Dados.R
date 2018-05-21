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


### - Carregando bibliotecas adicionais...
library(SmartEDA)
library(DataExplorer)
#library(PerformanceAnalytics)


### - Setando o diretorio com os arquivos...

setwd("D:/Documentos/Documentos/Bruno/Teste de Analytics para a Semantix/")

getwd()


### - Lendo o arquivo salvo anteriormente

load("Base.RData")


### - Verificando o consumo de RAM

memory.size() # Informa a quantidade de memória RAM alocada (em Mb)


### - Dados preliminares do arquivo lido...

dim(Base)   # Dimensão da base 45211 x 17

names(Base) # Função q retorna os nomes das variaveis

summary(Base) # Resumo da base.


### - Ferramentas da biblioteca SmartEDA - Toda a base
ExpData(data = Base, # Dados a serem examinados
        type = 1,    # Tipo 1 -> visao geral, tipo 2 -> estrutura
        DV = NULL    # Variável resposta, se houver
       )

ExpData(data = Base, # Dados a serem examinados
        type = 2,    # Tipo 1 -> visao geral, tipo 2 -> estrutura
        DV = NULL    # Variável resposta, se houver
       )


### - Ferramentas da biblioteca SmartEDA - Variaveis Numericas
ExpNumStat(data = Base,
           by=c("A","G","GA"),
           gp=NULL,
           Qnt=NULL,
           MesofShape=2,
           Outlier=FALSE,
           round=2
          )

ExpNumViz (data = Base,
           gp=NULL,
           type=1,
           nlim=NULL,
           fname=NULL,
           col=NULL,
           Page=c(2,4),
           sample=NULL
          )


### - Ferramentas da biblioteca SmartEDA - Variaveis Categoricas
ExpCTable(data = Base,
          Target=NULL,
          margin=1,
          clim=12,
          nlim=NULL,
          round=2,
          bin=NULL,
          per=FALSE
         )

ExpCatViz(data = Base,
          gp=NULL,
          fname=NULL,
          clim=12,
          col=NULL,
          margin=1,
          Page=c(2,3),
          Flip=F,
          sample=NULL
         )


### - Ferramentas da biblioteca DataExplorer - Toda a base
plot_str(Base)

plot_missing(Base)


### - Ferramentas da biblioteca DataExplorer - Variaveis Numericas
plot_histogram(Base)

plot_density(Base)

plot_correlation(data = Base,
#                 type = c("all", "discrete", "continuous"),
                 type = "continuous",
                 maxcat = 20,
                 title = NULL
                )


### - Ferramentas da biblioteca DataExplorer - Variaveis Categoricas
plot_correlation(data = Base,
                 #                 type = c("all", "discrete", "continuous"),
                 type = "discrete",
                 maxcat = 20,
                 title = NULL
)

plot_bar(data = Base,
         na.rm = TRUE,
         maxcat = 50,
         order_bar = TRUE,
         title = NULL)


### - Ferramentas da biblioteca DataExplorer - Gerando um resumo da base
create_report(data = Base,
              output_file = "report.html",
              output_dir = getwd()
)


### - Ferramentas da biblioteca PerformanceAnalytics - Toda a Base
# chart.Correlation(R = Base,
#                   histogram = TRUE,
#                   method = c("pearson", "kendall","spearman")
#                  )