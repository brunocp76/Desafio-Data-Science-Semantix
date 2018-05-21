###############################################
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
library(MASS)
library(SmartEDA)
library(fitdistrplus)
library(rpart)
library(rpart.plot)


### - Setando o diretorio com os arquivos...

setwd("D:/Documentos/Documentos/Bruno/Teste de Analytics para a Semantix/")

getwd()


### - Lendo o arquivo salvo anteriormente

system.time(load("Base.RData"))


### - Verificando o consumo de RAM

memory.size() # Informa a quantidade de memória RAM alocada (em Mb)


### - Pergunta 1: Qual profissão tem mais tendência a fazer um empréstimo? De qual tipo?

Questao1 <- Base[c("job","housing","loan")]

Questao1$anyloan <- as.factor(ifelse(Questao1$housing == "yes" | Questao1$loan == "yes", "yes", "no"))

ExpCTable(data = Questao1,
          Target=NULL,
          margin=1,
          clim=12,
          nlim=NULL,
          round=2,
          bin=NULL,
          per=FALSE
)

tab.camp.any <- as.table(ftable(Questao1[c("job","anyloan")]))

tab.camp.any <- as.table(cbind(tab.camp.any,
                                as.vector(tab.camp.any[,1]
                                          +tab.camp.any[,2])))

tab.camp.any <- as.table(rbind(tab.camp.any,
                                as.vector(c(sum(tab.camp.any[,1]),
                                            sum(tab.camp.any[,2]),
                                            sum(tab.camp.any[,3])))))

dim(tab.camp.any)
tab.camp.any
400*prop.table(tab.camp.any)
200*prop.table(tab.camp.any, margin=1)
200*prop.table(tab.camp.any, margin=2)

tab.camp.hou <- as.table(ftable(Questao1[c("job","housing")]))

tab.camp.hou <- as.table(cbind(tab.camp.hou,
                               as.vector(tab.camp.hou[,1]
                                         +tab.camp.hou[,2])))

tab.camp.hou <- as.table(rbind(tab.camp.hou,
                               as.vector(c(sum(tab.camp.hou[,1]),
                                           sum(tab.camp.hou[,2]),
                                           sum(tab.camp.hou[,3])))))

dim(tab.camp.hou)
tab.camp.hou
400*prop.table(tab.camp.hou)
200*prop.table(tab.camp.hou, margin=1)
200*prop.table(tab.camp.hou, margin=2)

tab.camp.loa <- as.table(ftable(Questao1[c("job","loan")]))

tab.camp.loa <- as.table(cbind(tab.camp.loa,
                               as.vector(tab.camp.loa[,1]
                                         +tab.camp.loa[,2])))

tab.camp.loa <- as.table(rbind(tab.camp.loa,
                               as.vector(c(sum(tab.camp.loa[,1]),
                                           sum(tab.camp.loa[,2]),
                                           sum(tab.camp.loa[,3])))))

dim(tab.camp.loa)
tab.camp.loa
400*prop.table(tab.camp.loa)
200*prop.table(tab.camp.loa, margin=1)
200*prop.table(tab.camp.loa, margin=2)

#summary(glm(anyloan ~ job, family=binomial(link="logit"), data=Questao1))

rm(tab.camp.any)
rm(tab.camp.hou)
rm(tab.camp.loa)
rm(Questao1)


### - Pergunta 2: Fazendo uma relacao entre numero de contatos e sucesso da campanha, quais sao os pontos relevantes a serem observados?

Questao2 <- Base[c("campaign","y")]

Questao2$camptrim <- as.factor(ifelse(Questao2$campaign == 1, "1", ifelse(Questao2$campaign == 2, "2", "2+")))

tab.camp.y <- as.table(ftable(Base[c("campaign","y")]))

tab.camp.y <- as.table(cbind(tab.camp.y,
                             as.vector(tab.camp.y[,1]
                                       +tab.camp.y[,2])))

tab.camp.y <- as.table(rbind(tab.camp.y,
                             as.vector(c(sum(tab.camp.y[,1]),
                                         sum(tab.camp.y[,2]),
                                         sum(tab.camp.y[,3])))))

dim(tab.camp.y)
tab.camp.y
400*prop.table(tab.camp.y)
200*prop.table(tab.camp.y, margin=1)
200*prop.table(tab.camp.y, margin=2)

200*prop.table(tab.camp.y[1:7,], margin=1)


### - Pergunta 3: Baseando-se nos resultados de adesao desta campanha qual o numero medio e o maximo de ligacoes que voce indica para otimizar a adesao?

tab.camp.res <- as.table(ftable(Questao2[c("camptrim","y")]))

tab.camp.res <- as.table(cbind(tab.camp.res,
                               as.vector(tab.camp.res[,1]
                                         +tab.camp.res[,2])))

tab.camp.res <- as.table(rbind(tab.camp.res,
                               as.vector(c(sum(tab.camp.res[,1]),
                                           sum(tab.camp.res[,2]),
                                           sum(tab.camp.res[,3])))))

dim(tab.camp.res)
tab.camp.res
400*prop.table(tab.camp.res)
200*prop.table(tab.camp.res, margin=1)
200*prop.table(tab.camp.res, margin=2)


### - Pergunta 4: O resultado da campanha anterior tem relevancia na campanha atual?

Questao4 <- Base[c("poutcome","y")]

tab.ant.y <- as.table(ftable(Questao4))

tab.ant.y <- as.table(cbind(tab.ant.y,
                            as.vector(tab.ant.y[,1]
                                      +tab.ant.y[,2])))

tab.ant.y <- as.table(rbind(tab.ant.y,
                            as.vector(c(sum(tab.ant.y[,1]),
                                        sum(tab.ant.y[,2]),
                                        sum(tab.ant.y[,3])))))

dim(tab.ant.y)
tab.ant.y
400*prop.table(ftable(tab.ant.y))
200*prop.table(ftable(tab.ant.y), margin = 1)
200*prop.table(ftable(tab.ant.y), margin = 2)

rm(tab.ant.y)
rm(Questao4)


### - Pergunta 5: Qual o fator determinante para que o banco exija um seguro de credito?

table(Base[c("default")])
100*prop.table(table(Base[c("default")]))

# - Definindo os filtros para a amostra balanceada
default.yes <- Base$default == "yes"
default.no <- Base$default == "no"

# - Aplicando os filtros para a amostra balanceada
Questao5.yes <- Base[default.yes,-c(10:11)]
dim(Questao5.yes)
previa.no <- Base[default.no,-c(10:11)]
Questao5.no <- previa.no[sample(x = nrow(previa.no),
                                size = 815,
                                replace = F),]
rm(previa.no)
rm(default.yes)
rm(default.no)
dim(Questao5.no)

Questao5 <- rbind(Questao5.no,Questao5.yes)
rm(Questao5.no)
rm(Questao5.yes)

Questao5 <- Questao5[sample(x = nrow(Questao5),
                            size = nrow(Questao5),
                            replace = F), ]
table(Questao5[c("default")])
100*prop.table(table(Questao5[c("default")]))


# Primeiro utilizando Arvore de Decisao (modelando default):
system.time(rtree_def <- rpart(default ~ .,data = Questao5))
# summary(rtree_def)
rpart.plot(rtree_def)

# Depois utilizando RegresSoes Logisticas (modelando default):
system.time(noglm_def <- glm(default ~ 1, family = binomial(link = "logit"), data = Questao5))
summary(noglm_def)

system.time(fullglm_def <- glm(default ~ ., family = binomial(link = "logit"), data = Questao5))
summary(fullglm_def)

system.time(backward_def <- step(object = fullglm_def,          # modelo inicial
                    scope = list(lower = formula(noglm_def),    # mais simples
                                 upper = formula(fullglm_def)), # mais completo
                    direction = "backward",
                    trace=9))

system.time(forward_def <- step(object = noglm_def,             # modelo inicial
                    scope = list(lower = formula(noglm_def),    # mais simples
                                 upper = formula(fullglm_def)), # mais completo
                    direction = "forward",
                    trace = 9))

system.time(stepwise_def <- step(object = noglm_def,            # modelo inicial
                    scope = list(lower = formula(noglm_def),    # mais simples
                                 upper = formula(fullglm_def)), # mais completo
                    direction = "both",
                    trace = 9))

summary(backward_def)
summary(forward_def)
summary(stepwise_def)

formula(backward_def)
formula(forward_def)
formula(stepwise_def)

backward_def$anova
forward_def$anova
stepwise_def$anova

rm(rtree_def)
rm(noglm_def)
rm(fullglm_def)
rm(backward_def)
rm(forward_def)
rm(stepwise_def)
rm(Questao5)


# Primeiro utilizando Arvore de Decisao (modelando y):
system.time(rtree_y <- rpart(y ~ .,data = Base))
# summary(rtree_y)
rpart.plot(rtree_y)

# Depois utilizando RegresSoes Logisticas (modelando y):
system.time(noglm_y <- glm(y ~ 1, family = binomial(link = "logit"), data = Base))
summary(noglm_y)

system.time(fullglm_y <- glm(y ~ ., family = binomial(link = "logit"), data = Base))
summary(fullglm_y)

system.time(backward_y <- step(object = fullglm_y,              # modelo inicial
                      scope = list(lower = formula(noglm_y),    # mais simples
                                   upper = formula(fullglm_y)), # mais completo
                      direction = "backward",
                      trace=9))

system.time(forward_y <- step(object = noglm_y,                 # modelo inicial
                      scope = list(lower = formula(noglm_y),    # mais simples
                                   upper = formula(fullglm_y)), # mais completo
                      direction = "forward",
                      trace = 9))

system.time(stepwise_y <- step(object = noglm_y,                # modelo inicial
                      scope = list(lower = formula(noglm_y),    # mais simples
                                   upper = formula(fullglm_y)), # mais completo
                      direction = "both",
                      trace = 9))

summary(backward_y)
summary(forward_y)
summary(stepwise_y)

formula(backward_y)
formula(forward_y)
formula(stepwise_y)

backward_y$anova
forward_y$anova
stepwise_y$anova

rm(rtree_y)
rm(noglm_y)
rm(fullglm_y)
rm(backward_y)
rm(forward_y)
rm(stepwise_y)



### - Pergunta 6: Quais sao as caracteristicas mais proeminentes de um cliente que possua emprestimo imobiliario?


# - Aplicando os filtros para a amostra balanceada
Questao6 <- Base[,-c(10:11)]

dim(Questao6)

names(Questao6)

# - Começando pelas variaveis categoricas:
tab.hous <- ExpCTable(data = Base,
                      Target="housing",
                      margin=1,
                      clim=12,
                      nlim=NULL,
                      round=5,
                      bin=NULL,
                      per=FALSE)

housing.no <- as.vector(100*tab.hous[,3]/20081)
housing.yes <- as.vector(100*tab.hous[,4]/25130)
housing.tot <- as.vector(100*tab.hous[,5]/45211)

tab.hous <- cbind(tab.hous,
                  housing.no,
                  housing.yes,
                  housing.tot)

rm(housing.no)
rm(housing.yes)
rm(housing.tot)
rm(tab.hous)

ExpNumStat(data = Base,
           by = "GA",
           gp="housing",
           Qnt=seq(0,1,0.1),
           MesofShape=2,
           Outlier=F,
           round=2)

ExpNumViz(data = Base[,-c(10:11)],
          gp="housing",
          type=1,
          nlim=NULL,
          fname=NULL,
          col=c("lightgray","lightblue","lightgreen"),
          Page=c(2,3),
          sample=8)


# Primeiro utilizando Arvore de Decisao (modelando housing):
system.time(rtree_hou <- rpart(housing ~ .,data = Questao6))
# summary(rtree_hou)
rpart.plot(rtree_hou)

# Depois utilizando RegresSoes Logisticas (modelando housing):
system.time(noglm_hou <- glm(housing ~ 1, family = binomial(link = "logit"), data = Questao6))
summary(noglm_hou)

system.time(fullglm_hou <- glm(housing ~ ., family = binomial(link = "logit"), data = Questao6))
summary(fullglm_hou)

system.time(backward_hou <- step(object = fullglm_hou,          # modelo inicial
                    scope = list(lower = formula(noglm_hou),    # mais simples
                                 upper = formula(fullglm_hou)), # mais completo
                    direction = "backward",
                    trace=9))

system.time(forward_hou <- step(object = noglm_hou,             # modelo inicial
                    scope = list(lower = formula(noglm_hou),    # mais simples
                                 upper = formula(fullglm_hou)), # mais completo
                    direction = "forward",
                    trace = 9))

system.time(stepwise_hou <- step(object = noglm_hou,            # modelo inicial
                    scope = list(lower = formula(noglm_hou),    # mais simples
                                 upper = formula(fullglm_hou)), # mais completo
                    direction = "both",
                    trace = 9))

summary(backward_hou)
summary(forward_hou)
summary(stepwise_hou)

formula(backward_hou)
formula(forward_hou)
formula(stepwise_hou)

backward_hou$anova
forward_hou$anova
stepwise_hou$anova

rm(rtree_hou)
rm(noglm_hou)
rm(fullglm_hou)
rm(backward_hou)
rm(forward_hou)
rm(stepwise_hou)
rm(Questao6)


### - Devaneios na pergunta 2 (pra aprender a fazer)...
lognormal.camp <- summary(fitdist(Questao2$campaign,
                                  distr = "lnorm",
                                  start=c(meanlog=mean(log(Questao2$campaign)),
                                          sdlog=sd(log(Questao2$campaign)))))

gamma.camp <- summary(fitdist(Questao2$campaign,
                              distr = "gamma"))

weibull.camp <- summary(fitdist(Questao2$campaign,
                                distr = "weibull"))

exp.camp <- summary(fitdist(Questao2$campaign,
                            distr = "exp",
                            start = 1/mean(Questao2$campaign)))

negbin.camp <- summary(fitdist(Questao2$campaign,
                               distr = "nbinom"))

geom.camp <- summary(fitdist(Questao2$campaign,
                             distr = "geom"))

pois.camp <- summary(fitdist(Questao2$campaign,
                             distr = "pois"))

norm.camp <- summary(fitdist(Questao2$campaign,
                             distr = "norm",
                             start=c(mean=mean(Questao2$campaign),
                                     sd=sd(Questao2$campaign))))

t.camp <- fitdistr(Questao2$campaign,
                   densfun = "t")

lognormal.camp
gamma.camp
weibull.camp
exp.camp
t.camp
t.camp$loglik
negbin.camp
geom.camp
pois.camp
norm.camp


### - Histograma do Numero de Contatos com Distribuicoes Continuas Ajustadas (mais devaneios a partir da pergunta 2)...
hist(Questao2$campaign,
     freq=FALSE,
     main="Histograma do Numero de Contatos",
     xlab = "Numero de Contatos",
     ylab = "Frequencia Relativa",
     nclass=80,
     xlim=c(0,20),
     ylim=c(0,0.4))

curve(dlnorm(x,
             meanlog = log(mean(Questao2$campaign)),
             sdlog = log(sd(Questao2$campaign))),
      col=4,
      add=T)

curve(dgamma(x,
             gamma.camp$estimate[1],
             gamma.camp$estimate[2]),
      col=2,
      add=T)

curve(dweibull(x,
               weibull.camp$estimate[1],
               weibull.camp$estimate[2]),
      col=3,
      add=T)

curve(dexp(x,
           rate = 1/mean(Questao2$campaign)),
      col=6,
      add=T)

curve(dt(x,
         t.camp$estimate[1]),
      col=5,
      add=T)

curve(dnorm(x,
            mean = mean(Questao2$campaign),
            sd = sd(Questao2$campaign)),
      col=7,
      add=T)

legend(x = 13,
       y = 0.40,
       legend = c("Log-Normal","Gamma","Weibull","Exponencial","t","Normal"),
       col = c(4,2,3,6,5,7),
       lty = 1,
       title = "Distribuicoes Teoricas",
       text.font=4,
       bg = "white")


### - Densidade Empirica do Numero de Contatos com Distribuicoes Discretas Ajustadas (mais devaneios a partir da pergunta 2)...
plot(density(Questao2$campaign),
     main = "Densidade Empirica do Numero de Contatos Previos",
     xlim=c(0,20))

curve(dnbinom(x,
              negbin.camp$estimate[1],
              negbin.camp$estimate[2]),
      col=2,
      add=T)

curve(dgeom(x,
            geom.camp$estimate[1]),
      col=4,
      add=T)

curve(dpois(x,
            pois.camp$estimate[1]),
      col=3,
      add=T)

legend(x = 11,
       y = 0.80,
       legend = c("Binomial Negativa", "Geometrica", "Poisson"),
       col = c(2,4,3),
       lty = 1,
       title = "Distribuicoes Teoricas",
       text.font=4,
       bg = "white")


### - Plots de Ajustes de Densidade do Numero de Contatos Previos
plotdist(data = Questao2$campaign,
         histo = T)

plotdist(data = Questao2$campaign,
         distr = "lnorm",
         para = list(meanlog = mean(log(Questao2$campaign)),
                     sdlog = sd(log(Questao2$campaign))),
         histo = T)

plotdist(data = Questao2$campaign,
         distr = "gamma",
         para = list(gamma.camp$estimate[1],
                     gamma.camp$estimate[2]),
         histo = T)

plotdist(data = Questao2$campaign,
         distr = "weibull",
         para = list(weibull.camp$estimate[1],
                     weibull.camp$estimate[2]),
         histo = T)

plotdist(data = Questao2$campaign,
         distr = "exp",
         para = list(rate = 1/mean(Questao2$campaign)),
         histo = T)

plotdist(data = Questao2$campaign,
         distr = "t",
         para = list(t.camp$estimate[1]),
         histo = T)

plotdist(data = Questao2$campaign,
         distr = "geom",
         para = list(geom.camp$estimate[1]),
         histo = TRUE)

plotdist(data = Questao2$campaign,
         distr = "pois",
         para = list(pois.camp$estimate[1]),
         histo = TRUE)

plotdist(data = Questao2$campaign,
         distr = "norm",
         para = list(mean = mean(Questao2$campaign),
                     sd = sd(Questao2$campaign)),
         histo = T)


### - Limpando variaveis auxiliares
rm(tab.camp.y)
rm(tab.camp.res)
rm(lognormal.camp)
rm(gamma.camp)
rm(weibull.camp)
rm(exp.camp)
rm(t.camp)
rm(negbin.camp)
rm(geom.camp)
rm(pois.camp)
rm(norm.camp)
rm(Questao2)
