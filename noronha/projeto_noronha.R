#f <- readClipboard("C:\Users\tonmo\Dropbox\Mestrado Turismo USP\Oficina R GPET"")

library(data.table) #mais rápido e versátil do que o dplyr'

setwd("C:/Users/tonmo/Dropbox/Mestrado Turismo USP/Oficina R GPET")
#alt ctrl k seleciona o mesmo caracter do text

dt <- fread("bd_noronha_2019.txt")

#alt- é atalho pra <-

head(dt)
summary(dt)
glimpse(dt) 'de outro pacote'
names(dt)
head(dt$genero)
head(dt$golfinhos)

yt <- 0.08 # verdadeiro percentual de estrangeiros dado pelo controle de demanda

setnames(dt, old = "noronha_estrangeiros", new = "yd") #alterar nome da variável principal, por facilidade

dt$yd <- dt$yd/100
mean <- mean(dt$yd)

med <- median(dt$yd)
med

dt[yd>yt, .N] #.N mostra quantas linhas entraram no filtro do primeiro argumento
dt[, .N] #quantidade total de linhas 
dt[yd>yt, .N] / dt[, .N] #Percentual dos entrevistados que estimaram um valor superior ao verdadeiro, ou seja, aos 8% do controle da demanda


#teste não paramétrico de médias, pois a distribuição da variável não é normal
wilcox <- wilcox.test(dt$yd, mu = yt, alternative = "greater")
# o teste indica que a mediana das estimativas é maior do que 8%

View(wilcox)
format(wilcox, scientific = FALSE)
options(scipen = 999) #não consegui usar essa fórmula pra evitar numero em exponencial

hist(dt$yd) #histograma da base do R

library(ggplot2)

ggplot(dt, aes(x=yd)) + 
  geom_histogram(fill = "#39c1d8", alpha=0.6, binwidth = 0.05) +
  labs(x="Share of foreign tourist", y="Count") +
  geom_vline(xintercept = yt, color = "#F99d2A", size = 2) +
  geom_label(aes(x=yt, y=7, label=paste(sep="\n", "true share:", yt)), color = "#F99d2A") +
  geom_vline(xintercept = med, color = "#F99d2A", size = 2) +
  geom_label(aes(x=mean(yt), y=7, label=paste(sep="\n", "true share:", yt)), color = "#F99d2A")
