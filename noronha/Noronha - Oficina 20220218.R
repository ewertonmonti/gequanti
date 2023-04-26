install.packages("data.table")

# Bibliotecas
library(data.table)
library(ggplot2)

setwd("M:/Docs/R/Oficina de R - Noronha/") # define diretório padrão

# Ler dados do banco original
dt <- fread("bd_noronha_2019.txt")
dt <- fread("M:/Docs/R/Oficina de R - Noronha/bd_noronha_2019.txt")

# Diferentes formas de apresentar data.table
head(dt)
names(dt)
head(dt$genero, 20)

setnames(dt, old = "noronha_estrangeiros", new = "yd") # alterar nome da variável principal por facilidade

dt$yd <- dt$yd/100 # computar yd em valores decimais

yt <- 0.08 # verdadeiro percentual de estrangeiros dado pelo controle de demanda

mean <- mean(dt$yd)
mean # média da estimativa

med <- median(dt$yd)
med # mediana da estimativa

dt[yd>yt, .N]
dt[, .N]
dt[yd>yt, .N]/dt[, .N] # Quantos % dos entrevistados estimaram um valor superior ao verdadeiro, ou seja, aos 8% dados no controle de demanda

###########################################
# Teste não paramétrico para mediana
wilcox.test(dt$yd, mu = yt, alternative = "greater")

# Teste sem notação científica
wilcox <- wilcox.test(dt$yd, mu = yt, alternative = "greater")
format(wilcox, scientific = FALSE)

hist(dt$yd) # Histograma da base do R

# Gráfico final
ggplot(dt, aes(x=yd)) +
  geom_histogram(fill="#39C1D8", alpha=0.6, binwidth = 0.05) + # Barras do histograma
  labs(x="Share of foreign tourists", y="Count") + # Rótulos de eixo
  geom_vline(xintercept = yt, color = "#F99D2A", size = 2) + # Linha vertical
  geom_label(aes(x=yt, y=7, label=paste(sep="\n","true share:", yt)), color = "#F99D2A") + # Rótulo de linha vertical
  geom_vline(aes(xintercept = mean(yd)), color="#00A1C0", size = 2) +
  geom_label(aes(x=mean(yd)+0.035, y=11, label=paste(sep = "\n","mean estimate:", format(round(mean, 2), nsmall = 2))), color="#00A1C0") +
  geom_vline(aes(xintercept = med), color="#106889", size = 2) +
  geom_label(aes(x=med-0.025, y=9, label=paste(sep = "\n","median estimate:", format(round(med, 2), nsmall = 2))), color="#106889")

  




