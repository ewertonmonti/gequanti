

library(haven)
library(psych)
library(magrittr)
bd <- read_sav("https://doi.org/10.1371/journal.pone.0236412.s004") 

bd  %<>% na.omit()

bd1 <- bd[,1:10]

# Teste de esfericidade dos dados
cortest.bartlett(bd1)

# Conjunto de dados adequado pra rodar uma análise fatorial
KMO(bd1)

# Análise paralela usada pra calcular o número de fatores
fa.parallel(bd1, main = "Gráfico Scree", fa = "fa", fm = "ml")


fit <- fa(bd1, nfactors = 3, fm = "ml", rotate = "varimax")

# factor loadings
# h2 (communalities)
# u2 (the uniquenesses, variancia residual)
print(fit)
print(fit, sort = TRUE, cut = 0.5)

psych::alpha(bd1)

fa.diagram(fit)

bd2 <- bd[,11:ncol(bd)]

# Teste de esfericidade dos dados
cortest.bartlett(bd2)

# Conjunto de dados adequado pra rodar uma análise fatorial
KMO(bd2)

# Análise paralela usada pra calcular o número de fatores
fa.parallel(bd2, main = "Gráfico Scree", fa = "fa", fm = "ml")

fit2 <- fa(bd2, nfactors = 2, fm = "ml", rotate = "varimax")

# factor loadings
# h2 (communalities)
# u2 (the uniquenesses, variancia residual)
print(fit2)
print(fit2, sort = TRUE, cut = 0.5)

psych::alpha(bd2)

fa.diagram(fit2)
