# https://livro.curso-r.com/7-2-dplyr.html

# Pipe
# Assignment operator %<>%
# Tibbles

# select() - seleciona colunas
# arrange() - ordena a base
# filter() - filtra linhas
# mutate() - cria/modifica colunas
# group_by() - agrupa a base
# summarise() - sumariza a base

install.packages("dplyr")
library(dplyr)

imdb <- readr::read_rds("dplyr/imdb.rds")
imdb

# Select ----
select(imdb, titulo)

select(imdb, titulo, ano, orcamento)

select(imdb, titulo:generos)

select(imdb, starts_with("num"))

select(imdb, -ano, -direcao)

select(imdb, -starts_with("num"))

## Exercícios Select ----
glimpse(imdb)

imdb_simples <- select(imdb, titulo, direcao, orcamento)
imdb_simples

select(imdb, contains("cao"))

select(imdb, -starts_with("num"))


# Arrange ----
arrange(imdb, orcamento)

arrange(imdb, desc(orcamento))

arrange(imdb, desc(ano), desc(orcamento))

## Exercícios arrange ----
filmes_ordenados <- arrange(imdb, ano, desc(receita))
filmes_ordenados

arrange(select(imdb, titulo, orcamento), desc(orcamento))

# Pipe ----
arrange(select(imdb, titulo, ano), ano)

tab_titulo_ano <- select(imdb, titulo, ano)
arrange(tab_titulo_ano, ano)

imdb %>% select(titulo, ano) %>% arrange(ano)

# Filter ----
imdb %>% filter(nota_imdb > 9)

imdb %>% 
  filter(nota_imdb > 9) %>% 
  select(titulo, nota_imdb)

imdb %>% filter(ano > 2010, nota_imdb > 8.5)

imdb %>% filter(receita - orcamento > 0)

imdb %>%
  filter(direcao %in% c("Quentin Tarantino", "Steven Spielberg"))

library(stringr)
str_detect(
  string = c("a", "aa","abc", "bc", "A", NA), 
  pattern = "a"
)

# A coluna gêneros apresenta todos os gêneros dos filmes concatenados
imdb$generos[1:6]

# Podemos detectar se o gênero Drama aparece na string
str_detect(
  string = imdb$generos[1:6],
  pattern = "Drama"
)

# Aplicamos essa lógica dentro da função filter, para a coluna completa
imdb %>% filter(str_detect(generos, "Drama"))

## Exercícios filter ----
filmes_ingles <- imdb %>% filter(idioma == "English")
filmes_ingles

curtos_legais <- imdb %>% filter(duracao <= 90, nota_imdb > 8.5)
curtos_legais

imdb %>% filter(str_detect(generos, "Action"), ano < 1950)

imdb %>% filter(direcao %in% c("Woody Allen", "Wes Anderson"))
imdb %>% filter(direcao %in% c("Woody Allen", "Wes Anderson")) %>% select(direcao)

imdb %>% filter(str_detect(generos, "Action") | str_detect(generos, "Comedy"))
imdb %>% filter(str_detect(generos, "Action") & str_detect(generos, "Comedy"))
imdb %>% filter(str_detect(generos, "Action") & str_detect(generos, "Comedy"))  %>% select(generos)
imdb %>% filter(is.na(receita) & is.na(orcamento))

# Mutate ----
imdb %>% mutate(duracao = duracao/60)
imdb %>% select(duracao)
imdb %>% mutate(duracao = duracao/60)  %>% select(duracao)
imdb %>% mutate(duracao_horas = duracao/60)
imdb %>% mutate(duracao_horas = duracao/60) %>% select(duracao, duracao_horas)

imdb %>% 
  mutate(lucro = receita - orcamento, pais = "Estados Unidos") %>% 
  select(titulo, lucro, pais)

## Exercícios mutate ----
imdb_prejuizo <- imdb %>% mutate(prejuizo = orcamento - receita)
imdb_prejuizo %>% filter(prejuizo > 0) %>% arrange(prejuizo) %>% select(titulo, orcamento, receita, prejuizo)

imdb %>% mutate(lucro = receita - orcamento,
                lucro_medio = mean(lucro, na.rm = TRUE),
                lucro_relativo = (lucro - lucro_medio) / lucro_medio,
                houve_lucro = ifelse(lucro > 0, "sim", "não")) %>% 
  select(titulo, receita, orcamento, lucro, lucro_medio, lucro_relativo, houve_lucro) %>% 
  arrange(desc(lucro_relativo))

imdb %>% mutate(epoca = ifelse(ano > 2000, "recente", "antigo")) %>% select(titulo, ano, epoca)

# Summarise ----
imdb %>% summarize(media_orcamento = mean(orcamento, na.rm = TRUE))

imdb %>% summarise(
  media_orcamento = mean(orcamento, na.rm = TRUE),
  mediana_orcamento = median(orcamento, na.rm = TRUE),
  variancia_orcamento = var(orcamento, na.rm = TRUE)
)

imdb %>% summarize(
  media_orcamento = mean(orcamento, na.rm = TRUE),
  media_receita = mean(receita, na.rm = TRUE),
  media_lucro = mean(receita - orcamento, na.rm = TRUE)
)

imdb %>% 
  filter(!is.na(producao), !is.na(receita))  %>% 
  group_by(producao) %>% 
  summarise(receita_media = mean(receita, na.rm = TRUE)) 

imdb %>% group_by(producao)

# Exercícios summarise ----
imdb %>% summarise(
  duracao_media = mean(duracao, na.rm = TRUE),
  duracao_mediana = median(duracao, na.rm = TRUE))

imdb %>% filter(duracao > 120) %>% mutate(lucro = receita - orcamento) %>% summarise(lucro_medio = mean(lucro, na.rm = TRUE))

imdb %>% mutate(duas_horas = ifelse(duracao < 120, "Menor que 2 horas", "Maior que 2 horas")) %>% 
                  group_by(duas_horas) %>% 
                  summarise(lucro_medio = mean(receita - orcamento, na.rm = TRUE))

imdb %>% group_by(ano) %>% summarise(nota_media = mean(nota_imdb, na.rm = TRUE))

imdb %>% group_by(ano) %>% summarise(receita_media = mean(receita, na.rm = TRUE),
                                     receita_mediana = median(receita, na.rm = TRUE)) %>% print(n = 110)


imdb %>% group_by(direcao) %>% count() %>% ungroup() %>% filter(n > 10) %>% arrange(n)
