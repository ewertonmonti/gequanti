empregos <- read.csv("desvantagem_salarial_hotelaria/minha_reproducao/tabela6450.csv", skip=4, sep = ";")
empregos <- empregos[1:5570,]

head(empregos)

# Esses comandos substituem os valores de QUALQUER CÉLULA que tenha - ou X por NA
empregos[empregos=="-"] <- NA
empregos[empregos=="X"] <- NA

# Type convert adivinha os tipos e ajustas
empregos <- type.convert(empregos)