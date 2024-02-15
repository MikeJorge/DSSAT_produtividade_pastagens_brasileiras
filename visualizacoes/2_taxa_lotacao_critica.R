########################################
# Calculo da taxa de lotacao critica
# Pietro Gragnolati Fernandes
# 15/02/2024
########################################

# Limpando a area de trabalho
rm(list = ls())

# Instalando os pacotes
library(pacman)
p_load(data.table, lubridate, ggplot2, RSQLite, tidyr, dplyr,rgdal, sf, raster, sp, tidyr)

# Fazendo a conexao com o banco
path <- "D:/arquivos/doutorado_michael/DSSAT-BPPP/outputs/outptus_michael.db" #endereco do banco de dados
conn <- dbConnect(SQLite(), path)
dbListTables(conn)

# Verificando se houveram pontos onde o pasto morreu antes do fim da simulacao
out <- dbGetQuery(conn,'select * from mensal_ext')

last.year.per.point <- out %>%
  group_by(PONTO_SIMULACAO) %>% 
  summarise(ano = max(year))

problems <- subset(last.year.per.point, ano < 2016) #nao houve pontos de morte, nem no cenario mais restritivo (extensivo)

print(paste("Em", nrow(problems), "pontos de simulação a pastagem morreu antes de 2016", sep = " "))


