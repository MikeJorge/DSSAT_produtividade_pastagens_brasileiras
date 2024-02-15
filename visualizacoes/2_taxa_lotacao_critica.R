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