###################################################
# Contagem de área de pastagem por célula do grid
# Pietro Gragnolati Fernandes
# 15/03/2024
###################################################

# Limpando area de trabalho
rm(list=ls())

# Instalando pacotes 
library(pacman)
p_load(data.table, raster, dplyr)

# Definindo o diretorio e lendo os dados
setwd("D:/arquivos/doutorado_michael/rasters")

grid <- raster("grid.tif")
pastagens <- raster("pa_br_pastagem_lapig_col8_30m_2022.tif")
ufs <- raster("pa_br_uf_ibge_1-250000_2022.tif")

# Criando o database SQLite
path <- "D:/arquivos/doutorado_michael/tabelas/" #endereco local para salvar o arquivo .db
conn <- dbConnect(SQLite(), paste(path, 'contagem_pixel_area_pastagem.db', sep = ""))

# Contando pixel
bss <- blockSize(grid)

for (i in 1:bss$n) {
  db <- data.table(grid = getValues(grid, row = bss$row[i], nrows = bss$nrows[i]),
                   uf = getValues(ufs, row = bss$row[i], nrows = bss$nrows[i]),
                   past = getValues(pastagens, row = bss$row[i], nrows = bss$nrows[i]))
  
  db <- db %>% group_by(grid, uf, past)%>%summarise(area = n()*0.087)  
  dbWriteTable(conn, 'contagem_pixel_area_pastagem', db, row.names = F, append = T)
  print(i)
}
