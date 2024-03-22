############################################################################
# Área de pastagem e região administrativa predominante por célula do grid
# Pietro Gragnolati Fernandes
# 15/03/2024
############################################################################

# Limpando area de trabalho
rm(list=ls())

# Instalando pacotes 
library(pacman)
p_load(data.table, raster, dplyr, RSQLite)

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

# Sumarizando os resultados
base <- dbGetQuery(conn, "SELECT * FROM contagem_pixel_area_pastagem")

uf_pred <- base %>% group_by(grid, uf)%>%
                 arrange(desc(area))%>%
                 mutate(uf_pred = first(uf))

uf_pred <- uf_pred%>%group_by(grid)%>%mutate(uf_pred = first(uf_pred))

base <- base %>% group_by(grid)%>%
  summarise(area_past_2022 = sum(area[past==1]))%>%
  ungroup()

base <- merge(base, uf_pred, by = "grid")
base$uf <- base$uf_pred

# Definindo a região admninistrativa do grid com base na uf predominante
regioes <- fread(paste(path, "regioes_administrativas.csv"))
base <- merge(base, regioes, by = "uf")

base <- base[,c("grid", "uf", "nm_regiao", "area_past_2022")]
colnames(base)[1] <- "ponto_simulacao"
# Exportando a tabela
write.table(base, paste(path, "area_pastagem_e_regiao_predominante_grid.csv"),
            row.names = F, sep = ";")