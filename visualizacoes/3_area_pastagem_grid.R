###################################################################################################################################################
# Área de pastagem e região administrativa predominante por célula do grid
# 15/03/2024
# O script realiza a contagem de pixels e cálculo da área de pastagem utilizando rasters. Ele cria um banco de dados SQLite para armazenar 
# os resultados intermediários, onde os dados são agrupados e sumarizados para determinar a área de pastagem e a região administrativa 
# predominante para cada ponto de simulação (grid). A tabela final é exportada para um arquivo CSV (area_pastagem_e_regiao_predominante_grid.csv), contendo informações sobre a área de 
# pastagem e a região administrativa associada a cada ponto de simulação.
###################################################################################################################################################

# Limpando area de trabalho
rm(list=ls())

# Instalando pacotes 
library(pacman)
p_load(data.table, raster, dplyr, RSQLite)

# Definindo o diretorio e lendo os dados
path <- "D:/arquivos/doutorado_michael/produtividade_pastagens_brasileiras"  #endereco da pasta do projeto

grid <- raster(paste(path, "rasters/grid.tif", sep = "/"))
pastagens <- raster(paste(path, "rasters/pa_br_pastagem_lapig_col8_30m_2022.tif", sep = "/"))
ufs <- raster(paste(path, "rasters/pa_br_uf_ibge_1-250000_2022.tif", sep = "/"))

# Criando o database SQLite
conn <- dbConnect(SQLite(), paste(path, 'tabelas/contagem_pixel_area_pastagem.db', sep = "/"))

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

base <- subset(base, grid > 0)

uf_pred <- base %>% group_by(grid, uf)%>%
                 summarise(area = sum(area, na.rm = T))%>%
                 arrange(desc(area))%>%
                 subset(uf > 0)%>%
                 mutate(uf_pr = first(uf))

uf_pred <- uf_pred%>%group_by(grid)%>%summarise(uf_pr = mean(uf_pr, na.rm = T))

base <- base %>% group_by(grid)%>%
  summarise(area_past_2022 = sum(area[past==1], na.rm = T))

base <- merge(base, uf_pred, by = "grid")
base$uf <- base$uf_pr

# Definindo a região admninistrativa do grid com base na uf predominante
regioes <- fread(paste(path, "regioes_administrativas.csv", sep = "/"))
base <- merge(base, regioes, by = "uf")

base <- base[,c("grid", "uf", "nm_regiao", "area_past_2022")]
colnames(base)[1] <- "ponto_simulacao"

# Exportando a tabela
write.table(base, paste(path, "tabelas/area_pastagem_e_regiao_predominante_grid.csv", sep="/"),
            row.names = F, sep = ";")
