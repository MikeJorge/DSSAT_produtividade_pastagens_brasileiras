############################################################################
# Gráficos e mapas dos resultados do DSSAT para os 5 cenários
# Pietro Gragnolati Fernandes
# 15/02/2024
############################################################################

# Limpando a área de trabalho
rm(list = ls())

# Instalando os pacotes
library(pacman)
p_load(data.table, lubridate, ggplot2, RSQLite, tidyr, dplyr,rgdal, sf, raster, sp, tidyr, maptools, forcats, showtext)

# Fazendo a conexao com o banco
path <- "D:/arquivos/doutorado_michael/DSSAT-BPPP/outputs/outptus_michael.db" #endereco do banco de dados
conn <- dbConnect(SQLite(), path)
dbListTables(conn)

# Mapas com o grid de biomassa seca (mensal e anual)----
# Ingerindo os dados
path <- "D:/arquivos/doutorado_michael/" #endereco da pasta DSSAT-BPPP
grid <- st_read(paste(path, "DSSAT-BPPP/data/grid/BR_grid.shp", sep = ""))
pontos_bioma <- fread(paste(path, "tabelas/ponto_bioma.csv", sep = ""))

# Selecionando o bioma mais representativo de cada ponto
pontos_bioma <- pontos_bioma %>%
  group_by(ponto_simulacao) %>%
  filter(area_ha == max(area_ha))

# Separando os pontos nos biomas Amazônia, Mata Atlântica e Cerrado
pontos_filtro <- filter(pontos_bioma, bioma %in% c("Amazônia", "Mata Atlântica", "Cerrado"))

# Lendo as tabelas mensais do banco
out_mensal_1 <- dbGetQuery(conn,'select * from mensal_1')
out_mensal_2 <- dbGetQuery(conn,'select * from mensal_2')
out_mensal_3 <- dbGetQuery(conn,'select * from mensal_3')
out_mensal_pot <- dbGetQuery(conn,'select * from mensal_pot')
out_mensal_ext <- dbGetQuery(conn,'select * from mensal_ext')


# Agrupando por mês e por ano
head(out_mensal_1)
mensal_1 <- out_mensal_1 %>% group_by(month, PONTO_SIMULACAO)%>%summarise(biomassa_mensal_1 = mean(`SUM(GANHO_DIARIO_CWAD)`, na.rm = T))
mensal_2<- out_mensal_2 %>% group_by(month, PONTO_SIMULACAO)%>%summarise(biomassa_mensal_2 = mean(`SUM(GANHO_DIARIO_CWAD)`, na.rm = T))
