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
grid <- st_read("D:/arquivos/GPP/pastagens_IABS/analises_finais/BR_grid.shp")
pontosfiltro <- fread("D:/arquivos/GPP/pastagens_IABS/analises_finais/pontos_filtro_N400.csv")

out_mensal_1 <- dbGetQuery(conn,'select * from mensal_1')
out_mensal_2 <- dbGetQuery(conn,'select * from mensal_2')
out_mensal_3 <- dbGetQuery(conn,'select * from mensal_3')
out_mensal_pot <- dbGetQuery(conn,'select * from mensal_pot')
out_mensal_ext <- dbGetQuery(conn,'select * from mensal_ext')


# Agrupando por mês e por ano
head(out_mensal_1)
mensal_1 <- out_mensal_1 %>% group_by(month, PONTO_SIMULACAO)%>%summarise(biomassa_mensal_1 = mean(`SUM(GANHO_DIARIO_CWAD)`, na.rm = T))
mensal_2<- out_mensal_2 %>% group_by(month, PONTO_SIMULACAO)%>%summarise(biomassa_mensal_2 = mean(`SUM(GANHO_DIARIO_CWAD)`, na.rm = T))
