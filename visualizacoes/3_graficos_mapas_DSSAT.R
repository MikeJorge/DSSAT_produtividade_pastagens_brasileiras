############################################################################
# Gráficos e mapas dos resultados do DSSAT para os 5 cenários
# Pietro Gragnolati Fernandes
# 15/02/2024
############################################################################

# Limpando a área de trabalho
rm(list = ls())

# Instalando os pacotes
library(pacman)
p_load(data.table, lubridate, ggplot2, RSQLite, tidyr, dplyr,sf, raster, sp, tidyr, maptools, forcats, showtext)

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
mensal_1 <- dbGetQuery(conn,'select * from mensal_modelo_1')
mensal_2 <- dbGetQuery(conn,'select * from mensal_modelo_2')
mensal_3 <- dbGetQuery(conn,'select * from mensal_modelo_3')
mensal_pot <- dbGetQuery(conn,'select * from mensal_modelo_pot')
mensal_ext <- dbGetQuery(conn,'select * from mensal_modelo_ext')


# Agrupando por ano
head(mensal_1)
anual_1 <- mensal_1 %>% group_by(PONTO_SIMULACAO)%>%summarise(biomassa_anual_1 = sum(biomassa_mensal, na.rm = T))
anual_2 <- mensal_2 %>% group_by(PONTO_SIMULACAO)%>%summarise(biomassa_anual_2 = sum(biomassa_mensal, na.rm = T))
anual_3 <- mensal_1 %>% group_by(PONTO_SIMULACAO)%>%summarise(biomassa_anual_3 = sum(biomassa_mensal, na.rm = T))
anual_pot <- mensal_pot %>% group_by(PONTO_SIMULACAO)%>%summarise(biomassa_anual_pot = sum(biomassa_mensal, na.rm = T))
anual_ext <- mensal_ext %>% group_by(PONTO_SIMULACAO)%>%summarise(biomassa_anual_ext = sum(biomassa_mensal, na.rm = T))

# Alterando o nome da coluna ponto_simulacao
colnames(anual_1)[1] <- "value"
colnames(anual_2)[1] <- "value"
colnames(anual_3)[1] <- "value"
colnames(anual_pot)[1] <- "value"
colnames(anual_ext)[1] <- "value"
colnames(mensal_1)[2] <- "value"
colnames(mensal_2)[2] <- "value"
colnames(mensal_3)[2] <- "value"
colnames(mensal_pot)[2] <- "value"
colnames(mensal_ext)[2] <- "value"


# Convertendo os resultados mensais para o formato wide
mensal_1 <- drop_na(mensal_1)
mensal_1 <- pivot_wider(mensal_1, names_from = month, values_from = biomassa_mensal)
names <- c("value","bio_1_01", "bio_1_02", "bio_1_03", "bio_1_04", "bio_1_05", 
           "bio_1_06", "bio_1_07", "bio_1_08", "bio_1_09", "bio_1_10", 
           "bio_1_11", "bio_1_12")

colnames(mensal_1) <- names

mensal_2 <- drop_na(mensal_2)
mensal_2 <- pivot_wider(mensal_2, names_from = month, values_from = biomassa_mensal)
names <- c("value","bio_2_01", "bio_2_02", "bio_2_03", "bio_2_04", "bio_2_05", 
           "bio_2_06", "bio_2_07", "bio_2_08", "bio_2_09", "bio_2_10", 
           "bio_2_11", "bio_2_12")

colnames(mensal_2) <- names

mensal_3 <- drop_na(mensal_3)
mensal_3 <- pivot_wider(mensal_3, names_from = month, values_from = biomassa_mensal)
names <- c("value","bio_3_01", "bio_3_02", "bio_3_03", "bio_3_04", "bio_3_05", 
           "bio_3_06", "bio_3_07", "bio_3_08", "bio_3_09", "bio_3_10", 
           "bio_3_11", "bio_3_12")

colnames(mensal_3) <- names

mensal_pot <- drop_na(mensal_pot)
mensal_pot <- pivot_wider(mensal_pot, names_from = month, values_from = biomassa_mensal)
names <- c("value","bio_pot_01", "bio_pot_02", "bio_pot_03", "bio_pot_04", "bio_pot_05", 
           "bio_pot_06", "bio_pot_07", "bio_pot_08", "bio_pot_09", "bio_pot_10", 
           "bio_pot_11", "bio_pot_12")

colnames(mensal_pot) <- names

mensal_ext <- drop_na(mensal_ext)
mensal_ext <- pivot_wider(mensal_ext, names_from = month, values_from = biomassa_mensal)
names <- c("value","bio_ext_01", "bio_ext_02", "bio_ext_03", "bio_ext_04", "bio_ext_05", 
           "bio_ext_06", "bio_ext_07", "bio_ext_08", "bio_ext_09", "bio_ext_10", 
           "bio_ext_11", "bio_ext_12")

colnames(mensal_ext) <- names


# Juntando os resultados na tabela de atributos do shape
grid <- merge(grid, anual_1, by = "value", all.x=T)
grid  <- merge(grid, anual_2, by = "value", all.x=T)
grid <- merge(grid, anual_3, by = "value", all.x=T)
grid  <- merge(grid, anual_pot, by = "value", all.x=T)
grid  <- merge(grid, anual_ext, by = "value", all.x=T)
grid  <- merge(grid, mensal_1, by = "value", all.x=T)
grid  <- merge(grid, mensal_2, by = "value", all.x=T)
grid  <- merge(grid, mensal_3, by = "value", all.x=T)
grid  <- merge(grid, mensal_pot, by = "value", all.x=T)
grid  <- merge(grid, mensal_ext, by = "value", all.x=T)

# Filtrando os pontos de interesse
pontosfiltro<- unique(pontos_filtro$ponto_simulacao)
grid <- filter(grid, value %in% pontosfiltro)

# Exportando resultado
st_write(grid, paste(path, "shapefiles/BR_grid_resultados.shp", sep = ""), append = FALSE)
