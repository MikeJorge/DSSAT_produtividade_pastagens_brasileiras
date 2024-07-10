############################################################################
# Gráficos e mapas dos resultados do DSSAT para os 5 cenários
# 15/02/2024
############################################################################

# Limpando a área de trabalho
rm(list = ls())

# Instalando os pacotes
library(pacman)
p_load(data.table, lubridate, ggplot2, RSQLite, tidyr, dplyr,sf, raster, sp, tidyr, forcats, showtext)

# Fazendo a conexao com o banco
path <- "D:/arquivos/doutorado_michael/produtividade_pastagens_brasileiras"  #endereco da pasta do projeto
conn <- dbConnect(SQLite(), paste(path, "DSSAT_produtividade_pastagens_brasileiras/DSSAT/outputs/outptus_michael.db"))
dbListTables(conn)

# Mapas com o grid de biomassa seca (mensal e anual)----
# Ingerindo os dados
grid <- st_read(paste(path, "DSSAT_produtividade_pastagens_brasileiras/DSSAT/data/grid/BR_grid.shp", sep = "/"))
pontos_bioma <- fread(paste(path, "tabelas/ponto_bioma.csv", sep = "/"))
pontos_regiao <- fread(paste(path, "tabelas/area_pastagem_e_regiao_predominante_grid.csv", sep = "/"))

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
anual_3 <- mensal_3 %>% group_by(PONTO_SIMULACAO)%>%summarise(biomassa_anual_3 = sum(biomassa_mensal, na.rm = T))
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
w_mensal_1 <- pivot_wider(mensal_1, names_from = month, values_from = biomassa_mensal)
names <- c("value","bio_1_01", "bio_1_02", "bio_1_03", "bio_1_04", "bio_1_05", 
           "bio_1_06", "bio_1_07", "bio_1_08", "bio_1_09", "bio_1_10", 
           "bio_1_11", "bio_1_12")

colnames(w_mensal_1) <- names

mensal_2 <- drop_na(mensal_2)
w_mensal_2 <- pivot_wider(mensal_2, names_from = month, values_from = biomassa_mensal)
names <- c("value","bio_2_01", "bio_2_02", "bio_2_03", "bio_2_04", "bio_2_05", 
           "bio_2_06", "bio_2_07", "bio_2_08", "bio_2_09", "bio_2_10", 
           "bio_2_11", "bio_2_12")

colnames(w_mensal_2) <- names

mensal_3 <- drop_na(mensal_3)
w_mensal_3 <- pivot_wider(mensal_3, names_from = month, values_from = biomassa_mensal)
names <- c("value","bio_3_01", "bio_3_02", "bio_3_03", "bio_3_04", "bio_3_05", 
           "bio_3_06", "bio_3_07", "bio_3_08", "bio_3_09", "bio_3_10", 
           "bio_3_11", "bio_3_12")

colnames(w_mensal_3) <- names

mensal_pot <- drop_na(mensal_pot)
w_mensal_pot <- pivot_wider(mensal_pot, names_from = month, values_from = biomassa_mensal)
names <- c("value","bio_pot_01", "bio_pot_02", "bio_pot_03", "bio_pot_04", "bio_pot_05", 
           "bio_pot_06", "bio_pot_07", "bio_pot_08", "bio_pot_09", "bio_pot_10", 
           "bio_pot_11", "bio_pot_12")

colnames(w_mensal_pot) <- names

mensal_ext <- drop_na(mensal_ext)
w_mensal_ext <- pivot_wider(mensal_ext, names_from = month, values_from = biomassa_mensal)
names <- c("value","bio_ext_01", "bio_ext_02", "bio_ext_03", "bio_ext_04", "bio_ext_05", 
           "bio_ext_06", "bio_ext_07", "bio_ext_08", "bio_ext_09", "bio_ext_10", 
           "bio_ext_11", "bio_ext_12")

colnames(w_mensal_ext) <- names

# Lendo e organizando a taxa de lotacao critica
tl_critica <- fread(paste(path, "tabelas/taxa_lotacao_critica_cenarios_epp_variavel.csv", sep = "/"))
colnames(tl_critica)[1] <- "value"

# Juntando os resultados na tabela de atributos do shape
grid <- merge(grid, anual_1, by = "value", all.x=T)
grid  <- merge(grid, anual_2, by = "value", all.x=T)
grid <- merge(grid, anual_3, by = "value", all.x=T)
grid  <- merge(grid, anual_pot, by = "value", all.x=T)
grid  <- merge(grid, anual_ext, by = "value", all.x=T)
grid  <- merge(grid, w_mensal_1, by = "value", all.x=T)
grid  <- merge(grid, w_mensal_2, by = "value", all.x=T)
grid  <- merge(grid, w_mensal_3, by = "value", all.x=T)
grid  <- merge(grid, w_mensal_pot, by = "value", all.x=T)
grid  <- merge(grid, w_mensal_ext, by = "value", all.x=T)
grid <- merge(grid, tl_critica, by = "value", all.x = T)

# Filtrando os pontos de interesse
pontosfiltro<- unique(pontos_filtro$ponto_simulacao)
grid <- filter(grid, value %in% pontosfiltro)

# Exportando resultado
st_write(grid, paste(path, "shapefiles/BR_grid_resultados.shp", sep = "/"), append = FALSE)


# Gráficos ----
# Médias mensais de acumulo diario de biomassa seca por bioma
# Ingerindo os dados
diario_1 <- dbGetQuery(conn,'select * from diario_1')
diario_2 <- dbGetQuery(conn,'select * from diario_2')
diario_3 <- dbGetQuery(conn,'select * from diario_3')
diario_pot <- dbGetQuery(conn,'select * from diario_pot')
diario_ext <- dbGetQuery(conn,'select * from diario_ext')

# Filtrando pontos de interesse
diario_1 <- filter(diario_1, PONTO_SIMULACAO %in% pontosfiltro)
diario_2 <- filter(diario_2, PONTO_SIMULACAO %in% pontosfiltro)
diario_3 <- filter(diario_3, PONTO_SIMULACAO %in% pontosfiltro)
diario_pot <- filter(diario_pot, PONTO_SIMULACAO %in% pontosfiltro)
diario_ext <- filter(diario_ext, PONTO_SIMULACAO %in% pontosfiltro)

# Corrigindo nome da coluna PONTO_SIMULACAO
colnames(diario_1)[3] <- "ponto_simulacao"
colnames(diario_2)[3] <- "ponto_simulacao"
colnames(diario_3)[3] <- "ponto_simulacao"
colnames(diario_pot)[3] <- "ponto_simulacao"
colnames(diario_ext)[3] <- "ponto_simulacao"

# Unindo a informacao de bioma e nm_regiao
diario_1 <- merge(diario_1, pontos_bioma[,c(1:2)], by = "ponto_simulacao", all.X =T)
diario_2 <- merge(diario_2, pontos_bioma[,c(1:2)], by = "ponto_simulacao", all.X =T)
diario_3 <- merge(diario_3, pontos_bioma[,c(1:2)], by = "ponto_simulacao", all.X =T)
diario_pot <- merge(diario_pot, pontos_bioma[,c(1:2)], by = "ponto_simulacao", all.X =T)
diario_ext <- merge(diario_ext, pontos_bioma[,c(1:2)], by = "ponto_simulacao", all.X =T)

diario_1 <- merge(diario_1, pontos_regiao[,c(1,3)], by = "ponto_simulacao", all.X =T)
diario_2 <- merge(diario_2, pontos_regiao[,c(1,3)], by = "ponto_simulacao", all.X =T)
diario_3 <- merge(diario_3, pontos_regiao[,c(1,3)], by = "ponto_simulacao", all.X =T)
diario_pot <- merge(diario_pot, pontos_regiao[,c(1,3)], by = "ponto_simulacao", all.X =T)
diario_ext <- merge(diario_ext, pontos_regiao[,c(1,3)], by = "ponto_simulacao", all.X =T)

# Definindo os cenarios e unindo as bases
diario_1$cenario <- "Baixa produtividade"
diario_2$cenario <- "Média produtividade"
diario_3$cenario <- "Alta produtividade"
diario_pot$cenario <- "Produtividade potencial"
diario_ext$cenario <- "Bovinocultura extensiva"

diario <- rbind(diario_1, diario_2, diario_3, diario_ext, diario_pot)
diario <- drop_na(diario)

# Agrupando por mes e bioma
b_mensal_diario <- diario %>% group_by(month, bioma, cenario)%>%summarise(ganho_diario_med = mean(biomassa_diaria, na.rm = T),
                                                                       ganho_diario_sd = sd(biomassa_diaria, na.rm = T))

# Agrupando por mes e nm_regiao
r_mensal_diario <- diario %>% group_by(month, nm_regiao, cenario)%>%summarise(ganho_diario_med = mean(biomassa_diaria, na.rm = T),
                                                                     ganho_diario_sd = sd(biomassa_diaria, na.rm = T))


# Arrumando os labels mensais
b_mensal_diario$mes <- rep(c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez"),each = 15, times= 1)
b_mensal_diario$month <- as.integer(b_mensal_diario$month)

r_mensal_diario$mes <- rep(c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez"),each = 25, times= 1)
r_mensal_diario$month <- as.integer(r_mensal_diario$month)

# Grafico de taxa diaria bioma
png(paste(path, "graficos/taxa_acumulo_diario_bioma.png", sep = "/"), width = 3500, height = 2200, res = 300)
p1 <- ggplot(b_mensal_diario, aes(x = fct_rev(reorder(mes, -month)), y = ganho_diario_med, color = cenario))+
  geom_line(aes(group = cenario))+
  geom_point()+
  #geom_errorbar(aes(ymin = mensal_diario$ganho_diario_med - mensal_diario$ganho_diario_sd, ymax = ganho_diario_med + ganho_diario_sd))+
  facet_wrap(~bioma, ncol = 2)+
  labs(x = "Mês",
       y = "Taxa de acúmulo diário de pastagem (kg MS/ha/dia)",
       color = "Cenário")+
  theme_bw(base_family = "Times New Roman")+
  theme(text = element_text(size = 12)); p1
dev.off()

# Grafico de taxa diaria nm_regiao
png(paste(path, "graficos/taxa_acumulo_diario_nm_regiao.png", sep = "/"), width = 3500, height = 2200, res = 300)
p2 <- ggplot(r_mensal_diario, aes(x = fct_rev(reorder(mes, -month)), y = ganho_diario_med, color = cenario))+
  geom_line(aes(group = cenario))+
  geom_point()+
  #geom_errorbar(aes(ymin = mensal_diario$ganho_diario_med - mensal_diario$ganho_diario_sd, ymax = ganho_diario_med + ganho_diario_sd))+
  facet_wrap(~nm_regiao, ncol = 2)+
  labs(x = "Mês",
       y = "Taxa de acúmulo diário de pastagem (kg MS/ha/dia)",
       color = "Cenário")+
  theme_bw(base_family = "Times New Roman")+
  theme(text = element_text(size = 12)); p2
dev.off()


# Taxas mensais de produção de biomassa seca por bioma
# Unindo a informacao de bioma e nm_regiao nas bases
colnames(pontos_bioma)[1] <- 'value'
colnames(pontos_regiao)[1] <- 'value'
mensal_1 <- merge(mensal_1, pontos_bioma[,1:2], by = "value", all.x = T)
mensal_2 <- merge(mensal_2, pontos_bioma[,1:2], by = "value", all.x = T)
mensal_3 <- merge(mensal_3, pontos_bioma[,1:2], by = "value", all.x = T)
mensal_pot <- merge(mensal_pot, pontos_bioma[,1:2], by = "value", all.x = T)
mensal_ext <- merge(mensal_ext, pontos_bioma[,1:2], by = "value", all.x = T)

mensal_1 <- merge(mensal_1, pontos_regiao[,c(1,3)], by = "value", all.x = T)
mensal_2 <- merge(mensal_2, pontos_regiao[,c(1,3)], by = "value", all.x = T)
mensal_3 <- merge(mensal_3, pontos_regiao[,c(1,3)], by = "value", all.x = T)
mensal_pot <- merge(mensal_pot, pontos_regiao[,c(1,3)], by = "value", all.x = T)
mensal_ext <- merge(mensal_ext, pontos_regiao[,c(1,3)], by = "value", all.x = T)

# Filtrando os biomas de interesse
mensal_1 <- filter(mensal_1, bioma %in% c("Amazônia", "Cerrado", "Mata Atlântica"))
mensal_2 <- filter(mensal_2, bioma %in% c("Amazônia", "Cerrado", "Mata Atlântica"))
mensal_3 <- filter(mensal_3, bioma %in% c("Amazônia", "Cerrado", "Mata Atlântica"))
mensal_pot <- filter(mensal_pot, bioma %in% c("Amazônia", "Cerrado", "Mata Atlântica"))
mensal_ext <- filter(mensal_ext, bioma %in% c("Amazônia", "Cerrado", "Mata Atlântica"))

# Definindo os cenarios e unindo as bases
mensal_1$cenario <- "Baixa produtividade"
mensal_2$cenario <- "Média produtividade"
mensal_3$cenario <- "Alta produtividade"
mensal_pot$cenario <- "Produtividade potencial"
mensal_ext$cenario <- "Bovinocultura extensiva"

mensal <- rbind(mensal_1, mensal_2, mensal_3, mensal_ext, mensal_pot)

# Agrupando por mes e por bioma
b_mensal <- mensal %>% group_by(month, bioma, cenario)%>%summarise(biom_med = mean(biomassa_mensal, na.rm = T),
                                                                biom_sd = sd(biomassa_mensal, na.rm = T))

# Agrupando por mes e por nm_regiao
r_mensal <- mensal %>% group_by(month, nm_regiao, cenario)%>%summarise(biom_med = mean(biomassa_mensal, na.rm = T),
                                                              biom_sd = sd(biomassa_mensal, na.rm = T))


# Arrumando os labels mensais
b_mensal$mes <- rep(c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez"),each = 15, times= 1)
b_mensal$month <- as.integer(b_mensal$month)

r_mensal$mes <- rep(c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez"),each = 25, times= 1)
r_mensal$month <- as.integer(r_mensal$month)

# Grafico de taxa mensal por bioma
png(paste(path, "graficos/taxa_acumulo_mensal_bioma.png", sep = "/"), width = 3500, height = 2200, res = 300)
p3 <- ggplot(b_mensal, aes(x = fct_rev(reorder(mes, -month)), y = biom_med, color = cenario))+
  geom_line(aes(group = cenario))+
  geom_point()+
  #geom_errorbar(aes(ymin = mensal_diario$ganho_diario_med - mensal_diario$ganho_diario_sd, ymax = ganho_diario_med + ganho_diario_sd))+
  facet_wrap(~bioma, ncol = 2)+
  labs(x = "Mês",
       y = "Taxa de acúmulo mensal de pastagem (kg MS/ha/mês)",
       color = "Cenário")+
  theme_bw(base_family = "Times New Roman")+
  theme(text = element_text(size = 12)); p3
dev.off()


# Grafico de taxa mensal por nm_regiao
png(paste(path, "graficos/taxa_acumulo_mensal_nm_regiao.png", sep = "/"), width = 3500, height = 2200, res = 300)
p4 <- ggplot(r_mensal, aes(x = fct_rev(reorder(mes, -month)), y = biom_med, color = cenario))+
  geom_line(aes(group = cenario))+
  geom_point()+
  #geom_errorbar(aes(ymin = mensal_diario$ganho_diario_med - mensal_diario$ganho_diario_sd, ymax = ganho_diario_med + ganho_diario_sd))+
  facet_wrap(~nm_regiao, ncol = 2)+
  labs(x = "Mês",
       y = "Taxa de acúmulo mensal de pastagem (kg MS/ha/mês)",
       color = "Cenário")+
  theme_bw(base_family = "Times New Roman")+
  theme(text = element_text(size = 12)); p4
dev.off()


# Producao de biomassa anual ao longo da serie
# Lendo as series anuais do banco
anual_serie_1 <- dbGetQuery(conn,'select PONTO_SIMULACAO value, year, SUM(biomassa_mensal) biomassa_anual FROM mensal_1 GROUP BY PONTO_SIMULACAO, year')
anual_serie_2 <- dbGetQuery(conn,'select PONTO_SIMULACAO value, year, SUM(biomassa_mensal) biomassa_anual FROM mensal_2 GROUP BY PONTO_SIMULACAO, year')
anual_serie_3 <- dbGetQuery(conn,'select PONTO_SIMULACAO value, year, SUM(biomassa_mensal) biomassa_anual FROM mensal_3 GROUP BY PONTO_SIMULACAO, year')
anual_serie_pot <- dbGetQuery(conn,'select PONTO_SIMULACAO value, year, SUM(biomassa_mensal) biomassa_anual FROM mensal_pot GROUP BY PONTO_SIMULACAO, year')
anual_serie_ext <- dbGetQuery(conn,'select PONTO_SIMULACAO value, year, SUM(biomassa_mensal) biomassa_anual FROM mensal_ext GROUP BY PONTO_SIMULACAO, year')

# Unindo o bioma
anual_serie_1 <- merge(anual_serie_1, pontos_bioma[,1:2], by = "value", all.x = T)
anual_serie_2 <- merge(anual_serie_2, pontos_bioma[,1:2], by = "value", all.x = T)
anual_serie_3 <- merge(anual_serie_3, pontos_bioma[,1:2], by = "value", all.x = T)
anual_serie_pot <- merge(anual_serie_pot, pontos_bioma[,1:2], by = "value", all.x = T)
anual_serie_ext <- merge(anual_serie_ext, pontos_bioma[,1:2], by = "value", all.x = T)

# Unindo a nm_regiao bioma
anual_serie_1 <- merge(anual_serie_1, pontos_regiao[,c(1,3)], by = "value", all.x = T)
anual_serie_2 <- merge(anual_serie_2, pontos_regiao[,c(1,3)], by = "value", all.x = T)
anual_serie_3 <- merge(anual_serie_3, pontos_regiao[,c(1,3)], by = "value", all.x = T)
anual_serie_pot <- merge(anual_serie_pot, pontos_regiao[,c(1,3)], by = "value", all.x = T)
anual_serie_ext <- merge(anual_serie_ext, pontos_regiao[,c(1,3)], by = "value", all.x = T)

# Filtrando os biomas de interesse
anual_serie_1 <- filter(anual_serie_1, bioma %in% c("Amazônia", "Cerrado", "Mata Atlântica"))
anual_serie_2 <- filter(anual_serie_2, bioma %in% c("Amazônia", "Cerrado", "Mata Atlântica"))
anual_serie_3 <- filter(anual_serie_3, bioma %in% c("Amazônia", "Cerrado", "Mata Atlântica"))
anual_serie_pot <- filter(anual_serie_pot, bioma %in% c("Amazônia", "Cerrado", "Mata Atlântica"))
anual_serie_ext <- filter(anual_serie_ext, bioma %in% c("Amazônia", "Cerrado", "Mata Atlântica"))

# Definindo os cenarios e unindo as bases
anual_serie_1$cenario <- "Baixa produtividade"
anual_serie_2$cenario <- "Média produtividade"
anual_serie_3$cenario <- "Alta produtividade"
anual_serie_pot$cenario <- "Produtividade potencial"
anual_serie_ext$cenario <- "Bovinocultura extensiva"

anual_serie <- rbind(anual_serie_1, anual_serie_2, anual_serie_3, anual_serie_ext, anual_serie_pot)
anual_serie$year <- as.integer(anual_serie$year)

# Agrupando por bioma e ano
anual_serie <- subset(anual_serie, year > 1980)
b_anual_serie <- anual_serie %>% group_by(year, bioma, cenario) %>% summarise(biomassa_anual = mean(biomassa_anual, na.rm = T))
r_anual_serie <- anual_serie %>% group_by(year, nm_regiao, cenario) %>% summarise(biomassa_anual = mean(biomassa_anual, na.rm = T))

# Grafico de taxa mensal por bioma
png(paste(path, "graficos/serie_acumulo_anual_bioma.png", sep = "/"), width = 3500, height = 2200, res = 300)
p5 <- ggplot(b_anual_serie, aes(x = fct_rev(reorder(year, -year)), y = biomassa_anual, color = cenario))+
  geom_line(aes(group = cenario))+
  geom_point()+
  #geom_errorbar(aes(ymin = mensal_diario$ganho_diario_med - mensal_diario$ganho_diario_sd, ymax = ganho_diario_med + ganho_diario_sd))+
  facet_wrap(~bioma, ncol = 2)+
  scale_x_discrete(breaks = seq(min(anual_serie$year), max(anual_serie$year), by = 5))+
  labs(x = "Ano",
       y = "Acúmulo anual de pastagem (kg MS/ha/mês)",
       color = "Cenário")+
  theme_bw(base_family = "Times New Roman")+
  theme(text = element_text(size = 12)); p5
dev.off()

# Grafico de taxa mensal por bioma
png(paste(path, "graficos/serie_acumulo_anual_nm_regiao.png", sep = "/"), width = 3500, height = 2200, res = 300)
p6 <- ggplot(r_anual_serie, aes(x = fct_rev(reorder(year, -year)), y = biomassa_anual, color = cenario))+
  geom_line(aes(group = cenario))+
  geom_point()+
  #geom_errorbar(aes(ymin = mensal_diario$ganho_diario_med - mensal_diario$ganho_diario_sd, ymax = ganho_diario_med + ganho_diario_sd))+
  facet_wrap(~nm_regiao, ncol = 2)+
  scale_x_discrete(breaks = seq(min(anual_serie$year), max(anual_serie$year), by = 5))+
  labs(x = "Ano",
       y = "Acúmulo anual de pastagem (kg MS/ha/mês)",
       color = "Cenário")+
  theme_bw(base_family = "Times New Roman")+
  theme(text = element_text(size = 12)); p6
dev.off()

