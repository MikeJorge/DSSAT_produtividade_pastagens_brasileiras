##################################################
# Processamento dos resultados do DSSAT - Marandu
# Pietro Gragnolati Fernandes
# 08/02/2024
##################################################

# Limpando a area de trabalho
rm(list = ls())

# Instalando pacotes
library(pacman)
p_load(dplyr, data.table, ggplot2, tidyr, RSQLite, DBI)

# Fazendo a conexao com o banco
path <- "D:/arquivos/doutorado_michael/DSSAT-BPPP/outputs/outptus_michael.db" #endereco do banco de dados
conn <- dbConnect(SQLite(), path)
dbListTables(conn)

# Fazendo o upload no banco da tabela de depreciacao da produtividade por plantas daninhas
setwd("D:/arquivos/GPP/git_workspace/") #endereco da pasta onde o repositorio foi clonado
tabela_pd <- fread("DSSAT_produtividade_pastagens_brasileiras/DSSAT/data/depreciacao_plantas_daninhas.csv")
dbWriteTable(conn, "depreciacao_plantas_daninhas", tabela_pd, overwrite = TRUE)

# Querys para agrupar os dados na escala mensal e diario

output_cen1 <- "CREATE TABLE output_1 AS
                SELECT ppg.DATE, ppg.value PONTO_SIMULACAO, ppg.CWAD CWAD_ORIGINAL, ppg2.CWAD CWAD_LAG, 
                  (ppg.HERB-(ppg2.CWAD*0.98-ppg.CWAD*0.98)) GANHO_DIARIO_CWAD, STRFTIME('%d', ppg.DATE) as 'day', 
                  STRFTIME('%m', ppg.DATE) as 'month', STRFTIME('%Y', ppg.DATE) as 'year'
                FROM PlantGro_cen1 ppg , PlantGro_cen1 ppg2 
                WHERE ppg.rowid = ppg2.rowid+1"

output_cen2 <- "CREATE TABLE output_2 AS
                SELECT ppg.DATE, ppg.value PONTO_SIMULACAO, ppg.CWAD CWAD_ORIGINAL, ppg2.CWAD CWAD_LAG, 
                  (ppg.HERB-(ppg2.CWAD*0.985-ppg.CWAD*0.985)) GANHO_DIARIO_CWAD, STRFTIME('%d', ppg.DATE) as 'day', 
                  STRFTIME('%m', ppg.DATE) as 'month', STRFTIME('%Y', ppg.DATE) as 'year'
                FROM PlantGro_cen2 ppg , PlantGro_cen2 ppg2 
                WHERE ppg.rowid = ppg2.rowid+1"

output_cen3 <- "CREATE TABLE output_3 AS
                SELECT ppg.DATE, ppg.value PONTO_SIMULACAO, ppg.CWAD CWAD_ORIGINAL, ppg2.CWAD CWAD_LAG, 
                  (ppg.HERB-(ppg2.CWAD*0.99-ppg.CWAD*0.99)) GANHO_DIARIO_CWAD, STRFTIME('%d', ppg.DATE) as 'day', 
                  STRFTIME('%m', ppg.DATE) as 'month', STRFTIME('%Y', ppg.DATE) as 'year'
                FROM PlantGro_cen3 ppg , PlantGro_cen3 ppg2 
                WHERE ppg.rowid = ppg2.rowid+1"

output_pot <- "CREATE TABLE output_pot AS
                SELECT ppg.DATE, ppg.value PONTO_SIMULACAO, ppg.CWAD CWAD_ORIGINAL, ppg2.CWAD CWAD_LAG, 
                  ppg.HERB-(ppg2.CWAD-ppg.CWAD) GANHO_DIARIO_CWAD, STRFTIME('%d', ppg.DATE) as 'day', 
                  STRFTIME('%m', ppg.DATE) as 'month', STRFTIME('%Y', ppg.DATE) as 'year'
                FROM PlantGro_potencial ppg , PlantGro_potencial ppg2 
                WHERE ppg.rowid = ppg2.rowid+1"

output_ext <- "CREATE TABLE output_ext AS
                SELECT ppg.DATE, ppg.value PONTO_SIMULACAO, ppg.CWAD CWAD_ORIGINAL, ppg2.CWAD CWAD_LAG,
                  (ppg.HERB-(ppg2.CWAD*pd.ext-ppg.CWAD*pd.ext)) GANHO_DIARIO_CWAD, STRFTIME('%d', ppg.DATE) as 'day', 
                  STRFTIME('%m', ppg.DATE) as 'month', STRFTIME('%Y', ppg.DATE) as 'year'
                FROM PlantGro_ext ppg , PlantGro_ext ppg2, depreciacao_plantas_daninhas pd 
                WHERE ppg.rowid = ppg2.rowid+1 AND STRFTIME('%Y', ppg.DATE) = pd.year"


mensal_cen1 <- "CREATE TABLE mensal_1 AS
                SELECT month, year, PONTO_SIMULACAO, SUM(GANHO_DIARIO_CWAD), FIRST_VALUE(CWAD_ORIGINAL) OVER(partition by PONTO_SIMULACAO)
                FROM output_1
                GROUP BY year, month, PONTO_SIMULACAO"

mensal_cen2 <- "CREATE TABLE mensal_2 AS
                SELECT month, year, PONTO_SIMULACAO, SUM(GANHO_DIARIO_CWAD), FIRST_VALUE(CWAD_ORIGINAL) OVER(partition by PONTO_SIMULACAO)
                FROM output_2
                GROUP BY year, month, PONTO_SIMULACAO"

mensal_cen3 <- "CREATE TABLE mensal_3 AS
                SELECT month, year, PONTO_SIMULACAO, SUM(GANHO_DIARIO_CWAD), FIRST_VALUE(CWAD_ORIGINAL) OVER(partition by PONTO_SIMULACAO)
                FROM output_3
                GROUP BY year, month, PONTO_SIMULACAO"

mensal_pot <- "CREATE TABLE mensal_pot AS
                SELECT month, year, PONTO_SIMULACAO, SUM(GANHO_DIARIO_CWAD), FIRST_VALUE(CWAD_ORIGINAL) OVER(partition by PONTO_SIMULACAO)
                FROM output_pot
                GROUP BY year, month, PONTO_SIMULACAO"

mensal_ext <- "CREATE TABLE mensal_ext AS
                SELECT month, year, PONTO_SIMULACAO, SUM(GANHO_DIARIO_CWAD), FIRST_VALUE(CWAD_ORIGINAL) OVER(partition by PONTO_SIMULACAO)
                FROM output_ext
                GROUP BY year, month, PONTO_SIMULACAO"


diario_cen1 <- "CREATE TABLE diario_1 as
                SELECT day, month, PONTO_SIMULACAO, AVG(GANHO_DIARIO_CWAD)
                FROM output_1
                GROUP BY day, month, PONTO_SIMULACAO"

diario_cen2 <- "CREATE TABLE diario_2 as
                SELECT day, month, PONTO_SIMULACAO, AVG(GANHO_DIARIO_CWAD)
                FROM output_2
                GROUP BY day, month, PONTO_SIMULACAO"

diario_cen3 <- "CREATE TABLE diario_3 as
                SELECT day, month, PONTO_SIMULACAO, AVG(GANHO_DIARIO_CWAD)
                FROM output_3
                GROUP BY day, month, PONTO_SIMULACAO"

diario_pot <- "CREATE TABLE diario_pot as
                SELECT day, month, PONTO_SIMULACAO, AVG(GANHO_DIARIO_CWAD)
                FROM output_pot
                GROUP BY day, month, PONTO_SIMULACAO"

diario_ext <- "CREATE TABLE diario_ext as
                SELECT day, month, PONTO_SIMULACAO, AVG(GANHO_DIARIO_CWAD)
                FROM output_ext
                GROUP BY day, month, PONTO_SIMULACAO"



# Executando as querys
dbSendQuery(conn, "DROP TABLE IF EXISTS output_1") 
dbSendQuery(conn, output_cen1)

dbSendQuery(conn, "DROP TABLE IF EXISTS output_2") 
dbSendQuery(conn, output_cen2)

dbSendQuery(conn, "DROP TABLE IF EXISTS output_3") 
dbSendQuery(conn, output_cen3)

dbSendQuery(conn, "DROP TABLE IF EXISTS output_pot") 
dbSendQuery(conn, output_pot)

dbSendQuery(conn, "DROP TABLE IF EXISTS output_ext") 
dbSendQuery(conn, output_ext)


dbSendQuery(conn, "DROP TABLE IF EXISTS mensal_1") 
dbSendQuery(conn, mensal_cen1)

dbSendQuery(conn, "DROP TABLE IF EXISTS mensal_2") 
dbSendQuery(conn, mensal_cen2)

dbSendQuery(conn, "DROP TABLE IF EXISTS mensal_3") 
dbSendQuery(conn, mensal_cen3)

dbSendQuery(conn, "DROP TABLE IF EXISTS mensal_pot") 
dbSendQuery(conn, mensal_pot)

dbSendQuery(conn, "DROP TABLE IF EXISTS mensal_ext") 
dbSendQuery(conn, mensal_ext)


dbSendQuery(conn, "DROP TABLE IF EXISTS diario_1") 
dbSendQuery(conn, diario_cen1)

dbSendQuery(conn, "DROP TABLE IF EXISTS diario_2") 
dbSendQuery(conn, diario_cen2)

dbSendQuery(conn, "DROP TABLE IF EXISTS diario_3") 
dbSendQuery(conn, diario_cen3)

dbSendQuery(conn, "DROP TABLE IF EXISTS diario_pot") 
dbSendQuery(conn, diario_pot)

dbSendQuery(conn, "DROP TABLE IF EXISTS diario_ext") 
dbSendQuery(conn, diario_ext)
