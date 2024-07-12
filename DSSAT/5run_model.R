##################################################################################################################################################
# O script analisado é utilizado para a modelagem agrícola com DSSAT, especificamente para simulações de produtividade de pastagens no Brasil.
# As variáveis de entrada são essenciais para configurar o ambiente de simulação e incluem dados climáticos, parâmetros de solo e detalhes de 
# simulação. As variáveis climáticas incluem radiação solar diária (SRAD), temperatura máxima (TMAX) e mínima (TMIN) diária, precipitação diária 
# (RAIN), velocidade do vento (WIND) e umidade relativa diária (RHUM). Os parâmetros de solo abrangem teor de argila e areia em diferentes 
# profundidades, altitude (SRTM), taxa de drenagem (SLDR), curva de runoff (SLRO), limites de água disponível (SLLL e SDUL), saturação (SSAT),
# condutividade hidráulica saturada (SSKS), densidade do solo (SBDM), conteúdo de argila (SLCL), silte (SLSI) e pH do solo (SLHW), além da
# capacidade de troca catiônica (SCEC).
# O script também inclui parâmetros específicos de simulação como detalhes de plantio, irrigação, controle de simulação e fertilização. Estes parâmetros são definidos
# para cada ponto de simulação utilizando dados locais e o arquivo X (denominada na tese de Arquivo Setup de CulturaP) do DSSAT é atualizado com estas informações. 
# A execução do DSSAT é realizada em paralelo para aumentar a eficiência, utilizando as funções de processamento paralelo disponíveis em R
# As variáveis de saída do modelo são coletadas e incluem várias métricas de crescimento das plantas ao longo do tempo, como a data da observação, 
# ano, dias após a semeadura (DAS) e indicador de morte da planta (DEATH). Esses resultados são armazenados em um banco de dados SQLite para posterior análise.
##################################################################################################################################################

#Carregando pacotes
library(pacman)
p_load(DSSAT, dplyr, sf, lubridate, rcropmod, RSQLite, raster, ggplot2, readr, foreach, doParallel)

# Limpando a area de trabalho
rm(list=ls())

#Definindo o diretorio (endereço onde o repositorio foi clonado)
setwd('D:/arquivos/doutorado_michael/produtividade_pastagens_brasileiras/DSSAT_produtividade_pastagens_brasileiras/DSSAT')

# Configuracoes
options(DSSAT.CSM = 'C:/DSSAT48/DSCSM048.EXE')
options(stringsAsFactors = F)
options(scipen = 999)

# Carregando os nomes dos arquivos de solo
zonals <- list.files('data/SoilGrids_data/')

#Carregando as funcoes
source('codes/soil_hyd.R')

#Carregando os dados necessarios
#Pastagem
pasture <- raster("data/pasture/pa_br_pasture_30m_2021_LAPIG.tif")
#Limites nacionais
br.limits <- st_read('data/br_limits/pa_br_limiteNacional_250_2021_ibge.shp')
#Limites estaduais
br.ufs <- st_read('data/br_limits/pa_br_limiteEstadual_250_2015_ibge.shp')
#DSSAT arquivo X
file.x <- read_filex('data/xfile/SPPI7901.BRX')
#Pontos de simulacao
simulation_points <- readRDS('data/simulation_points/simulation_points.rds')$grid
#Grid xavier
grid.xavier <- st_read('data/grid/BR_grid.shp')
#Grids IDs
ids <- readRDS("data/soil_ids/grid_id.rds")$id
#Clima
x <- readRDS("data/climate/climate_4.rds")
#Exemplo de arquivo MOW
mow.example <- DSSAT::read_filet('C:/DSSAT48/Brachiaria/SPPI1101.MOW')
#Exemplo de arquivo WTH
weather.file <- DSSAT::read_wth('data/WTH/ZXUJ8001.WTH')
#Datas de colheita e aplicacoes de N
mow.dates <- read.csv2('data/harvest_dates_n_aplication/harvest_dates_n_application_original.csv')
mow.dates$date <- as.Date(mow.dates$date, '%d/%m/%Y')
mow.dates$n_amount <- as.numeric(mow.dates$n_amount_ext) #selecionar a coluna da tabela em funcao do cenario desejado (ex. n_amout_ext = cenario extensivo)

#Juntando dados de solo com dados do Xavier
#Var names
vrs <- c('Clay', 'Sand')
for (a in vrs) {
  
  r <- read.csv(paste0('D:/arquivos/doutorado_michael/DSSAT-BPPP/data/SoilGrids_data/', grep(a, zonals, value = T)))[,c('value', 'b0', 'b10', 'b30', 
                                                                                                       'b60','b100','b200')]
  colnames(r) <- c('value', paste0(a, '_', c('b0', 'b10', 'b30', 
                                             'b60','b100','b200')))  
  
  grid.xavier <- left_join(grid.xavier, r , by = c("value"="value"))
  
  
  rm(r)
}


#Dias de fertilizacao inorganica
length.days.to.fertilize <- 1:nrow(mow.dates %>% filter(n_amount != 0))
days.to.fertilize <- which(mow.dates$n_amount!=0) 

#Definindo a variavel SASC - Stable Organic Carbon
file.x$`SOIL ANALYSIS`['SASC'] <- 0.8 #(Worou et al. 2019; Porter et al., 2009) 

#Definindo a fertilizacao
file.x$`FERTILIZERS (INORGANIC)`[length.days.to.fertilize,'F'] <- 1
file.x$`FERTILIZERS (INORGANIC)`[length.days.to.fertilize,'FDATE'] <- as.POSIXct(mow.dates$date[days.to.fertilize])
file.x$`FERTILIZERS (INORGANIC)`[length.days.to.fertilize,'FMCD'] <- file.x$`FERTILIZERS (INORGANIC)`[1,'FMCD']
file.x$`FERTILIZERS (INORGANIC)`[length.days.to.fertilize,'FACD'] <- file.x$`FERTILIZERS (INORGANIC)`[1,'FACD']
file.x$`FERTILIZERS (INORGANIC)`[length.days.to.fertilize,'FDEP'] <- file.x$`FERTILIZERS (INORGANIC)`[1,'FDEP']
file.x$`FERTILIZERS (INORGANIC)`[length.days.to.fertilize,'FAMN'] <- mow.dates$n_amount[days.to.fertilize]
file.x$`FERTILIZERS (INORGANIC)`[length.days.to.fertilize,'FERNAME'] <- file.x$`FERTILIZERS (INORGANIC)`[1,'FERNAME']

#Ativando os efeitos da agua e nitrogenio
file.x$`SIMULATION CONTROLS`[,'WATER'] <- 'Y'
file.x$`SIMULATION CONTROLS`[,'NITRO'] <- 'Y'

#Mudando o metodo de evapotranspiracao do solo de Suleiman-Ritchie to Ritchie-Ceres
file.x$`SIMULATION CONTROLS`[,'MESEV'] <- 'R'

#Construindo o arquivo MOW final
mow.example <- mow.example[1:nrow(mow.dates),]
mow.example[1:nrow(mow.dates),'TRNO'] <- 1
mow.example[1:nrow(mow.dates),'DATE'] <- as.POSIXct(mow.dates$date)
mow.example[1:nrow(mow.dates),'MOW'] <- 3000
mow.example[1:nrow(mow.dates),'RSPLF'] <- 27
mow.example[1:nrow(mow.dates),'MVS'] <- 3
mow.example[1:nrow(mow.dates),'RSHT'] <- 20

#Exportando o arquivo MOW
write_filet(mow.example, 'C:/DSSAT48/Brachiaria/SPPI7902.MOW', drop_duplicate_rows = T)

#Criando arquivo SQLite para armazenar os resultados (.db)
if(!dir.exists('outputs/outptus_michael.db')) dir.create('outputs/outptus_michael.db')
conn <- dbConnect(SQLite(), 'outputs/outptus_michael.db')

#Criando pasta para arquivos temporarios
if(!dir.exists('dssat_temp')) dir.create('dssat_temp')
setwd('dssat_temp')

# Rodando o DSSAT para os pontos de simulacao

system.time(for (k in simulation_points) {
  k <- which(grid.xavier$value==k)
  #Alterando o arquivo x com dados locais do ponto de simulacao
  file.x$`PLANTING DETAILS`[,'PDATE'] <- as.POSIXct(as.Date('15/10/1980', '%d/%m/%Y'))
  file.x$`IRRIGATION AND WATER MANAGEMENT`[,'IDATE'] <- as.POSIXct(as.Date('15/10/1980', '%d/%m/%Y'))
  
  file.x$`TREATMENTS                        -------------FACTOR LEVELS------------`[1,] <- file.x$`TREATMENTS                        -------------FACTOR LEVELS------------`[1,]
  file.x$`TREATMENTS                        -------------FACTOR LEVELS------------`[1, 'TNAME'] <- paste0('MARANDU', grid.xavier$value[k])    
  file.x$FIELDS[1,'L'] <- 1
  file.x$FIELDS[1, 'ID_FIELD'] <- 'AAAA0001'
  file.x$FIELDS[1,'WSTA'] <- ids[k]
  file.x$FIELDS[1,'ID_SOIL'] <- paste0('TX', (100000+grid.xavier$value[k])) %>% as.character
  file.x$FIELDS[1,'FLNAME'] <- 'TX'
  file.x$`INITIAL CONDITIONS`$SH2O[[1]] <- c(soil_hydraulics(grid.xavier$Sand_b0[k]/100,grid.xavier$Clay_b0[k]/100,1)[1],
                                             soil_hydraulics(grid.xavier$Sand_b10[k]/100,grid.xavier$Clay_b10[k]/100,1)[1],
                                             soil_hydraulics(grid.xavier$Sand_b30[k]/100,grid.xavier$Clay_b30[k]/100,1)[1],
                                             soil_hydraulics(grid.xavier$Sand_b60[k]/100,grid.xavier$Clay_b60[k]/100,1)[1],
                                             soil_hydraulics(grid.xavier$Sand_b100[k]/100,grid.xavier$Clay_b100[k]/100,1)[1],
                                             soil_hydraulics(grid.xavier$Sand_b200[k]/100,grid.xavier$Clay_b200[k]/100,1)[1]) %>% as.numeric
  
  
  file.x$`RESIDUES AND ORGANIC FERTILIZER`[['RAMT']] <- 0
  
  write_filex(file.x, 'C:/DSSAT48/Brachiaria/SPPI7902.BRX')
  # Exportando arquivo batch
  write_dssbatch(x='C:/DSSAT48/Brachiaria/SPPI7902.BRX', trtno=1, rp = 1,sq = 0, op = 0, co = 0)
  cat(paste0('run : ', k))
  
  #run
  run_dssat(run_mode = 'B')
  
  #Armazenando output PlantGro
  pgro <- data.frame(read_output('PlantGro.OUT'))
  pgro[, 'value'] <- grid.xavier$value[k]
  pgro$YEAR <- year(pgro$DATE)
  pgro$DEATH <- ifelse(max(pgro$YEAR) ==2016, 0, 1)
  pgro$DATE <- as.character(pgro$DATE)

  #Exportando o output para o banco SQLite
  dbWriteTable(conn, 'PlantGro_ext', pgro,  append = T) #nomear o output com base no cenario que esta sendo executado (ex. PlantGro_ext = cenario extensivo)
  
  cat('/n')
  cat('/n')
  
  
})


