# Instalando pacotes
library(pacman)
p_load(raster, foreach, parallel, dplyr, sf, doSNOW)

# Limpando a area de trabalho
rm(list=ls())

# Configuracoes
options(scipen = 999)
options(stringsAsFactors = T)

#Definindo o diretorio (endereço onde o repositorio foi clonado)
setwd('D:/arquivos/doutorado_michael/DSSAT-BPPP')

# Lendo os dados
pasture <- raster('data/pasture/pa_br_pastagens_30m_2021_LAPIG.tif')
grid <- raster('data/grid/pa_br_gridXavier_30m.tif')
grid.values <- st_read('data/grid/pa_br_gridXavier.shp') %>% st_drop_geometry()
grid.values$count <-  0

# Tamanho do bloco
bs <- blockSize(pasture) ; bs


#Teste (prototipo)
i <- 701
dt <- data.frame(pasture = getValues(pasture, row = bs$row[i], nrows = bs$nrows[i]),
                 grid = getValues(grid, row = bs$row[i], nrows = bs$nrows[i]))

dt <- dt[!is.na(dt$pasture),]
dt.agg <- aggregate(dt$pasture~dt$grid, FUN = 'sum')
colnames(dt.agg) <- c('grid', 'pasture') ; dt.agg

# Criando uma pasta para os dados dos pontos de simulacao na pasta 'data'
if(!dir.exists('data/simulation_points')) dir.create('data/simulation_points')

# Processamento paralelo
cl <- makeSOCKcluster(16)
registerDoSNOW(cl)
clusterExport(cl, ls())
snow::clusterEvalQ(cl, {library(dplyr)})
snow::clusterEvalQ(cl, {library(raster)})


#for(i in 1:bs$n) {
out.bind <- foreach(i=(1:bs$n), .combine = rbind) %dopar% {
  print(i)
  dt <- data.frame(count = getValues(pasture, row = bs$row[i], nrows = bs$nrows[i]),
                   grid = getValues(grid, row = bs$row[i], nrows = bs$nrows[i]))
  
  dt <- dt %>%
    filter(!(is.na(count) | is.na(grid)))
  if(nrow(dt)>0) {
    dt.agg <- aggregate(count~grid, data = dt, FUN = 'sum')
  } else { 
    dt.agg <- NA
  }
  
  return(dt.agg)}; out.bind

#Unindo os resultados e filtrando NAs
out.bind <- out.bind %>% filter(!is.na(grid) | !is.na(count))

# Salvando os resultados
saveRDS(out.bind, 'data/simulation_points/out.rds')

# Agregando
out.agg <- aggregate(count~grid,data = out.bind,  FUN = 'sum')

#Calculo de area (0.09 = area em ha do pixel com 30m)
out.agg$area_ha <- out.agg$count*0.09

# Salvando o resultado agregado
saveRDS(out.agg, 'data/simulation_points/simulation_points.rds')


