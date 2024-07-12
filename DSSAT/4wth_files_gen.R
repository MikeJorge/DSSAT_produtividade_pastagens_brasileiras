###############################################################################################################################################
# No script, foram modeladas as variáveis climáticas, incluindo SRAD (radiação solar), TMAX (temperatura máxima), TMIN (temperatura mínima),
# RAIN (precipitação), WIND (velocidade do vento), RHUM (umidade relativa), EA (pressão de vapor do ar) e TDEW (ponto de orvalho), 
# para gerar arquivos climáticos que foram utilizados no modelo de simulação de cultura DSSAT.
###############################################################################################################################################


# Instalando pacotes
library(pacman)
p_load(dplyr, sf, DSSAT, rcropmod, parallel, doSNOW, foreach, doParallel)

# Limpando a area de trabalho
rm(list=ls())

# Configuracoes
options(scipen = 999)
options(stringsAsFactors = F)

#Definindo o diretorio (endereço onde o repositorio foi clonado)
setwd('D:/arquivos/doutorado_michael/DSSAT-BPPP')

# Extraia os arquivos zipados: se voce nao fez isso ainda, remova o # da linha abaixo e execute
#unzip('data/climate/climate.rar', exdir = 'data/climate')

# Carregando os arquivos
grid.xavier <- st_read('data/grid/BR_grid.shp') #sirgas
coords <- data.frame(sp::coordinates(as_Spatial(grid.xavier)))
colnames(coords) <- c('x', 'y')
grid.xavier <- st_as_sf(cbind(grid.xavier, coords))
altitude <- read.csv(paste0('data/SoilGrids_data/BR_xavier_zonal_SRTM.csv'))[,c('value', 'mean')]
colnames(altitude) <- c('value', 'altitude')
grid.xavier <- left_join(grid.xavier, altitude , by = c("value"="value"))
uniqid <- readRDS("data/soil_ids/grid_id.rds")$id

# Datas dos arquivos climaticos (WTH)
anos <- seq(1980,2016, by = 1) %>%
  as.character
mes <- c("01", "12")
dia <- c("01","31")
inicio <- paste0('1980', '01', '01')
fim <- paste0('2016', '12', '31')

# Processamento paralelo
cl <- makeSOCKcluster(16)
registerDoSNOW(cl)
clusterExport(cl, ls())
snow::clusterEvalQ(cl, {library(rcropmod)})
#run
out <- foreach(u=grid.xavier$value) %dopar% {
  
  rec <- readRDS(paste0('data/climate/climate_', u, '.rds'))
  u <- which(grid.xavier$value==u)
  
  
  #Checando se todas as observacoes de Tmax sao maiores que Tmin
  if(all(rec$TMAX>rec$TMIN)) {
    
    
    weather(
      xy = c(grid.xavier$x[u],grid.xavier$y[u]),
      elev = grid.xavier$altitude[u],
      srad = rec$SRAD,
      tmax = rec$TMAX,
      tmin = rec$TMIN,
      prec = rec$RAIN,
      sdate = inicio,
      edate = fim,
      name = uniqid[u],
      ea = NULL,
      wind = rec$WIND,
      rh = rec$RHUM,
      tmu = NULL,
      sh=NULL,
      pres = NULL,
      tdew = NULL,
      outdir = "C:/DSSAT48/Weather/")
    
    return(NULL) } else {
      #se tiver algum problema                    
      #aumente 0.1º C na TMax
      rec[which(rec$TMAX<rec$TMIN),'TMAX'] <- rec[which(rec$TMAX<rec$TMIN),'TMIN']+0.1
      
      
      
      weather(
        xy = c(grid.xavier$x[u],grid.xavier$y[u]),
        elev = grid.xavier$altitude[u],
        srad = rec$SRAD,
        tmax = rec$TMAX,
        tmin = rec$TMIN,
        prec = rec$RAIN,
        sdate = inicio,
        edate = fim,
        name = uniqid[u],
        ea = NULL,
        wind = rec$WIND,
        rh = rec$RHUM,
        tmu = NULL,
        sh=NULL,
        pres = NULL,
        tdew = NULL,
        outdir = "C:/DSSAT48/Weather/")
      
      return(NULL) 
      
      
    }
  
  
}

#BUG::::weather() function misnaming files
# Renomeando arquivos
files <- list.files('C:/DSSAT48/Weather/', pattern = '.WTH')
for (i in files) {
  
  file.rename(from = paste0('C:/DSSAT48/Weather/', i), to = paste0('C:/DSSAT48/Weather/', gsub('[0-9][0-9][0-9][0-9]', '8001' ,i)))
  
}




