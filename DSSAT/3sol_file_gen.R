#########################################################################################################################################
# O script executa a integração dos dados de solo e clima para a modelagem no DSSAT, usando processamento em série para cada célula da grade. Ele lê dados
# de solo e clima, calcula propriedades hidráulicas e outros parâmetros do solo, e escreve os dados no formato necessário para o DSSAT.
# Na modelagem do solo para simulação no DSSAT, são considerados parâmetros como Clay (percentual de argila), Sand (percentual de areia),
# MOS (matéria orgânica do solo), pH (acidez ou alcalinidade), SRTM (altitude do solo), Slope (declividade do terreno) e SWC (conteúdo 
# de água no solo), que juntos fornecem uma descrição detalhada das características físicas e químicas do solo.
# Os parâmetros do solo calculados incluem textura (argila, areia), densidade do solo (SBDM), capacidade de campo (SDUL), ponto de murcha
# permanente (SLLL), capacidade de saturação (SSAT), condutividade hidráulica (SSKS), carbono orgânico do solo (SLOC), pH (SLHW), curva de
# retenção de água (SLDR), índice de fotossíntese (SLPF), e fator de mineralização (SLNF). Este processo visa a simulação precisa de condições
# de crescimento da forrageira em diferentes ambientes.
###########################################################################################################################################

# Instalando pacotes
# Se nao tiver o pacote rcropmod ja instalado, rode as linhas a seguir:
install.packages("devtools")
library(devtools)
install_github("ldemaz/dtraster")
install_github("ldemaz/rcropmod", build_vignettes = TRUE)

library(pacman)
p_load(dplyr, DSSAT, rcropmod, sf)

# Limpando a area de trabalho
rm(list=ls())

# Configuracoes
options(scipen = 999)
options(stringsAsFactors = F)


#Definindo o diretorio (endereço onde o repositorio foi clonado)
setwd('D:/arquivos/doutorado_michael/DSSAT-BPPP')

# Carregando dados
modelo.sol <- read_sol(paste0('data/SOL/TEST.SOL'))
grid.xavier <- st_read('data/grid/BR_grid.shp') #sirgas
coords <- data.frame(sp::coordinates(as_Spatial(grid.xavier)))
colnames(coords) <- c('x', 'y')
grid.xavier <- st_as_sf(cbind(grid.xavier, coords))

# Carregando funcoes
source('codes/soil_hyd.R')

# Carregando o nome dos arquivos
zonals <- list.files('data/SoilGrids_data/');zonals

# Juntando dados de solo com os dados climaticos do Xavier
# Nomes das variaveis
vrs <- c('Clay', 'Sand', 'MOS', 'pH', 'SRTM', 'SLOPE', 'SWC')
for (a in vrs) {
  
  if (a  == 'SLOPE') {
    
    r <- read.csv(paste0('data/SoilGrids_data/', zonals[grep('SLOPE', zonals)]))[,c('value', 'mean')]
    
    colnames(r) <- c('value', 'slope')
    
    grid.xavier <- left_join(grid.xavier, r , by = c("value"="value"))
    
  } else if  ( a == 'SRTM') {
    
    r <- read.csv(paste0('data/SoilGrids_data/', zonals[grep('SRTM', zonals)]))[,c('value', 'mean')]
    
    colnames(r) <- c('value', 'altitude')
    
    grid.xavier <- left_join(grid.xavier, r , by = c("value"="value"))
    
    
  }  else {
    
    r <- read.csv(paste0('data/SoilGrids_data/', zonals[grep(a, zonals)]))[,c('value', 'b0', 'b10', 'b30', 
                                                                              'b60','b100','b200')]
    colnames(r) <- c('value', paste0(a, '_', c('b0', 'b10', 'b30', 
                                               'b60','b100','b200')))  
    
    grid.xavier  <- left_join(grid.xavier, r , by = c("value"="value"))
  }
  
  rm(r)
}



for (i in 1:nrow(grid.xavier)) {
  
  
  modelo.sol$PEDON[[1]] <- paste0('TX', (100000+grid.xavier$value[i])) %>% as.character
  modelo.sol$TEXTURE[[1]] <- -99
  modelo.sol$DEPTH[[1]] <- 200
  modelo.sol$DESCRIPTION[[1]] <- 'PASTURE SIMULATION BR'
  modelo.sol$LAT[[1]] <- grid.xavier$y[i]
  modelo.sol$LONG[[1]] <- grid.xavier$x[i]
  modelo.sol$`SCS FAMILY`[[1]] <- -99
  #COLOR
  modelo.sol$SCOM[[1]] <- -99
  #ALBEDO GENERICO 0.2
  modelo.sol$SALB[[1]] <- 0.2
  #LIMITE DE EVAPOTRANSIPIRACAO
  modelo.sol$SLU1[[1]] <- 6
  #TAXA DE DRENAGEM
  modelo.sol$SLDR[[1]] <- sldr(grid.xavier$Clay_b0[i])
  #CURVA DE RUNOFF
  modelo.sol$SLRO[[1]] <- slro( slo  = grid.xavier$slope[i] ,
                                depth = 5 ,
                                texture = soil_class(grid.xavier$Clay_b0[i],grid.xavier$Sand_b0[i] )   , 
                                drainage = sldr(grid.xavier$Clay_b0[i]))  %>% as.numeric
  #FATOR DE MINERALIZACAO
  modelo.sol$SLNF <- 1
  #FATOR DE FOTOSSINTESE
  modelo.sol$SLPF <- 1
  # METODO DE DETERMINACAO DO PH (NA)
  modelo.sol$SMHB <- -99
  modelo.sol$SMPX <- -99
  modelo.sol$SMKE <- -99
  
  
  
  modelo.sol$SLLL[[1]] <- c(soil_hydraulics(grid.xavier$Sand_b0[i]/100,grid.xavier$Clay_b0[i]/100,1)[2],
                            soil_hydraulics(grid.xavier$Sand_b10[i]/100,grid.xavier$Clay_b10[i]/100,1)[2],
                            soil_hydraulics(grid.xavier$Sand_b30[i]/100,grid.xavier$Clay_b30[i]/100,1)[2],
                            soil_hydraulics(grid.xavier$Sand_b60[i]/100,grid.xavier$Clay_b60[i]/100,1)[2],
                            soil_hydraulics(grid.xavier$Sand_b100[i]/100,grid.xavier$Clay_b100[i]/100,1)[2],
                            soil_hydraulics(grid.xavier$Sand_b200[i]/100,grid.xavier$Clay_b200[i]/100,1)[2]) %>% as.numeric 
  
  modelo.sol$SDUL[[1]] <-   c(soil_hydraulics(grid.xavier$Sand_b0[i]/100,grid.xavier$Clay_b0[i]/100,1)[1],
                              soil_hydraulics(grid.xavier$Sand_b10[i]/100,grid.xavier$Clay_b10[i]/100,1)[1],
                              soil_hydraulics(grid.xavier$Sand_b30[i]/100,grid.xavier$Clay_b30[i]/100,1)[1],
                              soil_hydraulics(grid.xavier$Sand_b60[i]/100,grid.xavier$Clay_b60[i]/100,1)[1],
                              soil_hydraulics(grid.xavier$Sand_b100[i]/100,grid.xavier$Clay_b100[i]/100,1)[1],
                              soil_hydraulics(grid.xavier$Sand_b200[i]/100,grid.xavier$Clay_b200[i]/100,1)[1]) %>% as.numeric
  
  modelo.sol$SSAT[[1]] <-  c(soil_hydraulics(grid.xavier$Sand_b0[i]/100,grid.xavier$Clay_b0[i]/100,1)[3],
                             soil_hydraulics(grid.xavier$Sand_b10[i]/100,grid.xavier$Clay_b10[i]/100,1)[3],
                             soil_hydraulics(grid.xavier$Sand_b30[i]/100,grid.xavier$Clay_b30[i]/100,1)[3],
                             soil_hydraulics(grid.xavier$Sand_b60[i]/100,grid.xavier$Clay_b60[i]/100,1)[3],
                             soil_hydraulics(grid.xavier$Sand_b100[i]/100,grid.xavier$Clay_b100[i]/100,1)[3],
                             soil_hydraulics(grid.xavier$Sand_b200[i]/100,grid.xavier$Clay_b200[i]/100,1)[3]) %>% as.numeric
  
  
  modelo.sol$SSKS[[1]] <-   c(soil_hydraulics(grid.xavier$Sand_b0[i]/100,grid.xavier$Clay_b0[i]/100,1)[5],
                              soil_hydraulics(grid.xavier$Sand_b10[i]/100,grid.xavier$Clay_b10[i]/100,1)[5],
                              soil_hydraulics(grid.xavier$Sand_b30[i]/100,grid.xavier$Clay_b30[i]/100,1)[5],
                              soil_hydraulics(grid.xavier$Sand_b60[i]/100,grid.xavier$Clay_b60[i]/100,1)[5],
                              soil_hydraulics(grid.xavier$Sand_b100[i]/100,grid.xavier$Clay_b100[i]/100,1)[5],
                              soil_hydraulics(grid.xavier$Sand_b200[i]/100,grid.xavier$Clay_b200[i]/100,1)[5]) %>% as.numeric
  
  
  modelo.sol$SBDM[[1]] <-   c(soil_hydraulics(grid.xavier$Sand_b0[i]/100,grid.xavier$Clay_b0[i]/100,1)[4],
                              soil_hydraulics(grid.xavier$Sand_b10[i]/100,grid.xavier$Clay_b10[i]/100,1)[4],
                              soil_hydraulics(grid.xavier$Sand_b30[i]/100,grid.xavier$Clay_b30[i]/100,1)[4],
                              soil_hydraulics(grid.xavier$Sand_b60[i]/100,grid.xavier$Clay_b60[i]/100,1)[4],
                              soil_hydraulics(grid.xavier$Sand_b100[i]/100,grid.xavier$Clay_b100[i]/100,1)[4],
                              soil_hydraulics(grid.xavier$Sand_b200[i]/100,grid.xavier$Clay_b200[i]/100,1)[4]) %>% as.numeric
  
  modelo.sol$SLOC <- -99
  modelo.sol$SLCL[[1]]  <- grid.xavier[i,grep('Clay', colnames(grid.xavier), value = T)] %>% st_drop_geometry() %>% as.numeric 
  modelo.sol$SLSI[[1]] <- grid.xavier[1, grep('Clay', colnames(grid.xavier), value = T)] %>% st_drop_geometry() %>% as.numeric 
  modelo.sol$SLHW[[1]] <- (grid.xavier[i, grep('pH', colnames(grid.xavier), value = T)] %>% st_drop_geometry() %>% as.numeric)/10  #no gee o ph é dado em pH*10 
  modelo.sol$SCEC[[1]] <- c(-99,-99,-99,-99,-99,-99)
  modelo.sol$SLCF <- -99
  
  # Mover o arquivo TX para um arquivo de solo nao existente
  write_sol(modelo.sol, paste0('C:/DSSAT48/Soil/','TX' , '.SOL'), append = T)
  print(i) 
  
  
  
}

