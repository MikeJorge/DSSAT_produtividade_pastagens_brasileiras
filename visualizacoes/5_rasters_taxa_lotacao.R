##########################################
# Criando os rasters de taxa de lotacao
# Pietro Gragnolati Fernandes
# 07/03/2024
##########################################

# Limpando a area de trabalho
rm(list=ls())

# Instalando pacotes 
library(pacman)
p_load(data.table, raster, dplyr)

# Definindo o diretorio e lendo os dados
setwd("D:/arquivos/doutorado_michael/rasters")

tlpot <- raster("tl_pot_grid.tif")
tlext <- raster("tl_ext_grid.tif")
tl1 <- raster("tl_1_grid.tif")
tl2 <- raster("tl_2_grid.tif")
tl3 <- raster("tl_3_grid.tif")
pastagens <- raster("pa_br_pastagem_lapig_col8_30m_2022.tif")

# Função para extrair a tabela e modificar os valores dos rasters
fun_ex <- function(i){
  # Extrair tabela
    db <- data.frame(tl = getValues(taxa_lotacao, row = bss$row[i], nrows = bss$nrows[i]),
                     past = getValues(pastagens, row = bss$row[i], nrows = bss$nrows[i]))
  db$out <- ifelse(db$past == 1, db$tl, NA)
  return(db$out)
}


# Função para exportar os resultados
exporta <- function(i){
  d <- fun_ex(i)
  out <- writeValues(out, d, bss$row[i])
}

# Preparando o loop
taxas_list <- list(tlpot,tlext,tl1,tl2,tl3)
export_label <- c("pot","ext", "1", "2", "3")

for (n in 1:length(taxas_list)) {
  print(sprintf("Construindo raster %s", n))
  # Definir o raster de taxa de lotacao
  taxa_lotacao <- taxas_list[[n]]
  cenario <- export_label[n]
  # Criar os blocos de processamento
  bss <- blockSize(pastagens)
  
  # Executar o processo
  out <- raster(pastagens)
  path <- "D:/arquivos/doutorado_michael/rasters" #caminho local para exportar os rasters
  out <- writeStart(out, filename = paste(path, sprintf("taxa_lotacao_pixel_cenario_%s.tif", cenario), sep = "/"),
                    overwrite=TRUE)
  
  for (i in 1:bss$n) {
    exporta(i)
    print(i)
  }
  
  writeStop(out)
  
}
