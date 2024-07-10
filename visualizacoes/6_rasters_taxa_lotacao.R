##########################################
# Criando os rasters de taxa de lotacao
# 07/03/2024
##########################################

# Limpando a area de trabalho
rm(list=ls())

# Instalando pacotes 
library(pacman)
p_load(data.table, raster, dplyr)

# Definindo o diretorio e lendo os dados
path <- "D:/arquivos/doutorado_michael/produtividade_pastagens_brasileiras"  #endereco da pasta do projeto

tlpot <- raster(paste(path, "rasters/tl_pot_grid.tif", sep = "/"))
tlext <- raster(paste(path, "rasters/tl_ext_grid.tif", sep = "/"))
tl1 <- raster(paste(path, "rasters/tl_1_grid.tif", sep = "/"))
tl2 <- raster(paste(path, "rasters/tl_2_grid.tif", sep = "/"))
tl3 <- raster(paste(path, "rasters/tl_3_grid.tif", sep = "/"))
pastagens <- raster(paste(path, "rasters/pa_br_pastagem_lapig_col8_30m_2022.tif", sep = "/"))

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
  out <- writeStart(out, filename = paste(path, sprintf("rasters/taxa_lotacao_pixel_cenario_%s.tif", cenario), sep = "/"),
                    overwrite=TRUE)
  
  for (i in 1:bss$n) {
    exporta(i)
    print(i)
  }
  
  writeStop(out)
  
}
