# Instalando pacotes
library(pacman)
p_load(stringr, sf)

# Limpando a area de trabalho
rm(list = ls())

#Set seed para ids unicos
set.seed(1)

#Definindo o diretorio (endereço onde o repositorio foi clonado)
setwd('D:/arquivos/doutorado_michael/DSSAT-BPPP')

# Numero de ids
row.number <- nrow(st_read('data/grid/BR_grid.shp'))

#Funcao para criar id unico
unique_id_gen <- function(a, outfile) {
  v <- sapply(1:a, function(u)sample(LETTERS,4) %>% str_flatten)
  while (length(v[duplicated(v)])!=0) {
    v2 <- sapply(1:(length(v[duplicated(v)])), function(u){
      sample(LETTERS,4) %>% str_flatten})
    v <- v[-which(duplicated(v))]
    v <- c(v, v2)
  } 
  saveRDS(data.frame(id = v), outfile)
}


# Criando uma pasta para os dados de solo na pasta 'data'
if(!dir.exists('data/soil_ids')) dir.create('data/soil_ids')

# Criando o arquivo com os ids
unique_id_gen(row.number, 'data/soil_ids/grid_id.rds')


