##########################################################################################################################################
# Calculo da taxa de lotacao critica
# 15/02/2024
# O script realiza o cálculo da taxa de lotação crítica para diferentes cenários de produtividade de pastagens. Ele se conecta a um banco 
# de dados SQLite (outptus_michael.db), verifica a mortalidade das pastagens antes de 2016 e calcula a taxa de lotação crítica (TL critica) considerando a 
# eficiência de pastejo variável em função do período (águas e seca). O cálculo é feito iterativamente até que nenhum ponto de simulação 
# apresente déficit acima de 2500 kg MS/ha. Os resultados finais são exportados para um arquivo CSV.
##########################################################################################################################################

# Limpando a area de trabalho
rm(list = ls())

# Instalando os pacotes
library(pacman)
p_load(data.table, lubridate, ggplot2, RSQLite, tidyr, dplyr,sf, raster, sp, tidyr)

# Fazendo a conexao com o banco
path <- "D:/arquivos/doutorado_michael/produtividade_pastagens_brasileiras" #endereco do banco de dados
conn <- dbConnect(SQLite(), paste(path, "DSSAT_produtividade_pastagens_brasileiras/DSSAT/outputs/outptus_michael.db", sep="/"))
dbListTables(conn)

# Verificando se houveram pontos onde o pasto morreu antes do fim da simulacao
out <- dbGetQuery(conn,'select * from mensal_ext')

last.year.per.point <- out %>%
  group_by(PONTO_SIMULACAO) %>% 
  summarise(ano = max(year))

problems <- subset(last.year.per.point, ano < 2016)

print(paste("Em", nrow(problems), "pontos de simulação a pastagem morreu antes de 2016", sep = " "))

# Taxa de lotacao critica----

cenarios <- c("pot", "1", "2", "3", "ext") #definindo o sufixo dos cenarios

# Criando a estrutura da tabela final
ponto_simulacao <- unique(last.year.per.point$PONTO_SIMULACAO)
tl_critica <- data.frame(ponto_simulacao)

# Loop que calcula a taxa de lotacao critica para cada ponto em todos os cenarios

for (i in cenarios) {
  # Carregando os dados na escala diaria do ano modelo
  print(sprintf("Rodando o cenário %s", i))
  query <- sprintf("SELECT * FROM diario_%s", i)
  out_dia <- dbGetQuery(conn, query)
  colnames(out_dia) <- c("dia", "mes", "ponto_simulacao", "ganho_diario")
  
  # Agrupando por ponto de simulacao
  PAP <- out_dia%>%
    group_by(ponto_simulacao)%>%
    summarise(PAP = sum(ganho_diario))
  
  # Calculo das variaveis
  #TLsup
  DAUA <- 6428.4
  PAP$TL_sup <- with(PAP, PAP/DAUA)
  PAP$TL_sup <- ifelse(PAP$TL_sup < 0, 0, PAP$TL_sup)
  
  # Dt - Demanda diaria
  CD <- 8.8
  EPP <- 0.5 #variar a EPP em funcao do periodo: aguas (out-abr, maio-set)
  PAP$Dt <- with(PAP, TL_sup*CD/EPP)
  
  # Organizando a base na escala diaria
  out_dia <- merge(out_dia, PAP, by = "ponto_simulacao")
  out_dia$inicio <- ifelse(out_dia$dia == '01' & out_dia$mes == '01', 1, 0)
  out_dia$data <- paste(out_dia$dia, out_dia$mes, sep = "/")
  out_dia$data <- as.Date(out_dia$data, format = "%d/%m")
  out_dia <- out_dia%>%arrange(ponto_simulacao, data)
  
  # Calculando a demanda diaria com eficiencia de pastejo especifica pra cada periodo (aguas, seca)
  EPP_aguas <- 0.7
  EPP_seca <- 0.3
  out_dia$mes <- as.numeric(out_dia$mes)
  out_dia$Dt <- ifelse(out_dia$mes %in% c(10:12,1:4), (out_dia$TL_sup*CD/EPP_aguas), (out_dia$TL_sup*CD/EPP_seca))
  
  # CFD (Cumulative Forage Deficit) e TL critica
  cfd <-  out_dia$Dt[1] - out_dia$ganho_diario[1]
  out_dia$fd <- out_dia$Dt - out_dia$ganho_diario
  out_dia$fd <- ifelse(out_dia$fd < 0, 0, out_dia$fd)
  
  # 1ª iteracao de calculo do CFD
  for (k in 1:nrow(out_dia)) {
    cfd[k] <- ifelse(out_dia$inicio[k] == 1, out_dia$fd[k],
                     out_dia$fd[k] + cfd[k-1])
  }
  
  out_dia$cfd <- cfd
  
  # Verificando o deficit maximo de cada ponto
  pontos_deficit <- out_dia%>%
    group_by(ponto_simulacao)%>%
    summarise(deficit = max(cfd),
              TL = mean(TL_sup))
  
  # Filtrando pontos cujo deficit atingiu mais que 2500 kg MS/ha
  sem_deficit <- subset(pontos_deficit, deficit < 2500)
  
  com_deficit <- subset(pontos_deficit, deficit > 2500)
  
  out_dia <- merge(out_dia, com_deficit[,c(1:2)], by = "ponto_simulacao")
  out_dia <- out_dia%>%arrange(ponto_simulacao, data)
  out_dia$TL_crit <-  out_dia$TL_sup*0.95 # Reduzindo a TL sup em 5%
  out_dia$Dt <- with(out_dia, TL_crit*CD/EPP) # Atualizando a Demanda total diaria com base no novo valor de TL
  
  # Realizando o numero de iteracoes necessarias, ate nao restar nenhum ponto com deficit
  cfd <-  out_dia$Dt[1] - out_dia$ganho_diario[1]
  out_dia$fd <- out_dia$Dt - out_dia$ganho_diario
  out_dia$fd <- ifelse(out_dia$fd < 0, 0, out_dia$fd)
  n <- nrow(com_deficit)

  
  while (n > 0) { 
    
    
    for (k in 1:nrow(out_dia)) {
      cfd[k] <- ifelse(out_dia$inicio[k] == 1, out_dia$fd[k],
                       out_dia$fd[k] + cfd[k-1])
    }
    
    out_dia$cfd <- cfd
    
    pontos_deficit <- out_dia%>%
      group_by(ponto_simulacao)%>%
      summarise(deficit = max(cfd, na.rm = T),
                TL = mean(TL_crit))
    
    sem_deficit <- rbind(sem_deficit, subset(pontos_deficit, deficit < 2500))
    
    com_deficit <- subset(pontos_deficit, deficit > 2500)
    
    
    out_dia <- merge(out_dia, com_deficit[,c(1:2)], by = "ponto_simulacao")
    out_dia <- out_dia%>%arrange(ponto_simulacao, data)
    out_dia$TL_crit <-  out_dia$TL_crit*0.95
    
    out_dia$Dt <- with(out_dia, TL_crit*CD/EPP)
    out_dia <- out_dia[,-14]
    
    out_dia$fd <- out_dia$Dt - out_dia$ganho_diario
    out_dia$fd <- ifelse(out_dia$fd < 0, 0, out_dia$fd)
    cfd <-  out_dia$Dt[1] - out_dia$ganho_diario[1]
    n <- nrow(com_deficit)
    print(sprintf("Restam %g pontos com déficit", n))
  }
  
  # Criando tabela final
  sem_deficit <- sem_deficit[,c(1,3)]
  colnames(sem_deficit)[2] <- sprintf("tl_%s",i)
  tl_critica <- merge(tl_critica, sem_deficit, by = "ponto_simulacao", all.x = TRUE)
}

# Exportando a tabela final
write.table(tl_critica, paste(path, "tabelas/taxa_lotacao_critica_cenarios_epp_variavel.csv", sep = "/"),
            row.names = F, sep = ";")







