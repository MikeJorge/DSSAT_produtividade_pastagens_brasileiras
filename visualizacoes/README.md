# Visualizações

Esta pasta contém os scripts necessários para gerar as visualizações dos resultados do modelo do DSSAT, além da conversão da produtividade de biomassa em taxa de lotação.

1_tabelas_biomassa_mensal_diaria.R
Script que gera as tabelas de produção diaria e mensal para o ano modelo e armazena no banco de dados. Ao executa-lo, deve-se alterar o diretorio na linha 20, para o endereço da pasta central do projeto.


2_taxa_lotacao_critica.R
Script que calcula a taxa de lotação crítica para cada ponto de simulação. Ao executa-lo, deve-se alterar a linha 15 com o endereço da pasta central do projeto.


3_area_pastagem_grid.R
Script que calcula a área de pastagem do LAPIG (2022) por célula do grid. Ao executa-lo, deve-se alterar a linha 15 com o endereço da pasta central do projeto.


4_graficos_mapas_DSSAT.R
Script que gera os gráficos e arquivos necessários para geração dos mapas. Ao executa-lo, alterar a linha 15 com o endereço da pasta central do projeto.

5_rasterize_gdal.txt
Linha de código que deve ser rodada no software OSGeo4WSHELL. Alterar o parametro -a manualmente, inserindo o nome da coluna que deseja rasterizar. Alterar os nomes do output com base no nome da coluna que está rasterizando.

6_rasters_taxa_lotacao.R
Script que gera os rasters de taxa de lotação no nível do pixel de pastagem do LAPIG 2022. Ao executa-lo, alterar a linha 15 com o endereço da pasta central do projeto.