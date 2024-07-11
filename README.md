# DSSAT_produtividade_pastagens_brasileiras
# Rotinas de Processamento e Visualização do Crescimento da Forrageira Urochloa brizantha cv. Marandu
As rotinas de processamento descritas nesta pasta são essenciais para modelar e visualizar o crescimento da forrageira Urochloa brizantha cv. Marandu, calibrada para corresponder às condições de produção no ambiente tropical, conforme descrito por Pedreira et al. (2011), Pequeno et al. (2014), e Pequeno et al. (2018). Esses scripts permitem calcular a disponibilidade de pasto em diferentes cenários de fertilização nos biomas Amazônia, Cerrado e Mata Atlântica e, em sequência, a taxa de lotação do pasto em UA/ha, conforme descrito por Barioni (2020), Santos (2021) e Araujo (2024), adotada pelo MAPA (2024) no desenvolvimento do Zoneamento de Risco Climático da Pecuária (ZARC da Pecuária).

# Instruções para Uso

1. Verificação do DSSAT: Antes de clonar o repositório, verifique se o DSSAT está devidamente instalado no seu computador. Caso contrário, o instalador pode ser baixado no seguinte link: [Download do DSSAT](https://dssat.net/main-download/).

2. Clonagem do Repositório: Recomenda-se que a clonagem deste repositório seja feita dentro da pasta central do projeto, disponível neste link: [Pasta Central do Projeto](https://drive.google.com/drive/folders/1WcBbRi0yIjVeiidQ8hBD4Y7xME7NdfvU?usp=drive_link). 

3. Pasta "DSSAT": Nesta pasta estão contidos os arquivos e rotinas necessárias para rodar o DSSAT para os 9873 pontos do grid que possuem algum pixel de pastagem, segundo dados do LAPIG (2022).

4. Pasta "visualizacoes": Contém as rotinas necessárias para gerar tabelas, gráficos e mapas que permitem a visualização dos resultados do DSSAT para os diferentes cenários, regiões e biomas.

Essas rotinas são fundamentais para a análise detalhada e precisa dos dados de crescimento da forrageira, contribuindo para uma melhor compreensão e planejamento no contexto da bovinocultura em pasto em diferentes biomas brasileiros, bem como para a definição posterior do módulo mínimo através da integração e uso do modelo híbrido de simulação Módulo Mínimo (MHSMM) descrito por Jorge (2024), disponível neste link: [Módulo Mínimo](https://github.com/MikeJorge/modulo-minimo-MHSMM.git).

