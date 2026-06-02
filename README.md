# AgroRisk MS - Inteligência Atuarial 🌱

O **AgroRisk MS** é um sistema interativo de inteligência atuarial desenvolvido para a precificação de risco biológico, com foco na Ferrugem Asiática da Soja, no estado de Mato Grosso do Sul. A plataforma converte dados brutos geográficos, históricos e climáticos em um ecossistema visual para suporte à tomada de decisão.

🔗 **[Acessar a Aplicação Web](https://hosefbj21.shinyapps.io/Agrorisk/)**

## 🚀 O Pipeline do Projeto

O projeto foi estruturado em um fluxo de processamento de dados ponta a ponta (Pipeline) contendo 6 etapas principais:

1. **Fusão Geográfica e Histórica (Data Prep)**
   Integração da malha cartográfica digital de MS (geobr/IBGE) com o cálculo de centroides. Acoplamento de um banco de dados histórico da produção regional (2009–2024), reestruturado em formato Tidy Data.

2. **Motor Climático Global (NASA POWER API)**
   Captura automatizada e dinâmica de dados meteorológicos diários via satélites da NASA (radiação, temperatura e precipitação), com filtros de frequência de chuvas mapeados por safras climáticas (El Niño, La Niña e Neutro).

3. **Algoritmo Epidemiológico Estocástico (Modelo BR3)**
   Aplicação do modelo preditivo empírico de Del Ponte para a Ferrugem Asiática da Soja. A severidade foliar calculada é traduzida em equações atuariais de perda de produtividade física (em toneladas e sacas/ha) e no cálculo do Índice de Vulnerabilidade de Mercado.

4. **Validação em Tempo Real (API SIDRA/IBGE)**
   Motor de checagem cruzada que consome iterativamente a API oficial do SIDRA/IBGE, isolando variáveis reais de produção e área colhida para validar estatisticamente as predições do modelo.

5. **Análise Avançada & Clusterização (Machine Learning)**
   Execução do algoritmo de aprendizado não supervisionado K-Means para segmentação automática dos municípios em Perfis de Risco (Alto, Moderado, Baixo). Modelagem de regressão linear para detecção de tendências temporais.

6. **Interface de Decisão (Shiny UI/UX)**
   Consolidação dos dados em um ecossistema interativo de dashboards e mapas temáticos reativos (desenvolvido em R Shiny), oferecendo ferramentas práticas para precificação de seguro agrícola.

## 🛠️ Tecnologias Utilizadas

- **Linguagem Principal**: R
- **Interface e Dashboards**: R Shiny
- **Apresentação (Landing Page)**: HTML, CSS, JavaScript (na pasta `agrorisk-web`)
- **APIs Integradas**: NASA POWER API, SIDRA (IBGE), geobr
- **Modelagem e ML**: K-Means Clustering, Regressão Linear, Modelo BR3 Estocástico

## 📂 Estrutura Principal do Repositório

- `app.R`: Script principal da aplicação Shiny.
- `agrorisk-web/`: Contém a landing page com o descritivo visual do pipeline.
- `prepara_dados.R` (e auxiliares): Scripts para extração, limpeza e preparação dos dados brutos.
- Arquivos `.Rmd` / `.qmd`: Relatórios dinâmicos que compõem o estudo técnico.

## 💻 Como Executar Localmente

1. Clone este repositório:
   ```bash
   git clone https://github.com/SEU_USUARIO/SEU_REPOSITORIO.git
   ```
2. Abra o arquivo do projeto (`Hackaton.Rproj`) no RStudio.
3. Instale os pacotes e dependências do R listados no início do `app.R` (ex: `shiny`, `leaflet`, `dplyr`, `ggplot2`, etc).
4. Rode a aplicação Shiny clicando em **Run App** no RStudio, ou através do console:
   ```R
   shiny::runApp()
   ```

---
*Desenvolvido durante o Hackaton da disciplina FIP 606.*
