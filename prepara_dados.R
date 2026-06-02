# prepara_dados.R
# Script para baixar e salvar a malha municipal do MS para carregamento rapido no app.

# Verifica se os pacotes necessarios estao instalados
if (!requireNamespace("geobr", quietly = TRUE)) install.packages("geobr", repos = "https://cloud.r-project.org")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf", repos = "https://cloud.r-project.org")

library(geobr)
library(sf)

print("Baixando malha municipal do Mato Grosso do Sul (2022)...")
malha_ms <- read_municipality(code_muni = "MS", year = 2022, showProgress = FALSE)

# Calcular centroides e adicionar as coordenadas, conforme original
centroides <- suppressWarnings(st_centroid(malha_ms))
coordenadas <- st_coordinates(centroides)
malha_ms$centroide_x <- coordenadas[, 1]
malha_ms$centroide_y <- coordenadas[, 2]

# Salvar como um arquivo .rds (formato nativo do R, rapido e comprimido)
saveRDS(malha_ms, "malha_ms.rds")

print("Malha salva com sucesso em 'malha_ms.rds'!")
