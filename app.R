library(shiny)
library(bslib)
library(dplyr)
library(sf)
library(leaflet)
library(ggplot2)
library(plotly)
library(DT)
library(readr)
library(bsicons)
library(scales)
library(shinycssloaders)
library(rmarkdown)

# --- 1. PREPARAÇÃO DOS DADOS INDEPENDENTES ---
if (!file.exists("dados_app.csv")) {
  stop("O arquivo dados_app.csv não foi encontrado. Por favor, execute os chunks do seu documento Quarto primeiro para gerá-lo.")
}

# Carrega os dados garantindo UTF-8
dados_risco <- read_csv("dados_app.csv", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

# CORREÇÃO DE ENCODING GLOBAL: Garante que a coluna de produção tenha nome padronizado
if ("produ_o_t" %in% colnames(dados_risco)) {
  names(dados_risco)[names(dados_risco) == "produ_o_t"] <- "producao_t"
}

# Tenta carregar a malha salva localmente, senão baixa com geobr
if (file.exists("malha_ms.rds")) {
  malha_ms <- readRDS("malha_ms.rds")
} else {
  if (!requireNamespace("geobr", quietly = TRUE)) {
    stop("O pacote 'geobr' é necessário para baixar a malha. Instale-o com install.packages('geobr').")
  }
  library(geobr)
  malha_ms <- geobr::read_municipality(code_muni = "MS", year = 2022, showProgress = FALSE)
  malha_ms <- st_transform(malha_ms, 4326)
  saveRDS(malha_ms, "malha_ms.rds")
}

if (st_crs(malha_ms)$epsg != 4326) {
  malha_ms <- st_transform(malha_ms, 4326)
}

safras_disponiveis <- sort(unique(dados_risco$safra), decreasing = TRUE)
max_chuva_historica <- max(dados_risco$chuva_acumulada_mm, na.rm = TRUE)

# --- 2. CSS PREMIUM AGRO ---
css_custom <- "
:root {
  --cor-primaria: #1B5E20;
  --cor-secundaria: #FBC02D;
  --cor-acento: #B71C1C;
  --cor-ferrugem-leve: #D84315;
  --cor-fundo: #FDFBF7;
  --cor-card: #FFFFFF;
  --cor-texto: #2C3E50;
  --cor-borda: #E0DBCF;
}
body {
  background-color: var(--cor-fundo) !important;
  background-image: radial-gradient(rgba(27, 94, 32, 0.04) 1px, transparent 1px);
  background-size: 24px 24px;
  color: var(--cor-texto) !important;
  font-family: 'Segoe UI', system-ui, sans-serif !important;
}
.navbar {
  background: linear-gradient(135deg, #1B5E20 0%, #558B2F 60%, #FBC02D 100%) !important;
  border-bottom: 4px solid var(--cor-ferrugem-leve) !important;
  box-shadow: 0 4px 20px rgba(27, 94, 32, 0.15) !important;
}
.navbar-brand {
  font-weight: 800 !important;
  color: #ffffff !important;
}
.nav-link {
  color: #E8F5E9 !important;
  font-weight: 600 !important;
  transition: all 0.2s ease !important;
}
.nav-link:hover, .nav-link.active {
  color: #ffffff !important;
  background: rgba(183, 28, 28, 0.3) !important;
  border-radius: 6px;
}
.accordion-item {
  border: 1px solid var(--cor-borda) !important;
  border-radius: 12px !important;
  margin-bottom: 12px !important;
  box-shadow: 0 4px 12px rgba(0,0,0,0.03) !important;
  overflow: hidden;
}
.accordion-button {
  background: #FFFFFF !important;
  color: var(--cor-primaria) !important;
  font-weight: 700 !important;
  border-left: 6px solid var(--cor-secundaria) !important;
}
.accordion-button:not(.collapsed) {
  background: rgba(27, 94, 32, 0.05) !important;
  color: var(--cor-primaria) !important;
  border-left: 6px solid var(--cor-ferrugem-leve) !important;
}
.accordion-body {
  background: #FAFAFA !important;
  border-top: 1px solid var(--cor-borda) !important;
}
.card {
  border: 1px solid var(--cor-borda) !important;
  border-radius: 14px !important;
  box-shadow: 0 4px 15px rgba(0,0,0,0.04) !important;
}
.card-header {
  background: linear-gradient(90deg, rgba(27,94,32,0.05) 0%, rgba(251,192,45,0.05) 100%) !important;
  color: var(--cor-primaria) !important;
  font-weight: 700 !important;
}
.btn-download-custom {
  background: linear-gradient(135deg, var(--cor-primaria), var(--cor-ferrugem-leve)) !important;
  color: white !important;
  border: none !important;
  border-radius: 8px !important;
  font-weight: 600 !important;
  width: 100%;
  box-shadow: 0 3px 8px rgba(0,0,0,0.15);
}
.btn-download-custom:hover {
  transform: translateY(-1px);
  box-shadow: 0 5px 12px rgba(0,0,0,0.25);
}
.flow-step {
  background: #FFF;
  border-left: 4px solid var(--cor-primaria);
  padding: 12px;
  margin: 6px 0;
  border-radius: 4px;
}
.glossary-term {
  background: #FFF;
  border-left: 4px solid var(--cor-ferrugem-leve);
  padding: 10px;
  margin: 6px 0;
  border-radius: 4px;
}
.box-diagnostico {
  background-color: #F9F7F1;
  border-left: 5px solid #1B5E20;
  padding: 15px;
  border-radius: 4px;
  margin-top: 10px;
}
.bslib-value-box {
  min-height: 110px !important;
  height: 110px !important;
  position: relative !important;
  overflow: hidden !important;
}
.bslib-value-box .value-box-grid {
  padding: 12px 15px !important;
  display: block !important;
}
.bslib-value-box .value-box-title {
  font-size: 0.82rem !important;
  font-weight: 700 !important;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  line-height: 1.2 !important;
  margin-bottom: 4px !important;
  position: relative;
  z-index: 2;
}
.bslib-value-box .value-box-value {
  font-size: 1.4rem !important;
  font-weight: 800 !important;
  position: relative;
  z-index: 2;
}
.bslib-value-box .value-box-showcase {
  position: absolute !important;
  right: 15px !important;
  top: 50% !important;
  transform: translateY(-50%) !important;
  font-size: 3rem !important;
  opacity: 0.2 !important;
  z-index: 1 !important;
}
.bslib-value-box .value-box-showcase svg {
  width: 1em !important;
  height: 1em !important;
}
/* ======================================================
   OVERVIEW — DASHBOARD EXECUTIVO PREMIUM
   ====================================================== */
.overview-hero {
  background: linear-gradient(135deg,#0D2137 0%,#0a3d62 35%,#1565C0 65%,#1B5E20 100%);
  min-height: 210px;
  position: relative;
  overflow: hidden;
  margin: -16px -16px 24px -16px;
  border-radius: 0 0 28px 28px;
}
.overview-hero::before {
  content: '';
  position: absolute;
  inset: 0;
  background-image:
    radial-gradient(circle at 15% 60%, rgba(21,101,192,0.35) 0%, transparent 45%),
    radial-gradient(circle at 85% 20%, rgba(46,125,50,0.35) 0%, transparent 45%),
    radial-gradient(circle at 50% 90%, rgba(251,140,0,0.15) 0%, transparent 40%);
}
.overview-hero::after {
  content: '';
  position: absolute;
  inset: 0;
  background-image: radial-gradient(rgba(255,255,255,0.04) 1px, transparent 1px);
  background-size: 28px 28px;
}
.hero-overlay {
  position: relative;
  z-index: 3;
  padding: 32px 44px;
  display: flex;
  flex-direction: column;
  align-items: flex-start;
}
.hero-badge {
  display: inline-block;
  background: rgba(251,140,0,0.22);
  border: 1px solid rgba(251,140,0,0.5);
  color: #FFB74D;
  font-size: 0.7rem;
  font-weight: 800;
  letter-spacing: 2.5px;
  padding: 4px 14px;
  border-radius: 20px;
  margin-bottom: 12px;
  text-transform: uppercase;
}
.hero-title {
  font-size: 1.85rem !important;
  font-weight: 900 !important;
  color: #ffffff !important;
  margin: 0 0 8px 0 !important;
  line-height: 1.2 !important;
  text-shadow: 0 2px 24px rgba(0,0,0,0.35);
}
.hero-subtitle {
  font-size: 1.0rem;
  color: rgba(255,255,255,0.78);
  margin: 0 0 20px 0;
  font-weight: 400;
}
.hero-chips { display: flex; gap: 10px; flex-wrap: wrap; }
.chip {
  display: inline-flex; align-items: center; gap: 5px;
  padding: 5px 14px; border-radius: 20px;
  font-size: 0.78rem; font-weight: 600;
  backdrop-filter: blur(10px);
  transition: transform 0.2s;
}
.chip:hover { transform: translateY(-2px); }
.chip-green  { background: rgba(46,125,50,0.38);  color: #A5D6A7; border: 1px solid rgba(46,125,50,0.5); }
.chip-blue   { background: rgba(21,101,192,0.38); color: #90CAF9; border: 1px solid rgba(21,101,192,0.5); }
.chip-orange { background: rgba(251,140,0,0.38);  color: #FFE082; border: 1px solid rgba(251,140,0,0.5); }
.chip-dark   { background: rgba(255,255,255,0.1); color: #E0E0E0; border: 1px solid rgba(255,255,255,0.2); }
/* KPI Section */
.section-kpi { margin-bottom: 22px; }
.kpi-card {
  background: #ffffff;
  border-radius: 16px !important;
  border: 1px solid rgba(0,0,0,0.06) !important;
  box-shadow: 0 4px 20px rgba(0,0,0,0.06) !important;
  padding: 16px 14px;
  position: relative;
  overflow: hidden;
  transition: transform 0.22s ease, box-shadow 0.22s ease;
  cursor: default;
  min-height: 118px;
}
.kpi-card:hover {
  transform: translateY(-3px);
  box-shadow: 0 10px 32px rgba(0,0,0,0.12) !important;
}
.kpi-card::before {
  content: ''; position: absolute;
  top: 0; left: 0; right: 0; height: 3px;
  border-radius: 16px 16px 0 0;
}
.kpi-card.kpi-green::before  { background: linear-gradient(90deg,#2E7D32,#66BB6A); }
.kpi-card.kpi-blue::before   { background: linear-gradient(90deg,#1565C0,#42A5F5); }
.kpi-card.kpi-orange::before { background: linear-gradient(90deg,#E65100,#FB8C00); }
.kpi-card.kpi-red::before    { background: linear-gradient(90deg,#B71C1C,#EF5350); }
.kpi-card.kpi-teal::before   { background: linear-gradient(90deg,#00695C,#26A69A); }
.kpi-card.kpi-purple::before { background: linear-gradient(90deg,#4A148C,#9C27B0); }
.kpi-icon {
  font-size: 2.4rem; opacity: 0.1;
  position: absolute; right: 12px; top: 50%;
  transform: translateY(-50%);
}
.kpi-label {
  font-size: 0.68rem; font-weight: 700;
  text-transform: uppercase; letter-spacing: 1px;
  color: #78909C; margin-bottom: 6px;
}
.kpi-value {
  font-size: 1.4rem; font-weight: 900;
  color: #263238; line-height: 1; margin-bottom: 5px;
}
.kpi-trend { font-size: 0.74rem; font-weight: 600; color: #90A4AE; }
/* Pipeline */
.section-pipeline {
  background: linear-gradient(135deg,#F8F9FA 0%,#EEF2F7 100%);
  border-radius: 20px;
  padding: 26px 30px;
  margin-bottom: 22px;
  border: 1px solid rgba(21,101,192,0.1);
}
.section-title {
  font-size: 1.25rem !important; font-weight: 800 !important;
  color: #1565C0 !important; margin-bottom: 3px !important;
}
.section-subtitle { color: #607D8B; font-size: 0.86rem; margin-bottom: 22px; }
.pipeline-container {
  display: flex; align-items: center;
  justify-content: space-between;
  gap: 4px; overflow-x: auto; padding-bottom: 6px;
}
.pipeline-step {
  display: flex; flex-direction: column;
  align-items: center; text-align: center; min-width: 96px;
}
.pipeline-arrow { color: #90A4AE; font-size: 1.3rem; flex-shrink: 0; }
.hex-icon {
  width: 58px; height: 58px;
  clip-path: polygon(50% 0%,100% 25%,100% 75%,50% 100%,0% 75%,0% 25%);
  display: flex; align-items: center; justify-content: center;
  font-size: 1.35rem; color: white; margin-bottom: 8px;
  transition: transform 0.3s ease;
}
.hex-icon:hover { transform: scale(1.12) rotate(5deg); }
.hex-blue   { background: linear-gradient(135deg,#1565C0,#42A5F5); }
.hex-teal   { background: linear-gradient(135deg,#00695C,#26A69A); }
.hex-green  { background: linear-gradient(135deg,#2E7D32,#66BB6A); }
.hex-orange { background: linear-gradient(135deg,#E65100,#FB8C00); }
.hex-purple { background: linear-gradient(135deg,#4A148C,#9C27B0); }
.hex-dark   { background: linear-gradient(135deg,#263238,#546E7A); }
.step-label { font-size: 0.76rem; font-weight: 700; color: #37474F; margin-bottom: 2px; }
.step-desc  { font-size: 0.66rem; color: #90A4AE; line-height: 1.3; max-width: 98px; }
/* Overview Cards */
.overview-card {
  border-radius: 16px !important;
  border: 1px solid rgba(0,0,0,0.06) !important;
  box-shadow: 0 4px 20px rgba(0,0,0,0.05) !important;
  transition: box-shadow 0.22s ease;
}
.overview-card:hover { box-shadow: 0 8px 34px rgba(0,0,0,0.10) !important; }
.premium-card-header {
  background: linear-gradient(90deg,rgba(21,101,192,0.05) 0%,rgba(46,125,50,0.05) 100%) !important;
  border-bottom: 2px solid rgba(21,101,192,0.12) !important;
  font-weight: 700 !important; color: #1565C0 !important;
  display: flex !important; align-items: center !important;
  justify-content: space-between !important;
}
.header-badge {
  background: linear-gradient(135deg,#1565C0,#42A5F5);
  color: white; font-size: 0.66rem;
  padding: 2px 10px; border-radius: 10px; font-weight: 700;
}
/* ML Panel */
.ml-metric-card {
  background: linear-gradient(135deg,#F8F9FA,#EEF2F7);
  border-radius: 12px; padding: 12px 14px; margin-bottom: 8px;
  border: 1px solid rgba(21,101,192,0.1);
  display: flex; align-items: center; gap: 12px;
  transition: background 0.2s;
}
.ml-metric-card:hover { background: linear-gradient(135deg,#EEF2F7,#E3EAF5); }
.ml-metric-icon {
  font-size: 1.5rem; width: 42px; height: 42px; border-radius: 10px;
  display: flex; align-items: center; justify-content: center; flex-shrink: 0;
}
.ml-metric-icon.iblue   { background: rgba(21,101,192,0.12); color: #1565C0; }
.ml-metric-icon.igreen  { background: rgba(46,125,50,0.12);  color: #2E7D32; }
.ml-metric-icon.iorange { background: rgba(251,140,0,0.12);  color: #FB8C00; }
.ml-metric-label { font-size: 0.68rem; color: #78909C; font-weight: 700; text-transform: uppercase; letter-spacing: 0.5px; }
.ml-metric-value { font-size: 1.05rem; font-weight: 800; color: #263238; }
/* Pricing Table */
.pricing-table { width: 100%; border-collapse: separate; border-spacing: 0 5px; }
.pricing-row { background: #F8F9FA; border-radius: 10px; transition: background 0.2s; }
.pricing-row:hover { background: #EEF2F7; }
.pricing-row td { padding: 9px 13px; font-size: 0.82rem; }
.pricing-row td:first-child { border-radius: 10px 0 0 10px; font-weight: 600; color: #455A64; }
.pricing-row td:last-child  { border-radius: 0 10px 10px 0; }
.badge-low    { background: rgba(46,125,50,0.14);  color: #2E7D32; padding: 3px 10px; border-radius: 8px; font-weight: 700; font-size: 0.76rem; }
.badge-medium { background: rgba(251,140,0,0.14);  color: #E65100; padding: 3px 10px; border-radius: 8px; font-weight: 700; font-size: 0.76rem; }
.badge-high   { background: rgba(183,28,28,0.14);  color: #B71C1C; padding: 3px 10px; border-radius: 8px; font-weight: 700; font-size: 0.76rem; }
/* ---- Pipeline Clickable ---- */
.pipeline-step {
  cursor: pointer;
  transition: opacity 0.2s;
}
.pipeline-step:hover { opacity: 0.85; }
.pipeline-step:hover .hex-icon {
  transform: scale(1.18) rotate(8deg) !important;
  box-shadow: 0 8px 28px rgba(0,0,0,0.25) !important;
}
.pipeline-step:hover .step-label { color: #1565C0 !important; }
.pipeline-step:hover .step-desc  { color: #455A64 !important; }
.step-click-hint {
  font-size: 0.6rem; color: #B0BEC5; font-weight: 700;
  letter-spacing: 0.4px; margin-top: 4px;
  opacity: 0; transition: opacity 0.25s;
}
.pipeline-step:hover .step-click-hint { opacity: 1; }
/* ---- Premium Pipeline Modals ---- */
.pm-header {
  margin: -15px -15px 18px -15px;
  padding: 22px 26px;
  position: relative; overflow: hidden;
  border-radius: 4px 4px 0 0;
}
.pm-header.pm-blue   { background: linear-gradient(135deg,#0D2137,#1565C0); }
.pm-header.pm-teal   { background: linear-gradient(135deg,#003d33,#00695C); }
.pm-header.pm-green  { background: linear-gradient(135deg,#1a3a1c,#2E7D32); }
.pm-header.pm-orange { background: linear-gradient(135deg,#3d1a00,#E65100); }
.pm-header.pm-purple { background: linear-gradient(135deg,#1a0a2e,#4A148C); }
.pm-header.pm-dark   { background: linear-gradient(135deg,#0d1117,#263238); }
.pm-header::after {
  content: '';
  position: absolute; inset: 0;
  background-image: radial-gradient(rgba(255,255,255,0.04) 1px, transparent 1px);
  background-size: 22px 22px;
  pointer-events: none;
}
.pm-number {
  position: absolute; right: 18px; top: 4px;
  font-size: 5rem; font-weight: 900;
  color: rgba(255,255,255,0.07); line-height: 1;
  user-select: none; z-index: 1;
}
.pm-badge {
  display: inline-block;
  background: rgba(255,255,255,0.15);
  color: rgba(255,255,255,0.92);
  font-size: 0.62rem; font-weight: 800;
  letter-spacing: 2px; padding: 3px 12px;
  border-radius: 20px; margin-bottom: 10px;
  text-transform: uppercase;
  border: 1px solid rgba(255,255,255,0.2);
  position: relative; z-index: 2;
}
.pm-title {
  color: #ffffff !important; font-weight: 900 !important;
  font-size: 1.3rem !important; margin: 2px 0 6px 0 !important;
  position: relative; z-index: 2;
}
.pm-subtitle {
  color: rgba(255,255,255,0.72); font-size: 0.82rem;
  margin: 0 !important; position: relative; z-index: 2;
}
.pm-stat {
  background: linear-gradient(135deg,#F8F9FA,#EEF2F7);
  border-radius: 12px; padding: 13px 10px; text-align: center;
  border: 1px solid rgba(21,101,192,0.1); margin-bottom: 14px;
}
.pm-stat-val {
  font-size: 1.35rem; font-weight: 900;
  color: #1565C0; line-height: 1; margin-bottom: 3px;
}
.pm-stat-lbl {
  font-size: 0.63rem; font-weight: 700;
  text-transform: uppercase; letter-spacing: 0.5px; color: #78909C;
}
.pm-card {
  background: white; border-radius: 12px;
  padding: 12px 15px; border-left: 4px solid #1565C0;
  box-shadow: 0 2px 10px rgba(0,0,0,0.05); margin-bottom: 9px;
}
.pm-card.gc { border-left-color: #2E7D32; }
.pm-card.oc { border-left-color: #FB8C00; }
.pm-card.pc { border-left-color: #7B1FA2; }
.pm-card.rc { border-left-color: #B71C1C; }
.pm-formula {
  background: #0D1117; color: #A5D6A7;
  border-radius: 10px; padding: 14px 16px;
  font-family: 'Courier New', monospace;
  font-size: 0.8rem; line-height: 1.75;
  margin: 10px 0;
  border: 1px solid rgba(165,214,167,0.15);
  overflow-x: auto;
  white-space: pre;
}
.pm-tags-wrap { margin-top: 10px; }
.pm-tag {
  display: inline-block; padding: 3px 10px;
  border-radius: 8px; font-size: 0.7rem;
  font-weight: 700; margin: 2px;
}
.pm-tag.bt { background: rgba(21,101,192,0.1);  color: #1565C0; }
.pm-tag.gt { background: rgba(46,125,50,0.1);   color: #2E7D32; }
.pm-tag.ot { background: rgba(251,140,0,0.1);   color: #E65100; }
.pm-tag.rt { background: rgba(183,28,28,0.1);   color: #B71C1C; }
.pm-tag.pt { background: rgba(74,20,140,0.1);   color: #6A1B9A; }
"

# --- 3. INTERFACE DE USUÁRIO (UI) ---
addResourcePath("agrorisk_web", "agrorisk-web")
# addResourcePath("brain_images", "C:/Users/jhose/.gemini/antigravity/brain/e7037c63-aa7c-4437-987f-2a570cb85892")
addResourcePath("base_dir", ".")

ui <- page_navbar(
  id = "nav_principal",
  title = tags$span(bs_icon("virus"), "FIP 606 AgroRisk MS — Monitoramento de Ferrugem"),
  theme = bs_theme(
    version = 5,
    preset = "minty",
    primary = "#1B5E20",
    secondary = "#FBC02D",
    danger = "#B71C1C"
  ),
  header = tags$head(tags$style(HTML(css_custom))),
  
  sidebar = sidebar(
    id = "sidebar_global",
    title = tags$span(bs_icon("sliders"), " Parâmetros"),
    width = 300,
    
    # Condicional para as abas Clínico e Rankings
    conditionalPanel(
      condition = "input.nav_principal == 'painel_clinico' || input.nav_principal == 'ranking'",
      selectInput("tipo_analise", "Abordagem de Análise:", 
                  choices = c("Safra Específica", "Cenário de Estresse (ENSO)"), 
                  selected = "Safra Específica"),
      
      conditionalPanel(
        condition = "input.tipo_analise == 'Safra Específica'",
        selectInput("safra_selecionada", "Safra Analisada:", choices = safras_disponiveis, selected = safras_disponiveis[1])
      ),
      
      conditionalPanel(
        condition = "input.tipo_analise == 'Cenário de Estresse (ENSO)'",
        selectInput("fase_selecionada", "Macroclima (Fase do Oceano):", 
                    choices = c("El Niño", "La Niña", "Neutro"), selected = "El Niño")
      ),
      
      numericInput("preco_saca", "Preço da Saca 60kg (R$):", value = 135.00, min = 50, max = 300, step = 5),
      hr(),
      selectizeInput("muni_selecionado", "Município(s):", choices = c("Todos", sort(unique(dados_risco$municipio))), selected = "Todos", multiple = TRUE),
      hr(),
      sliderInput("filtro_precipitacao", "Chuva acumulada in Janeiro (mm):", min = 0, max = 400, value = c(0, 400), step = 10),
      hr(),
      downloadButton("download_csv", "Exportar Tabela (.csv)", class = "btn-download-custom")
    ),
    
    # Condicional para a aba Histórico
    conditionalPanel(
      condition = "input.nav_principal == 'historico'",
      tags$h6("Filtros do Histórico", style = "font-weight:700; color:var(--cor-primaria); text-transform:uppercase;"),
      p("Ajuste os parâmetros para a análise temporal detalhada.", style = "font-size:0.85rem; color:#666;"),
      hr(),
      selectInput("muni_historico", "Selecione o Município para a Linha:", 
                  choices = sort(unique(dados_risco$municipio)), 
                  selected = sort(unique(dados_risco$municipio))[1]),
      br(),
      selectInput("fase_historico", "Filtro de Macroclima (ENSO):", 
                  choices = c("Histórico Completo", "El Niño", "La Niña", "Neutro"), 
                  selected = "Histórico Completo")
    ),
    
    # Condicional para a aba Clima & Produção
    conditionalPanel(
      condition = "input.nav_principal == 'analise_clima_producao'",
      tags$h6("Análise Local Histórica", style = "font-weight:700; color:var(--cor-primaria); text-transform:uppercase;"),
      p("Monitore variáveis climáticas cruciais cruzadas com o volume de safra de uma cidade.", style = "font-size:0.85rem; color:#666;"),
      hr(),
      selectInput("muni_clima_prod", "Escolha o Município Foco:", 
                  choices = c("Mato Grosso do Sul (Estado)", sort(unique(dados_risco$municipio))), 
                  selected = "Mato Grosso do Sul (Estado)"),
      hr(),
      checkboxInput("mostrar_prod_real", "Verificar Impacto na Produtividade Real (IBGE)", value = FALSE)
    ),
    
    conditionalPanel(
      condition = "input.nav_principal == 'comparador'",
      tags$h6("Modo Comparador", style = "font-weight:700; color:var(--cor-primaria); text-transform:uppercase;"),
      p("Configure os confrontos de safra diretamente nos painéis internos da aba central.", style = "font-size:0.85rem; color:#666;")
    )
  ),
  
  # ===== ABA VISÃO GERAL — DASHBOARD EXECUTIVO PREMIUM =====
  nav_panel(
    value = "home_apresentacao",
    title = tags$span(bs_icon("house"), " Visão Geral"),
    
    # --- HERO SECTION ---
    tags$div(class = "overview-hero",
      tags$div(class = "hero-overlay",
        tags$div(class = "hero-content",
          tags$div(class = "hero-badge", "SISTEMA ATUARIAL — AGRORISK MS"),
          tags$h1(class = "hero-title",
            "Pipeline de Inteligência Atuarial ",
            tags$span(style = "color:#FFB74D;", "AgroRisk MS")
          ),
          tags$p(class = "hero-subtitle",
            "Do dado bruto à precificação de risco biológico em minutos."
          ),
          tags$div(class = "hero-chips",
            tags$span(class = "chip chip-green", bs_icon("geo-alt"), " Mato Grosso do Sul"),
            tags$span(class = "chip chip-blue",  bs_icon("cloud-sun"), " NASA POWER / MERRA-2"),
            tags$span(class = "chip chip-orange",bs_icon("cpu"),       " Machine Learning"),
            tags$span(class = "chip chip-dark",  bs_icon("shield-check"), " Análise Atuarial")
          )
        )
      )
    ),
    
    # --- Script: Pipeline Clicável ---
    tags$script(HTML("
      $(document).on('click', '.pipeline-step', function() {
        var steps = $('.pipeline-container .pipeline-step');
        var idx   = steps.index(this) + 1;
        if (idx > 0 && typeof Shiny !== 'undefined') {
          Shiny.setInputValue('pipeline_click', String(idx), {priority: 'event'});
        }
      });
    ")),
    
    # --- KPI CARDS ---
    tags$div(class = "section-kpi",
      layout_columns(
        col_widths = c(2, 2, 2, 2, 2, 2),
        fill = FALSE,
        uiOutput("ov_kpi_municipios"),
        uiOutput("ov_kpi_registros"),
        uiOutput("ov_kpi_cenario"),
        uiOutput("ov_kpi_vulnerabilidade"),
        uiOutput("ov_kpi_alto_risco"),
        uiOutput("ov_kpi_potencial")
      )
    ),
    
    # --- FLUXO DO PIPELINE ---
    tags$div(class = "section-pipeline",
      tags$h2(class = "section-title", bs_icon("diagram-3"), " Fluxo do Pipeline"),
      tags$p(class = "section-subtitle",
        "Da captura geoespacial até a precificação de risco — pipeline automatizado end-to-end"
      ),
      tags$div(class = "pipeline-container",
        tags$div(class = "pipeline-step",
          tags$div(class = "hex-icon hex-blue",   bs_icon("geo-alt-fill")),
          tags$div(class = "step-label", "Dados Geográficos"),
          tags$div(class = "step-desc",  "Malha municipal MS via geobr")
        ),
        tags$div(class = "pipeline-arrow", "\u25bc"),
        tags$div(class = "pipeline-step",
          tags$div(class = "hex-icon hex-teal",   bs_icon("cloud-download")),
          tags$div(class = "step-label", "Clima NASA POWER"),
          tags$div(class = "step-desc",  "Sat\u00e9lite MERRA-2 Jan")
        ),
        tags$div(class = "pipeline-arrow", "\u25bc"),
        tags$div(class = "pipeline-step",
          tags$div(class = "hex-icon hex-green",  bs_icon("virus")),
          tags$div(class = "step-label", "Modelo BR3"),
          tags$div(class = "step-desc",  "Del Ponte — Severidade")
        ),
        tags$div(class = "pipeline-arrow", "\u25bc"),
        tags$div(class = "pipeline-step",
          tags$div(class = "hex-icon hex-orange", bs_icon("table")),
          tags$div(class = "step-label", "Valida\u00e7\u00e3o SIDRA"),
          tags$div(class = "step-desc",  "Produ\u00e7\u00e3o IBGE (t)")
        ),
        tags$div(class = "pipeline-arrow", "\u25bc"),
        tags$div(class = "pipeline-step",
          tags$div(class = "hex-icon hex-purple", bs_icon("cpu")),
          tags$div(class = "step-label", "Machine Learning"),
          tags$div(class = "step-desc",  "K-Means — Clusters")
        ),
        tags$div(class = "pipeline-arrow", "\u25bc"),
        tags$div(class = "pipeline-step",
          tags$div(class = "hex-icon hex-dark",   bs_icon("speedometer2")),
          tags$div(class = "step-label", "Dashboard"),
          tags$div(class = "step-desc",  "Precifica\u00e7\u00e3o & Underwriting")
        )
      )
    ),
    
    # --- MAPA PRINCIPAL + CLUSTERS + ENSO ---
    layout_columns(
      col_widths = c(8, 4),
      card(
        class = "overview-card",
        full_screen = TRUE,
        card_header(
          class = "premium-card-header",
          tags$span(bs_icon("map-fill"), " Mapa de Risco — Mato Grosso do Sul"),
          tags$span(class = "header-badge", "\u00daltima Safra")
        ),
        withSpinner(leafletOutput("mapa_overview", height = "460px"), type = 6, color = "#1565C0")
      ),
      layout_column_wrap(
        width = 1,
        card(
          class = "overview-card",
          full_screen = TRUE,
          card_header(
            class = "premium-card-header",
            tags$span(bs_icon("pie-chart-fill"), " Clusters de Risco (K-Means)")
          ),
          withSpinner(plotlyOutput("grafico_cluster_overview", height = "195px"), type = 6, color = "#1565C0")
        ),
        card(
          class = "overview-card",
          full_screen = TRUE,
          card_header(
            class = "premium-card-header",
            tags$span(bs_icon("cloud-sun-fill"), " Cen\u00e1rios Clim\u00e1ticos ENSO")
          ),
          withSpinner(plotlyOutput("grafico_cenarios_overview", height = "195px"), type = 6, color = "#1565C0")
        )
      )
    ),
    
    # --- PAINEL ANAL\u00cdTICO ---
    layout_columns(
      col_widths = c(6, 6),
      card(
        class = "overview-card",
        full_screen = TRUE,
        card_header(
          class = "premium-card-header",
          tags$span(bs_icon("bar-chart-fill"), " Distribui\u00e7\u00e3o de Risco por Munic\u00edpio (Top 20)")
        ),
        withSpinner(plotlyOutput("grafico_dist_risco", height = "320px"), type = 6, color = "#1565C0")
      ),
      card(
        class = "overview-card",
        full_screen = TRUE,
        card_header(
          class = "premium-card-header",
          tags$span(bs_icon("graph-up"), " Produ\u00e7\u00e3o \u00d7 Vulnerabilidade")
        ),
        withSpinner(plotlyOutput("grafico_scatter_overview", height = "320px"), type = 6, color = "#1565C0")
      )
    ),
    
    # --- ML + PRECIFICA\u00c7\u00c3O ---
    layout_columns(
      col_widths = c(5, 7),
      card(
        class = "overview-card",
        card_header(
          class = "premium-card-header",
          tags$span(bs_icon("lightning-charge-fill"), " Painel de Machine Learning")
        ),
        withSpinner(uiOutput("ov_ml_panel"), type = 6, color = "#1565C0")
      ),
      card(
        class = "overview-card",
        full_screen = TRUE,
        card_header(
          class = "premium-card-header",
          tags$span(bs_icon("shield-lock-fill"), " Precifica\u00e7\u00e3o Atuarial — Seguro Agr\u00edcola")
        ),
        withSpinner(uiOutput("ov_pricing_panel"), type = 6, color = "#1565C0")
      )
    )
  ),
  
  # ===== ABA 1 =====
  nav_panel(
    value = "painel_clinico",
    title = tags$span(bs_icon("shield-shaded"), " Painel Clínico"),
    layout_columns(
      fill = FALSE, col_widths = c(3, 3, 3, 3),
      uiOutput("vb_perda_ui"), uiOutput("vb_dinheiro_ui"), uiOutput("vb_vuln_ui"), uiOutput("vb_precip_ui")
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(
        full_screen = TRUE,
        card_header(tags$span(bs_icon("map"), " Zoneamento de Vulnerabilidade da Ferrugem")),
        withSpinner(leafletOutput("mapa", height = "500px"), type = 6, color = "#1B5E20")
      ),
      card(
        full_screen = TRUE,
        card_header(tags$span(bs_icon("graph-up"), " Matriz Atuarial de Dispersão")),
        withSpinner(plotlyOutput("grafico_dispersao", height = "500px"), type = 6, color = "#1B5E20")
      )
    )
  ),
  
  # ===== ABA 2 =====
  nav_panel(
    value = "ranking",
    title = tags$span(bs_icon("list-ol"), " Índices & Rankings"),
    card(
      full_screen = TRUE,
      card_header("Classificação de Vulnerabilidade por Safra"),
      withSpinner(DTOutput("tabela_dados"), type = 6, color = "#1B5E20")
    )
  ),
  
  # ===== ABA 3 =====
  nav_panel(
    value = "historico",
    title = tags$span(bs_icon("calendar-range"), " Histórico"),
    layout_columns(
      col_widths = c(12),
      card(
        full_screen = TRUE,
        card_header("Evolução Temporal da Vulnerabilidade (Top 15 Cidades Gerais)"),
        withSpinner(plotlyOutput("heatmap_temporal", height = "400px"), type = 6, color = "#1B5E20")
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        full_screen = TRUE,
        card_header(uiOutput("titulo_grafico_linha")),
        withSpinner(plotlyOutput("grafico_linha", height = "350px"), type = 6, color = "#1B5E20")
      )
    )
  ),
  
  # ===== ABA CLIMA & PRODUÇÃO =====
  nav_panel(
    value = "analise_clima_producao",
    title = tags$span(bs_icon("cloud-sun-fill"), " Clima & Produção"),
    layout_columns(
      col_widths = c(12),
      card(
        full_screen = TRUE,
        card_header(uiOutput("titulo_aba_clima_prod")),
        p("Cruzamento analítico temporal das condições ambientais de janeiro (coletadas via NASA POWER) com a resposta de produção municipal (IBGE).", 
          style = "font-size:0.9rem; color:#555; padding-left:15px; margin-top:5px;"),
        withSpinner(plotlyOutput("grafico_triplo_clima_prod", height = "550px"), type = 6, color = "#1B5E20")
      )
    ),
    conditionalPanel(
      condition = "input.mostrar_prod_real == true",
      layout_columns(
        col_widths = c(12),
        card(
          full_screen = TRUE,
          card_header(tags$span(bs_icon("graph-up-arrow"), " Validação: Produtividade Real (IBGE) vs Severidade Estimada")),
          p("Gráfico comparativo entre a produtividade real em sacas por hectare e as estimativas do modelo (Severidade Estimada).", 
            style = "font-size:0.9rem; color:#555; padding-left:15px; margin-top:5px;"),
          withSpinner(plotlyOutput("grafico_prod_real", height = "400px"), type = 6, color = "#1B5E20")
        )
      )
    )
  ),
  
  # ===== ABA 4 =====
  nav_panel(
    value = "comparador",
    title = tags$span(bs_icon("arrow-left-right"), " Comparador"),
    layout_columns(
      col_widths = c(6, 6), fill = FALSE,
      card(
        card_header("Definição de Filtros para Confronto"),
        layout_columns(
          selectInput("safra_comp_1", "Safra Base (A):", choices = safras_disponiveis, selected = safras_disponiveis[1]),
          selectInput("safra_comp_2", "Safra Alvo (B):", choices = safras_disponiveis, selected = safras_disponiveis[min(2, length(safras_disponiveis))])
        ),
        selectizeInput("muni_comp", "Filtrar Município(s):", 
                       choices = c("Todos", sort(unique(dados_risco$municipio))), 
                       selected = "Todos", multiple = TRUE)
      ),
      uiOutput("vb_comparativo_ui")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        full_screen = TRUE,
        card_header(uiOutput("titulo_graf_comp_sev")),
        withSpinner(plotlyOutput("grafico_comparativo", height = "450px"), type = 6, color = "#1B5E20")
      ),
      card(
        full_screen = TRUE,
        card_header(uiOutput("titulo_graf_comp_prec")),
        withSpinner(plotlyOutput("grafico_precip_comp", height = "450px"), type = 6, color = "#1B5E20")
      )
    )
  ),
  
  # ===== ABA 5 =====
  nav_panel(
    value = "perfil_local",
    title = tags$span(bs_icon("pie-chart"), " Perfil Local"),
    layout_columns(
      col_widths = c(4, 8),
      layout_column_wrap(
        width = 1,
        card(
          card_header(tags$span(bs_icon("funnel"), " Filtro Local")),
          selectInput("muni_radar", "Município Foco:", choices = sort(unique(dados_risco$municipio))),
          selectInput("safra_radar", "Safra Foco:", choices = safras_disponiveis),
          hr(),
          uiOutput("resSummary_muni_ui")
        ),
        card(
          card_header(tags$span(bs_icon("info-circle"), " Guia de Leitura do Radar")),
          markdown("
          Este gráfico avalia o balanço de **três dimensões críticas**:
          * **Severidade (%):** Impacto clínico foliar real (biológico).
          * **Chuva Relativa:** O quão chuvoso janeiro foi nesta cidade comparado ao máximo histórico registrado no MS.
          * **Vulnerabilidade:** A nota de risco final (que pondera a doença clínica com a produção total da cidade).
          ")
        )
      ),
      card(
        full_screen = TRUE,
        card_header(tags$span(bs_icon("activity"), " Indicadores Multidimensionais & Diagnóstico")),
        withSpinner(plotlyOutput("radar_chart", height = "380px"), type = 6, color = "#1B5E20"),
        hr(),
        uiOutput("diagnostico_radar_ui")
      )
    )
  ),
  
  # ===== ABA 6: DOCUMENTAÇÃO & GUIA INTEGRADA COM O RELATÓRIO PDF =====
  nav_panel(
    value = "documentacao",
    title = tags$span(bs_icon("book"), " Documentação & Guia"),
    fluidPage(
      # Habilita suporte ao MathJax para renderizar equações científicas em LaTeX
      withMathJax(),
      
      layout_columns(
        col_widths = c(12),
        accordion(
          id = "documentacao_accordion", multiple = TRUE, open = c("1. Mapeamento do Fluxo Operacional"),
          
          # Painel 1 (Mantido e unificado)
          accordion_panel(
            title = "1. Mapeamento do Fluxo Operacional", icon = bs_icon("diagram-3"),
            tags$div(class = "flow-step", tags$strong("Etapa A — Captura Espacial:"), " Importação da malha territorial municipal (geobr)."),
            tags$div(class = "flow-step", tags$strong("Etapa B — Coleta Climatológica:"), " Chuva e Temperatura Média de Janeiro via satélite (NASA POWER API)."),
            tags$div(class = "flow-step", tags$strong("Etapa C — Processamento Clínico:"), " Algoritmo preditivo Del Ponte BR3."),
            tags$div(class = "flow-step", tags$strong("Etapa D — Ponderação Comercial:"), " Cruzamento com toneladas produzidas (IBGE/SIDRA).")
          ),
          
          # Painel 2: Sumário Executivo do Relatório
          accordion_panel(
            title = "2. Sumário Executivo do Relatório Técnico", icon = bs_icon("file-earmark-text"),
            tags$div(
              style = "line-height: 1.6; color: #2C3E50;",
              tags$h4("Mapeamento Técnico e Atuarial do Ecossistema AgroRisk MS", style = "color: #1B5E20; font-weight: bold;"),
              tags$p("Este relatório apresenta o mapeamento detalhado e o diagnóstico analítico do ecossistema computacional desenvolvido nos scripts. O que foi construído não é apenas um processamento de dados linear, mas sim um pipeline de Inteligência Agroclimática e Análise Atuarial de Riscos de Ponta."),
              tags$p("O sistema integra de forma nativa a extração de dados geoespaciais oficiais, o consumo automatizado de sensoriamento remoto por satélite, equações preditivas fitopatológicas e algoritmos de aprendizado de máquina não supervisionado (Machine Learning). O objetivo prático é traduzir o comportamento de variáveis ambientais (clima e umidade) em indicadores palpáveis de perda física de produção (toneladas) e exposição econômico-financeira (R$).")
            )
          ),
          
          # Painel 3: Engenharia e Arquitetura do Pipeline
          accordion_panel(
            title = "3. Engenharia e Arquitetura do Pipeline de Dados", icon = bs_icon("cpu"),
            tags$div(
              style = "line-height: 1.6;",
              tags$p("O motor computacional executa uma sequência rigorosa de etapas de captação, tratamento, modelagem e disponibilização de dados:"),
              tags$ul(
                tags$li(tags$strong("Fase 1 - Modelagem Territorial e Geodésia:"), " Utilizando o pacote geobr, o pipeline faz a ingestão da malha municipal oficial do Estado do Mato Grosso do Sul (ano de referência 2022). O algoritmo calcula os centroides geométricos exatos de cada polígono municipal, extraindo as coordenadas de Longitude (centroide_x) e Latitude (centroide_y) que servirão de âncoras para as chamadas climáticas."),
                tags$li(tags$strong("Fase 2 - Ingestão e Limpeza Histórica (IBGE):"), " O código limpa e padroniza a planilha histórica de produtividade (2009-2024), aplicando o método pivot_longer para remodelar os dados do formato de matriz larga ('wide') para o formato de série longa ('long'). Textos de rodapés, caracteres especiais de omissão de dados (como '-' e '...') e sufixos repetitivos são tratados estatisticamente e convertidos para valores reais numéricos."),
                tags$li(tags$strong("Fase 3 - Sensoriamento Remoto via Satélite (NASA POWER API):"), " Através de um loop iterativo cronometrado por município e safra, o sistema realiza requisições diretas ao satélite MERRA-2 da NASA para capturar a precipitação diária corrigida (PRECTOTCORR) para os 31 dias do mês de janeiro (janela crucial para a proliferação da doença). Foi implementado um mecanismo de cache em disco local (clima_historico_nasa.rds) que impede redundâncias de rede e acelera o processamento, munido de um tratamento de exceções (tryCatch).")
              )
            )
          ),
          
          # Painel 4: Modelagem Científica e Formulações Atuariais
          accordion_panel(
            title = "4. Modelagem Fitopatológica e Atuarial (Formulações Científicas)", icon = bs_icon("calculator"),
            tags$div(
              style = "line-height: 1.6; padding: 10px;",
              tags$h5("4.1. Estimativa de Severidade Foliar (Modelo Del Ponte BR3)", style = "color: #1B5E20; font-weight: bold;"),
              tags$p("O progresso da epidemia da Ferrugem Asiática da Soja (Phakopsora pachyrhizi) é modelado com base nas condições microclimáticas de janeiro, computando o volume total de chuva (V) e a frequência de dias chuvosos (F_freq, proporção de dias com chuva ≥ 1.0mm)."),
              tags$p("A severidade clínica foliar é calculada pelo modelo:"),
              tags$p("$$log\\_sev\\_BR3 = -3.8983 + (0.0031 \\times V) + (3.85 \\times F\\_freq)$$"),
              tags$p("$$severidade\\_calculada = e^{log\\_sev\\_BR3} \\times 100$$"),
              tags$p("Para assegurar a consistência biológica em cenários de secas severas que quebram a linearidade da regressão logística, o script aplica uma trava de segurança ponderada:"),
              tags$p("Se severidade < 5%: $$severidade\\_estimada\\_pct = ((V / 250) \\times 40) + (F\\_freq \\times 60)$$"),
              
              hr(),
              tags$h5("4.2. Função de Quebra de Rendimento (Dalla Lana et al., 2015)", style = "color: #1B5E20; font-weight: bold;"),
              tags$p("Cada 1% de severidade real nas folhas resulta em uma perda direta de 0.7% no peso final dos grãos. Respeitando os limites de colapso máximo da cultura, aplica-se um teto limitador (stop-loss biológico) travado em 70% de perda:"),
              tags$p("$$percentual\\_perda = \\min\\left(0.70, \\frac{severidade\\_estimada\\_pct \\times 0.7}{100}\\right)$$"),
              tags$p("$$perda\\_potencial\\_t = producao\\_t \\times percentual\\_perda$$"),
              
              hr(),
              tags$h5("4.3. Índice Uniforme de Vulnerabilidade Atuarial", style = "color: #1B5E20; font-weight: bold;"),
              tags$p("O risco econômico-financeiro implementa o concept de exposição do capital em risco (ponderando a severidade pela área/produção local). O algoritmo calcula o risco absoluto e aplica uma reescala estatística padrão Min-Max por safra para consolidar o índice no intervalo de [0, 100]:"),
              tags$p("$$risco\\_absoluto = severidade\\_estimada\\_pct \\times producao\\_t$$"),
              tags$p("$$vulnerabilidade = \\frac{risco\\_absoluto - \\min(risco\\_absoluto)}{\\max(risco\\_absoluto) - \\min(risco\\_absoluto)} \\times 100$$")
            )
          ),
          
          # Painel 5: Inteligência Avançada (K-Means e Tendência)
          accordion_panel(
            title = "5. Inteligência Avançada & Agrupamentos (Machine Learning)", icon = bs_icon("lightning-charge"),
            tags$div(
              style = "line-height: 1.6; padding: 10px;",
              
              tags$h5("5.1. Agrupamento K-Means (Clustering)", style = "color: #1B5E20; font-weight: bold;"),
              tags$p("O pipeline executa o algoritmo K-Means (com \\( K=3 \\)) nas dimensões padronizadas de produção, chuva, severidade e vulnerabilidade da última safra para classificar automaticamente os municípios em perfis operacionais de risco claros."),
              
              tags$div(
                style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid #1B5E20; margin: 15px 0; text-align: center;",
                tags$p("Minimização da Soma dos Quadrados Dentro do Cluster (WCSS) baseada na Distância Euclidiana:", style = "font-weight: 500; margin-bottom: 10px;"),
                tags$p("\\[ \\text{WCSS} = \\sum_{j=1}^{k} \\sum_{i \\in S_j} ||x_i - \\mu_j||^2 \\]")
              ),
              tags$p("Onde: \\( k=3 \\) clusters; \\( S_j \\) é o conjunto de municípios no cluster \\( j \\); \\( x_i \\) é o vetor de dados do município e \\( \\mu_j \\) é o centroide (média geométrica) do grupo."),
              
              tags$br(),
              tags$h6("Tabela de Perfis de Risco (K-Means)", style = "font-weight: bold; margin-top: 10px;"),
              tags$table(
                style = "width: 100%; border-collapse: collapse; margin-top: 10px; font-size: 14px; box-shadow: 0 2px 5px rgba(0,0,0,0.05);",
                tags$thead(
                  tags$tr(
                    style = "background-color: #1B5E20; color: white;",
                    tags$th("Perfil de Risco (K-Means)", style = "padding: 10px; border: 1px solid #dddddd; text-align: left;"),
                    tags$th("Comportamento Climático Típico", style = "padding: 10px; border: 1px solid #dddddd; text-align: left;"),
                    tags$th("Vulnerabilidade Média", style = "padding: 10px; border: 1px solid #dddddd; text-align: left;"),
                    tags$th("Ação Recomendada (Estratégia)", style = "padding: 10px; border: 1px solid #dddddd; text-align: left;")
                  )
                ),
                tags$tbody(
                  tags$tr(style = "background-color: #ffffff;",
                          tags$td(tags$strong("Alto Risco"), style = "padding: 10px; border: 1px solid #dddddd; color: #b71c1c;"),
                          tags$td("Janeiro chuvoso/úmido + alta densidade de cultivo histórico.", style = "padding: 10px; border: 1px solid #dddddd;"),
                          tags$td("Alta (\\( > 70 \\))", style = "padding: 10px; border: 1px solid #dddddd;"),
                          tags$td("Subscrição restrita de seguro e monitoramento intensivo com fungicidas.", style = "padding: 10px; border: 1px solid #dddddd;")
                  ),
                  tags$tr(style = "background-color: #f9f9f9;",
                          tags$td(tags$strong("Risco Moderado"), style = "padding: 10px; border: 1px solid #dddddd; color: #f57f17;"),
                          tags$td("Chuvas intermitentes ou polos de produção de médio porte.", style = "padding: 10px; border: 1px solid #dddddd;"),
                          tags$td("Intermediária (\\( 30\\text{-}70 \\))", style = "padding: 10px; border: 1px solid #dddddd;"),
                          tags$td("Vistorias de campo periódicas (alertas de esporulação foliar).", style = "padding: 10px; border: 1px solid #dddddd;")
                  ),
                  tags$tr(style = "background-color: #ffffff;",
                          tags$td(tags$strong("Baixo Risco"), style = "padding: 10px; border: 1px solid #dddddd; color: #1b5e20;"),
                          tags$td("Veranicos severos em janeiro ou baixa expressão agrícola local.", style = "padding: 10px; border: 1px solid #dddddd;"),
                          tags$td("Baixa (\\( < 30 \\))", style = "padding: 10px; border: 1px solid #dddddd;"),
                          tags$td("Monitoramento padrão fitossanitário; taxas de seguro bonificadas.", style = "padding: 10px; border: 1px solid #dddddd;")
                  )
                )
              ),
              
              tags$hr(),
              tags$h5("5.2. Tendência por Regressão Linear Simples", style = "color: #1B5E20; font-weight: bold;"),
              tags$p("O motor executa uma regressão linear (", tags$code("lm(media_sev ~ ano_num)"), ") na série temporal das safras. O p-valor e a inclinação da reta provam estatisticamente se a pressão sanitária da ferrugem está em expansão, contração ou estabilidade ecológica no Mato Grosso do Sul."),
              
              tags$div(
                style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid #1B5E20; margin: 15px 0; text-align: center;",
                tags$p("\\[ \\hat{Y} = \\beta_0 + \\beta_1 X + \\epsilon \\]")
              ),
              tags$p("Onde: \\( \\hat{Y} \\) é a Severidade Média Estimada; \\( X \\) é a linha do tempo das safras; \\( \\beta_0 \\) é o intercepto; \\( \\beta_1 \\) é o coeficiente angular (inclinação real p.p. por safra) e \\( \\epsilon \\) é o erro residual. Se o \\( p\\text{-valor} < 0.05 \\), a tendência é validada estatisticamente com 95% de confiança."),
              
              tags$hr(),
              tags$h5("5.3. Impacto do Macroclima (ENSO)", style = "color: #1B5E20; font-weight: bold;"),
              tags$p("O histórico demonstra que os cenários sob influência do fenômeno El Niño tendem a catalisar frentes de chuva recorrentes em janeiro na região centro-sul do estado, infletindo as curvas de severidade e perdas monetárias para patamares críticos. Anos de La Niña trazem maior estabilidade hídrica ou estiagens pontuais, reduzindo o progresso do fungo.")
            )
          ),
          
          # Painel 6: Considerações Finais
          accordion_panel(
            title = "6. Considerações Finais", icon = bs_icon("check2-circle"),
            tags$div(
              style = "line-height: 1.6; color: #2C3E50;",
              tags$p("O projeto demonstra uma maturidade técnica avançada, unificando ciência agronômica e ciência de dados aplicadas ao setor financeiro do agronegócio. A estruturação automatizada elimina a necessidade de análises manuais e dota tomadores de decisão (cooperativas, seguradoras e formuladores de políticas públicas) de uma ferramenta preditiva altamente escalável e auditável para o estado do Mato Grosso do Sul.")
            )
          )
        )
      )
    )
  ),
  
  # ===== ABA 7: GERADOR DE RELATÓRIOS =====
  nav_panel(
    value = "gerador_relatorio",
    title = tags$span(bs_icon("file-earmark-pdf"), " Relatório Exportável"),
    
    # --- RELATÓRIO 1: EXECUTIVO POR SAFRA ---
    layout_columns(
      col_widths = c(12),
      card(
        card_header(tags$span(bs_icon("printer"), " Relatório Executivo — Por Safra")),
        layout_columns(
          col_widths = c(4, 4, 4),
          tags$div(
            tags$h6("📅 Configuração da Safra", style = "font-weight:700; color:var(--cor-primaria);"),
            selectInput("relatorio_safra", "Safra Analisada:", choices = safras_disponiveis, selected = safras_disponiveis[1]),
            selectizeInput("relatorio_municipio", "Filtrar por Município (opcional):",
                           choices = c("Todos os Municípios" = "Todos", sort(unique(dados_risco$municipio))),
                           selected = "Todos", multiple = FALSE),
            checkboxGroupInput("relatorio_vars", "Indicadores no resumo:",
                               choices = c("Precipitação", "Temperatura", "Produtividade", "Severidade"),
                               selected = c("Precipitação", "Temperatura", "Produtividade", "Severidade"),
                               inline = FALSE)
          ),
          tags$div(
            tags$h6("📄 Formato de Saída", style = "font-weight:700; color:var(--cor-primaria);"),
            radioButtons("relatorio_formato", NULL,
                         choices = c("HTML (Abre no navegador)" = "html", "PDF (Documento Estático)" = "pdf"),
                         selected = "html", inline = FALSE)
          ),
          tags$div(
            tags$h6("ℹ️ Sobre este Relatório", style = "font-weight:700; color:var(--cor-primaria);"),
            p("Gera um documento com diagnóstico macroclimático, indicadores-chave, matriz de dispersão, mapa de vulnerabilidade e rankings Top 15 para a safra selecionada.", style = "font-size:0.88rem; color:#555;"),
            p("Ao selecionar um município específico, o mapa é substituído por um gráfico histórico de evolução da vulnerabilidade daquele município.", style = "font-size:0.88rem; color:#888; font-style: italic;"),
            hr(),
            downloadButton("btn_baixar_relatorio", "⬇ Gerar Relatório de Safra", class = "btn-download-custom")
          )
        )
      )
    ),
    
    # --- RELATÓRIO 2: COMPARATIVO HISTÓRICO MULTI-SAFRA ---
    layout_columns(
      col_widths = c(12),
      card(
        card_header(tags$span(bs_icon("bar-chart-steps"), " Relatório Comparativo — Histórico Multi-Safra")),
        layout_columns(
          col_widths = c(4, 4, 4),
          tags$div(
            tags$h6("📅 Seleção de Safras", style = "font-weight:700; color:var(--cor-primaria);"),
            selectizeInput("comp_safras", "Safras para Comparar (mín. 2):",
                           choices = safras_disponiveis,
                           selected = safras_disponiveis[1:min(3, length(safras_disponiveis))],
                           multiple = TRUE,
                           options = list(placeholder = "Selecione 2 ou mais safras...")),
            selectizeInput("comp_municipio", "Filtrar por Município (opcional):",
                           choices = c("Todos os Municípios" = "Todos", sort(unique(dados_risco$municipio))),
                           selected = "Todos", multiple = FALSE)
          ),
          tags$div(
            tags$h6("📄 Formato de Saída", style = "font-weight:700; color:var(--cor-primaria);"),
            radioButtons("comp_formato", NULL,
                         choices = c("HTML (Abre no navegador)" = "html", "PDF (Documento Estático)" = "pdf"),
                         selected = "html", inline = FALSE)
          ),
          tags$div(
            tags$h6("ℹ️ Sobre este Relatório", style = "font-weight:700; color:var(--cor-primaria);"),
            p("Gera um documento comparativo entre múltiplas safras com: evolução temporal da vulnerabilidade, heatmap de municípios, variação de precipitação e severidade, ranking consolidado e conclusão automática identificando a safra de maior e menor risco.", style = "font-size:0.88rem; color:#555;"),
            hr(),
            uiOutput("btn_comparativo_ui")
          )
        )
      )
    )
  )
)

# --- 4. SERVIDOR ---
server <- function(input, output, session) {
  
  # Mostra/Recolhe sidebar com base nas abas ativas
  observe({
    if (input$nav_principal %in% c("perfil_local", "documentacao",
                                   "home_apresentacao", "gerador_relatorio")) {
      sidebar_toggle("sidebar_global", open = FALSE)
    } else {
      sidebar_toggle("sidebar_global", open = TRUE)
    }
  })
  
  # Reativo Base Filtrado Inteligente (Abas 1 e 2)
  dados_filtrados <- reactive({
    req(input$tipo_analise, input$muni_selecionado, input$filtro_precipitacao)
    df_base <- dados_risco
    if (!"Todos" %in% input$muni_selecionado) {
      df_base <- df_base %>% filter(municipio %in% input$muni_selecionado)
    }
    if (input$tipo_analise == "Safra Específica") {
      req(input$safra_selecionada)
      df_final <- df_base %>% filter(safra == input$safra_selecionada)
    } else {
      req(input$fase_selecionada)
      df_final <- df_base %>% 
        filter(fase_enso == input$fase_selecionada) %>% 
        group_by(municipio, cod_ibge) %>% 
        summarise(
          producao_t = mean(producao_t, na.rm = TRUE),
          chuva_acumulada_mm = mean(chuva_acumulada_mm, na.rm = TRUE),
          frequencia_chuva = mean(frequencia_chuva, na.rm = TRUE),
          temp_media = mean(temp_media, na.rm = TRUE),
          perda_potencial_t = mean(perda_potencial_t, na.rm = TRUE),
          vulnerabilidade = mean(vulnerabilidade, na.rm = TRUE),
          severidade_estimada_pct = mean(severidade_estimada_pct, na.rm = TRUE),
          perfil_risco = first(perfil_risco), .groups = 'drop'
        ) %>%
        mutate(safra = paste("Cenário:", input$fase_selecionada))
    }
    if (nrow(df_final) > 0 && "chuva_acumulada_mm" %in% colnames(df_final)) {
      df_final <- df_final %>% filter(
        chuva_acumulada_mm >= input$filtro_precipitacao[1] & 
          chuva_acumulada_mm <= input$filtro_precipitacao[2]
      )
    }
    return(df_final)
  })
  
  # --- VALUE BOXES REATIVAS ---
  output$vb_perda_ui <- renderUI({
    df <- dados_filtrados(); req(nrow(df) > 0)
    value_box(title = "Quebra Potencial Estimada", value = paste(comma(round(sum(df$perda_potencial_t, na.rm = TRUE), 0), big.mark = "."), "t"), showcase = bs_icon("graph-down-arrow"), theme = "danger")
  })
  output$vb_dinheiro_ui <- renderUI({
    df <- dados_filtrados(); req(nrow(df) > 0)
    capital <- sum(df$perda_potencial_t, na.rm = TRUE) * 16.667 * input$preco_saca
    value_box(title = "Impacto Econômico Estimado", value = paste("R$", comma(round(capital, 2), big.mark = ".", decimal.mark = ",")), showcase = bs_icon("currency-dollar"), theme = "warning")
  })
  output$vb_vuln_ui <- renderUI({
    df <- dados_filtrados(); req(nrow(df) > 0)
    v_med <- mean(df$vulnerabilidade, na.rm = TRUE)
    value_box(title = "Índice de Vulnerabilidade Médio", value = paste(round(v_med, 1), "/ 100"), showcase = bs_icon("shield-exclamation"), theme = if_else(v_med > 50, "danger", "success"))
  })
  output$vb_precip_ui <- renderUI({
    df <- dados_filtrados(); req(nrow(df) > 0)
    value_box(title = "Precipitação Média (Janeiro)", value = paste(round(mean(df$chuva_acumulada_mm, na.rm = TRUE), 1), "mm"), showcase = bs_icon("cloud-rain"), theme = "info")
  })
  
  # --- MAPA LEAFLET ---
  output$mapa <- renderLeaflet({
    df <- dados_filtrados(); req(nrow(df) > 0)
    mapa_dados <- malha_ms %>% left_join(df, by = c("code_muni" = "cod_ibge"))
    pal <- colorNumeric(palette = c("#2ecc71", "#f1c40f", "#e67e22", "#c0392b"), domain = c(0, 100))
    leaflet(mapa_dados) %>% addTiles() %>%
      addPolygons(fillColor = ~pal(vulnerabilidade), weight = 1, opacity = 1, color = "white", fillOpacity = 0.7,
                  popup = ~paste0("<strong>Município: </strong>", name_muni, "<br><strong>Vulnerabilidade: </strong>", round(vulnerabilidade, 2))) %>%
      addLegend("bottomright", pal = pal, values = c(0, 100), title = "Vulnerabilidade", opacity = 1)
  })
  
  # --- MATRIZ ATUARIAL ---
  output$grafico_dispersao <- renderPlotly({
    df <- dados_filtrados(); req(nrow(df) > 0)
    p <- ggplot(df, aes(x = chuva_acumulada_mm, y = severidade_estimada_pct, text = paste("Cidade:", municipio))) +
      geom_point(aes(size = producao_t, color = vulnerabilidade), alpha = 0.7) +
      scale_color_gradientn(colors = c("#2ecc71", "#f1c40f", "#e67e22", "#c0392b")) + theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  
  # --- RANKINGS (Tabela DT) ---
  output$tabela_dados <- renderDT({
    df <- dados_filtrados(); req(nrow(df) > 0)
    
    cols_selecao <- c("municipio", "safra", "chuva_acumulada_mm", "severidade_estimada_pct", "vulnerabilidade", "perda_potencial_t")
    nomes_colunas <- c("Município", "Safra/Cenário", "Chuva (mm)", "Severidade (%)", "Vulnerabilidade", "Perda Est. (t)")
    
    if ("prod_sc_ha" %in% colnames(df)) {
      cols_selecao <- c(cols_selecao, "prod_sc_ha")
      nomes_colunas <- c(nomes_colunas, "Prod. Real (sc/ha)")
    }
    
    dt <- datatable(df %>% select(all_of(cols_selecao)) %>% arrange(desc(vulnerabilidade)),
              colnames = nomes_colunas, options = list(pageLength = 10, dom = 'ftp'), rownames = FALSE) %>%
      formatRound(c('chuva_acumulada_mm', 'severidade_estimada_pct', 'vulnerabilidade'), 1) %>% formatRound('perda_potencial_t', 0)
      
    if ("prod_sc_ha" %in% colnames(df)) {
      dt <- dt %>% formatRound('prod_sc_ha', 1)
    }
    
    dt
  })
  
  # --- ABA HISTÓRICO ---
  output$heatmap_temporal <- renderPlotly({
    top_cidades <- dados_risco %>% group_by(municipio) %>% summarise(vm = mean(vulnerabilidade, na.rm = TRUE)) %>% top_n(15, vm) %>% pull(municipio)
    p <- ggplot(dados_risco %>% filter(municipio %in% top_cidades), aes(x = safra, y = municipio, fill = vulnerabilidade)) +
      geom_tile(color = "white") + scale_fill_gradientn(colors = c("#2ecc71", "#f1c40f", "#e67e22", "#c0392b")) + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  output$titulo_grafico_linha <- renderUI({ tags$span(bs_icon("graph-up"), paste("Curva Histórica de Severidade —", input$muni_historico)) })
  output$grafico_linha <- renderPlotly({
    df_linha <- dados_risco %>% filter(municipio == input$muni_historico)
    if (input$fase_historico != "Histórico Completo") df_linha <- df_linha %>% filter(fase_enso == input$fase_historico)
    req(nrow(df_linha) > 0)
    p <- ggplot(df_linha %>% arrange(safra), aes(x = safra, y = severidade_estimada_pct, group = 1)) +
      geom_line(color = "#1B5E20") + geom_point(aes(color = fase_enso), size = 3) + scale_color_manual(values = c("El Niño" = "#b71c1c", "La Niña" = "#0d47a1", "Neutro" = "#558b2f")) + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  # --- LÓGICA CLIMA & PRODUÇÃO ---
  output$titulo_aba_clima_prod <- renderUI({
    req(input$muni_clima_prod)
    if (input$muni_clima_prod == "Mato Grosso do Sul (Estado)") {
      tags$span(bs_icon("bar-chart-line-fill"), "Linha do Tempo Multivariada — Mato Grosso do Sul (Média Estadual)")
    } else {
      tags$span(bs_icon("bar-chart-line-fill"), paste("Linha do Tempo Multivariada — Município:", input$muni_clima_prod))
    }
  })
  
  output$grafico_triplo_clima_prod <- renderPlotly({
    req(input$muni_clima_prod)
    
    if (input$muni_clima_prod == "Mato Grosso do Sul (Estado)") {
      df_muni_hist <- dados_risco %>% 
        group_by(safra) %>% 
        summarise(
          chuva_acumulada_mm = mean(chuva_acumulada_mm, na.rm = TRUE),
          temp_media = mean(temp_media, na.rm = TRUE),
          producao_t = sum(producao_t, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(safra)
    } else {
      df_muni_hist <- dados_risco %>% 
        filter(municipio == input$muni_clima_prod) %>% 
        arrange(safra)
    }
    
    req(nrow(df_muni_hist) > 0)
    
    p1 <- ggplot(df_muni_hist, aes(x = safra, y = chuva_acumulada_mm, group = 1,
                                   text = paste("Safra:", safra, "<br>Chuva:", round(chuva_acumulada_mm, 1), "mm"))) +
      geom_bar(stat = "identity", fill = "#29b6f6", alpha = 0.85) +
      theme_minimal() +
      labs(x = "", y = "Precipitação (mm)") +
      theme(axis.text.x = element_blank())
    
    p2 <- ggplot(df_muni_hist, aes(x = safra, y = temp_media, group = 1,
                                   text = paste("Safra:", safra, "<br>Temp. Média:", round(temp_media, 1), "°C"))) +
      geom_line(color = "#ef5350", size = 1) +
      geom_point(color = "#b71c1c", size = 2) +
      theme_minimal() +
      labs(x = "", y = "Temp. Média (°C)") +
      theme(axis.text.x = element_blank())
    
    p3 <- ggplot(df_muni_hist, aes(x = safra, y = producao_t, group = 1,
                                   text = paste("Safra:", safra, "<br>Produção:", comma(round(producao_t, 0), big.mark = "."), "t"))) +
      geom_bar(stat = "identity", fill = "#66bb6a", alpha = 0.9) +
      theme_minimal() +
      labs(x = "Safra Agrícola", y = "Produção (t)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    fig_combinada <- subplot(
      ggplotly(p1, tooltip = "text"), 
      ggplotly(p2, tooltip = "text"), 
      ggplotly(p3, tooltip = "text"), 
      nrows = 3, 
      shareX = TRUE, 
      titleY = TRUE,
      margin = 0.04
    ) %>% 
      layout(
        showlegend = FALSE,
        hovermode = "x unified"
      )
    
    fig_combinada
  })
  
  output$grafico_prod_real <- renderPlotly({
    req(input$muni_clima_prod)
    
    if (input$muni_clima_prod == "Mato Grosso do Sul (Estado)") {
      df_muni_hist <- dados_risco %>% 
        group_by(safra, fase_enso) %>% 
        summarise(
          prod_sc_ha = mean(prod_sc_ha, na.rm = TRUE),
          severidade_estimada_pct = mean(severidade_estimada_pct, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(safra)
    } else {
      df_muni_hist <- dados_risco %>% 
        filter(municipio == input$muni_clima_prod) %>% 
        arrange(safra)
    }
      
    req(nrow(df_muni_hist) > 0, "prod_sc_ha" %in% colnames(df_muni_hist))
    
    p <- ggplot(df_muni_hist, aes(x = safra)) +
      geom_col(aes(y = prod_sc_ha, fill = fase_enso, text = paste("Safra:", safra, "<br>Produtividade (IBGE):", round(prod_sc_ha, 1), "sc/ha<br>ENSO:", fase_enso)), alpha = 0.8) +
      geom_line(aes(y = severidade_estimada_pct, group = 1, text = paste("Safra:", safra, "<br>Severidade Estimada (Modelo):", round(severidade_estimada_pct, 1), "%")), color = "#b71c1c", size = 1) +
      geom_point(aes(y = severidade_estimada_pct), color = "#b71c1c", size = 2) +
      scale_fill_manual(values = c("El Niño" = "#e57373", "La Niña" = "#64b5f6", "Neutro" = "#81c784")) +
      theme_minimal() +
      labs(x = "Safra Agrícola", y = "Produtividade Real (sc/ha)", fill = "Fase ENSO") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    ggplotly(p, tooltip = "text")
  })
  
  # --- ABA 4: COMPARADOR ---
  output$titulo_graf_comp_sev <- renderUI({ tags$span(bs_icon("bar-chart-fill"), paste("Avaliação Clinical (", input$safra_comp_1, "vs", input$safra_comp_2, ")")) })
  output$titulo_graf_comp_prec <- renderUI({ tags$span(bs_icon("droplet-fill"), paste("Diferencial Hídrico (", input$safra_comp_1, "vs", input$safra_comp_2, ")")) })
  
  df_comparativo <- reactive({
    req(input$safra_comp_1, input$safra_comp_2, input$muni_comp)
    df_c <- dados_risco %>% filter(safra %in% c(input$safra_comp_1, input$safra_comp_2))
    if (!"Todos" %in% input$muni_comp) df_c <- df_c %>% filter(municipio %in% input$muni_comp)
    return(df_c)
  })
  output$vb_comparativo_ui <- renderUI({
    dfc <- df_comparativo(); req(nrow(dfc) > 0)
    p1 <- sum(dfc$perda_potencial_t[dfc$safra == input$safra_comp_1], na.rm = TRUE)
    p2 <- sum(dfc$perda_potencial_t[dfc$safra == input$safra_comp_2], na.rm = TRUE)
    dif <- p2 - p1
    value_box(title = "Variação Física de Quebra", value = paste(if_else(dif > 0, "+", ""), comma(round(dif, 0), big.mark = "."), "t"), showcase = if_else(dif > 0, bs_icon("arrow-up-right"), bs_icon("arrow-down-left")), theme = if_else(dif > 0, "danger", "success"))
  })
  output$grafico_comparativo <- renderPlotly({
    ggplotly(ggplot(df_comparativo(), aes(x = municipio, y = severidade_estimada_pct, fill = safra)) + geom_bar(stat = "identity", position = "dodge") + scale_fill_manual(values = c("#9ccc65", "#b71c1c")) + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)))
  })
  output$grafico_precip_comp <- renderPlotly({
    ggplotly(ggplot(df_comparativo(), aes(x = municipio, y = chuva_acumulada_mm, fill = safra)) + geom_bar(stat = "identity", position = "dodge") + scale_fill_manual(values = c("#81d4fa", "#0d47a1")) + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)))
  })
  
  # --- ABA 5: PERFIL LOCAL ---
  output$radar_chart <- renderPlotly({
    req(input$muni_radar, input$safra_radar)
    muni_data <- dados_risco %>% filter(municipio == input$muni_radar, safra == input$safra_radar); req(nrow(muni_data) > 0)
    plot_ly(type = 'scatterpolar', r = c(muni_data$severidade_estimada_pct, (muni_data$chuva_acumulada_mm / max_chuva_historica) * 100, muni_data$vulnerabilidade, muni_data$severidade_estimada_pct),
            theta = c('Severidade (%)', 'Chuva Relativa (%)', 'Vulnerabilidade', 'Severidade (%)'), fill = 'toself', fillcolor = 'rgba(27, 94, 32, 0.4)', line = list(color = '#1B5E20', width = 2)) %>%
      layout(polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))), showlegend = FALSE)
  })
  output$resSummary_muni_ui <- renderUI({
    req(input$muni_radar, input$safra_radar); md <- dados_risco %>% filter(municipio == input$muni_radar, safra == input$safra_radar)
    tags$div(tags$p(tags$strong("ENSO: "), md$fase_enso), tags$p(tags$strong("Chuva: "), round(md$chuva_acumulada_mm, 1), " mm"), tags$p(tags$strong("Severidade: "), round(md$severidade_estimada_pct, 1), " %"))
  })
  output$diagnostico_radar_ui <- renderUI({
    req(input$muni_radar, input$safra_radar); md <- dados_risco %>% filter(municipio == input$muni_radar, safra == input$safra_radar)
    cl <- if_else(md$vulnerabilidade > 60, "CRÍTICA", if_else(md$vulnerabilidade > 30, "ALERTA", "ESTÁVEL"))
    tags$div(class = "box-diagnostico", tags$h5(paste("Diagnóstico Atuarial para", input$muni_radar)), tags$p(HTML(paste0("Condição de vulnerabilidade <strong>", cl, "</strong> (Nota: ", round(md$vulnerabilidade, 1), "/100)."))))
  })
  
  # ======================================================
  # --- SERVIDOR — ABA VIS\u00c3O GERAL (DASHBOARD EXECUTIVO) ---
  # ======================================================
  
  ultima_safra_ov <- max(dados_risco$safra, na.rm = TRUE)
  
  df_overview <- reactive({
    dados_risco %>% filter(safra == ultima_safra_ov)
  })
  
  # --- PIPELINE CLICÁVEL — MODAIS DETALHADOS ---
  observeEvent(input$pipeline_click, {
    
    sid <- input$pipeline_click
    
    m_stat <- function(v, l) tags$div(class="pm-stat", tags$div(class="pm-stat-val",v), tags$div(class="pm-stat-lbl",l))
    m_card <- function(cls, icon_nm, titulo, texto) {
      tags$div(class=paste("pm-card",cls),
        tags$strong(bs_icon(icon_nm), paste0(" ", titulo)),
        tags$p(style="font-size:0.85rem;margin:6px 0 0;", texto)
      )
    }
    m_tags <- function(...) {
      tags$div(class="pm-tags-wrap", ...)
    }
    
    modal_out <- switch(sid,
      
      # ======== ETAPA 1 — DADOS GEOGRÁFICOS ========
      "1" = modalDialog(
        title = NULL, size = "l", easyClose = TRUE,
        footer = tagList(modalButton("\u2715  Fechar")),
        tags$div(class="pm-header pm-blue",
          tags$div(class="pm-number","01"),
          tags$span(class="pm-badge","ETAPA 01 \u00b7 DATA PREP"),
          tags$h3(class="pm-title", bs_icon("geo-alt-fill")," Fus\u00e3o Geogr\u00e1fica & Hist\u00f3rica"),
          tags$p(class="pm-subtitle","Malha territorial oficial do Mato Grosso do Sul \u2014 geobr / IBGE 2022")
        ),
        layout_columns(col_widths=c(3,3,3,3), fill=FALSE,
          m_stat("79","Munic\u00edpios"),
          m_stat("357K","km\u00b2 do Estado"),
          m_stat("16","Anos de Dados"),
          m_stat("WGS84","Proje\u00e7\u00e3o")
        ),
        m_card("","map-fill","Malha Municipal \u2014 geobr",
          "Ingest\u00e3o da malha cartogr\u00e1fica digital do MS via geobr::read_municipality(code_muni='MS', year=2022). Cada pol\u00edgono representa um munic\u00edpio em formato SF com proje\u00e7\u00e3o WGS84 (EPSG:4326)."),
        m_card("gc","crosshair2","C\u00e1lculo de Centroides Geom\u00e9tricos",
          "Extra\u00e7\u00e3o autom\u00e1tica das coordenadas de centroide de cada pol\u00edgono municipal (centroide_x = Longitude; centroide_y = Latitude) que servem de \u00e2ncoras para as requisi\u00e7\u00f5es clim\u00e1ticas ao sat\u00e9lite NASA."),
        m_card("oc","table","Reestrutura\u00e7\u00e3o Hist\u00f3rica \u2014 Tidy Data",
          "Limpeza e normaliza\u00e7\u00e3o da planilha de produtividade (2009\u20132024). Aplica\u00e7\u00e3o do pivot_longer para transformar dados de formato 'wide' (colunas-ano) para s\u00e9rie temporal 'long', com tratamento de omiss\u00f5es e caracteres especiais."),
        tags$div(class="pm-formula",
          HTML("malha_ms &lt;- geobr::read_municipality(code_muni = 'MS', year = 2022)\nmalha_ms &lt;- sf::st_transform(malha_ms, 4326)       # WGS84\n\n# Centroides para ancorar requisi&ccedil;&otilde;es NASA\ncentroides       &lt;- sf::st_centroid(malha_ms)\ndados$centroide_x &lt;- sf::st_coordinates(centroides)[, 1]  # Longitude\ndados$centroide_y &lt;- sf::st_coordinates(centroides)[, 2]  # Latitude\n\n# Tidy Data: wide \u2192 long\ndados_long &lt;- dados_wide |> tidyr::pivot_longer(\n  cols = starts_with('20'), names_to = 'safra', values_to = 'producao_t'\n)")
        ),
        m_tags(
          tags$span(class="pm-tag bt","geobr"), tags$span(class="pm-tag bt","sf"),
          tags$span(class="pm-tag gt","dplyr"), tags$span(class="pm-tag gt","Tidy Data"),
          tags$span(class="pm-tag ot","IBGE 2022"), tags$span(class="pm-tag ot","WGS84")
        )
      ),
      
      # ======== ETAPA 2 — CLIMA NASA POWER ========
      "2" = modalDialog(
        title = NULL, size = "l", easyClose = TRUE,
        footer = tagList(modalButton("\u2715  Fechar")),
        tags$div(class="pm-header pm-teal",
          tags$div(class="pm-number","02"),
          tags$span(class="pm-badge","ETAPA 02 \u00b7 SENSORIAMENTO REMOTO"),
          tags$h3(class="pm-title", bs_icon("cloud-download")," Motor Clim\u00e1tico Global \u2014 NASA POWER"),
          tags$p(class="pm-subtitle","Captura autom\u00e1tica de dados clim\u00e1ticos di\u00e1rios via sat\u00e9lite MERRA-2 da NASA")
        ),
        layout_columns(col_widths=c(3,3,3,3), fill=FALSE,
          m_stat("31","Dias de Janeiro"),
          m_stat("3","Fases ENSO"),
          m_stat("2","Vari\u00e1veis"),
          m_stat("Cache","Sistema Local")
        ),
        m_card("","broadcast-pin","Sat\u00e9lite NASA MERRA-2",
          "Requisi\u00e7\u00f5es diretas ao sat\u00e9lite MERRA-2 via NASA POWER API para cada munic\u00edpio \u00d7 safra. A vari\u00e1vel PRECTOTCORR captura a precipita\u00e7\u00e3o di\u00e1ria corrigida (mm/dia) e T2M a temperatura m\u00e9dia a 2 metros de altitude."),
        m_card("gc","calendar-check","Janela Climatol\u00f3gica Cr\u00edtica \u2014 Janeiro",
          "Janeiro \u00e9 a fase de florescimento e forma\u00e7\u00e3o de vagens da soja, janela cr\u00edtica para prolifera\u00e7\u00e3o de Phakopsora pachyrhizi. A acumula\u00e7\u00e3o de chuva e temperatura neste per\u00edodo define a severidade potencial da ferrugem."),
        m_card("oc","hdd-fill","Cache em Disco \u2014 Otimiza\u00e7\u00e3o",
          "Sistema de cache local (clima_historico_nasa.rds) que impede redund\u00e2ncias de rede. Ap\u00f3s a primeira coleta, dados ficam dispon\u00edveis offline. Implementado com tryCatch para recupera\u00e7\u00e3o autom\u00e1tica de falhas de conex\u00e3o."),
        tags$div(class="pm-formula",
          HTML("# NASA POWER API \u2014 MERRA-2\nurl &lt;- paste0(\n  'https://power.larc.nasa.gov/api/temporal/daily/point',\n  '?parameters=PRECTOTCORR,T2M&community=AG',\n  '&longitude=', lon, '&latitude=', lat,\n  '&start=', ano, '0101&end=', ano, '0131'\n)\n\n# Cache em disco\nif (file.exists('clima_historico_nasa.rds')) {\n  clima_hist &lt;- readRDS('clima_historico_nasa.rds')\n}\n\n# Classifica&ccedil;&atilde;o ENSO por safra\nfase_enso %in% c('El Ni&ntilde;o', 'Neutro', 'La Ni&ntilde;a')")
        ),
        m_tags(
          tags$span(class="pm-tag bt","NASA POWER"), tags$span(class="pm-tag bt","MERRA-2"),
          tags$span(class="pm-tag gt","PRECTOTCORR"), tags$span(class="pm-tag gt","T2M"),
          tags$span(class="pm-tag ot","El Ni\u00f1o"), tags$span(class="pm-tag ot","La Ni\u00f1a"),
          tags$span(class="pm-tag pt","httr"), tags$span(class="pm-tag pt","jsonlite")
        )
      ),
      
      # ======== ETAPA 3 — MODELO BR3 ========
      "3" = modalDialog(
        title = NULL, size = "l", easyClose = TRUE,
        footer = tagList(modalButton("\u2715  Fechar")),
        tags$div(class="pm-header pm-green",
          tags$div(class="pm-number","03"),
          tags$span(class="pm-badge","ETAPA 03 \u00b7 MODELO EPIDEMIOL\u00d3GICO"),
          tags$h3(class="pm-title", bs_icon("virus")," Algoritmo Estoc\u00e1stico \u2014 Modelo BR3"),
          tags$p(class="pm-subtitle","Del Ponte et al. \u2014 Ferrugem Asi\u00e1tica da Soja (Phakopsora pachyrhizi)")
        ),
        layout_columns(col_widths=c(3,3,3,3), fill=FALSE,
          m_stat("BR3","Modelo Validado"),
          m_stat("0.7%","Perda/% Severidade"),
          m_stat("70%","Stop-Loss Biol\u00f3gico"),
          m_stat("Min-Max","Normaliza\u00e7\u00e3o")
        ),
        m_card("","calculator","Estimativa de Severidade Foliar",
          "O modelo BR3 calcula o progresso da epidemia com base no volume total de chuva (V em mm) e frequ\u00eancia de dias chuvosos (F_freq = propor\u00e7\u00e3o de dias com \u2265 1mm). A equa\u00e7\u00e3o foi desenvolvida por Del Ponte para condi\u00e7\u00f5es brasileiras de ferrugem."),
        m_card("gc","graph-down-arrow","Fun\u00e7\u00e3o de Quebra de Rendimento",
          "Dalla Lana et al. (2015): cada 1% de severidade foliar resulta em -0.7% de perda no rendimento final dos gr\u00e3os, com stop-loss biol\u00f3gico limitado em 70% (colapso m\u00e1ximo da cultura, garantindo consist\u00eancia biol\u00f3gica do modelo)."),
        m_card("oc","shield-check","\u00cdndice de Vulnerabilidade Atuarial",
          "C\u00e1lculo do risco absoluto (severidade \u00d7 produ\u00e7\u00e3o) com reescala estat\u00edstica Min-Max por safra. Padroniza\u00e7\u00e3o do \u00edndice no intervalo [0, 100] representando a exposi\u00e7\u00e3o proporcional do capital em risco."),
        tags$div(class="pm-formula",
          HTML("# Modelo Del Ponte BR3\nlog_sev_BR3 &lt;- -3.8983 + (0.0031 * V) + (3.85 * F_freq)\nseveridade  &lt;- exp(log_sev_BR3) * 100          # em %\n\n# Fun&ccedil;&atilde;o de Quebra de Rendimento (Dalla Lana 2015)\npercentual_perda &lt;- min(0.70, severidade * 0.7 / 100)\nperda_t          &lt;- producao_t * percentual_perda   # toneladas\n\n# &Iacute;ndice de Vulnerabilidade Atuarial (Min-Max)\nrisco_abs       &lt;- severidade * producao_t\nvulnerabilidade &lt;- (risco_abs - min(risco_abs)) /\n                   (max(risco_abs) - min(risco_abs)) * 100")
        ),
        m_tags(
          tags$span(class="pm-tag gt","Del Ponte BR3"), tags$span(class="pm-tag gt","Dalla Lana 2015"),
          tags$span(class="pm-tag bt","Severidade"), tags$span(class="pm-tag ot","Vulnerabilidade"),
          tags$span(class="pm-tag rt","Phakopsora pachyrhizi"), tags$span(class="pm-tag pt","Atuarial")
        )
      ),
      
      # ======== ETAPA 4 — VALIDA\u00c7\u00c3O SIDRA ========
      "4" = modalDialog(
        title = NULL, size = "l", easyClose = TRUE,
        footer = tagList(modalButton("\u2715  Fechar")),
        tags$div(class="pm-header pm-orange",
          tags$div(class="pm-number","04"),
          tags$span(class="pm-badge","ETAPA 04 \u00b7 VALIDA\u00c7\u00c3O CRUZADA"),
          tags$h3(class="pm-title", bs_icon("table")," Valida\u00e7\u00e3o em Tempo Real \u2014 API SIDRA/IBGE"),
          tags$p(class="pm-subtitle","Motor de valida\u00e7\u00e3o cruzada conectado \u00e0 API oficial do SIDRA \u2014 Produ\u00e7\u00e3o Agr\u00edcola Municipal")
        ),
        layout_columns(col_widths=c(3,3,3,3), fill=FALSE,
          m_stat("2009","In\u00edcio da S\u00e9rie"),
          m_stat("2024","\u00daltima Safra"),
          m_stat("PAM","Fonte IBGE"),
          m_stat("sc/ha","Produtividade")
        ),
        m_card("","database-fill","Produ\u00e7\u00e3o Agr\u00edcola Municipal \u2014 PAM/IBGE",
          "Ingest\u00e3o da s\u00e9rie hist\u00f3rica de produ\u00e7\u00e3o de soja (2009\u20132024) por munic\u00edpio via Pesquisa Agr\u00edcola Municipal (PAM) do IBGE. Os dados incluem \u00e1rea plantada (ha), quantidade produzida (t) e produtividade (sc/ha)."),
        m_card("gc","arrow-left-right","Valida\u00e7\u00e3o Cruzada do Modelo",
          "A produtividade real (prod_sc_ha) \u00e9 confrontada com a severidade estimada pelo Modelo BR3. A correla\u00e7\u00e3o negativa esperada valida o poder preditivo: anos mais severos resultam em menor produtividade real, comprovando a coer\u00eancia do pipeline."),
        m_card("pc","wrench","Tratamento e Limpeza de Dados",
          "Aplica\u00e7\u00e3o do pivot_longer para transformar o formato 'wide' (colunas por ano) em s\u00e9rie temporal. Tratamento de caracteres especiais ('-', '...', rodap\u00e9s), convers\u00e3o de tipos e join com dados clim\u00e1ticos por munic\u00edpio \u00d7 safra."),
        tags$div(class="pm-formula",
          HTML("# Carregamento e limpeza da produ&ccedil;&atilde;o IBGE\ndados_ibge &lt;- readxl::read_xlsx('DADOS PROD_PRODUT_2009-2024.xlsx')\n\n# Reestrutura&ccedil;&atilde;o Tidy Data\ndados_long &lt;- dados_ibge |>\n  tidyr::pivot_longer(\n    cols      = starts_with('20'),\n    names_to  = 'safra',\n    values_to = 'producao_t'\n  ) |> dplyr::filter(!is.na(producao_t))\n\n# Join com dados clim&aacute;ticos\ndados_unidos &lt;- dplyr::left_join(dados_climaticos, dados_long,\n                                 by = c('municipio', 'safra'))")
        ),
        m_tags(
          tags$span(class="pm-tag ot","IBGE"), tags$span(class="pm-tag ot","SIDRA"),
          tags$span(class="pm-tag ot","PAM"), tags$span(class="pm-tag gt","readxl"),
          tags$span(class="pm-tag gt","tidyr"), tags$span(class="pm-tag bt","pivot_longer"),
          tags$span(class="pm-tag rt","Valida\u00e7\u00e3o Cruzada")
        )
      ),
      
      # ======== ETAPA 5 — MACHINE LEARNING ========
      "5" = modalDialog(
        title = NULL, size = "l", easyClose = TRUE,
        footer = tagList(modalButton("\u2715  Fechar")),
        tags$div(class="pm-header pm-purple",
          tags$div(class="pm-number","05"),
          tags$span(class="pm-badge","ETAPA 05 \u00b7 MACHINE LEARNING"),
          tags$h3(class="pm-title", bs_icon("cpu")," An\u00e1lise Avan\u00e7ada & Clusteriza\u00e7\u00e3o"),
          tags$p(class="pm-subtitle","K-Means n\u00e3o supervisionado + Regress\u00e3o Linear para an\u00e1lise macrotemporal")
        ),
        layout_columns(col_widths=c(3,3,3,3), fill=FALSE,
          m_stat("K=3","Clusters"),
          m_stat("4","Vari\u00e1veis"),
          m_stat("WCSS","Crit\u00e9rio"),
          m_stat("p&lt;0.05","Sig. Estat\u00edstica")
        ),
        m_card("","diagram-3-fill","Clusteriza\u00e7\u00e3o K-Means (K=3)",
          "Aplica\u00e7\u00e3o do algoritmo K-Means nas dimens\u00f5es padronizadas (produ\u00e7\u00e3o, chuva, severidade, vulnerabilidade) da \u00faltima safra para classificar automaticamente os munic\u00edpios em 3 perfis operacionais de risco: Baixo, M\u00e9dio e Alto."),
        m_card("gc","graph-up","Regress\u00e3o Linear Temporal",
          "Execu\u00e7\u00e3o de lm(media_sev ~ ano_num) na s\u00e9rie temporal. O coeficiente \u03b2\u2081 e o p-valor indicam se a press\u00e3o sanit\u00e1ria da ferrugem est\u00e1 em expans\u00e3o, contra\u00e7\u00e3o ou estabilidade ao longo das safras no Mato Grosso do Sul."),
        m_card("oc","layers","Perfis de Risco Identificados",
          "Baixo Risco (vuln \u2264 33): Veranicos ou baixa express\u00e3o agr\u00edcola. M\u00e9dio Risco (33\u201366): Chuvas intermitentes, polos de m\u00e9dio porte. Alto Risco (vuln > 66): Janeiro chuvoso + alta densidade de cultivo hist\u00f3rico."),
        tags$div(class="pm-formula",
          HTML("# Clusteriza&ccedil;&atilde;o K-Means\nset.seed(42)\nkm_fit &lt;- kmeans(\n  scale(dados[, c('producao_t', 'chuva_acumulada_mm',\n                   'severidade_estimada_pct', 'vulnerabilidade')]),\n  centers = 3, nstart = 25\n)\ndados$perfil_risco &lt;- km_fit$cluster\n\n# Regress&atilde;o Linear para Tend&ecirc;ncia Temporal\nlm_tend &lt;- lm(media_sev ~ ano_num, data = serie_temporal)\ncoef    &lt;- summary(lm_tend)$coefficients\n# Se p-valor &lt; 0.05: tend&ecirc;ncia validada com 95% de confian&ccedil;a")
        ),
        m_tags(
          tags$span(class="pm-tag pt","K-Means"), tags$span(class="pm-tag pt","Regress\u00e3o Linear"),
          tags$span(class="pm-tag bt","kmeans()"), tags$span(class="pm-tag bt","lm()"),
          tags$span(class="pm-tag gt","scale()"), tags$span(class="pm-tag gt","WCSS"),
          tags$span(class="pm-tag ot","N\u00e3o Supervisionado")
        )
      ),
      
      # ======== ETAPA 6 — DASHBOARD ========
      "6" = modalDialog(
        title = NULL, size = "l", easyClose = TRUE,
        footer = tagList(modalButton("\u2715  Fechar")),
        tags$div(class="pm-header pm-dark",
          tags$div(class="pm-number","06"),
          tags$span(class="pm-badge","ETAPA 06 \u00b7 INTERFACE DE DECIS\u00c3O"),
          tags$h3(class="pm-title", bs_icon("speedometer2")," Dashboard Executivo \u2014 Shiny UI/UX"),
          tags$p(class="pm-subtitle","Consolida\u00e7\u00e3o dos dados em pain\u00e9is interativos de an\u00e1lise de risco e precifica\u00e7\u00e3o")
        ),
        layout_columns(col_widths=c(3,3,3,3), fill=FALSE,
          m_stat("7","Abas Funcionais"),
          m_stat("Leaflet","Mapas Interativos"),
          m_stat("PDF/HTML","Relat\u00f3rios"),
          m_stat("Plotly","Gr\u00e1ficos")
        ),
        m_card("","map-fill","Mapeamento de Risco Geoespacial",
          "Visualiza\u00e7\u00e3o do zoneamento de vulnerabilidade via Leaflet com heatmap municipal, popups informativos, legenda profissional e sistema de clusters K-Means representado por escala de cores verde \u2192 laranja \u2192 vermelho."),
        m_card("gc","shield-lock-fill","M\u00f3dulo de Precifica\u00e7\u00e3o Atuarial",
          "Sistema de underwriting que estima pr\u00eamios de seguro agr\u00edcola por munic\u00edpio com base no \u00edndice de vulnerabilidade. Classifica em faixas atuariais: Padr\u00e3o, Agravado e Restritivo, com taxa de pr\u00eamio proporcional ao risco calculado."),
        m_card("oc","file-earmark-pdf","Relat\u00f3rios Export\u00e1veis",
          "Gera\u00e7\u00e3o din\u00e2mica de relat\u00f3rios t\u00e9cnicos em HTML ou PDF via rmarkdown::render(). Inclui relat\u00f3rio executivo por safra e relat\u00f3rio comparativo hist\u00f3rico multi-safra com todos os indicadores e visualiza\u00e7\u00f5es embutidas."),
        tags$div(class="pm-formula",
          HTML("# Stack tecnol&oacute;gico do Dashboard\nlibrary(shiny)      # Framework web reativo\nlibrary(bslib)      # UI premium Bootstrap 5\nlibrary(leaflet)    # Mapas interativos\nlibrary(plotly)     # Gr&aacute;ficos din&acirc;micos\nlibrary(DT)         # Tabelas interativas\nlibrary(rmarkdown)  # Exporta&ccedil;&atilde;o de relat&oacute;rios\n\n# Precifica&ccedil;&atilde;o Atuarial\npremio_pct &lt;- dplyr::case_when(\n  vulnerabilidade &lt;= 33 ~ vuln * 0.08 + 0.5,   # Padr&atilde;o\n  vulnerabilidade &lt;= 66 ~ vuln * 0.10 + 1.0,   # Agravado\n  TRUE               ~ vuln * 0.13 + 2.0    # Restritivo\n)")
        ),
        m_tags(
          tags$span(class="pm-tag bt","Shiny"), tags$span(class="pm-tag bt","bslib"),
          tags$span(class="pm-tag gt","leaflet"), tags$span(class="pm-tag gt","plotly"),
          tags$span(class="pm-tag ot","rmarkdown"), tags$span(class="pm-tag ot","DT"),
          tags$span(class="pm-tag pt","Underwriting"), tags$span(class="pm-tag rt","Precifica\u00e7\u00e3o")
        )
      ),
      
      NULL
    ) # end switch
    
    if (!is.null(modal_out)) showModal(modal_out)
  })
  
  # --- KPI CARDS ---
  output$ov_kpi_municipios <- renderUI({
    n <- n_distinct(dados_risco$municipio)
    tags$div(class = "kpi-card kpi-green",
      tags$div(class = "kpi-icon", bs_icon("geo-alt-fill")),
      tags$div(class = "kpi-label", "Munic\u00edpios Monitorados"),
      tags$div(class = "kpi-value", n),
      tags$div(class = "kpi-trend", "100% do Mato Grosso do Sul")
    )
  })
  
  output$ov_kpi_registros <- renderUI({
    n <- nrow(dados_risco)
    n_s <- n_distinct(dados_risco$safra)
    tags$div(class = "kpi-card kpi-blue",
      tags$div(class = "kpi-icon", bs_icon("database-fill")),
      tags$div(class = "kpi-label", "Registros Clim\u00e1ticos"),
      tags$div(class = "kpi-value", format(n, big.mark = ".")),
      tags$div(class = "kpi-trend", paste(n_s, "safras analisadas"))
    )
  })
  
  output$ov_kpi_cenario <- renderUI({
    tbl <- table(dados_risco$fase_enso[dados_risco$safra == ultima_safra_ov])
    cenario <- if (length(tbl) > 0) names(which.max(tbl)) else "N/D"
    cor_cls <- switch(cenario, "El Ni\u00f1o" = "kpi-orange", "La Ni\u00f1a" = "kpi-blue", "kpi-teal")
    tags$div(class = paste("kpi-card", cor_cls),
      tags$div(class = "kpi-icon", bs_icon("cloud-sun-fill")),
      tags$div(class = "kpi-label", "Cen\u00e1rio Clim\u00e1tico Atual"),
      tags$div(class = "kpi-value", style = "font-size:1.15rem;", cenario),
      tags$div(class = "kpi-trend", paste("Safra", ultima_safra_ov))
    )
  })
  
  output$ov_kpi_vulnerabilidade <- renderUI({
    df <- df_overview()
    v  <- round(mean(df$vulnerabilidade, na.rm = TRUE), 1)
    cor_cls <- if (v > 60) "kpi-red" else if (v > 35) "kpi-orange" else "kpi-green"
    trend_txt <- if (v > 60) "\u26a0 N\u00edvel Cr\u00edtico" else if (v > 35) "\u26a1 N\u00edvel de Alerta" else "\u2713 N\u00edvel Est\u00e1vel"
    tags$div(class = paste("kpi-card", cor_cls),
      tags$div(class = "kpi-icon", bs_icon("shield-exclamation")),
      tags$div(class = "kpi-label", "Vulnerabilidade M\u00e9dia"),
      tags$div(class = "kpi-value", paste(v, "/ 100")),
      tags$div(class = "kpi-trend", trend_txt)
    )
  })
  
  output$ov_kpi_alto_risco <- renderUI({
    df    <- df_overview()
    n     <- sum(df$vulnerabilidade > 66, na.rm = TRUE)
    total <- nrow(df)
    pct   <- if (total > 0) round(n / total * 100, 1) else 0
    tags$div(class = "kpi-card kpi-red",
      tags$div(class = "kpi-icon", bs_icon("exclamation-triangle-fill")),
      tags$div(class = "kpi-label", "Munic\u00edpios Alto Risco"),
      tags$div(class = "kpi-value", n),
      tags$div(class = "kpi-trend", paste(pct, "% do total"))
    )
  })
  
  output$ov_kpi_potencial <- renderUI({
    df  <- df_overview()
    val <- sum(df$perda_potencial_t, na.rm = TRUE) * 16.667 * 135
    val_m <- round(val / 1e6, 1)
    tags$div(class = "kpi-card kpi-purple",
      tags$div(class = "kpi-icon", bs_icon("currency-dollar")),
      tags$div(class = "kpi-label", "Potencial Econ\u00f4mico Exposto"),
      tags$div(class = "kpi-value", style = "font-size:1.1rem;",
        paste0("R$ ", format(val_m, big.mark = ".", decimal.mark = ","), " M")
      ),
      tags$div(class = "kpi-trend", "Estimativa de perda potencial")
    )
  })
  
  # --- MAPA OVERVIEW ---
  output$mapa_overview <- renderLeaflet({
    df <- df_overview()
    mapa_dados <- malha_ms %>% left_join(df, by = c("code_muni" = "cod_ibge"))
    mapa_dados <- mapa_dados %>% mutate(
      classe_risco = case_when(
        is.na(vulnerabilidade)   ~ "Sem Dados",
        vulnerabilidade <= 33    ~ "Baixo Risco",
        vulnerabilidade <= 66    ~ "M\u00e9dio Risco",
        TRUE                     ~ "Alto Risco"
      ),
      cor_risco = case_when(
        is.na(vulnerabilidade)   ~ "#BDBDBD",
        vulnerabilidade <= 33    ~ "#2E7D32",
        vulnerabilidade <= 66    ~ "#FB8C00",
        TRUE                     ~ "#B71C1C"
      )
    )
    leaflet(mapa_dados) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor   = ~cor_risco,
        weight      = 1.5,
        opacity     = 1,
        color       = "white",
        fillOpacity = 0.75,
        highlightOptions = highlightOptions(
          weight = 3, color = "#1565C0",
          fillOpacity = 0.92, bringToFront = TRUE
        ),
        popup = ~paste0(
          "<div style='font-family:Segoe UI,sans-serif;min-width:190px;padding:6px;'>",
          "<strong style='font-size:1rem;color:#1565C0;'>", name_muni, "</strong><br>",
          "<hr style='margin:5px 0;border-color:#eee;'>",
          "<b>Cluster:</b> ", classe_risco, "<br>",
          "<b>Vulnerabilidade:</b> ",
          ifelse(is.na(vulnerabilidade), "N/D",
                 paste(round(vulnerabilidade, 1), "/ 100")), "<br>",
          "<b>Severidade:</b> ",
          ifelse(is.na(severidade_estimada_pct), "N/D",
                 paste(round(severidade_estimada_pct, 1), "%")), "<br>",
          "<b>Produ\u00e7\u00e3o:</b> ",
          ifelse(is.na(producao_t), "N/D",
                 paste(format(round(producao_t, 0), big.mark = "."), "t")),
          "</div>"
        ),
        label = ~name_muni
      ) %>%
      addLegend(
        "bottomright",
        colors = c("#2E7D32", "#FB8C00", "#B71C1C", "#BDBDBD"),
        labels = c("Baixo Risco", "M\u00e9dio Risco", "Alto Risco", "Sem Dados"),
        title  = "Classifica\u00e7\u00e3o de Risco",
        opacity = 1
      ) %>%
      setView(lng = -54.5, lat = -20.5, zoom = 6)
  })
  
  # --- CLUSTER PIE CHART ---
  output$grafico_cluster_overview <- renderPlotly({
    df <- df_overview() %>%
      mutate(classe = case_when(
        vulnerabilidade <= 33 ~ "Baixo",
        vulnerabilidade <= 66 ~ "M\u00e9dio",
        TRUE                  ~ "Alto"
      )) %>%
      count(classe)
    
    plot_ly(
      df,
      labels = ~classe, values = ~n, type = "pie",
      marker = list(
        colors = c("#B71C1C", "#FB8C00", "#2E7D32"),
        line   = list(color = "white", width = 2)
      ),
      textinfo     = "label+percent",
      hovertemplate = "<b>%{label}</b><br>%{value} munic\u00edpios<extra></extra>"
    ) %>%
      layout(
        showlegend   = TRUE,
        margin       = list(t = 5, b = 30, l = 5, r = 5),
        paper_bgcolor = "transparent",
        plot_bgcolor  = "transparent",
        legend        = list(orientation = "h", x = 0.05, y = -0.15, font = list(size = 11))
      )
  })
  
  # --- ENSO BAR CHART ---
  output$grafico_cenarios_overview <- renderPlotly({
    df_enso <- dados_risco %>% count(fase_enso) %>% arrange(desc(n))
    plot_ly(
      df_enso,
      x = ~fase_enso, y = ~n,
      type  = "bar",
      color = ~fase_enso,
      colors = c("El Ni\u00f1o" = "#FB8C00", "Neutro" = "#1565C0", "La Ni\u00f1a" = "#2E7D32"),
      hovertemplate = "<b>%{x}</b><br>%{y} registros<extra></extra>"
    ) %>%
      layout(
        xaxis        = list(title = ""),
        yaxis        = list(title = "Registros"),
        paper_bgcolor = "transparent",
        plot_bgcolor  = "transparent",
        showlegend    = FALSE,
        margin        = list(t = 5)
      )
  })
  
  # --- DISTRIBUI\u00c7\u00c3O DE RISCO ---
  output$grafico_dist_risco <- renderPlotly({
    df <- df_overview() %>%
      arrange(desc(vulnerabilidade)) %>%
      head(20) %>%
      mutate(cor = case_when(
        vulnerabilidade > 66 ~ "#B71C1C",
        vulnerabilidade > 33 ~ "#FB8C00",
        TRUE                 ~ "#2E7D32"
      ))
    
    plot_ly(
      df,
      x = ~reorder(municipio, vulnerabilidade),
      y = ~vulnerabilidade,
      type = "bar",
      marker = list(color = ~cor, line = list(color = "white", width = 0.5)),
      hovertemplate = "<b>%{x}</b><br>Vulnerabilidade: %{y:.1f}<extra></extra>"
    ) %>%
      layout(
        xaxis         = list(title = "", tickangle = -45, tickfont = list(size = 10)),
        yaxis         = list(title = "\u00cdndice de Vulnerabilidade", range = c(0, 108)),
        paper_bgcolor = "transparent",
        plot_bgcolor  = "rgba(248,249,250,0.5)",
        margin        = list(b = 110)
      )
  })
  
  # --- SCATTER PRODU\u00c7\u00c3O x VULNERABILIDADE ---
  output$grafico_scatter_overview <- renderPlotly({
    df <- df_overview() %>%
      mutate(classe = case_when(
        vulnerabilidade <= 33 ~ "Baixo",
        vulnerabilidade <= 66 ~ "M\u00e9dio",
        TRUE                  ~ "Alto"
      ))
    
    plot_ly(
      df,
      x    = ~producao_t, y = ~vulnerabilidade,
      color = ~classe,
      colors = c("Baixo" = "#2E7D32", "M\u00e9dio" = "#FB8C00", "Alto" = "#B71C1C"),
      type = "scatter", mode = "markers",
      marker = list(size = 10, opacity = 0.82, line = list(color = "white", width = 1)),
      text = ~municipio,
      hovertemplate = paste0(
        "<b>%{text}</b><br>",
        "Produ\u00e7\u00e3o: %{x:,.0f} t<br>",
        "Vulnerabilidade: %{y:.1f}<extra></extra>"
      )
    ) %>%
      layout(
        xaxis         = list(title = "Produ\u00e7\u00e3o (t) — escala log", type = "log"),
        yaxis         = list(title = "Vulnerabilidade"),
        paper_bgcolor = "transparent",
        plot_bgcolor  = "rgba(248,249,250,0.5)",
        legend        = list(title = list(text = "Cluster"), orientation = "h", y = -0.18)
      )
  })
  
  # --- PAINEL DE MACHINE LEARNING ---
  output$ov_ml_panel <- renderUI({
    df <- df_overview()
    n_munis <- nrow(df)
    # M\u00e9tricas baseadas na rela\u00e7\u00e3o vulnerabilidade ~ severidade (modelo preditivo)
    df_lm <- df %>% filter(!is.na(vulnerabilidade), !is.na(severidade_estimada_pct))
    r2_val  <- if (nrow(df_lm) > 2) {
      round(cor(df_lm$vulnerabilidade, df_lm$severidade_estimada_pct)^2 * 100, 1)
    } else NA
    rmse_val <- if (nrow(df_lm) > 2) {
      lm_fit <- lm(vulnerabilidade ~ severidade_estimada_pct, data = df_lm)
      round(sqrt(mean(residuals(lm_fit)^2)), 2)
    } else NA
    mae_val  <- if (nrow(df_lm) > 2) {
      lm_fit2 <- lm(vulnerabilidade ~ severidade_estimada_pct, data = df_lm)
      round(mean(abs(residuals(lm_fit2))), 2)
    } else NA
    
    tags$div(style = "padding:6px;",
      tags$div(class = "ml-metric-card",
        tags$div(class = "ml-metric-icon iblue",   bs_icon("diagram-3-fill")),
        tags$div(
          tags$div(class = "ml-metric-label", "Algoritmo"),
          tags$div(class = "ml-metric-value", "K-Means (K = 3)")
        )
      ),
      tags$div(class = "ml-metric-card",
        tags$div(class = "ml-metric-icon igreen",  bs_icon("collection-fill")),
        tags$div(
          tags$div(class = "ml-metric-label", "Munic\u00edpios Clusterizados"),
          tags$div(class = "ml-metric-value", paste(n_munis, "munic\u00edpios"))
        )
      ),
      tags$div(class = "ml-metric-card",
        tags$div(class = "ml-metric-icon iblue",   bs_icon("list-check")),
        tags$div(
          tags$div(class = "ml-metric-label", "Vari\u00e1veis Utilizadas"),
          tags$div(class = "ml-metric-value",
            style = "font-size:0.82rem; font-weight:700;",
            "Produ\u00e7\u00e3o \u00b7 Chuva \u00b7 Severidade \u00b7 Vulnerabilidade"
          )
        )
      ),
      tags$hr(style = "margin:10px 0; border-color:#eee;"),
      tags$p(style = "font-size:0.68rem;font-weight:800;text-transform:uppercase;letter-spacing:1px;color:#78909C;margin-bottom:6px;",
        "M\u00e9tricas do Modelo (Regressão Preditiva)"),
      tags$div(class = "ml-metric-card",
        tags$div(class = "ml-metric-icon igreen",   bs_icon("bullseye")),
        tags$div(
          tags$div(class = "ml-metric-label", "R\u00b2"),
          tags$div(class = "ml-metric-value",
            if (!is.na(r2_val)) paste0(r2_val, "%") else "N/D")
        )
      ),
      tags$div(class = "ml-metric-card",
        tags$div(class = "ml-metric-icon iorange",  bs_icon("ruler")),
        tags$div(
          tags$div(class = "ml-metric-label", "RMSE"),
          tags$div(class = "ml-metric-value",
            if (!is.na(rmse_val)) rmse_val else "N/D")
        )
      ),
      tags$div(class = "ml-metric-card",
        tags$div(class = "ml-metric-icon iorange",  bs_icon("arrows-collapse")),
        tags$div(
          tags$div(class = "ml-metric-label", "MAE"),
          tags$div(class = "ml-metric-value",
            if (!is.na(mae_val)) mae_val else "N/D")
        )
      )
    )
  })
  
  # --- PAINEL DE PRECIFICA\u00c7\u00c3O ---
  output$ov_pricing_panel <- renderUI({
    df <- df_overview() %>%
      mutate(
        classe = case_when(
          vulnerabilidade <= 33 ~ "Baixo",
          vulnerabilidade <= 66 ~ "M\u00e9dio",
          TRUE                  ~ "Alto"
        ),
        premio_pct = case_when(
          vulnerabilidade <= 33 ~ paste0(round(vulnerabilidade * 0.08 + 0.5, 1), "%"),
          vulnerabilidade <= 66 ~ paste0(round(vulnerabilidade * 0.10 + 1.0, 1), "%"),
          TRUE                  ~ paste0(round(vulnerabilidade * 0.13 + 2.0, 1), "%")
        ),
        faixa = case_when(
          vulnerabilidade <= 33 ~ "Padr\u00e3o",
          vulnerabilidade <= 66 ~ "Agravado",
          TRUE                  ~ "Restritivo"
        )
      ) %>%
      arrange(desc(vulnerabilidade)) %>%
      head(14)
    
    badge_fn <- function(cl) {
      switch(cl, "Baixo" = "badge-low", "M\u00e9dio" = "badge-medium", "badge-high")
    }
    
    tags$div(style = "padding:4px; overflow-x:auto;",
      tags$p(
        style = "font-size:0.78rem;color:#607D8B;margin-bottom:8px;",
        bs_icon("info-circle"),
        paste(" Estimativa: \u00faltima safra (", ultima_safra_ov, ") | Cultura: Soja | Pr\u00e7: R$135/sc")
      ),
      tags$table(
        class = "pricing-table",
        tags$thead(
          tags$tr(
            tags$th("Munic\u00edpio",
              style = "padding:7px 12px;color:#455A64;font-size:0.78rem;font-weight:700;"),
            tags$th("Risco",
              style = "padding:7px 12px;color:#455A64;font-size:0.78rem;font-weight:700;"),
            tags$th("Pr\u00eamio Est.",
              style = "padding:7px 12px;color:#455A64;font-size:0.78rem;font-weight:700;"),
            tags$th("Faixa",
              style = "padding:7px 12px;color:#455A64;font-size:0.78rem;font-weight:700;")
          )
        ),
        do.call(tags$tbody, lapply(seq_len(nrow(df)), function(i) {
          row <- df[i, ]
          tags$tr(
            class = "pricing-row",
            tags$td(row$municipio),
            tags$td(tags$span(class = badge_fn(row$classe), row$classe)),
            tags$td(style = "font-weight:700;color:#263238;", row$premio_pct),
            tags$td(style = "color:#607D8B;font-size:0.78rem;", row$faixa)
          )
        }))
      )
    )
  })
  
  # --- DOWNLOAD ---
  output$download_csv <- downloadHandler(
    filename = function() { paste("dados_risco_agro_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(dados_filtrados(), file, row.names = FALSE) }
  )
  
  # --- GERADOR DE RELATÓRIO 1: EXECUTIVO POR SAFRA ---
  output$btn_baixar_relatorio <- downloadHandler(
    filename = function() {
      ext   <- input$relatorio_formato
      muni  <- if (input$relatorio_municipio == "Todos") "EstadoMS" else gsub(" ", "_", input$relatorio_municipio)
      paste0("Relatorio_AgroRisk_", gsub("/", "-", input$relatorio_safra), "_", muni, "_", Sys.Date(), ".", ext)
    },
    content = function(file) {
      id <- showNotification("Renderizando relatório... Isso pode levar alguns instantes.", duration = NULL, type = "message")
      on.exit(removeNotification(id), add = TRUE)
      
      df_report <- dados_risco %>% filter(safra == input$relatorio_safra)
      
      tempReport <- file.path(tempdir(), "relatorio_dinamico.Rmd")
      file.copy("relatorio_dinamico.Rmd", tempReport, overwrite = TRUE)
      
      params_list <- list(
        safra      = input$relatorio_safra,
        variaveis  = input$relatorio_vars,
        dados      = df_report,
        malha      = malha_ms,
        municipio  = input$relatorio_municipio,
        dados_hist = dados_risco   # para o gráfico histórico por município
      )
      
      out_fmt <- if (input$relatorio_formato == "pdf") "pdf_document" else "html_document"
      
      tryCatch({
        rmarkdown::render(
          tempReport,
          output_file    = file,
          params         = params_list,
          output_format  = out_fmt,
          output_options = if(out_fmt == "html_document") list(self_contained = TRUE) else list(latex_engine = "xelatex"),
          envir          = new.env(parent = globalenv()),
          encoding       = "UTF-8"
        )
      }, error = function(e) {
        showNotification(paste("Erro na geração do relatório:", e$message), type = "error", duration = 10)
      })
    }
  )
  
  # --- BOTÃO COMPARATIVO: desabilita se < 2 safras selecionadas ---
  output$btn_comparativo_ui <- renderUI({
    n <- length(input$comp_safras)
    if (is.null(n) || n < 2) {
      tags$div(
        downloadButton("btn_baixar_comparativo", "⬇ Gerar Relatório Comparativo", class = "btn-download-custom", disabled = NA),
        tags$small("Selecione pelo menos 2 safras para habilitar.", style = "color:#999; display:block; margin-top:6px;")
      )
    } else {
      downloadButton("btn_baixar_comparativo", paste0("⬇ Gerar Comparativo (", n, " safras)"), class = "btn-download-custom")
    }
  })
  
  # --- GERADOR DE RELATÓRIO 2: COMPARATIVO HISTÓRICO MULTI-SAFRA ---
  output$btn_baixar_comparativo <- downloadHandler(
    filename = function() {
      ext    <- input$comp_formato
      safras <- paste(gsub("/", "-", input$comp_safras), collapse = "_vs_")
      muni   <- if (input$comp_municipio == "Todos") "" else paste0("_", gsub(" ", "_", input$comp_municipio))
      paste0("Relatorio_Comparativo_AgroRisk", muni, "_", Sys.Date(), ".", ext)
    },
    content = function(file) {
      req(length(input$comp_safras) >= 2)
      id <- showNotification("Gerando relatório comparativo... Aguarde.", duration = NULL, type = "message")
      on.exit(removeNotification(id), add = TRUE)
      
      df_comp <- dados_risco %>% filter(safra %in% input$comp_safras)
      if (input$comp_municipio != "Todos") {
        df_comp <- df_comp %>% filter(municipio == input$comp_municipio)
      }
      
      tempComp <- file.path(tempdir(), "relatorio_comparativo.Rmd")
      file.copy("relatorio_comparativo.Rmd", tempComp, overwrite = TRUE)
      
      params_comp <- list(
        safras    = input$comp_safras,
        municipio = input$comp_municipio,
        dados     = df_comp,
        malha     = malha_ms
      )
      
      out_fmt <- if (input$comp_formato == "pdf") "pdf_document" else "html_document"
      
      tryCatch({
        rmarkdown::render(
          tempComp,
          output_file    = file,
          params         = params_comp,
          output_format  = out_fmt,
          output_options = if(out_fmt == "html_document") list(self_contained = TRUE) else list(latex_engine = "xelatex"),
          envir          = new.env(parent = globalenv()),
          encoding       = "UTF-8"
        )
      }, error = function(e) {
        showNotification(paste("Erro no relatório comparativo:", e$message), type = "error", duration = 10)
      })
    }
  )
}

# CONSOLIDAR E LANÇAR O APLICATIVO APP.R
shinyApp(ui = ui, server = server)