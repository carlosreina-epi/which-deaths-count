# =============================================================================
# 00_config.R  ·  Configuración común del proyecto
# Artículo: Crisis globales y mortalidad — Gran Hambruna China (1959–1961)
#           vs pandemia de COVID-19 (2020–2021): exceso de mortalidad ajustado
#           por tamaño y estructura etaria de la población.
#
# USO: abra el proyecto (Articulo_CrisisGlobales_Mortalidad.Rproj) o fije el
#      directorio de trabajo en la carpeta raíz del artículo y ejecute:
#        source("03_codigo/run_all.R")
#      Cada script hace source() de este archivo; no hace falta correrlo aparte.
# =============================================================================

# ---- 1. Paquetes -------------------------------------------------------------
# Dependencias mínimas a propósito: data.table (velocidad con ~4,5 M filas de
# tasas por edad simple), ggplot2/scales (figuras), openxlsx (tablas para Word)
# El cruce ISO3 -> código numérico ONU usa 02_datos/manual/iso3_codigo_numerico.csv.
pkgs <- c("data.table", "ggplot2", "scales", "openxlsx")
faltan <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltan) > 0) install.packages(faltan, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only = TRUE))

options(timeout = 1200)          # mx1dt.rda pesa ~87 MB
setDTthreads(0)                  # usa todos los núcleos disponibles

# ---- 2. Rutas (relativas a la raíz del proyecto) ----------------------------
if (!dir.exists("03_codigo")) {
  stop("Fije el directorio de trabajo en la carpeta raíz del artículo ",
       "(la que contiene 02_datos/, 03_codigo/, 04_resultados/).")
}
DIR_RAW   <- "02_datos/raw"
DIR_MAN   <- "02_datos/manual"
DIR_PROC  <- "02_datos/processed"
DIR_FIG   <- "04_resultados/figuras"
DIR_TAB   <- "04_resultados/tablas"
for (d in c(DIR_RAW, DIR_MAN, DIR_PROC, DIR_FIG, DIR_TAB)) dir.create(d, FALSE, TRUE)

# ---- 3. Periodos de crisis y líneas de base ----------------------------------
# Periodo principal de cada crisis
CRISIS <- list(
  hambruna = 1959:1961,
  covid    = 2020:2021
)

# Especificaciones de la línea base (contrafactual "sin crisis").
#   ajuste_anios : años usados para estimar la tendencia de log(m_x)
#   metodo       : "lineal" = log(m_x) ~ año (extrapolado)
#                  "media"  = media de log(m_x) en los años de ajuste (sin tendencia)
# La especificación "A" es la principal; el resto es análisis de sensibilidad.
# Nota: 1958 se excluye de la principal porque varias provincias chinas ya
# mostraban sobremortalidad ese año (Peng 1987; Zhao & Reimondos 2012).
BASELINES <- data.table::rbindlist(list(
  list(crisis = "hambruna", spec = "A", label = "Lineal 1950–1957 (principal)",
       ajuste = list(1950:1957), metodo = "lineal"),
  list(crisis = "hambruna", spec = "B", label = "Lineal 1953–1957",
       ajuste = list(1953:1957), metodo = "lineal"),
  list(crisis = "hambruna", spec = "C", label = "Media 1955–1957",
       ajuste = list(1955:1957), metodo = "media"),
  list(crisis = "hambruna", spec = "D", label = "Lineal 1950–1958",
       ajuste = list(1950:1958), metodo = "lineal"),
  list(crisis = "hambruna", spec = "E", label = "Interpolación 1950–1957 + 1963–1966",
       ajuste = list(c(1950:1957, 1963:1966)), metodo = "lineal"),
  list(crisis = "covid", spec = "A", label = "Lineal 2015–2019 (principal)",
       ajuste = list(2015:2019), metodo = "lineal"),
  list(crisis = "covid", spec = "B", label = "Lineal 2010–2019",
       ajuste = list(2010:2019), metodo = "lineal"),
  list(crisis = "covid", spec = "C", label = "Media 2017–2019",
       ajuste = list(2017:2019), metodo = "media"),
  list(crisis = "covid", spec = "D", label = "Lineal 2012–2019",
       ajuste = list(2012:2019), metodo = "lineal")
))
SPEC_PRINCIPAL <- "A"

# ---- 4. Población estándar OMS 2000–2025 (Ahmad et al., 2001) ---------------
# Pesos por 100 000 en grupos quinquenales; se usa para tasas estandarizadas.
STD_WHO <- data.table(
  grupo = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
            "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84",
            "85-89","90-94","95-99","100+"),
  peso  = c(8860, 8690, 8600, 8470, 8220, 7930, 7610, 7150, 6590, 6040, 5370,
            4550, 3720, 2960, 2210, 1520, 910, 440, 150, 40, 5)
)
STD_WHO[, peso := peso / sum(peso)]   # suma exacta = 1

grupo_edad <- function(edad) {
  lo <- pmin(floor(edad / 5) * 5, 100)
  factor(ifelse(lo == 100, "100+", paste0(lo, "-", lo + 4)), levels = STD_WHO$grupo)
}

# ---- 5. Regiones -------------------------------------------------------------
# Se usan las 6 áreas geográficas de la ONU (UNlocations: area_name). Se
# mantienen separadas América Latina y el Caribe y América del Norte porque su
# experiencia en COVID-19 fue distinta; en el texto pueden agruparse.
REGION_ES <- c(
  "Africa"                          = "África",
  "Asia"                            = "Asia",
  "Europe"                          = "Europa",
  "Latin America and the Caribbean" = "América Latina y el Caribe",
  "Northern America"                = "América del Norte",
  "Oceania"                         = "Oceanía"
)

# ---- 6. Estilo gráfico (escala de grises: requisito de la revista) ----------
theme_articulo <- function(base = 11) {
  theme_bw(base_size = base, base_family = "") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
          strip.background = element_rect(fill = "grey92", colour = "grey60"),
          legend.position  = "bottom",
          plot.title.position = "plot")
}
guardar_fig <- function(p, nombre, ancho = 17, alto = 11) {
  # PNG y TIFF a 300 ppp, ancho en cm (columna doble ≈ 17 cm)
  ggsave(file.path(DIR_FIG, paste0(nombre, ".png")),  p, width = ancho, height = alto,
         units = "cm", dpi = 300, bg = "white")
  ggsave(file.path(DIR_FIG, paste0(nombre, ".tiff")), p, width = ancho, height = alto,
         units = "cm", dpi = 300, compression = "lzw", bg = "white")
  invisible(p)
}

# Formato numérico de la revista: miles con espacio fino, decimales con coma
fmt_n <- function(x, d = 0) formatC(x, format = "f", digits = d, big.mark = " ",
                                     decimal.mark = ",")
