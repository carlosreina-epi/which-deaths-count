# =============================================================================
# 05_validacion_externa.R  ·  Contraste con estimaciones independientes
#
# COVID-19 (2020–2021)
#   - The Economist (modelo global, IC 95 %) a 31-dic-2021, país por país.
#   - OMS (Msemburi et al., Nature 2023): 14,83 M (IC 95 %: 13,2–16,6 M). Si el
#     ZIP de la OMS está en 02_datos/raw/who_excess/ se intenta leer por país.
# Gran Hambruna China (1959–1961)
#   - No existe un "estándar de oro": se compara con el rango de la literatura
#     demográfica (02_datos/manual/china_hambruna_estimaciones_literatura.csv).
#     Ojo: WPP reconstruye los años 50–60 de China con las mismas fuentes
#     (censos 1953/1964/1982, encuesta de fecundidad 1982) que usaron Coale o
#     Banister: la coincidencia no es independiente, es coherencia.
# =============================================================================
source("03_codigo/00_config.R")

pais_anio <- readRDS(file.path(DIR_PROC, "exceso_pais_anio.rds"))
periodo   <- fread(file.path(DIR_TAB, "exceso_region_periodo.csv"))
iso       <- fread(file.path(DIR_MAN, "iso3_codigo_numerico.csv"))

nuestro_covid <- pais_anio[crisis == "covid" & spec == SPEC_PRINCIPAL & year %in% CRISIS$covid,
                           .(exceso_wpp = sum(exceso), pop = mean(pop)),
                           by = .(country_code, pais, region)]

# ---- 1. The Economist ----------------------------------------------------------
eco <- fread(file.path(DIR_RAW, "economist_export_country_cumulative.csv"))
eco[, date := as.IDate(date)]
eco <- eco[date == max(date[date <= as.IDate("2021-12-31")])]
eco <- merge(eco, iso[, .(iso3c, country_code)], by = "iso3c")
eco <- eco[, .(country_code,
               exceso_eco     = cumulative_estimated_daily_excess_deaths,
               exceso_eco_inf = cumulative_estimated_daily_excess_deaths_ci_95_bot,
               exceso_eco_sup = cumulative_estimated_daily_excess_deaths_ci_95_top)]
comp <- merge(nuestro_covid, eco, by = "country_code")
comp[, dentro_ic95 := exceso_wpp >= exceso_eco_inf & exceso_wpp <= exceso_eco_sup]

resumen_eco <- comp[, .(
  paises            = .N,
  exceso_wpp_M      = sum(exceso_wpp) / 1e6,
  exceso_eco_M      = sum(exceso_eco) / 1e6,
  exceso_eco_inf_M  = sum(exceso_eco_inf) / 1e6,   # suma de límites: solo orientativa
  exceso_eco_sup_M  = sum(exceso_eco_sup) / 1e6,
  pct_dentro_ic95   = 100 * mean(dentro_ic95),
  spearman_tasas    = cor(exceso_wpp / pop, exceso_eco / pop, method = "spearman")
)]
fwrite(comp,        file.path(DIR_TAB, "validacion_covid_economist_por_pais.csv"))
fwrite(resumen_eco, file.path(DIR_TAB, "validacion_covid_economist_resumen.csv"))
cat("\n== COVID-19: WPP 2024 (este estudio) vs The Economist ==\n"); print(resumen_eco)
cat("Referencia OMS: 14,83 M (IC 95 % 13,2–16,6 M) — Msemburi et al. 2023\n")

p <- ggplot(comp, aes(x = 1e5 * exceso_eco / pop, y = 1e5 * exceso_wpp / pop)) +
  geom_abline(linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = 1e5 * exceso_eco_inf / pop, xmax = 1e5 * exceso_eco_sup / pop),
                 colour = "grey75", height = 0) +
  geom_point(shape = 21, fill = "grey30", colour = "white", size = 1.8) +
  labs(x = "Exceso por 100 000 hab., The Economist (IC 95 %)",
       y = "Exceso por 100 000 hab., este estudio (WPP 2024)",
       caption = "Cada punto es un país; 2020–2021 acumulado. Línea discontinua: concordancia perfecta.") +
  theme_articulo()
guardar_fig(p, "FigS1_validacion_covid_economist", ancho = 14, alto = 13)

# ---- 2. OMS (opcional) -----------------------------------------------------------
dir_who <- file.path(DIR_RAW, "who_excess")
if (dir.exists(dir_who)) {
  cand <- list.files(dir_who, pattern = "\\.(csv|xlsx)$", recursive = TRUE, full.names = TRUE)
  message("Archivos OMS encontrados: ", paste(basename(cand), collapse = ", "),
          "\nRevise los nombres de columnas y complete este bloque si desea el cruce por país.")
  # Estructura esperada (puede variar entre versiones de la OMS): país ISO3,
  # año, media e IC de exceso. Adapte aquí cuando tenga el archivo.
} else {
  message("Sin archivos OMS: se reporta solo el total publicado (14,83 M).")
}

# ---- 3. China 1959–1961 vs literatura ------------------------------------------
lit <- fread(file.path(DIR_MAN, "china_hambruna_estimaciones_literatura.csv"))
china_wpp <- periodo[crisis == "hambruna" & unidad == "China",
                     .(spec, exceso_millones = exceso / 1e6)]
china_wpp <- merge(china_wpp, BASELINES[crisis == "hambruna", .(spec, label)], by = "spec")
tab_china <- rbindlist(list(
  lit[, .(fuente = paste0(autor, " (", anio_publicacion, ")"), periodo,
          exceso_millones = exceso_muertes_millones, tipo = "Literatura")],
  china_wpp[, .(fuente = paste0("Este estudio, WPP 2024 — ", label), periodo = "1959-1961",
                exceso_millones, tipo = "Este estudio")]
))
fwrite(tab_china, file.path(DIR_TAB, "validacion_china_literatura.csv"))
cat("\n== China 1959–1961: este estudio vs literatura ==\n")
print(tab_china[, .(fuente, periodo, exceso_millones = round(exceso_millones, 1))])
