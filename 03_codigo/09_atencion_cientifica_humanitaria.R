# =============================================================================
# 09_atencion_cientifica_humanitaria.R  ·  ¿Cuánta atención recibe cada muerte?
#
# HIPÓTESIS (eje del artículo)
#   La atención científica y financiera por muerte en exceso es mucho mayor para
#   la COVID-19 que para guerras, genocidios y hambrunas de intensidad igual o
#   superior. Se mide, no se afirma.
#
# MÉTRICAS
#   A. Atención científica (PubMed, E-utilities de NCBI; acceso libre)
#      Publicaciones sobre el episodio publicadas desde su inicio hasta 3 años
#      después del fin, por 1000 muertes en exceso. Para controlar el crecimiento
#      de PubMed se reporta también la proporción sobre todo lo indexado en esa
#      ventana (por millón de registros).
#      Comparación principal: episodios que empiezan en 2000 o después (PubMed y
#      la financiación humanitaria son comparables en ese periodo). Los
#      episodios anteriores se reportan como material suplementario.
#   B. Atención financiera (OCHA Financial Tracking Service, API pública)
#      Financiación humanitaria recibida por el país en los años del episodio,
#      en USD por muerte en exceso. Solo episodios no-COVID desde 2000: la
#      respuesta a la COVID-19 se financió sobre todo fuera del sistema
#      humanitario, así que se compara con la cifra mundial publicada
#      (ver referencia en el texto) y no país a país.
#
# EJECUCIÓN: requiere internet. Tarda ~5–10 min (NCBI limita a 3 consultas/s;
#   con clave gratuita de NCBI, 10/s: guárdela en .Renviron como ENTREZ_KEY).
#   Los resultados se guardan en caché: si se interrumpe, vuelva a correrlo.
# =============================================================================
source("03_codigo/00_config.R")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite", repos = "https://cloud.r-project.org")
library(jsonlite)

episodios <- fread(file.path(DIR_TAB, "choques_episodios_1955_2023.csv"), encoding = "UTF-8")
# Solo episodios incluidos en el análisis principal (script 07), sin fluctuaciones menores
episodios <- episodios[incluido == TRUE & !grepl("^Fluctuación", tipo)]
iso <- fread(file.path(DIR_MAN, "iso3_codigo_numerico.csv"))
episodios <- merge(episodios, iso[, .(country_code, iso3c)], by = "country_code", all.x = TRUE)

# ---- Nombre de búsqueda del país (WPP usa nombres oficiales largos) ----------
nombre_pubmed <- c(
  "Syrian Arab Republic" = "Syria", "Iran (Islamic Republic of)" = "Iran",
  "Venezuela (Bolivarian Republic of)" = "Venezuela",
  "Bolivia (Plurinational State of)" = "Bolivia", "Russian Federation" = "Russia",
  "Republic of Korea" = "South Korea", "Dem. People's Republic of Korea" = "North Korea",
  "Viet Nam" = "Vietnam", "State of Palestine" = "Palestine OR Gaza",
  "Turkiye" = "Turkey", "Myanmar" = "Myanmar OR Burma",
  "Kosovo (under UNSC res. 1244)" = "Kosovo", "China, Hong Kong SAR" = "Hong Kong",
  "Republic of Moldova" = "Moldova", "United States of America" = "United States",
  "Czechia" = "Czech Republic OR Czechia", "United Republic of Tanzania" = "Tanzania",
  "Central African Republic" = "Central African Republic",
  "Bosnia and Herzegovina" = "Bosnia", "Lao People's Democratic Republic" = "Laos",
  "China, Taiwan Province of China" = "Taiwan", "China, Macao SAR" = "Macao OR Macau",
  "Cote d'Ivoire" = "Ivory Coast OR Cote d'Ivoire", "Democratic Republic of the Congo" = "Congo"
)
episodios[, pais_busqueda := fifelse(pais %in% names(nombre_pubmed),
                                     nombre_pubmed[pais], sub(" \\(.*", "", pais))]

terminos_tipo <- c(
  "Conflicto armado"             = "war OR armed conflict OR conflict OR violence OR refugees",
  "Genocidio / violencia masiva" = "genocide OR mass killing OR massacre OR war OR violence",
  "Hambruna"                     = "famine OR starvation OR malnutrition OR food insecurity",
  "Desastre natural"             = "earthquake OR cyclone OR hurricane OR tsunami OR flood OR disaster",
  "Pandemia COVID-19"            = "COVID-19 OR SARS-CoV-2 OR coronavirus",
  "Epidemia de VIH/sida"         = "HIV OR AIDS",
  "Crisis socioeconómica"        = "mortality crisis OR economic crisis OR transition",
  "Mixto"                        = "war OR conflict OR famine OR genocide OR disaster",
  "Otra epidemia"                = "influenza OR epidemic",
  "Evento climático extremo"     = "cold wave OR heat wave OR cold spell OR extreme weather"
)
construir_query <- function(pais_b, tipo, ini, fin) {
  pais_q <- paste0("(", paste0('"', trimws(strsplit(pais_b, " OR ")[[1]]), '"[tiab]',
                               collapse = " OR "), ")")
  tema_q <- paste0("(", paste0('"', trimws(strsplit(terminos_tipo[[tipo]], " OR ")[[1]]),
                               '"[tiab]', collapse = " OR "), ")")
  sprintf('%s AND %s AND ("%d"[dp] : "%d"[dp])', pais_q, tema_q, ini, fin + 3)
}
episodios[, query := mapply(construir_query, pais_busqueda, tipo, inicio, fin)]

# ---- Consultas a PubMed con caché -------------------------------------------------
cache_f <- file.path(DIR_PROC, "cache_pubmed.csv")
cache <- if (file.exists(cache_f)) fread(cache_f, integer64 = "double") else data.table(query = character(), n = numeric())
pubmed_count <- function(q) {
  hit <- cache[query == q]
  if (nrow(hit)) return(hit$n[1])
  key <- Sys.getenv("ENTREZ_KEY")
  url <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed",
                "&retmode=json&rettype=count&term=", URLencode(q, reserved = TRUE),
                if (nzchar(key)) paste0("&api_key=", key) else "")
  n <- tryCatch(as.numeric(fromJSON(url)$esearchresult$count), error = function(e) NA_real_)
  Sys.sleep(if (nzchar(key)) 0.12 else 0.4)
  if (!is.na(n)) {
    cache <<- rbind(cache, data.table(query = q, n = n))
    fwrite(cache, cache_f)
  }
  n
}
message("Consultando PubMed para ", nrow(episodios), " episodios...")
episodios[, pubmed_n := vapply(query, pubmed_count, numeric(1))]
# Denominador: todo PubMed en la misma ventana
episodios[, query_total := sprintf('("%d"[dp] : "%d"[dp])', inicio, fin + 3)]
episodios[, pubmed_total := vapply(query_total, pubmed_count, numeric(1))]
episodios[, `:=`(
  pub_por_1000_muertes = 1000 * pubmed_n / exceso_total,
  pub_por_millon_pubmed = 1e6 * pubmed_n / pubmed_total
)]

# ---- Sensibilidad de la búsqueda en PubMed (episodios desde 2000) -------------
# (1) Solo título: país y términos del tipo en [ti] en vez de [tiab] -> menos
#     falsos positivos (artículos que mencionan el país de pasada).
# (2) Solo registros MEDLINE (indexados con MeSH): AND medline[sb].
# Si el orden de los tipos por publicaciones/1000 muertes se mantiene, el
# hallazgo no depende de la definición de la búsqueda.
ep_s <- episodios[inicio >= 2000]
ep_s[, query_ti      := gsub("[tiab]", "[ti]", query, fixed = TRUE)]
ep_s[, query_medline := paste0("(", query, ") AND medline[sb]")]
message("Sensibilidad PubMed: ", 2 * nrow(ep_s), " consultas adicionales...")
ep_s[, pubmed_n_ti      := vapply(query_ti, pubmed_count, numeric(1))]
ep_s[, pubmed_n_medline := vapply(query_medline, pubmed_count, numeric(1))]
sens_pm <- ep_s[, .(
  episodios       = .N,
  mediana_tiab    = median(1000 * pubmed_n / exceso_total, na.rm = TRUE),
  mediana_ti      = median(1000 * pubmed_n_ti / exceso_total, na.rm = TRUE),
  mediana_medline = median(1000 * pubmed_n_medline / exceso_total, na.rm = TRUE)
), by = tipo][order(-mediana_tiab)]
fwrite(sens_pm, file.path(DIR_TAB, "atencion_sensibilidad_pubmed.csv"))
fwrite(ep_s[, .(id_episodio, pais, inicio, fin, tipo, exceso_total,
                pubmed_n, pubmed_n_ti, pubmed_n_medline)],
       file.path(DIR_TAB, "atencion_sensibilidad_pubmed_por_episodio.csv"))
cat("\n== Sensibilidad PubMed: publicaciones por 1000 muertes (mediana por tipo) ==\n")
print(sens_pm, digits = 3)
cat(sprintf("Spearman orden de tipos: [ti] vs [tiab] = %.2f; MEDLINE vs [tiab] = %.2f\n",
            cor(sens_pm$mediana_ti, sens_pm$mediana_tiab, method = "spearman", use = "complete.obs"),
            cor(sens_pm$mediana_medline, sens_pm$mediana_tiab, method = "spearman", use = "complete.obs")))

# ---- Financiación humanitaria (OCHA FTS) --------------------------------------
fts_cache_f <- file.path(DIR_PROC, "cache_fts.csv")
fts_cache <- if (file.exists(fts_cache_f)) fread(fts_cache_f, integer64 = "double") else
  data.table(iso3c = character(), year = integer(), usd = numeric())
fts_country_year <- function(iso3, yr) {
  hit <- fts_cache[iso3c == iso3 & year == yr]
  if (nrow(hit)) return(hit$usd[1])
  url <- sprintf("https://api.hpc.tools/v1/public/fts/flow?countryISO3=%s&year=%d", iso3, yr)
  usd <- tryCatch({
    js <- fromJSON(url, simplifyVector = FALSE)
    as.numeric(js$data$incoming$fundingTotal)
  }, error = function(e) NA_real_)
  if (length(usd) == 0) usd <- NA_real_
  Sys.sleep(0.3)
  if (!is.na(usd)) {
    fts_cache <<- rbind(fts_cache, data.table(iso3c = iso3, year = yr, usd = usd))
    fwrite(fts_cache, fts_cache_f)
  }
  usd
}
ep_fts <- episodios[inicio >= 2000 & tipo != "Pandemia COVID-19" & !is.na(iso3c)]
if (nrow(ep_fts)) {
  message("Consultando OCHA FTS para ", nrow(ep_fts), " episodios...")
  ep_fts[, fts_usd := mapply(function(i3, a, b) {
    v <- vapply(a:b, function(y) fts_country_year(i3, y), numeric(1))
    if (all(is.na(v))) NA_real_ else sum(v, na.rm = TRUE)
  }, iso3c, inicio, fin)]
  ep_fts[, usd_por_muerte := fts_usd / exceso_total]
  episodios <- merge(episodios, ep_fts[, .(id_episodio, fts_usd, usd_por_muerte)],
                     by = "id_episodio", all.x = TRUE)
}
fwrite(episodios, file.path(DIR_TAB, "atencion_por_episodio.csv"))

# ---- Resumen por tipo ----------------------------------------------------------
res <- episodios[inicio >= 2000, .(
  episodios                 = .N,
  exceso_total_M            = sum(exceso_total) / 1e6,
  mediana_pub_por_1000      = median(pub_por_1000_muertes, na.rm = TRUE),
  mediana_pub_por_millon    = median(pub_por_millon_pubmed, na.rm = TRUE),
  mediana_usd_por_muerte    = median(usd_por_muerte, na.rm = TRUE)
), by = tipo][order(-mediana_pub_por_1000)]
fwrite(res, file.path(DIR_TAB, "atencion_resumen_por_tipo_2000_2023.csv"))
cat("\n== Atención por tipo de evento (episodios desde 2000) ==\n"); print(res)

# Figura 7: intensidad (Δe0) vs atención científica por muerte, episodios >= 2000
f7d <- episodios[inicio >= 2000 & pubmed_n > 0]
f7d[, es_covid := fifelse(tipo == "Pandemia COVID-19", "COVID-19", "Guerras, hambrunas, desastres y otros")]
etiq <- f7d[es_covid != "COVID-19"][order(delta_e0_min)][1:6]
etiq[, etiqueta := paste(sub(" \\(.*", "", pais), inicio)]
f7 <- ggplot(f7d, aes(delta_e0_min, pub_por_1000_muertes, shape = es_covid)) +
  geom_point(size = 2, fill = "white") +
  geom_text(data = etiq, aes(label = etiqueta), size = 2.3, vjust = -1, hjust = 0.1, show.legend = FALSE) +
  scale_x_continuous(expand = expansion(mult = c(0.08, 0.03))) +
  scale_shape_manual(values = c(21, 16)) +
  scale_y_log10(labels = label_number(big.mark = " ", decimal.mark = ",")) +
  labs(x = "Cambio en la esperanza de vida en el peor año (años)",
       y = "Publicaciones en PubMed por 1000\nmuertes en exceso (escala log)",
       shape = NULL,
       caption = "Episodios de choque de mortalidad iniciados entre 2000 y 2023. Fuentes: UN WPP 2024; PubMed.") +
  theme_articulo()
guardar_fig(f7, "Figura7_intensidad_vs_atencion", alto = 12)
