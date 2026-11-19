# =============================================================================
# 07_choques_mortalidad_1950_2023.R  ·  Inventario de choques de mortalidad
#
# OBJETIVO
#   Identificar, con un criterio único y reproducible, todos los episodios de
#   sobremortalidad ("choques") en 237 países entre 1955 y 2023, medir su
#   intensidad con métricas comparables (P-score, exceso por 1000 hab., Δe0) y
#   clasificarlos por tipo de evento (conflicto armado, genocidio, hambruna,
#   desastre natural, epidemia/pandemia).
#
# MÉTODO
#   Para cada país y año t (1955–2023):
#     1. Línea base móvil: tendencia log-lineal de m(x) por grupo quinquenal
#        ajustada en los 10 años previos (t-10 … t-1), con al menos 5 años.
#     2. Dos pasadas: en la 1.ª se marcan los años con P-score >= UMBRAL_P;
#        en la 2.ª se reajusta excluyendo esos años de la línea base, para que
#        un choque no "contamine" el contrafactual del siguiente.
#     3. Exceso, P-score, exceso por 1000 hab. y Δe0 (tabla de vida con tasas
#        contrafactuales, igual que en el script 04).
#   Episodio = años consecutivos con P-score >= UMBRAL_P y exceso >= MIN_EXCESO.
#
# LIMITACIONES QUE DEBEN IR AL TEXTO
#   - Antes de 1955 no hay 5 años de línea base: la Guerra de Corea (1950–53)
#     queda fuera; las guerras mundiales se analizan con HMD (script 08).
#   - En contextos sin registro vital, WPP construye los choques a partir de
#     estimaciones externas de muertes por conflicto o desastre: la magnitud es
#     tan buena como esas estimaciones (no son datos independientes).
#   - Resolución anual: un choque de semanas se diluye en el año.
# =============================================================================
source("03_codigo/00_config.R")

UMBRAL_P   <- 10      # % sobre lo esperado para marcar un año como choque
MIN_EXCESO <- 1000    # defunciones en exceso mínimas por año
VENTANA    <- 10      # años de línea base
MIN_BASE   <- 5       # años mínimos válidos en la línea base
UMBRAL_CAIDA <- -0.25 # caída OBSERVADA mínima de e0 (años) frente al máximo de
                      # los 3 años previos. Sin caída observada, el "exceso" solo
                      # refleja que la mejora se desaceleró respecto a una tendencia
                      # extrapolada (p. ej., Corea del Sur 1955–60 tras la guerra):
                      # se marca como artefacto y se excluye del análisis principal.

panel  <- readRDS(file.path(DIR_PROC, "panel_pais_anio_edad.rds"))
paises <- readRDS(file.path(DIR_PROC, "paises_regiones.rds"))
e0_wpp <- readRDS(file.path(DIR_PROC, "e0_wpp.rds"))
panel[, grupo := grupo_edad(age)]

grp <- panel[, .(deaths = sum(deaths), pop = sum(pop)), by = .(country_code, year, grupo)]
grp[, lmx := fifelse(deaths > 0 & pop > 0, log(deaths / pop), NA_real_)]
setkey(grp, country_code, grupo, year)

# ---- Contrafactual para el año t con una línea base dada ---------------------
predecir_anio <- function(t, excluir) {
  base <- grp[year %between% c(t - VENTANA, t - 1)]
  if (!is.null(excluir)) base <- base[!excluir, on = .(country_code, year)]
  fit <- base[is.finite(lmx),
              .(n = .N, tbar = mean(year), ybar = mean(lmx),
                b = if (.N >= 3) sum((year - mean(year)) * (lmx - mean(lmx))) /
                                 sum((year - mean(year))^2) else 0),
              by = .(country_code, grupo)]
  fit <- fit[n >= MIN_BASE]
  obj <- grp[year == t, .(country_code, grupo, year, deaths, pop)]
  obj <- merge(obj, fit, by = c("country_code", "grupo"))
  obj[, deaths_esp := exp(ybar + b * (t - tbar)) * pop]
  obj[, .(country_code, grupo, year, deaths, pop, deaths_esp)]
}

calcular <- function(excluir = NULL) {
  pred <- rbindlist(lapply(1955:2023, predecir_anio, excluir = excluir))
  # Solo país-años con los 21 grupos de edad evaluados
  pred[, ng := .N, by = .(country_code, year)]
  pred[ng == nrow(STD_WHO)][, ng := NULL][]
}

message("Pasada 1 (línea base con todos los años previos)...")
p1 <- calcular()
anual1 <- p1[, .(obs = sum(deaths), esp = sum(deaths_esp)), by = .(country_code, year)]
marcados <- anual1[100 * (obs - esp) / esp >= UMBRAL_P & obs - esp >= MIN_EXCESO,
                   .(country_code, year)]
message("Años marcados en pasada 1: ", nrow(marcados))

message("Pasada 2 (excluye años de choque de la línea base)...")
pred <- calcular(excluir = marcados)

# ---- Métricas país-año ---------------------------------------------------------
anual <- pred[, .(obs = sum(deaths), esp = sum(deaths_esp), pop = sum(pop)),
              by = .(country_code, year)]
anual[, `:=`(exceso = obs - esp,
             p_score = 100 * (obs - esp) / esp,
             exceso_1000 = 1000 * (obs - esp) / pop)]

# Δe0: tabla de vida con tasas contrafactuales por edad simple (como script 04)
e0_tabla_vida <- function(mx) {
  n <- length(mx); m0 <- mx[1]
  a <- c(if (m0 >= 0.107) 0.34 else 0.049 + 2.742 * m0, rep(0.5, n - 1))
  q <- pmin(mx / (1 + (1 - a) * mx), 1); q[n] <- 1
  l <- c(1, cumprod(1 - q)[-n]); d <- l * q
  L <- l - (1 - a) * d; L[n] <- if (mx[n] > 0) l[n] / mx[n] else 0
  sum(L)
}
razon <- pred[, .(country_code, year, grupo,
                  razon = fifelse(deaths > 0, deaths_esp / deaths, 1))]
ev <- merge(panel[year >= 1955, .(country_code, year, age, grupo, pop, deaths, mx)],
            razon, by = c("country_code", "year", "grupo"))
setorder(ev, country_code, year, age)
e0 <- ev[, .(e0_obs = e0_tabla_vida(mx),
             e0_esp = e0_tabla_vida(fifelse(pop > 0, deaths * razon / pop, 0))),
         by = .(country_code, year)]
e0[, delta_e0 := e0_obs - e0_esp]
anual <- merge(anual, e0, by = c("country_code", "year"))
anual <- merge(anual, paises[, .(country_code, pais, region)], by = "country_code")
saveRDS(anual, file.path(DIR_PROC, "choques_pais_anio.rds"))

# ---- Episodios -------------------------------------------------------------------
anual[, choque := p_score >= UMBRAL_P & exceso >= MIN_EXCESO]
setorder(anual, country_code, year)
anual[, id_run := rleid(choque), by = country_code]
episodios <- anual[choque == TRUE, .(
  inicio = min(year), fin = max(year), anios = .N,
  exceso_total = sum(exceso),
  exceso_1000_acum = sum(exceso_1000),          # muertes en exceso por 1000 hab.
  p_score_max = max(p_score),
  delta_e0_min = min(delta_e0),                  # peor año
  anio_peor = year[which.min(delta_e0)],
  pop_inicio = pop[1]
), by = .(country_code, pais, region, id_run)]
episodios[, id_run := NULL]
episodios[, id_episodio := sprintf("%s_%d", country_code, inicio)]

# Criterio de caída observada (ver UMBRAL_CAIDA)
setkey(e0_wpp, country_code, year)
episodios[, caida_e0_obs := mapply(function(cc, a, b) {
  pre <- e0_wpp[.(cc, (a - 3):(a - 1)), e0_wpp]
  dur <- e0_wpp[.(cc, a:b), e0_wpp]
  min(dur, na.rm = TRUE) - max(pre, na.rm = TRUE)
}, country_code, inicio, fin)]
episodios[, artefacto_desaceleracion := caida_e0_obs > UMBRAL_CAIDA]
message("Episodios sin caída observada de e0 (artefactos de desaceleración): ",
        episodios[artefacto_desaceleracion == TRUE, .N])
setorder(episodios, delta_e0_min)

# Tipo de evento: se une con la clasificación manual (ver archivo en 02_datos/manual).
clas_f <- file.path(DIR_MAN, "clasificacion_choques.csv")
if (file.exists(clas_f)) {
  clas <- fread(clas_f, encoding = "UTF-8")
  episodios <- merge(episodios, clas[, .(id_episodio, tipo, evento, verificado)],
                     by = "id_episodio", all.x = TRUE)
  setorder(episodios, delta_e0_min)
  message("Episodios sin clasificar: ", episodios[is.na(tipo), .N],
          " (complete 02_datos/manual/clasificacion_choques.csv)")
}
fwrite(episodios, file.path(DIR_TAB, "choques_episodios_1955_2023.csv"))
fwrite(episodios[artefacto_desaceleracion == TRUE,
                 .(id_episodio, pais, inicio, fin, delta_e0_min, caida_e0_obs)],
       file.path(DIR_TAB, "choques_artefactos_desaceleracion.csv"))

# Plantilla para clasificar los episodios nuevos (no sobrescribe la existente)
plantilla <- episodios[, .(id_episodio, pais, inicio, fin,
                           exceso_total = round(exceso_total),
                           delta_e0_min = round(delta_e0_min, 1))]
fwrite(plantilla, file.path(DIR_TAB, "plantilla_clasificacion_choques.csv"))

cat("\n== Episodios detectados: ", nrow(episodios), " ==\n")
print(head(episodios[, .(pais, inicio, fin, exceso_total = round(exceso_total),
                         p_max = round(p_score_max), d_e0 = round(delta_e0_min, 1))], 30))

# ---- Resumen por tipo de evento y figura ---------------------------------------
if ("tipo" %in% names(episodios)) {
  # Análisis principal: excluye artefactos, datos dudosos y episodios sin clasificar
  episodios[, incluido := !is.na(tipo) & !artefacto_desaceleracion &
                          !grepl("^Revisar|^Artefacto|^Dato dudoso", tipo)]
  fwrite(episodios, file.path(DIR_TAB, "choques_episodios_1955_2023.csv"))
  res_tipo <- episodios[incluido == TRUE, .(
    episodios          = .N,
    exceso_total_M     = sum(exceso_total) / 1e6,
    delta_e0_mediana   = median(delta_e0_min),
    delta_e0_peor      = min(delta_e0_min),
    exceso_1000_mediana = median(exceso_1000_acum)
  ), by = tipo][order(delta_e0_mediana)]
  fwrite(res_tipo, file.path(DIR_TAB, "choques_resumen_por_tipo.csv"))
  cat("\n== Resumen por tipo (ver columna nivel_verificacion en clasificacion_choques.csv) ==\n")
  print(res_tipo[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 1) else x)])

  # Figura 5: intensidad de cada episodio (Δe0 del peor año) por tipo de evento
  fig <- episodios[incluido == TRUE & !grepl("^Fluctuación", tipo)]
  fig[, tipo := reorder(tipo, delta_e0_min, median)]
  top <- fig[order(delta_e0_min)][, .SD[1], by = tipo][delta_e0_min < -10]  # peor episodio por tipo
  top[, etiqueta := paste(sub(" \\(.*", "", pais), inicio)]
  f5 <- ggplot(fig, aes(delta_e0_min, tipo)) +
    geom_vline(xintercept = 0, colour = "grey60") +
    geom_boxplot(outlier.shape = NA, fill = "grey90", colour = "grey40", width = 0.6) +
    geom_point(aes(size = pmax(exceso_total, 1e3)), shape = 21, fill = "grey30",
               colour = "white", alpha = 0.8) +
    geom_text(data = top, aes(label = etiqueta), size = 2.5, vjust = -1.2, hjust = 0.2) +
    scale_x_continuous(expand = expansion(mult = c(0.06, 0.03))) +
    scale_size_area(max_size = 9, breaks = c(1e4, 1e5, 1e6, 1e7),
                    labels = c("10 mil", "100 mil", "1 M", "10 M")) +
    labs(x = "Cambio en la esperanza de vida en el peor año del episodio (años)",
         y = NULL, size = "Exceso de muertes",
         caption = paste0("Episodios con P-score >= ", UMBRAL_P, " % (237 países, 1955–2023).\n",
                          "Excluye artefactos, datos dudosos y fluctuaciones menores. Fuente: UN WPP 2024.")) +
    theme_articulo()
  guardar_fig(f5, "Figura5_choques_por_tipo", alto = 13)
}
