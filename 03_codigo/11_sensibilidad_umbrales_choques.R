# =============================================================================
# 11_sensibilidad_umbrales_choques.R  ·  ¿Depende la jerarquía de tipos de
#                                        crisis del umbral del P-score?
#
# Repite la detección de choques del script 07 con umbrales de P-score de 5 %,
# 10 % (principal) y 20 %, con los mismos demás criterios (≥ 1000 muertes en
# exceso, línea base móvil de 10 años en dos pasadas, caída observada de e0
# ≤ −0,25 años).
#
# Los episodios nuevos o recortados no tienen clasificación propia: heredan el
# tipo del episodio clasificado (umbral 10 %) del mismo país con el que más
# años comparten. Los que no se solapan con ninguno quedan "Sin clasificar" y
# se reportan aparte (a 5 % son, por construcción, choques pequeños).
#
# SALIDAS (04_resultados/tablas)
#   sensibilidad_umbrales_episodios.csv   episodios por umbral
#   sensibilidad_umbrales_por_tipo.csv    n y mediana de Δe0 por tipo y umbral
#   (en consola: correlación de Spearman del orden de tipos vs. umbral 10 %)
#
# Requiere haber corrido 07 (usa su clasificación). Tiempo: ~3–6 min.
# =============================================================================
source("03_codigo/00_config.R")

UMBRALES   <- c(5, 10, 20)
MIN_EXCESO <- 1000; VENTANA <- 10; MIN_BASE <- 5; UMBRAL_CAIDA <- -0.25

panel  <- readRDS(file.path(DIR_PROC, "panel_pais_anio_edad.rds"))
paises <- readRDS(file.path(DIR_PROC, "paises_regiones.rds"))
e0_wpp <- readRDS(file.path(DIR_PROC, "e0_wpp.rds"))
panel[, grupo := grupo_edad(age)]
grp <- panel[, .(deaths = sum(deaths), pop = sum(pop)), by = .(country_code, year, grupo)]
grp[, lmx := fifelse(deaths > 0 & pop > 0, log(deaths / pop), NA_real_)]

# Clasificación de referencia (umbral 10 %, script 07)
ref <- fread(file.path(DIR_TAB, "choques_episodios_1955_2023.csv"), encoding = "UTF-8")
ref <- ref[incluido == TRUE & !grepl("^Fluctuación", tipo),
           .(country_code, inicio_ref = inicio, fin_ref = fin, tipo)]

# ---- Funciones (mismas que en el script 07) ---------------------------------
predecir_anio <- function(t, excluir) {
  base <- grp[year %between% c(t - VENTANA, t - 1)]
  if (!is.null(excluir)) base <- base[!excluir, on = .(country_code, year)]
  fit <- base[is.finite(lmx),
              .(n = .N, tbar = mean(year), ybar = mean(lmx),
                b = if (.N >= 3) sum((year - mean(year)) * (lmx - mean(lmx))) /
                                 sum((year - mean(year))^2) else 0),
              by = .(country_code, grupo)][n >= MIN_BASE]
  obj <- merge(grp[year == t, .(country_code, grupo, year, deaths, pop)], fit,
               by = c("country_code", "grupo"))
  obj[, deaths_esp := exp(ybar + b * (t - tbar)) * pop]
  obj[, .(country_code, grupo, year, deaths, pop, deaths_esp)]
}
calcular <- function(excluir = NULL) {
  pred <- rbindlist(lapply(1955:2023, predecir_anio, excluir = excluir))
  pred[, ng := .N, by = .(country_code, year)]
  pred[ng == nrow(STD_WHO)][, ng := NULL][]
}
e0_tabla_vida <- function(mx) {
  n <- length(mx); m0 <- mx[1]
  a <- c(if (m0 >= 0.107) 0.34 else 0.049 + 2.742 * m0, rep(0.5, n - 1))
  q <- pmin(mx / (1 + (1 - a) * mx), 1); q[n] <- 1
  l <- c(1, cumprod(1 - q)[-n]); d <- l * q
  L <- l - (1 - a) * d; L[n] <- if (mx[n] > 0) l[n] / mx[n] else 0
  sum(L)
}
setkey(e0_wpp, country_code, year)

detectar <- function(umbral) {
  message("Umbral ", umbral, " %: pasada 1...")
  p1 <- calcular()
  a1 <- p1[, .(obs = sum(deaths), esp = sum(deaths_esp)), by = .(country_code, year)]
  marcados <- a1[100 * (obs - esp) / esp >= umbral & obs - esp >= MIN_EXCESO,
                 .(country_code, year)]
  message("Umbral ", umbral, " %: pasada 2...")
  pred <- calcular(excluir = marcados)
  anual <- pred[, .(obs = sum(deaths), esp = sum(deaths_esp), pop = sum(pop)),
                by = .(country_code, year)]
  anual[, `:=`(exceso = obs - esp, p_score = 100 * (obs - esp) / esp)]
  anual[, choque := p_score >= umbral & exceso >= MIN_EXCESO]
  # Δe0 solo para los años de choque (ahorra tiempo)
  ch <- anual[choque == TRUE, .(country_code, year)]
  razon <- pred[ch, on = .(country_code, year)][
    , .(country_code, year, grupo, razon = fifelse(deaths > 0, deaths_esp / deaths, 1))]
  ev <- merge(panel[ch, on = .(country_code, year)][, .(country_code, year, age, grupo, pop, deaths, mx)],
              razon, by = c("country_code", "year", "grupo"))
  setorder(ev, country_code, year, age)
  e0 <- ev[, .(delta_e0 = e0_tabla_vida(mx) -
                 e0_tabla_vida(fifelse(pop > 0, deaths * razon / pop, 0))),
           by = .(country_code, year)]
  anual <- merge(anual, e0, by = c("country_code", "year"), all.x = TRUE)
  setorder(anual, country_code, year)
  anual[, id_run := rleid(choque), by = country_code]
  ep <- anual[choque == TRUE, .(inicio = min(year), fin = max(year),
                                exceso_total = sum(exceso),
                                delta_e0_min = min(delta_e0)),
              by = .(country_code, id_run)][, id_run := NULL]
  ep[, caida_e0_obs := mapply(function(cc, a, b) {
    min(e0_wpp[.(cc, a:b), e0_wpp], na.rm = TRUE) -
      max(e0_wpp[.(cc, (a - 3):(a - 1)), e0_wpp], na.rm = TRUE)
  }, country_code, inicio, fin)]
  ep <- ep[caida_e0_obs <= UMBRAL_CAIDA]
  # Tipo heredado por máximo solapamiento con la clasificación de referencia
  sol <- merge(ep[, .(country_code, inicio, fin)], ref, by = "country_code",
               allow.cartesian = TRUE)
  sol[, anios_comunes := pmin(fin, fin_ref) - pmax(inicio, inicio_ref) + 1]
  sol <- sol[anios_comunes > 0][order(-anios_comunes)][, .SD[1], by = .(country_code, inicio)]
  ep <- merge(ep, sol[, .(country_code, inicio, tipo)], by = c("country_code", "inicio"),
              all.x = TRUE)
  ep[is.na(tipo), tipo := "Sin clasificar"]
  ep[, umbral_p := umbral][]
}

eps <- rbindlist(lapply(UMBRALES, detectar))
eps <- merge(eps, paises[, .(country_code, pais)], by = "country_code")
fwrite(eps, file.path(DIR_TAB, "sensibilidad_umbrales_episodios.csv"))

res <- eps[, .(episodios = .N, delta_e0_mediana = median(delta_e0_min),
               delta_e0_peor = min(delta_e0_min), exceso_total_M = sum(exceso_total) / 1e6),
           by = .(umbral_p, tipo)]
setorder(res, umbral_p, delta_e0_mediana)
fwrite(res, file.path(DIR_TAB, "sensibilidad_umbrales_por_tipo.csv"))

# Estabilidad del orden de los tipos frente al umbral principal
anc <- dcast(res[tipo != "Sin clasificar"], tipo ~ umbral_p, value.var = "delta_e0_mediana")
setnames(anc, as.character(UMBRALES), paste0("p", UMBRALES))
cat("\n== Mediana de Δe0 (peor año) por tipo y umbral de P-score ==\n")
print(anc[order(p10)], digits = 2)
for (u in setdiff(UMBRALES, 10)) {
  x <- anc[!is.na(get(paste0("p", u))) & !is.na(p10)]
  cat(sprintf("Spearman orden de tipos, umbral %d %% vs 10 %%: %.2f (%d tipos)\n",
              u, cor(x[[paste0("p", u)]], x$p10, method = "spearman"), nrow(x)))
}
cat("\nEpisodios por umbral (sin clasificar entre paréntesis):\n")
print(eps[, .(episodios = .N, sin_clasificar = sum(tipo == "Sin clasificar")), by = umbral_p])
