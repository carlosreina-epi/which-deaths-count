# =============================================================================
# 03_exceso_mortalidad.R  ·  Exceso de mortalidad ajustado por tamaño y edad
#
# MÉTODO (respuesta directa a la objeción editorial)
#   Para cada país c, grupo quinquenal de edad x y especificación de línea base s:
#     1. Se ajusta la tendencia pre-crisis de la tasa de mortalidad por edad:
#          log m(c,x,t) = a + b·t      (o solo a, si metodo == "media")
#        usando los años de ajuste de la especificación s.
#     2. Tasa esperada sin crisis:   m*(c,x,t) = exp(a + b·t)
#     3. Defunciones esperadas:      E(c,x,t)  = m*(c,x,t) · N_obs(c,x,t)
#        -> el contrafactual usa la población OBSERVADA del año de crisis, de
#           modo que el crecimiento y el envejecimiento poblacional quedan
#           neutralizados: el exceso solo refleja cambios en el riesgo.
#     4. Exceso = O − E;   P-score = 100·(O − E)/E;   exceso por 100 000 hab.
#     5. Tasas estandarizadas por edad (directa, población estándar OMS
#        2000–2025) observadas y esperadas, para comparar regiones y épocas.
#   Las sumas regionales y mundiales se hacen sobre países (nunca agregados).
#
# SALIDAS (02_datos/processed y 04_resultados/tablas)
#   exceso_pais_anio.rds, exceso_region_periodo.csv, exceso_mundo_anual.csv,
#   exceso_grupo_edad.csv, sensibilidad_lineas_base.csv
# =============================================================================
source("03_codigo/00_config.R")

panel  <- readRDS(file.path(DIR_PROC, "panel_pais_anio_edad.rds"))
paises <- readRDS(file.path(DIR_PROC, "paises_regiones.rds"))
panel[, grupo := grupo_edad(age)]

# La línea base se ajusta por GRUPOS QUINQUENALES de edad (21 grupos, 0–4 a
# 100+). Con edad simple, las áreas pequeñas tienen tasas nulas o muy ruidosas
# en algunas edades y la extrapolación log-lineal explota. El grupo quinquenal
# es el estándar en estimaciones de exceso (OMS, Msemburi et al. 2023).
grp <- panel[, .(deaths = sum(deaths), pop = sum(pop)),
             by = .(country_code, year, grupo)]
grp[, mx := fifelse(pop > 0, deaths / pop, NA_real_)]

# Años para los que se calcula el contrafactual (incluye años vecinos para las
# figuras anuales; los totales del periodo usan solo CRISIS[[k]]).
ANIOS_PRED <- list(hambruna = 1956:1966, covid = 2015:2023)

# ---- Ajuste vectorizado de la línea base -------------------------------------
# MCO cerrado por país × grupo: b = cov(t, y)/var(t); a = ȳ − b·t̄, con
# y = log m. Si en algún año de ajuste la tasa es 0 o no definida (áreas con
# muy poca población), se usa la tasa media del periodo sin tendencia.
ajustar_base <- function(crisis_k, spec_k) {
  cfg <- BASELINES[crisis == crisis_k & spec == spec_k]
  anios <- cfg$ajuste[[1]]
  fit <- grp[year %in% anios,
             {
               ok <- is.finite(mx) & mx > 0
               if (all(ok) && cfg$metodo == "lineal") {
                 y <- log(mx); t <- year
                 b <- sum((t - mean(t)) * (y - mean(y))) / sum((t - mean(t))^2)
                 list(tbar = mean(t), ybar = mean(y), b = b, m_media = NA_real_)
               } else if (all(ok)) {
                 list(tbar = mean(year), ybar = mean(log(mx)), b = 0, m_media = NA_real_)
               } else {
                 list(tbar = mean(year), ybar = NA_real_, b = 0,
                      m_media = sum(deaths) / sum(pop))
               }
             },
             by = .(country_code, grupo)]
  pred <- grp[year %in% ANIOS_PRED[[crisis_k]], .(country_code, year, grupo, pop, deaths, mx)]
  pred <- merge(pred, fit, by = c("country_code", "grupo"))
  pred[, mx_esp := fifelse(is.na(ybar), m_media, exp(ybar + b * (year - tbar)))]
  pred[is.na(mx_esp), mx_esp := 0]
  pred[, deaths_esp := mx_esp * pop]
  pred[, `:=`(crisis = crisis_k, spec = spec_k,
              tbar = NULL, ybar = NULL, b = NULL, m_media = NULL)]
  pred[]
}

combos <- BASELINES[, .(crisis, spec)]
pred_all <- rbindlist(Map(ajustar_base, combos$crisis, combos$spec))
pred_all <- merge(pred_all, paises[, .(country_code, pais, region)], by = "country_code")
stopifnot(!anyNA(pred_all$deaths_esp))

# ---- Agregación por país-año (todas las especificaciones) --------------------
pais_anio <- pred_all[, .(obs = sum(deaths), esp = sum(deaths_esp), pop = sum(pop)),
                      by = .(crisis, spec, country_code, pais, region, year)]
pais_anio[, `:=`(exceso = obs - esp, p_score = 100 * (obs - esp) / esp,
                 exceso_100k = 1e5 * (obs - esp) / pop)]
saveRDS(pais_anio, file.path(DIR_PROC, "exceso_pais_anio.rds"))
saveRDS(pred_all, file.path(DIR_PROC, "pred_grupo_edad.rds"))   # usado en 04 (esperanza de vida)

# ---- Tasas estandarizadas por edad (directa, estándar OMS) -------------------
tasa_std <- function(dt, by) {
  g <- dt[, .(D = sum(deaths), E = sum(deaths_esp), N = sum(pop)), by = c(by, "grupo")]
  g <- merge(g, STD_WHO, by = "grupo")
  g[, .(tee_obs = 1e5 * sum(peso * D / N), tee_esp = 1e5 * sum(peso * E / N)), by = by]
}

# ---- Resumen por periodo de crisis: región, mundo y mundo sin China ----------
resumir_periodo <- function(dt, nivel) {
  res <- dt[, .(obs = sum(deaths), esp = sum(deaths_esp)), by = .(crisis, spec, unidad)]
  # población media del periodo (para exceso por 100 000 hab.-año)
  popm <- dt[, .(pop = sum(pop)), by = .(crisis, spec, unidad, year)][
    , .(pop_media = mean(pop), n_anios = .N), by = .(crisis, spec, unidad)]
  res <- merge(res, popm, by = c("crisis", "spec", "unidad"))
  std <- tasa_std(dt, c("crisis", "spec", "unidad"))
  res <- merge(res, std, by = c("crisis", "spec", "unidad"))
  res[, `:=`(nivel = nivel,
             exceso = obs - esp,
             p_score = 100 * (obs - esp) / esp,
             exceso_100k_anio = 1e5 * (obs - esp) / (pop_media * n_anios),
             dif_tee = tee_obs - tee_esp)]
  res[]
}
en_periodo <- pred_all[(crisis == "hambruna" & year %in% CRISIS$hambruna) |
                       (crisis == "covid"    & year %in% CRISIS$covid)]
periodo <- rbindlist(list(
  resumir_periodo(copy(en_periodo)[, unidad := region], "Región"),
  resumir_periodo(copy(en_periodo)[, unidad := "Mundo"], "Mundo"),
  resumir_periodo(copy(en_periodo)[country_code != 156][, unidad := "Mundo sin China"], "Mundo"),
  resumir_periodo(copy(en_periodo)[country_code == 156][, unidad := "China"], "País")
), use.names = TRUE)
setcolorder(periodo, c("crisis", "spec", "nivel", "unidad"))
fwrite(periodo, file.path(DIR_TAB, "exceso_region_periodo.csv"))

# ---- Serie anual mundial (especificación principal) --------------------------
mundo_anual <- pred_all[spec == SPEC_PRINCIPAL,
                        .(obs = sum(deaths), esp = sum(deaths_esp), pop = sum(pop)),
                        by = .(crisis, year)]
mundo_anual <- merge(mundo_anual,
                     tasa_std(pred_all[spec == SPEC_PRINCIPAL], c("crisis", "year")),
                     by = c("crisis", "year"))
mundo_anual[, `:=`(exceso = obs - esp, p_score = 100 * (obs - esp) / esp)]
fwrite(mundo_anual, file.path(DIR_TAB, "exceso_mundo_anual.csv"))

# ---- Exceso por grandes grupos de edad (principal) ---------------------------
en_periodo[, gran_grupo := fcase(grupo == "0-4", "0–4",
                                 grupo %in% c("5-9", "10-14"), "5–14",
                                 grupo %in% STD_WHO$grupo[4:13], "15–64",
                                 default = "65+")]
en_periodo[, gran_grupo := factor(gran_grupo, levels = c("0–4", "5–14", "15–64", "65+"))]
por_edad <- en_periodo[spec == SPEC_PRINCIPAL,
                       .(obs = sum(deaths), esp = sum(deaths_esp)),
                       by = .(crisis, gran_grupo)]
por_edad[, `:=`(exceso = obs - esp, p_score = 100 * (obs - esp) / esp)]
por_edad[, pct_del_exceso := 100 * exceso / sum(exceso), by = crisis]
setorder(por_edad, crisis, gran_grupo)
fwrite(por_edad, file.path(DIR_TAB, "exceso_grupo_edad.csv"))

# ---- Sensibilidad: rango entre especificaciones -------------------------------
sens <- periodo[unidad %in% c("Mundo", "Mundo sin China", "China")]
sens <- merge(sens, BASELINES[, .(crisis, spec, label)], by = c("crisis", "spec"))
fwrite(sens[, .(crisis, unidad, spec, label, exceso, p_score, exceso_100k_anio)],
       file.path(DIR_TAB, "sensibilidad_lineas_base.csv"))

# ---- Resumen en consola ------------------------------------------------------
cat("\n== Exceso de mortalidad, especificación principal ==\n")
print(periodo[spec == SPEC_PRINCIPAL,
              .(crisis, unidad,
                exceso_millones = round(exceso / 1e6, 2),
                p_score = round(p_score, 1),
                exceso_100k_anio = round(exceso_100k_anio, 1))])
cat("\n== Rango entre líneas base (mundo) ==\n")
print(sens[unidad == "Mundo", .(min_M = round(min(exceso) / 1e6, 1),
                                max_M = round(max(exceso) / 1e6, 1),
                                min_P = round(min(p_score), 1),
                                max_P = round(max(p_score), 1)), by = crisis])
