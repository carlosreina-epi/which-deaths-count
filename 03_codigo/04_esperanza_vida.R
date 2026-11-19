# =============================================================================
# 04_esperanza_vida.R  ·  Pérdida de esperanza de vida al nacer (Δe0)
#
# La esperanza de vida de periodo es una medida independiente del tamaño y de
# la estructura etaria de la población: permite comparar 1960 con 2021 y China
# con Europa sin el sesgo que señaló el comité editorial (Aburto et al., 2022;
# Schöley et al., 2022).
#
#   Δe0 = e0(observada) − e0(contrafactual sin crisis)
#
# La tabla de vida contrafactual usa tasas por edad simple:
#   m*(x) = m_obs(x) · [ m*_g / m_obs_g ]
# es decir, la razón esperado/observado del grupo quinquenal g (script 03) se
# aplica a cada edad simple del grupo, conservando la forma de la curva.
# =============================================================================
source("03_codigo/00_config.R")

panel  <- readRDS(file.path(DIR_PROC, "panel_pais_anio_edad.rds"))
pred   <- readRDS(file.path(DIR_PROC, "pred_grupo_edad.rds"))[spec == SPEC_PRINCIPAL]
e0_wpp <- readRDS(file.path(DIR_PROC, "e0_wpp.rds"))
panel[, grupo := grupo_edad(age)]

# ---- Tabla de vida abreviada por edad simple (0..100+) -----------------------
# a0 según Coale-Demeny (promedio de sexos); ax = 0,5 en el resto; grupo
# abierto 100+: e = 1/m.
e0_tabla_vida <- function(mx) {
  n  <- length(mx)
  m0 <- mx[1]
  a  <- c(if (m0 >= 0.107) 0.34 else 0.049 + 2.742 * m0, rep(0.5, n - 1))
  q  <- pmin(mx / (1 + (1 - a) * mx), 1)
  q[n] <- 1
  l  <- c(1, cumprod(1 - q)[-n])
  d  <- l * q
  L  <- l - (1 - a) * d
  L[n] <- if (mx[n] > 0) l[n] / mx[n] else 0
  sum(L)
}

# ---- Tasas contrafactuales por edad simple -----------------------------------
razon <- pred[, .(country_code, year, grupo, crisis,
                  razon = fifelse(deaths > 0, deaths_esp / deaths, 1))]
base  <- merge(panel[, .(country_code, year, age, grupo, pop, deaths, mx, region)],
               razon, by = c("country_code", "year", "grupo"))
base[, deaths_esp := deaths * razon]
setorder(base, crisis, country_code, year, age)

# ---- e0 por país ---------------------------------------------------------------
e0_pais <- base[, .(e0_obs = e0_tabla_vida(mx),
                    e0_esp = e0_tabla_vida(fifelse(pop > 0, deaths_esp / pop, 0))),
                by = .(crisis, country_code, year)]
e0_pais[, delta_e0 := e0_obs - e0_esp]

# Validación: e0 observada calculada vs e0 publicada por WPP
val <- merge(e0_pais, e0_wpp, by = c("country_code", "year"))
message(sprintf("Validación tabla de vida vs WPP: diferencia mediana %.2f años; p95 |dif| %.2f",
                median(val$e0_obs - val$e0_wpp), quantile(abs(val$e0_obs - val$e0_wpp), 0.95)))

# ---- e0 por región y mundo (tasas agregadas de países) -----------------------
agregar_e0 <- function(dt, unidad) {
  dt <- copy(dt)[, unidad := unidad]
  g <- dt[, .(D = sum(deaths), E = sum(deaths_esp), N = sum(pop)),
          by = .(crisis, unidad, year, age)]
  setorder(g, crisis, unidad, year, age)
  g[, .(e0_obs = e0_tabla_vida(D / N), e0_esp = e0_tabla_vida(E / N)),
    by = .(crisis, unidad, year)]
}
e0_agr <- rbindlist(list(
  agregar_e0(base, base$region),
  agregar_e0(base, "Mundo"),
  agregar_e0(base[country_code != 156], "Mundo sin China"),
  agregar_e0(base[country_code == 156], "China")
))
e0_agr[, delta_e0 := e0_obs - e0_esp]

saveRDS(e0_pais, file.path(DIR_PROC, "e0_pais.rds"))
fwrite(e0_agr, file.path(DIR_TAB, "esperanza_vida_region_anual.csv"))

# Resumen por periodo: mayor caída anual y caída media del periodo
res <- e0_agr[(crisis == "hambruna" & year %in% CRISIS$hambruna) |
              (crisis == "covid" & year %in% CRISIS$covid),
              .(delta_e0_medio = mean(delta_e0), delta_e0_max = min(delta_e0),
                anio_max = year[which.min(delta_e0)]),
              by = .(crisis, unidad)]
fwrite(res, file.path(DIR_TAB, "esperanza_vida_resumen_periodo.csv"))
cat("\n== Pérdida de esperanza de vida (años), especificación principal ==\n")
print(res[, .(crisis, unidad, delta_e0_medio = round(delta_e0_medio, 2),
              delta_e0_max = round(delta_e0_max, 2), anio_max)])
