# =============================================================================
# 10_incertidumbre_montecarlo.R  ·  Intervalos de predicción y rango entre
#                                   líneas base para los dos casos de referencia
#                                   (hambruna china 1959–61 y COVID-19 2020–21)
#
# QUÉ HACE
#   A. Intervalos de predicción al 95 % por simulación Monte Carlo
#      (especificación principal "A"). Para cada país × grupo quinquenal g se
#      ajusta por MCO  log m(g,t) = ȳ + b·(t − t̄) + ε,  ε ~ N(0, σ²),
#      y en cada simulación s:
#        1. ȳ_s ~ N(ȳ, s² / n);  b_s ~ N(b, s² / Sxx)
#           (con t centrado, ȳ y b son independientes)
#        2. log m*_s(g,t) = ȳ_s + b_s·(t − t̄) + ε_s,  ε_s ~ N(0, s²)
#      Recoge la incertidumbre de la tendencia y la variación anual alrededor
#      de ella, condicionada a la varianza residual estimada s² (no se muestrea
#      σ²: con 5 años de línea base, df = 3, eso produce colas absurdas).
#      NO recoge el error de las series WPP.
#      Con las defunciones esperadas de cada simulación se recalculan exceso,
#      P-score y Δe0 (tabla de vida igual que en el script 04).
#      Los grupos sin tendencia estimable (alguna tasa nula en la línea base)
#      usan la tasa media del periodo y se mantienen fijos.
#   B. Rango determinista entre las especificaciones de línea base A–E
#      (00_config.R), ahora también para Δe0 (el script 03 solo lo hacía
#      para exceso y P-score).
#
# SALIDAS (04_resultados/tablas)
#   incertidumbre_montecarlo.csv     estimación puntual, mediana e IP 95 %
#   incertidumbre_lineas_base.csv    las 9 especificaciones × unidad × métrica
#   Tabla_incertidumbre_resumen.csv  tabla lista para el artículo
#
# TIEMPO: ~1–3 min con N_SIM = 1000.
# =============================================================================
source("03_codigo/00_config.R")
set.seed(20261119)
N_SIM <- 1000

panel  <- readRDS(file.path(DIR_PROC, "panel_pais_anio_edad.rds"))
paises <- readRDS(file.path(DIR_PROC, "paises_regiones.rds"))
panel[, grupo := grupo_edad(age)]

grp <- panel[, .(deaths = sum(deaths), pop = sum(pop)), by = .(country_code, year, grupo)]
grp[, mx := fifelse(pop > 0, deaths / pop, NA_real_)]

# Unidades de agregación: regiones, mundo, mundo sin China, China
paises_ord <- sort(unique(panel$country_code))
reg <- paises[match(paises_ord, country_code), region]
unidades <- c(sort(unique(reg)), "Mundo", "Mundo sin China", "China")
U <- sapply(unidades, function(u) switch(u,
  "Mundo"           = rep(1, length(paises_ord)),
  "Mundo sin China" = as.numeric(paises_ord != 156),
  "China"           = as.numeric(paises_ord == 156),
  as.numeric(reg == u)))
dimnames(U) <- list(paises_ord, unidades)          # países × unidades

# ---- Tabla de vida (idéntica al script 04) ------------------------------------
e0_tabla_vida <- function(mx) {
  n  <- length(mx); m0 <- mx[1]
  a  <- c(if (m0 >= 0.107) 0.34 else 0.049 + 2.742 * m0, rep(0.5, n - 1))
  q  <- pmin(mx / (1 + (1 - a) * mx), 1); q[n] <- 1
  l  <- c(1, cumprod(1 - q)[-n]); d <- l * q
  L  <- l - (1 - a) * d; L[n] <- if (mx[n] > 0) l[n] / mx[n] else 0
  sum(L)
}

# ---- Ajuste de la línea base con sus estadísticos de error ----------------------
ajustar <- function(crisis_k, spec_k) {
  cfg   <- BASELINES[crisis == crisis_k & spec == spec_k]
  anios <- cfg$ajuste[[1]]
  grp[year %in% anios, {
    ok <- is.finite(mx) & mx > 0
    if (all(ok)) {
      y <- log(mx); t <- year; n <- .N; tb <- mean(t)
      if (cfg$metodo == "lineal") {
        sxx <- sum((t - tb)^2)
        b   <- sum((t - tb) * (y - mean(y))) / sxx
        res <- y - mean(y) - b * (t - tb)
        list(tbar = tb, ybar = mean(y), b = b, sxx = sxx, n = n,
             s2 = sum(res^2) / (n - 2), m_media = NA_real_)
      } else {
        list(tbar = tb, ybar = mean(y), b = 0, sxx = NA_real_, n = n,
             s2 = var(y), m_media = NA_real_)
      }
    } else {
      list(tbar = mean(year), ybar = NA_real_, b = 0, sxx = NA_real_, n = .N,
           s2 = NA_real_, m_media = sum(deaths) / sum(pop))
    }
  }, by = .(country_code, grupo)]
}

# ---- Motor: defunciones esperadas por simulación y métricas ---------------------
# S = 1 y ruido = FALSE  ->  estimación puntual determinista
simular <- function(crisis_k, spec_k, S = 1, ruido = FALSE) {
  fit <- ajustar(crisis_k, spec_k)
  setorder(fit, country_code, grupo)
  F <- nrow(fit)
  libre <- !is.na(fit$ybar)                  # grupos con tendencia estimable
  # Parámetros por simulación (F × S)
  if (ruido) {
    # σ² se fija en su estimación (s²). Con líneas base de 5 años (COVID: df = 3)
    # muestrear σ² de su distribución χ² genera colas absurdas (σ² x 1000).
    s2s <- matrix(fit$s2, F, S)
    ys  <- fit$ybar + sqrt(s2s / fit$n) * matrix(rnorm(F * S), F, S)
    bs  <- fit$b + ifelse(is.na(fit$sxx), 0, 1) *
           sqrt(s2s / ifelse(is.na(fit$sxx), 1, fit$sxx)) * matrix(rnorm(F * S), F, S)
  } else {
    s2s <- matrix(0, F, 1); ys <- matrix(fit$ybar, F, 1); bs <- matrix(fit$b, F, 1)
  }

  out <- list()
  for (t in CRISIS[[crisis_k]]) {
    obs <- grp[year == t][fit, on = .(country_code, grupo)]     # mismo orden que fit
    lm  <- ys + bs * (t - fit$tbar)
    if (ruido) lm <- lm + sqrt(s2s) * matrix(rnorm(F * S), F, S)
    m_esp <- exp(lm)
    m_esp[!libre, ] <- fit$m_media[!libre]
    m_esp[is.na(m_esp)] <- 0
    D_esp <- m_esp * obs$pop                                     # F × S

    # Totales por país (filas = países en orden de paises_ord)
    idx_c   <- match(obs$country_code, paises_ord)
    esp_c   <- rowsum(D_esp, idx_c, reorder = TRUE)             # países × S
    obs_c   <- rowsum(obs$deaths, idx_c, reorder = TRUE)[, 1]
    esp_u   <- crossprod(U, esp_c)                               # unidades × S
    obs_u   <- crossprod(U, obs_c)[, 1]

    # Δe0: razón esperado/observado del grupo aplicada a cada edad simple
    razon <- D_esp / obs$deaths                                  # F × S
    razon[!(obs$deaths > 0), ] <- 1
    pa <- panel[year == t, .(country_code, age, grupo, deaths, pop)]
    setorder(pa, country_code, age)
    edades <- sort(unique(pa$age))
    Dm <- matrix(pa$deaths, nrow = length(edades))               # edades × países
    Nm <- matrix(pa$pop,    nrow = length(edades))
    g_edad <- as.integer(grupo_edad(edades))                     # 1..21
    Du <- Dm %*% U; Nu <- Nm %*% U                               # edades × unidades
    e0_obs <- apply(Du / Nu, 2, e0_tabla_vida)
    ng <- nlevels(fit$grupo)
    fila <- (match(fit$country_code, paises_ord) - 1) * ng + as.integer(fit$grupo)
    e0_esp <- matrix(NA_real_, length(unidades), S)
    for (s in seq_len(S)) {
      R <- matrix(1, ng, length(paises_ord)); R[fila] <- razon[, s] # grupos × países
      Eu <- (Dm * R[g_edad, ]) %*% U
      e0_esp[, s] <- apply(Eu / Nu, 2, e0_tabla_vida)
    }
    out[[as.character(t)]] <- list(obs = obs_u, esp = esp_u,
                                    d_e0 = e0_obs - e0_esp)
  }
  # Métricas del periodo por simulación (unidades × S)
  obs_p <- Reduce(`+`, lapply(out, `[[`, "obs"))
  esp_p <- Reduce(`+`, lapply(out, `[[`, "esp"))
  dlist <- lapply(out, `[[`, "d_e0")
  list(exceso      = obs_p - esp_p,
       p_score     = 100 * (obs_p - esp_p) / esp_p,
       d_e0_medio  = Reduce(`+`, dlist) / length(dlist),
       d_e0_peor   = Reduce(pmin, dlist))
}

a_tabla <- function(res, crisis_k, spec_k) {
  rbindlist(lapply(names(res), function(m) {
    x <- res[[m]]
    data.table(crisis = crisis_k, spec = spec_k, unidad = unidades, metrica = m,
               valor = x[, 1])
  }))
}

# ---- A. Monte Carlo, especificación principal --------------------------------
mc <- rbindlist(lapply(names(CRISIS), function(k) {
  message("Monte Carlo ", k, " (", N_SIM, " simulaciones)...")
  r  <- simular(k, SPEC_PRINCIPAL, S = N_SIM, ruido = TRUE)
  p0 <- simular(k, SPEC_PRINCIPAL)
  rbindlist(lapply(names(r), function(m) data.table(
    crisis = k, unidad = unidades, metrica = m,
    puntual = p0[[m]][, 1],
    mediana = apply(r[[m]], 1, median),
    ip_inf  = apply(r[[m]], 1, quantile, 0.025),
    ip_sup  = apply(r[[m]], 1, quantile, 0.975))))
}))
fwrite(mc, file.path(DIR_TAB, "incertidumbre_montecarlo.csv"))

# ---- B. Todas las especificaciones de línea base -----------------------------
lb <- rbindlist(Map(function(k, s) a_tabla(simular(k, s), k, s),
                    BASELINES$crisis, BASELINES$spec))
lb <- merge(lb, BASELINES[, .(crisis, spec, label)], by = c("crisis", "spec"))
fwrite(lb, file.path(DIR_TAB, "incertidumbre_lineas_base.csv"))

# Control: la estimación puntual debe coincidir con los scripts 03 y 04
ref <- fread(file.path(DIR_TAB, "exceso_region_periodo.csv"))[spec == SPEC_PRINCIPAL]
chk <- merge(mc[metrica == "exceso", .(crisis, unidad, puntual)],
             ref[, .(crisis, unidad, exceso)], by = c("crisis", "unidad"))
message(sprintf("Control vs script 03: diferencia relativa máxima en exceso = %.2e",
                max(abs(chk$puntual / chk$exceso - 1))))

# ---- Tabla resumen para el artículo -------------------------------------------
rango <- lb[, .(min_lb = min(valor), max_lb = max(valor)), by = .(crisis, unidad, metrica)]
tab <- merge(mc, rango, by = c("crisis", "unidad", "metrica"))
escala <- function(m, x) if (m == "exceso") x / 1e6 else x
tab[, `:=`(puntual = escala(metrica[1], puntual), ip_inf = escala(metrica[1], ip_inf),
           ip_sup = escala(metrica[1], ip_sup), min_lb = escala(metrica[1], min_lb),
           max_lb = escala(metrica[1], max_lb)), by = metrica]
tab[, metrica := factor(metrica, levels = c("exceso", "p_score", "d_e0_medio", "d_e0_peor"),
                        labels = c("Exceso de muertes (millones)", "P-score (%)",
                                   "Δe0 medio del periodo (años)", "Δe0 peor año (años)"))]
tab[, texto := sprintf("%.2f (IP95 %.2f a %.2f; líneas base %.2f a %.2f)",
                       puntual, ip_inf, ip_sup, min_lb, max_lb)]
setorder(tab, crisis, unidad, metrica)
fwrite(tab[, .(crisis, unidad, metrica, puntual, ip_inf, ip_sup, min_lb, max_lb, texto)],
       file.path(DIR_TAB, "Tabla_incertidumbre_resumen.csv"))

cat("\n== Incertidumbre: mundo y China ==\n")
print(tab[unidad %in% c("Mundo", "China"), .(crisis, unidad, metrica, texto)])
