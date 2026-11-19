# =============================================================================
# 08_guerras_mundiales_hmd.R  ·  Primera y Segunda Guerra Mundial, Guerra Civil
#                                Española y COVID-19 en los MISMOS países
#
# FUENTE: Human Mortality Database (HMD, mortality.org). Registro gratuito.
#   1. Cree su cuenta en https://www.mortality.org (Register).
#   2. Data > Download data > "HMD statistics" (archivo ZIP con todos los países)
#   3. Descomprima el ZIP dentro de 02_datos/raw/hmd/  (la estructura interna
#      no importa: el script busca los archivos por nombre, p. ej.
#      FRATNP.Deaths_1x1.txt y FRATNP.Exposures_1x1.txt).
#
# DISEÑO (comparación intra-país, misma fuente, mismo método que el script 03)
#   Evento           Años        Línea base (log-lineal por grupo de edad)
#   Primera Guerra   1914–1918   1904–1913
#   Gripe de 1918    —           se separa comparando con países NEUTRALES
#   Guerra Civil Esp 1936–1939   1926–1935 (solo España)
#   Segunda Guerra   1939–1945   1929–1938 (España: 1926–1935)
#   COVID-19         2020–2022   2015–2019
#
#   Países neutrales en la Primera Guerra (Países Bajos, Dinamarca, Noruega,
#   Suecia, Suiza, España) funcionan como control: su exceso en 1918 es casi
#   todo gripe; la diferencia con los beligerantes aproxima el efecto de la
#   guerra (diferencia en diferencias descriptiva).
#
# CUIDADOS QUE DEBEN DECLARARSE
#   - En algunos países HMD ofrece serie de población total y serie civil
#     (FRATNP/FRACNP, GBRTENW/GBRCENW). Se usa la TOTAL; revise en la
#     documentación de cada país (carpeta "country notes") si incluye las
#     muertes militares en el frente: si no, el exceso bélico se subestima.
#   - Bélgica no tiene datos 1914–1918 en HMD.
#   - Cambios de territorio: HMD reporta años "1919-" / "1919+"; se usa la
#     versión "+" (nuevo territorio).
# =============================================================================
source("03_codigo/00_config.R")

DIR_HMD <- file.path(DIR_RAW, "hmd")
if (length(list.files(DIR_HMD, pattern = "Deaths_1x1\\.txt$", recursive = TRUE)) == 0) {
  stop("No se encontraron archivos de HMD en ", DIR_HMD,
       ". Descárguelos de https://www.mortality.org (ver encabezado del script).")
}

PAISES_HMD <- data.table(
  codigo = c("FRATNP", "GBRTENW", "ITA", "NLD", "BEL", "DNK", "NOR", "SWE", "CHE",
             "FIN", "ESP", "PRT", "USA", "CAN", "AUS"),
  pais   = c("Francia", "Inglaterra y Gales", "Italia", "Países Bajos", "Bélgica",
             "Dinamarca", "Noruega", "Suecia", "Suiza", "Finlandia", "España",
             "Portugal", "Estados Unidos", "Canadá", "Australia"),
  ww1    = c("Beligerante", "Beligerante", "Beligerante", "Neutral", "Beligerante",
             "Neutral", "Neutral", "Neutral", "Neutral", "Guerra civil 1918",
             "Neutral", "Beligerante", "Beligerante", "Beligerante", "Beligerante"),
  ww2    = c("Beligerante/ocupado", "Beligerante", "Beligerante", "Ocupado", "Ocupado",
             "Ocupado", "Ocupado", "Neutral", "Neutral", "Beligerante", "Neutral",
             "Neutral", "Beligerante", "Beligerante", "Beligerante")
)

EVENTOS <- data.table(
  evento  = c("Primera Guerra Mundial", "Guerra Civil Española", "Segunda Guerra Mundial",
              "COVID-19"),
  inicio  = c(1914, 1936, 1939, 2020),
  fin     = c(1918, 1939, 1945, 2022),
  base_i  = c(1904, 1926, 1929, 2015),
  base_f  = c(1913, 1935, 1938, 2019)
)

# ---- Lectura de archivos HMD -------------------------------------------------
# Admite los dos formatos de descarga de HMD:
#   (a) un archivo por país:   FRATNP.Deaths_1x1.txt
#   (b) archivo combinado:      Deaths_1x1.txt con columna PopName (descarga
#       "By statistic" de la web nueva de HMD, 2026)
limpiar_hmd <- function(x, tipo) {
  x[, Age := as.integer(sub("\\+", "", Age))]
  # años con cambio territorial ("1919-" / "1919+"): conservar la versión "+"
  x <- x[!grepl("-$", Year)]
  x[, Year := as.integer(sub("[-+]$", "", Year))]
  for (v in c("Female", "Male", "Total")) set(x, j = v, value = as.numeric(x[[v]]))
  melt(x, id.vars = c("Year", "Age"), measure.vars = c("Female", "Male", "Total"),
       variable.name = "sexo", value.name = tipo)
}
combinados <- list()
leer_hmd <- function(codigo, tipo) {
  f <- list.files(DIR_HMD, pattern = paste0("^", codigo, "\\.", tipo, "_1x1\\.txt$"),
                  recursive = TRUE, full.names = TRUE)
  if (length(f) > 0) {
    x <- fread(f[1], skip = 2, header = TRUE, na.strings = ".", colClasses = "character")
    return(limpiar_hmd(x, tipo))
  }
  if (is.null(combinados[[tipo]])) {
    fc <- list.files(DIR_HMD, pattern = paste0("^", tipo, "_1x1\\.txt$"),
                     recursive = TRUE, full.names = TRUE)
    if (length(fc) == 0) return(NULL)
    combinados[[tipo]] <<- fread(fc[1], skip = 2, header = TRUE, na.strings = ".",
                                 colClasses = "character")
  }
  x <- combinados[[tipo]][PopName == codigo][, PopName := NULL]
  if (nrow(x) == 0) return(NULL)
  limpiar_hmd(x, tipo)
}

hmd <- rbindlist(lapply(PAISES_HMD$codigo, function(cc) {
  d <- leer_hmd(cc, "Deaths"); e <- leer_hmd(cc, "Exposures")
  if (is.null(d) || is.null(e)) { message("Sin datos para ", cc); return(NULL) }
  out <- merge(d, e, by = c("Year", "Age", "sexo"))
  out[, codigo := cc]
}))
setnames(hmd, c("Year", "Age"), c("year", "age"))
hmd[, sexo := factor(sexo, levels = c("Total", "Male", "Female"),
                     labels = c("Ambos sexos", "Hombres", "Mujeres"))]
hmd[, age := pmin(age, 100L)]                         # grupo abierto 100+
hmd <- hmd[, .(deaths = sum(Deaths, na.rm = TRUE), pop = sum(Exposures, na.rm = TRUE)),
           by = .(codigo, sexo, year, age)]
hmd[, mx := fifelse(pop > 0, deaths / pop, 0)]
hmd[, grupo := grupo_edad(age)]
saveRDS(hmd, file.path(DIR_PROC, "hmd_panel.rds"))
message("Países HMD leídos: ", paste(unique(hmd$codigo), collapse = ", "))

# ---- Contrafactual por evento (mismo método que script 03) -------------------
e0_tabla_vida <- function(mx) {
  n <- length(mx); m0 <- mx[1]
  a <- c(if (m0 >= 0.107) 0.34 else 0.049 + 2.742 * m0, rep(0.5, n - 1))
  q <- pmin(mx / (1 + (1 - a) * mx), 1); q[n] <- 1
  l <- c(1, cumprod(1 - q)[-n]); d <- l * q
  L <- l - (1 - a) * d; L[n] <- if (mx[n] > 0) l[n] / mx[n] else 0
  sum(L)
}

evaluar_evento <- function(ev) {
  bi <- ev$base_i; bf <- ev$base_f
  sub <- copy(hmd)
  if (ev$evento == "Segunda Guerra Mundial") {
    # España: la línea base 1929–38 contiene la Guerra Civil
    sub[codigo == "ESP", base_ok := year %between% c(1926, 1935)]
    sub[codigo != "ESP", base_ok := year %between% c(bi, bf)]
  } else {
    sub[, base_ok := year %between% c(bi, bf)]
  }
  if (ev$evento == "Guerra Civil Española") sub <- sub[codigo == "ESP"]

  g <- sub[, .(deaths = sum(deaths), pop = sum(pop), base_ok = base_ok[1]),
           by = .(codigo, sexo, year, grupo)]
  # exigir línea base con al menos 5 años con datos (COVID: 2015–2019)
  fit <- g[base_ok == TRUE & deaths > 0 & pop > 0,
           .(n = .N, tbar = mean(year), ybar = mean(log(deaths / pop)),
             b = sum((year - mean(year)) * (log(deaths / pop) - mean(log(deaths / pop)))) /
                 sum((year - mean(year))^2)),
           by = .(codigo, sexo, grupo)][n >= 5]
  anios <- (ev$inicio - 2):(ev$fin + 2)
  pr <- merge(g[year %in% anios], fit, by = c("codigo", "sexo", "grupo"))
  pr[, deaths_esp := exp(ybar + b * (year - tbar)) * pop]
  pr[, razon := fifelse(deaths > 0, deaths_esp / deaths, 1)]

  # Δe0 con tasas por edad simple
  ed <- merge(sub[year %in% anios, .(codigo, sexo, year, age, grupo, mx, pop, deaths)],
              pr[, .(codigo, sexo, year, grupo, razon)],
              by = c("codigo", "sexo", "year", "grupo"))
  setorder(ed, codigo, sexo, year, age)
  e0 <- ed[, .(e0_obs = e0_tabla_vida(mx),
               e0_esp = e0_tabla_vida(fifelse(pop > 0, deaths * razon / pop, 0))),
           by = .(codigo, sexo, year)]
  anual <- pr[, .(obs = sum(deaths), esp = sum(deaths_esp), pop = sum(pop)),
              by = .(codigo, sexo, year)]
  anual <- merge(anual, e0, by = c("codigo", "sexo", "year"))
  anual[, `:=`(evento = ev$evento, en_evento = year %between% c(ev$inicio, ev$fin),
               exceso = obs - esp, p_score = 100 * (obs - esp) / esp,
               delta_e0 = e0_obs - e0_esp)]
  # Solo países con TODOS los años del evento (p. ej., Bélgica no tiene 1914–1918)
  n_ev <- ev$fin - ev$inicio + 1
  completos <- anual[en_evento == TRUE & pop > 0 & obs > 0, .N, by = codigo][N == 3 * n_ev, codigo]
  anual[codigo %in% completos]
}
anual <- rbindlist(lapply(split(EVENTOS, by = "evento"), evaluar_evento))
anual <- merge(anual, PAISES_HMD, by = "codigo")
fwrite(anual, file.path(DIR_TAB, "hmd_eventos_anual.csv"))

resumen <- anual[en_evento == TRUE, .(
  anios          = .N,
  exceso         = sum(exceso),
  p_score        = 100 * sum(exceso) / sum(esp),
  exceso_1000    = 1000 * sum(exceso) / mean(pop),
  delta_e0_medio = mean(delta_e0),
  delta_e0_peor  = min(delta_e0),
  anio_peor      = year[which.min(delta_e0)]
), by = .(evento, pais, codigo, sexo, ww1, ww2)]
fwrite(resumen, file.path(DIR_TAB, "hmd_resumen_eventos.csv"))

# Primera Guerra: beligerantes vs neutrales, separando 1918 (gripe)
ww1 <- anual[evento == "Primera Guerra Mundial" & sexo == "Ambos sexos" & en_evento == TRUE]
ww1[, periodo := fifelse(year == 1918, "1918 (guerra + gripe)", "1914–1917 (guerra)")]
ww1_res <- ww1[, .(p_score = 100 * sum(exceso) / sum(esp), delta_e0_medio = mean(delta_e0)),
               by = .(grupo_ww1 = fifelse(ww1 == "Neutral", "Neutrales", "Beligerantes"), periodo)]
fwrite(ww1_res, file.path(DIR_TAB, "hmd_ww1_beligerantes_vs_neutrales.csv"))
cat("\n== Primera Guerra: beligerantes vs neutrales ==\n"); print(ww1_res)

# ---- Figura 6: Δe0 del peor año por país y evento ----------------------------
f6d <- resumen[sexo != "Ambos sexos"]
f6d[, evento := factor(evento, levels = EVENTOS$evento)]
f6 <- ggplot(f6d, aes(delta_e0_peor, reorder(pais, delta_e0_peor), shape = evento)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_point(size = 2.2, fill = "white") +
  scale_shape_manual(values = c(16, 15, 21, 4)) +
  facet_wrap(~sexo) +
  guides(shape = guide_legend(nrow = 2)) +
  labs(x = "Cambio en la esperanza de vida en el peor año del evento (años)", y = NULL,
       shape = NULL, caption = "Fuente: Human Mortality Database. Línea base: tendencia pre-evento por grupo de edad.") +
  theme_articulo()
guardar_fig(f6, "Figura6_guerras_mundiales_vs_covid_hmd", alto = 13)

# Serie de e0 observada 1900–2023 (material suplementario)
e0_serie <- hmd[sexo != "Ambos sexos"][order(codigo, sexo, year, age)][
  , .(e0 = e0_tabla_vida(mx)), by = .(codigo, sexo, year)]
e0_serie <- merge(e0_serie, PAISES_HMD[, .(codigo, pais)], by = "codigo")
fwrite(e0_serie, file.path(DIR_TAB, "hmd_e0_serie.csv"))
fs2 <- ggplot(e0_serie[year >= 1900], aes(year, e0, linetype = sexo)) +
  annotate("rect", xmin = c(1914, 1939, 2020), xmax = c(1918.9, 1945.9, 2022.9),
           ymin = -Inf, ymax = Inf, fill = "grey85") +
  geom_line(linewidth = 0.4) +
  facet_wrap(~pais, ncol = 4) +
  labs(x = "Año", y = "Esperanza de vida al nacer (años)", linetype = NULL) +
  theme_articulo(9)
guardar_fig(fs2, "FigS2_e0_hmd_1900_2023", alto = 18)
