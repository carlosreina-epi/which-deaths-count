# =============================================================================
# 02_preparar_datos.R  ·  Panel país × año × edad simple con defunciones
#
# Construye, para cada uno de los 237 países/áreas de la ONU y cada año
# 1950–2023, la población y las defunciones por edad simple (ambos sexos):
#     D(x,t) = m(x,t) × N(x,t)
# donde m es la tasa central de mortalidad y N la población a mitad de año.
#
# Punto crítico: se trabaja SOLO con
# países (location_type == 4). Sumar todas las filas de la fuente mezcla países
# con agregados (Mundo, continentes, regiones, grupos de ingreso) y multiplica
# las defunciones ≈ 6,4 veces — el error de la Gráfica 1 y la Tabla 1 originales.
# =============================================================================
source("03_codigo/00_config.R")

cargar_rda <- function(f) { e <- new.env(); load(file.path(DIR_RAW, f), envir = e); as.data.table(get(ls(e)[1], e)) }

mx   <- cargar_rda("mx1dt.rda")[year <= 2023, .(country_code, year, age, mx = mxB)]
pop  <- cargar_rda("popAge1dt.rda")[year >= 1950 & year <= 2023, .(country_code, year, age, pop)]
e0   <- cargar_rda("e01dt.rda")[year <= 2023, .(country_code, year, e0_wpp = e0B)]
misc <- cargar_rda("misc1dt.rda")[year <= 2023, .(country_code, year, deaths_wpp = deaths)]

# ---- Jerarquía geográfica ----------------------------------------------------
loc <- fread(file.path(DIR_RAW, "UNlocations.txt"), encoding = "UTF-8")
paises <- loc[location_type == 4, .(country_code, pais = name, area_name)]
paises[, region := REGION_ES[area_name]]
stopifnot(!anyNA(paises$region))
message("Países/áreas: ", nrow(paises))  # esperado: 237

# ---- Panel principal ---------------------------------------------------------
# Población en miles en WPP -> se pasa a personas
panel <- merge(mx, pop, by = c("country_code", "year", "age"))
panel <- panel[country_code %in% paises$country_code]
panel[, `:=`(pop = pop * 1000)]
panel[, deaths := mx * pop]
panel <- merge(panel, paises[, .(country_code, pais, region)], by = "country_code")
setkey(panel, country_code, year, age)

# ---- Controles de calidad ----------------------------------------------------
# 1) Edades 0..100 completas para cada país-año
chk_edades <- panel[, .N, by = .(country_code, year)][, unique(N)]
stopifnot(identical(chk_edades, 101L))

# 2) m(x) × población a mitad de año no reproduce exactamente las defunciones
#    oficiales de WPP (que usan años-persona de exposición). Diferencia típica
#    ~1 %. Se hace un ajuste proporcional ("raking") para que la suma por edad
#    coincida con el total oficial de cada país-año; la distribución por edad
#    se conserva y m(x) se recalcula como D(x)/N(x).
tot <- panel[, .(deaths_sum = sum(deaths)), by = .(country_code, year)]
tot <- merge(tot, misc, by = c("country_code", "year"))
tot[, factor_raking := (deaths_wpp * 1000) / deaths_sum]
message(sprintf("Factor de ajuste a totales WPP: mediana %.4f; p1–p99 %.3f–%.3f",
                median(tot$factor_raking), quantile(tot$factor_raking, 0.01),
                quantile(tot$factor_raking, 0.99)))
panel <- merge(panel, tot[, .(country_code, year, factor_raking)],
               by = c("country_code", "year"))
panel[, `:=`(deaths = deaths * factor_raking, factor_raking = NULL)]
# Áreas pequeñas pueden tener población 0 en edades extremas: tasa 0, no NaN
panel[, mx := fifelse(pop > 0, deaths / pop, 0)]
stopifnot(!anyNA(panel[, .(mx, deaths, pop)]))
setkey(panel, country_code, year, age)

# 3) Suma de países = Mundo (código 900): confirma que no hay doble conteo
mundo_paises <- panel[, .(deaths = sum(deaths)), by = year]
mundo_wpp    <- misc[country_code == 900, .(year, deaths_mundo = deaths_wpp * 1000)]
chk <- merge(mundo_paises, mundo_wpp, by = "year")
message(sprintf("Suma de países / total Mundo WPP: rango %.4f – %.4f",
                min(chk$deaths / chk$deaths_mundo), max(chk$deaths / chk$deaths_mundo)))

# ---- Guardar -----------------------------------------------------------------
saveRDS(panel,  file.path(DIR_PROC, "panel_pais_anio_edad.rds"))
saveRDS(paises, file.path(DIR_PROC, "paises_regiones.rds"))
saveRDS(e0,     file.path(DIR_PROC, "e0_wpp.rds"))
saveRDS(misc,   file.path(DIR_PROC, "misc_wpp.rds"))
fwrite(chk, file.path(DIR_TAB, "control_suma_paises_vs_mundo.csv"))
message("Panel guardado: ", format(nrow(panel), big.mark = " "), " filas.")
