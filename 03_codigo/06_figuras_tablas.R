# =============================================================================
# 06_figuras_tablas.R  ·  Figuras (escala de grises, 300 ppp) y tablas finales
#
# Máximo de la revista: 6 figuras + tablas en total. Propuesta:
#   Figura 1  Mortalidad mundial 1950–2023: defunciones absolutas vs tasa bruta
#             vs tasa estandarizada por edad (muestra por qué el conteo engaña)
#   Figura 2  Defunciones observadas vs esperadas, mundo, ambos periodos
#   Figura 3  P-score y pérdida de esperanza de vida por región, ambos periodos
#   Figura 4  Distribución del exceso por grupo de edad
#   Tabla 1   Resumen comparativo (mundo, sin China, China, regiones)
#   Tabla 2   Sensibilidad a la línea base y validación externa
#   (FigS1 de validación con The Economist queda como material suplementario)
# =============================================================================
source("03_codigo/00_config.R")

panel   <- readRDS(file.path(DIR_PROC, "panel_pais_anio_edad.rds"))
periodo <- fread(file.path(DIR_TAB, "exceso_region_periodo.csv"))
anual   <- fread(file.path(DIR_TAB, "exceso_mundo_anual.csv"))
e0_res  <- fread(file.path(DIR_TAB, "esperanza_vida_resumen_periodo.csv"))
edad    <- fread(file.path(DIR_TAB, "exceso_grupo_edad.csv"), encoding = "UTF-8")
sens    <- fread(file.path(DIR_TAB, "sensibilidad_lineas_base.csv"), encoding = "UTF-8")
val_eco <- fread(file.path(DIR_TAB, "validacion_covid_economist_resumen.csv"))
val_chn <- fread(file.path(DIR_TAB, "validacion_china_literatura.csv"), encoding = "UTF-8")

etq_crisis <- c(hambruna = "Gran Hambruna China (1959–1961)", covid = "COVID-19 (2020–2021)")
sombras <- data.table(xmin = c(1958.5, 2019.5), xmax = c(1961.5, 2021.5))

# ---- Figura 1 ----------------------------------------------------------------------
panel[, grupo := grupo_edad(age)]
g <- panel[, .(D = sum(deaths), N = sum(pop)), by = .(year, grupo)]
g <- merge(g, STD_WHO, by = "grupo")
serie <- g[, .(`Defunciones (millones)` = sum(D) / 1e6,
               `Tasa bruta (por 1000 hab.)` = 1000 * sum(D) / sum(N),
               `Tasa estandarizada por edad (por 1000 hab.)` = 1000 * sum(peso * D / N)),
           by = year]
serie_l <- melt(serie, id.vars = "year")
fwrite(serie, file.path(DIR_TAB, "serie_mundial_1950_2023.csv"))

f1 <- ggplot(serie_l, aes(year, value)) +
  geom_rect(data = sombras, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "grey85") +
  geom_line(linewidth = 0.6) +
  facet_wrap(~variable, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(1950, 2020, 10)) +
  labs(x = "Año", y = NULL,
       caption = "Franjas grises: 1959–1961 y 2020–2021. Población estándar: OMS 2000–2025. Fuente: UN WPP 2024.") +
  theme_articulo()
guardar_fig(f1, "Figura1_mortalidad_mundial_1950_2023", alto = 16)

# ---- Figura 2 ----------------------------------------------------------------------
an <- melt(anual[, .(crisis, year, Observadas = obs / 1e6, `Esperadas sin crisis` = esp / 1e6)],
           id.vars = c("crisis", "year"))
an[, crisis := factor(etq_crisis[crisis], levels = etq_crisis)]
f2 <- ggplot(an, aes(year, value, linetype = variable, shape = variable)) +
  geom_line() + geom_point(size = 1.6, fill = "white") +
  scale_shape_manual(values = c(16, 21)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  facet_wrap(~crisis, scales = "free") +
  scale_x_continuous(breaks = function(l) seq(ceiling(l[1]), floor(l[2]), 2)) +
  labs(x = "Año", y = "Defunciones en el mundo (millones)", linetype = NULL, shape = NULL,
       caption = "Esperadas: tasas por edad proyectadas desde la tendencia pre-crisis × población observada.") +
  theme_articulo()
guardar_fig(f2, "Figura2_observadas_vs_esperadas")

# ---- Figura 3 ----------------------------------------------------------------------
reg <- merge(periodo[spec == SPEC_PRINCIPAL, .(crisis, unidad, p_score)],
             e0_res[, .(crisis, unidad, delta_e0_medio)], by = c("crisis", "unidad"))
reg <- melt(reg, id.vars = c("crisis", "unidad"))
reg[, variable := factor(variable, levels = c("p_score", "delta_e0_medio"),
                         labels = c("P-score\n(% sobre lo esperado)",
                                    "Δ esperanza de vida\n(años, media anual)"))]
orden <- c("Mundo", "Mundo sin China", "China", rev(sort(setdiff(unique(reg$unidad),
                                                  c("Mundo", "Mundo sin China", "China")))))
reg[, unidad := factor(unidad, levels = rev(orden))]
reg[, crisis := factor(etq_crisis[crisis], levels = etq_crisis)]
f3 <- ggplot(reg, aes(value, unidad, shape = crisis)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_point(size = 2.4, fill = "white") +
  scale_shape_manual(values = c(16, 21)) +
  facet_wrap(~variable, scales = "free_x") +
  labs(x = NULL, y = NULL, shape = NULL) +
  theme_articulo()
guardar_fig(f3, "Figura3_pscore_esperanza_vida_region", alto = 12)

# ---- Figura 4 ----------------------------------------------------------------------
edad[, crisis := factor(etq_crisis[crisis], levels = etq_crisis)]
edad[, gran_grupo := factor(gran_grupo, levels = c("0–4", "5–14", "15–64", "65+"))]
f4 <- ggplot(edad, aes(gran_grupo, p_score)) +
  geom_col(fill = "grey45", width = 0.65) +
  geom_text(aes(label = ifelse(abs(pct_del_exceso) < 0.5, "≈0 %",
                               paste0(fmt_n(pct_del_exceso, 0), " %")),
                vjust = ifelse(p_score >= 0, -0.4, 1.3)), size = 3) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.08, 0.12))) +
  facet_wrap(~crisis) +
  labs(x = "Grupo de edad (años)", y = "P-score\n(% sobre lo esperado)",
       caption = "Etiquetas: porcentaje del exceso total del periodo en cada grupo de edad.") +
  theme_articulo()
guardar_fig(f4, "Figura4_exceso_por_edad", alto = 9)

# ---- Tabla 1 -----------------------------------------------------------------------
t1 <- merge(periodo[spec == SPEC_PRINCIPAL],
            e0_res[, .(crisis, unidad, delta_e0_medio, delta_e0_max)],
            by = c("crisis", "unidad"))
t1[, orden := match(unidad, orden)]
setorder(t1, -crisis, orden)
tabla1 <- t1[, .(
  `Crisis` = etq_crisis[crisis],
  `Unidad` = unidad,
  `Defunciones observadas` = fmt_n(obs),
  `Defunciones esperadas` = fmt_n(esp),
  `Exceso de defunciones` = fmt_n(exceso),
  `P-score (%)` = fmt_n(p_score, 1),
  `Exceso por 100 000 hab.-año` = fmt_n(exceso_100k_anio, 1),
  `TEE observada (por 100 000)` = fmt_n(tee_obs, 1),
  `TEE esperada (por 100 000)` = fmt_n(tee_esp, 1),
  `Δe0 medio (años)` = fmt_n(delta_e0_medio, 2),
  `Δe0 máximo anual (años)` = fmt_n(delta_e0_max, 2)
)]

# ---- Tabla 2 -----------------------------------------------------------------------
tabla2a <- sens[, .(`Crisis` = etq_crisis[crisis], `Unidad` = unidad, `Línea base` = label,
                    `Exceso (millones)` = fmt_n(exceso / 1e6, 2),
                    `P-score (%)` = fmt_n(p_score, 1))]
tabla2b <- rbindlist(list(
  data.table(Comparación = "COVID-19 mundo, este estudio",       `Exceso (millones)` = fmt_n(val_eco$exceso_wpp_M, 2)),
  data.table(Comparación = "COVID-19 mundo, The Economist",     `Exceso (millones)` = fmt_n(val_eco$exceso_eco_M, 2)),
  data.table(Comparación = "COVID-19 mundo, OMS (Msemburi 2023)", `Exceso (millones)` = "14,83 (13,2–16,6)"),
  val_chn[, .(Comparación = paste0("China: ", fuente, " [", periodo, "]"),
              `Exceso (millones)` = fmt_n(exceso_millones, 1))]
))

wb <- createWorkbook()
for (nm in c("Tabla1", "Tabla2a_sensibilidad", "Tabla2b_validacion")) addWorksheet(wb, nm)
writeData(wb, "Tabla1", tabla1)
writeData(wb, "Tabla2a_sensibilidad", tabla2a)
writeData(wb, "Tabla2b_validacion", tabla2b)
for (nm in names(wb)) setColWidths(wb, nm, cols = 1:12, widths = "auto")
saveWorkbook(wb, file.path(DIR_TAB, "Tablas_articulo.xlsx"), overwrite = TRUE)
fwrite(tabla1, file.path(DIR_TAB, "Tabla1_resumen.csv"))
message("Figuras en ", DIR_FIG, " · Tablas en ", DIR_TAB, "/Tablas_articulo.xlsx")
