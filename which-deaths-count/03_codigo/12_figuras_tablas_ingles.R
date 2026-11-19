# =============================================================================
# 12_figuras_tablas_ingles.R  ·  Display items for the Demographic Research
#                               submission (English, final numbering)
#
# Reads only tables already written by scripts 03–11 (no recomputation), so it
# runs in seconds. Output:
#   04_resultados/figuras_en/  Figure1–4 and FigureA1–A6 (PNG, 300 dpi, greyscale)
#   04_resultados/tablas_en/   Tables_DemRes.xlsx (Tables 1–4, Appendix A1–A3)
#                              and one CSV per table
#
# Main text
#   Figure 1  Life expectancy, observed vs counterfactual: famine vs COVID-19
#   Figure 2  National mortality shocks 1955–2023 by type (worst-year Δe0)
#   Figure 3  World wars vs COVID-19 in 15 HMD populations
#   Figure 4  Intensity vs scientific attention, episodes since 2000
#   Table 1   Reference cases with 95% PI and baseline range
#   Table 2   Shocks by type + threshold sensitivity
#   Table 3   Worst-year Δe0 by country and event (HMD)
#   Table 4   Attention by type (PubMed main + sensitivity, FTS funding)
# Appendix
#   Figure A1 World deaths, crude and age-standardised rates 1950–2023
#   Figure A2 Observed vs expected deaths, world
#   Figure A3 P-score and Δe0 by region
#   Figure A4 Excess by age group
#   Figure A5 Validation against The Economist (COVID-19, by country)
#   Figure A6 Life expectancy 1900–2023, 15 HMD populations
#   Table A1  All baseline specifications
#   Table A2  Chinese famine: literature vs this study
#   Table A3  Episode list with type, Δe0 and attention
# =============================================================================
source("03_codigo/00_config.R")
DIR_FIG_EN <- "04_resultados/figuras_en"; DIR_TAB_EN <- "04_resultados/tablas_en"
dir.create(DIR_FIG_EN, FALSE, TRUE); dir.create(DIR_TAB_EN, FALSE, TRUE)
leer <- function(f) fread(file.path(DIR_TAB, f), encoding = "UTF-8")
save_fig <- function(p, name, w = 17, h = 11)
  ggsave(file.path(DIR_FIG_EN, paste0(name, ".png")), p, width = w, height = h,
         units = "cm", dpi = 300, bg = "white")
fmt <- function(x, d = 0) formatC(x, format = "f", digits = d, big.mark = ",")

# ---- Translation dictionaries -------------------------------------------------
TYPE_EN <- c(
  "Genocidio / violencia masiva" = "Genocide / mass violence",
  "Hambruna"                     = "Famine",
  "Desastre natural"             = "Natural disaster",
  "Mixto"                        = "Mixed (conflict and famine)",
  "Conflicto armado"             = "Armed conflict",
  "Epidemia de VIH/sida"         = "HIV/AIDS epidemic",
  "Crisis socioeconómica"        = "Socioeconomic crisis",
  "Pandemia COVID-19"            = "COVID-19",
  "Evento climático extremo"     = "Extreme weather",
  "Otra epidemia"                = "Other epidemic",
  "Sin clasificar"               = "Unclassified")
UNIT_EN <- c("Mundo" = "World", "Mundo sin China" = "World excluding China", "China" = "China",
             "África" = "Africa", "Asia" = "Asia", "Europa" = "Europe",
             "América Latina y el Caribe" = "Latin America and the Caribbean",
             "América del Norte" = "Northern America", "Oceanía" = "Oceania")
CRISIS_EN <- c(hambruna = "Chinese famine, 1959–1961", covid = "COVID-19, 2020–2021")
EVENT_EN <- c("Primera Guerra Mundial" = "First World War (1914–1918)",
              "Guerra Civil Española"  = "Spanish Civil War (1936–1939)",
              "Segunda Guerra Mundial" = "Second World War (1939–1945)",
              "COVID-19"               = "COVID-19 (2020–2022)")
COUNTRY_EN <- c("Australia" = "Australia", "Bélgica" = "Belgium", "Canadá" = "Canada",
                "Dinamarca" = "Denmark", "España" = "Spain", "Estados Unidos" = "United States",
                "Finlandia" = "Finland", "Francia" = "France",
                "Inglaterra y Gales" = "England and Wales", "Italia" = "Italy",
                "Noruega" = "Norway", "Países Bajos" = "Netherlands", "Portugal" = "Portugal",
                "Suecia" = "Sweden", "Suiza" = "Switzerland")
STATUS_EN <- c("Beligerante" = "belligerent", "Neutral" = "neutral", "Ocupado" = "occupied",
               "Beligerante/ocupado" = "belligerent/occupied",
               "Guerra civil 1918" = "civil war 1918")
METRIC_EN <- c("Exceso de muertes (millones)" = "Excess deaths (millions)",
               "P-score (%)" = "P-score (%)",
               "Δe0 medio del periodo (años)" = "Δe0, period mean (years)",
               "Δe0 peor año (años)" = "Δe0, worst year (years)")
short_country <- function(x) {
  x <- sub(" \\(.*", "", x)
  x <- sub("^Syrian Arab Republic$", "Syria", x)
  x <- sub("^State of Palestine$", "Palestine", x)
  sub("^Central African Republic$", "CAR", x)
}
shade <- data.table(xmin = c(1958.5, 2019.5), xmax = c(1961.5, 2021.5),
                    crisis = factor(CRISIS_EN, levels = CRISIS_EN))

# ==============================================================================
# FIGURE 1  Life expectancy, observed vs counterfactual
# ==============================================================================
ev <- leer("esperanza_vida_region_anual.csv")[unidad %in% c("Mundo", "Mundo sin China", "China")]
ev[, unit := factor(UNIT_EN[unidad], levels = UNIT_EN[c("Mundo", "Mundo sin China", "China")])]
ev[, crisis := factor(CRISIS_EN[crisis], levels = CRISIS_EN)]
evl <- melt(ev[, .(crisis, unit, year, Observed = e0_obs, `Counterfactual (no crisis)` = e0_esp)],
            id.vars = c("crisis", "unit", "year"))
f1 <- ggplot(evl, aes(year, value, linetype = variable)) +
  geom_rect(data = shade, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "grey88") +
  geom_line(linewidth = 0.6) +
  facet_grid(unit ~ crisis, scales = "free") +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_x_continuous(breaks = function(l) seq(ceiling(l[1]), floor(l[2]), 2)) +
  labs(x = "Year", y = "Period life expectancy at birth (years)", linetype = NULL) +
  theme_articulo()
save_fig(f1, "Figure1_life_expectancy_famine_vs_covid", h = 15)

# ==============================================================================
# FIGURE 2 and TABLE 2  National mortality shocks by type
# ==============================================================================
ep <- leer("choques_episodios_1955_2023.csv")
ep <- ep[incluido == TRUE & !grepl("^Fluctuación", tipo)]
ep[, type := TYPE_EN[tipo]]
ep[, type := reorder(factor(type), delta_e0_min, median)]
lab <- ep[order(delta_e0_min)][, .SD[1], by = type][delta_e0_min < -10]
lab[, label := paste(short_country(pais), inicio)]
f2 <- ggplot(ep, aes(delta_e0_min, type)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_boxplot(outlier.shape = NA, fill = "grey92", colour = "grey40", width = 0.6) +
  geom_point(aes(size = pmax(exceso_total, 1e3)), shape = 21, fill = "grey30",
             colour = "white", alpha = 0.8) +
  geom_text(data = lab, aes(label = label), size = 2.5, vjust = -1.2, hjust = 0.2) +
  scale_x_continuous(expand = expansion(mult = c(0.06, 0.03))) +
  scale_size_area(max_size = 9, breaks = c(1e4, 1e5, 1e6, 1e7),
                  labels = c("10,000", "100,000", "1 million", "10 million")) +
  labs(x = "Change in life expectancy in the worst year of the episode (years)",
       y = NULL, size = "Excess deaths") +
  theme_articulo()
save_fig(f2, "Figure2_mortality_shocks_by_type", h = 13)

tp  <- leer("choques_resumen_por_tipo.csv")[!grepl("^Fluctuación", tipo)]
thr <- dcast(leer("sensibilidad_umbrales_por_tipo.csv")[tipo != "Sin clasificar"],
             tipo ~ umbral_p, value.var = "delta_e0_mediana")
setnames(thr, c("5", "20"), c("p5", "p20"), skip_absent = TRUE)
tab2 <- merge(tp, thr[, .(tipo, p5, p20)], by = "tipo", all.x = TRUE)[order(delta_e0_mediana)]
table2 <- tab2[, .(
  `Type of crisis` = TYPE_EN[tipo],
  `Episodes` = episodios,
  `Excess deaths (millions)` = fmt(exceso_total_M, 2),
  `Median excess deaths per 1,000 population` = fmt(exceso_1000_mediana, 1),
  `Median worst-year Δe0 (years)` = fmt(delta_e0_mediana, 1),
  `Most severe worst-year Δe0 (years)` = fmt(delta_e0_peor, 1),
  `Median worst-year Δe0, 5% threshold` = ifelse(is.na(p5), "–", fmt(p5, 1)),
  `Median worst-year Δe0, 20% threshold` = ifelse(is.na(p20), "–", fmt(p20, 1)))]

# ==============================================================================
# FIGURE 3 and TABLE 3  World wars vs COVID-19 (HMD)
# ==============================================================================
h <- leer("hmd_resumen_eventos.csv")[sexo == "Ambos sexos"]
h[, country := COUNTRY_EN[pais]]
h[, event := factor(EVENT_EN[evento], levels = EVENT_EN)]
ord <- h[evento == "COVID-19"][order(-delta_e0_peor), country]
ord <- c(setdiff(unique(h$country), ord), ord)
h[, country := factor(country, levels = rev(ord))]
f3 <- ggplot(h, aes(delta_e0_peor, country, shape = event)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_point(size = 2.4, fill = "white", stroke = 0.6) +
  scale_shape_manual(values = c(16, 15, 21, 4)) +
  guides(shape = guide_legend(nrow = 2)) +
  labs(x = "Change in life expectancy in the worst year of the event (years)",
       y = NULL, shape = NULL) +
  theme_articulo()
save_fig(f3, "Figure3_world_wars_vs_covid_hmd", h = 13)

tab3 <- dcast(h, country + ww1 + ww2 ~ evento, value.var = "delta_e0_peor")
table3 <- tab3[order(as.character(country)), .(
  `Population` = as.character(country),
  `Status, First World War` = STATUS_EN[ww1],
  `Status, Second World War` = STATUS_EN[ww2],
  `First World War` = ifelse(is.na(`Primera Guerra Mundial`), "–", fmt(`Primera Guerra Mundial`, 1)),
  `Spanish Civil War` = ifelse(is.na(`Guerra Civil Española`), "–", fmt(`Guerra Civil Española`, 1)),
  `Second World War` = ifelse(is.na(`Segunda Guerra Mundial`), "–", fmt(`Segunda Guerra Mundial`, 1)),
  `COVID-19` = ifelse(is.na(`COVID-19`), "–", fmt(`COVID-19`, 1)))]

# ==============================================================================
# FIGURE 4 and TABLE 4  Attention per excess death
# ==============================================================================
at <- leer("atencion_por_episodio.csv")[inicio >= 2000 & pubmed_n > 0]
at[, group := fifelse(tipo == "Pandemia COVID-19", "COVID-19", "Wars, famines, disasters and other crises")]
lab4 <- unique(rbind(at[group != "COVID-19"][order(delta_e0_min)][1:6], at[group != "COVID-19" & pub_por_1000_muertes < 0.15]))
lab4[, label := paste(short_country(pais), inicio)]
f4 <- ggplot(at, aes(delta_e0_min, pub_por_1000_muertes, shape = group)) +
  geom_point(size = 2, fill = "white") +
  geom_text(data = lab4, aes(label = label), size = 2.4, vjust = -1, hjust = 0.1,
            show.legend = FALSE) +
  scale_x_continuous(expand = expansion(mult = c(0.08, 0.03))) +
  scale_shape_manual(values = c(21, 16)) +
  scale_y_log10(labels = label_number(big.mark = ",")) +
  labs(x = "Change in life expectancy in the worst year (years)",
       y = "PubMed records per 1,000\nexcess deaths (log scale)", shape = NULL) +
  theme_articulo()
save_fig(f4, "Figure4_intensity_vs_attention")

a_all <- leer("atencion_por_episodio.csv")[inicio >= 2000]
q <- function(x, p) quantile(x, p, na.rm = TRUE)
a_sum <- a_all[, .(n = .N,
                   pm_med = median(pub_por_1000_muertes, na.rm = TRUE),
                   pm_q1 = q(pub_por_1000_muertes, .25), pm_q3 = q(pub_por_1000_muertes, .75),
                   usd_med = median(usd_por_muerte, na.rm = TRUE),
                   usd_q1 = q(usd_por_muerte, .25), usd_q3 = q(usd_por_muerte, .75),
                   pm_mill = median(pub_por_millon_pubmed, na.rm = TRUE)), by = tipo]
sp <- leer("atencion_sensibilidad_pubmed.csv")
a_sum <- merge(a_sum, sp[, .(tipo, mediana_ti, mediana_medline)], by = "tipo")[order(-pm_med)]
table4 <- a_sum[, .(
  `Type of crisis` = TYPE_EN[tipo],
  `Episodes` = n,
  `PubMed records per 1,000 excess deaths, median (IQR)` =
    sprintf("%s (%s–%s)", fmt(pm_med, 2), fmt(pm_q1, 2), fmt(pm_q3, 2)),
  `Title-only search, median` = fmt(mediana_ti, 2),
  `MEDLINE only, median` = fmt(mediana_medline, 2),
  `Records per million PubMed records, median` = fmt(pm_mill, 1),
  `Humanitarian funding, USD per excess death, median (IQR)` =
    ifelse(is.na(usd_med), "not compared",
           sprintf("%s (%s–%s)", fmt(usd_med), fmt(usd_q1), fmt(usd_q3))))]

# ==============================================================================
# TABLE 1  Reference cases with uncertainty
# ==============================================================================
u <- leer("Tabla_incertidumbre_resumen.csv")[unidad %in% c("Mundo", "Mundo sin China", "China")]
u[, d := fifelse(grepl("^Exceso|^P-score", metrica), 1L, 2L)]
u[, cell := sprintf("%s (%s to %s)", fmt(puntual, d[1]), fmt(ip_inf, d[1]), fmt(ip_sup, d[1])),
  by = .(metrica)]
u[, rng := sprintf("%s to %s", fmt(min_lb, d[1]), fmt(max_lb, d[1])), by = .(metrica)]
u[, `:=`(Crisis = CRISIS_EN[crisis], Unit = UNIT_EN[unidad], Measure = METRIC_EN[metrica])]
u[, Crisis := factor(Crisis, levels = CRISIS_EN)]
u[, Unit := factor(Unit, levels = UNIT_EN[c("Mundo", "Mundo sin China", "China")])]
u[, Measure := factor(Measure, levels = METRIC_EN)]
setorder(u, Crisis, Unit, Measure)
table1 <- u[, .(Crisis = as.character(Crisis), Population = as.character(Unit),
                Measure = as.character(Measure),
                `Main estimate (95% prediction interval)` = cell,
                `Range across baseline specifications` = rng)]

# ==============================================================================
# APPENDIX FIGURES
# ==============================================================================
s <- leer("serie_mundial_1950_2023.csv")
setnames(s, 2:4, c("Deaths (millions)", "Crude death rate (per 1,000)",
                   "Age-standardised death rate (per 1,000)"))
sl <- melt(s, id.vars = "year")
fa1 <- ggplot(sl, aes(year, value)) +
  geom_rect(data = shade, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "grey88") +
  geom_line(linewidth = 0.6) +
  facet_wrap(~variable, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(1950, 2020, 10)) +
  labs(x = "Year", y = NULL) + theme_articulo()
save_fig(fa1, "FigureA1_world_mortality_1950_2023", h = 16)

an <- leer("exceso_mundo_anual.csv")
anl <- melt(an[, .(crisis, year, Observed = obs / 1e6, `Expected (no crisis)` = esp / 1e6)],
            id.vars = c("crisis", "year"))
anl[, crisis := factor(CRISIS_EN[crisis], levels = CRISIS_EN)]
fa2 <- ggplot(anl, aes(year, value, linetype = variable, shape = variable)) +
  geom_line() + geom_point(size = 1.6, fill = "white") +
  scale_shape_manual(values = c(16, 21)) + scale_linetype_manual(values = c("solid", "dashed")) +
  facet_wrap(~crisis, scales = "free") +
  scale_x_continuous(breaks = function(l) seq(ceiling(l[1]), floor(l[2]), 2)) +
  labs(x = "Year", y = "World deaths (millions)", linetype = NULL, shape = NULL) +
  theme_articulo()
save_fig(fa2, "FigureA2_observed_vs_expected_deaths")

per <- leer("exceso_region_periodo.csv")[spec == SPEC_PRINCIPAL, .(crisis, unidad, p_score)]
e0r <- leer("esperanza_vida_resumen_periodo.csv")[, .(crisis, unidad, delta_e0_medio)]
rg <- melt(merge(per, e0r, by = c("crisis", "unidad")), id.vars = c("crisis", "unidad"))
rg[, variable := factor(variable, levels = c("p_score", "delta_e0_medio"),
                        labels = c("P-score (%)", "Δe0, period mean (years)"))]
rg[, unit := UNIT_EN[unidad]]
ordr <- c("World", "World excluding China", "China",
          sort(setdiff(unique(rg$unit), c("World", "World excluding China", "China"))))
rg[, unit := factor(unit, levels = rev(ordr))]
rg[, crisis := factor(CRISIS_EN[crisis], levels = CRISIS_EN)]
fa3 <- ggplot(rg, aes(value, unit, shape = crisis)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_point(size = 2.4, fill = "white") + scale_shape_manual(values = c(16, 21)) +
  facet_wrap(~variable, scales = "free_x") +
  labs(x = NULL, y = NULL, shape = NULL) + theme_articulo()
save_fig(fa3, "FigureA3_pscore_life_expectancy_by_region", h = 12)

ag <- leer("exceso_grupo_edad.csv")
ag[, crisis := factor(CRISIS_EN[crisis], levels = CRISIS_EN)]
ag[, gran_grupo := factor(gran_grupo, levels = c("0–4", "5–14", "15–64", "65+"))]
fa4 <- ggplot(ag, aes(gran_grupo, p_score)) +
  geom_col(fill = "grey45", width = 0.65) +
  geom_text(aes(label = ifelse(abs(pct_del_exceso) < 0.5, "≈0%", paste0(fmt(pct_del_exceso), "%")),
                vjust = ifelse(p_score >= 0, -0.4, 1.3)), size = 3) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.08, 0.12))) +
  facet_wrap(~crisis) +
  labs(x = "Age group (years)", y = "P-score (%)") + theme_articulo()
save_fig(fa4, "FigureA4_excess_by_age", h = 9)

vc <- leer("validacion_covid_economist_por_pais.csv")[pop > 0]
vc[, `:=`(ours = 1e5 * exceso_wpp / pop, eco = 1e5 * exceso_eco / pop,
          lo = 1e5 * exceso_eco_inf / pop, hi = 1e5 * exceso_eco_sup / pop)]
fa5 <- ggplot(vc, aes(eco, ours)) +
  geom_abline(linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), colour = "grey75", height = 0) +
  geom_point(size = 1.2) +
  labs(x = "Excess deaths per 100,000, The Economist (95% interval)",
       y = "Excess deaths per 100,000, this study") + theme_articulo()
save_fig(fa5, "FigureA5_validation_economist", w = 14, h = 13)

hs <- leer("hmd_e0_serie.csv")[sexo %in% c("Ambos sexos", "Total")]
if (!nrow(hs)) hs <- leer("hmd_e0_serie.csv")
hs[, country := COUNTRY_EN[pais]]
hs[, sex := fifelse(sexo == "Hombres", "Males", fifelse(sexo == "Mujeres", "Females", "Both sexes"))]
fa6 <- ggplot(hs[year >= 1900], aes(year, e0, linetype = sex)) +
  annotate("rect", xmin = c(1914, 1939, 2020) - 0.5, xmax = c(1918, 1945, 2022) + 0.5,
           ymin = -Inf, ymax = Inf, fill = "grey88") +
  geom_line(linewidth = 0.35) +
  facet_wrap(~country, ncol = 3) +
  scale_x_continuous(breaks = seq(1900, 2020, 40)) +
  labs(x = "Year", y = "Life expectancy at birth (years)", linetype = NULL) + theme_articulo(9)
save_fig(fa6, "FigureA6_life_expectancy_hmd_1900_2023", h = 20)

# ==============================================================================
# APPENDIX TABLES
# ==============================================================================
lb <- leer("incertidumbre_lineas_base.csv")[unidad %in% c("Mundo", "China")]
LB_EN <- c("Lineal" = "Linear", "Media" = "Mean rate", "Interpolación" = "Interpolation",
           "(principal)" = "(main)")
for (k in names(LB_EN)) lb[, label := gsub(k, LB_EN[[k]], label, fixed = TRUE)]
lbw <- dcast(lb, crisis + unidad + spec + label ~ metrica, value.var = "valor")
tableA1 <- lbw[order(-crisis, unidad, spec), .(
  Crisis = CRISIS_EN[crisis], Population = UNIT_EN[unidad], Baseline = label,
  `Excess deaths (millions)` = fmt(exceso / 1e6, 2), `P-score (%)` = fmt(p_score, 1),
  `Δe0, period mean (years)` = fmt(d_e0_medio, 2), `Δe0, worst year (years)` = fmt(d_e0_peor, 2))]

ch <- leer("validacion_china_literatura.csv")
ch[, fuente := sub("Este estudio, WPP 2024 — ", "This study, WPP 2024 — ", fuente)]
SRC_EN <- c("Jiang Zhenghua & Li Nan (1986)" = "Jiang and Li (1988)", "Peng Xizhe (1987)" = "Peng (1987)",
            "Cao Shuji (2005)" = "Cao (2005)", "Yang Jisheng (2008)" = "Yang (2008)")
ch[fuente %in% names(SRC_EN), fuente := SRC_EN[fuente]]
for (k in names(LB_EN)) ch[, fuente := gsub(k, LB_EN[[k]], fuente, fixed = TRUE)]
tableA2 <- ch[, .(Source = fuente, Period = periodo, `Excess deaths (millions)` = fmt(exceso_millones, 1))]

epa <- merge(ep, leer("atencion_por_episodio.csv")[, .(id_episodio, pubmed_n, pub_por_1000_muertes, usd_por_muerte)],
             by = "id_episodio", all.x = TRUE)
tableA3 <- epa[order(delta_e0_min), .(
  Country = pais, Start = inicio, End = fin, Type = as.character(type),
  `Excess deaths` = fmt(exceso_total), `Peak P-score (%)` = fmt(p_score_max),
  `Worst-year Δe0 (years)` = fmt(delta_e0_min, 1),
  `PubMed records (since 2000)` = ifelse(inicio >= 2000 & !is.na(pubmed_n), fmt(pubmed_n), "–"),
  `USD per excess death` = ifelse(is.na(usd_por_muerte), "–", fmt(usd_por_muerte)))]

# ---- Write workbook and CSVs -----------------------------------------------------
tabs <- list(Table1 = table1, Table2 = table2, Table3 = table3, Table4 = table4,
             TableA1 = tableA1, TableA2 = tableA2, TableA3 = tableA3)
wb <- createWorkbook()
for (nm in names(tabs)) {
  addWorksheet(wb, nm); writeData(wb, nm, tabs[[nm]])
  setColWidths(wb, nm, cols = seq_along(tabs[[nm]]), widths = "auto")
  fwrite(tabs[[nm]], file.path(DIR_TAB_EN, paste0(nm, ".csv")))
}
saveWorkbook(wb, file.path(DIR_TAB_EN, "Tables_DemRes.xlsx"), overwrite = TRUE)
message("English figures: ", DIR_FIG_EN, " · English tables: ", DIR_TAB_EN)
