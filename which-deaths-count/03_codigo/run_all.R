# =============================================================================
# run_all.R  ·  Ejecuta el análisis completo de principio a fin
# Desde la carpeta raíz del artículo:   source("03_codigo/run_all.R")
#
#   01–07  Solo requieren internet la primera vez (descarga WPP 2024, ~110 MB).
#   08     Requiere los archivos de la Human Mortality Database en
#          02_datos/raw/hmd/ (registro gratuito; ver encabezado del script).
#   09     Requiere internet (PubMed y OCHA FTS); ~5–10 min la primera vez.
#   10     Intervalos Monte Carlo y rango entre líneas base (~1 min).
#   11     Sensibilidad del inventario al umbral del P-score (~1–5 min).
#   12     Figuras y tablas en inglés para Demographic Research (segundos).
#   08 y 09 se omiten con un aviso si fallan, sin detener el resto.
# =============================================================================
scripts <- c("01_descarga_datos.R", "02_preparar_datos.R", "03_exceso_mortalidad.R",
             "04_esperanza_vida.R", "05_validacion_externa.R", "06_figuras_tablas.R",
             "07_choques_mortalidad_1950_2023.R", "08_guerras_mundiales_hmd.R",
             "09_atencion_cientifica_humanitaria.R", "10_incertidumbre_montecarlo.R",
             "11_sensibilidad_umbrales_choques.R", "12_figuras_tablas_ingles.R")
opcionales <- c("08_guerras_mundiales_hmd.R", "09_atencion_cientifica_humanitaria.R")
for (s in scripts) {
  message("\n================ ", s, " ================")
  t0 <- Sys.time()
  ok <- tryCatch({
    source(file.path("03_codigo", s), echo = FALSE, encoding = "UTF-8"); TRUE
  }, error = function(e) {
    if (s %in% opcionales) { message("OMITIDO: ", conditionMessage(e)); FALSE } else stop(e)
  })
  if (ok) message(sprintf("OK (%.1f s)", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
writeLines(capture.output(sessionInfo()), "04_resultados/sessionInfo.txt")
