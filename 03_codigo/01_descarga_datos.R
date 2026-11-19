# =============================================================================
# 01_descarga_datos.R  ·  Descarga de fuentes primarias y registro de versiones
#
# FUENTE PRINCIPAL
#   UN World Population Prospects 2024 (WPP 2024), División de Población de la
#   ONU. Es la única fuente que ofrece, con un método homogéneo, población y
#   tasas de mortalidad por edad simple (0–100+) para 237 países y áreas desde
#   1950 hasta 2023. Se descargan los archivos de datos del paquete oficial de R
#   "wpp2024" (repositorio PPgp/wpp2024, mantenido por la ONU / U. Washington):
#     - mx1dt.rda      tasas centrales de mortalidad m(x) por edad simple y sexo
#     - popAge1dt.rda  población a mitad de año por edad simple y sexo
#     - e01dt.rda      esperanza de vida al nacer (para validar las tablas de vida)
#     - misc1dt.rda    nacimientos, defunciones y tasas brutas (validación)
#     - UNlocations.txt jerarquía de países y regiones
#
# FUENTES DE VALIDACIÓN (solo COVID-19; no existen equivalentes para 1959–61)
#   - The Economist, modelo global de exceso de muertes (Solstad et al.), con
#     intervalos de 95 %, en GitHub.
#   - OMS: estimaciones de exceso de mortalidad 2020–2021 (Msemburi et al.,
#     Nature 2023). El portal de la OMS no permite descarga directa estable:
#     descargue el ZIP manualmente (ver mensaje al final) y colóquelo en
#     02_datos/raw/who_excess/ — el script 05 lo usa si está presente.
# =============================================================================
source("03_codigo/00_config.R")

base_wpp <- "https://raw.githubusercontent.com/PPgp/wpp2024/main/data/"
archivos <- data.table(
  fuente  = c(rep("UN WPP 2024 (paquete wpp2024 v1.1-3)", 5),
              "The Economist excess deaths model"),
  archivo = c("mx1dt.rda", "popAge1dt.rda", "e01dt.rda", "misc1dt.rda",
              "UNlocations.txt", "economist_export_country_cumulative.csv"),
  url     = c(paste0(base_wpp, c("mx1dt.rda", "popAge1dt.rda", "e01dt.rda",
                                 "misc1dt.rda", "UNlocations.txt")),
              paste0("https://raw.githubusercontent.com/TheEconomist/",
                     "covid-19-the-economist-global-excess-deaths-model/main/",
                     "output-data/export_country_cumulative.csv"))
)

for (i in seq_len(nrow(archivos))) {
  destino <- file.path(DIR_RAW, archivos$archivo[i])
  if (file.exists(destino)) {
    message("Ya existe: ", archivos$archivo[i], " (bórrelo para volver a descargar)")
    next
  }
  message("Descargando ", archivos$archivo[i], " ...")
  download.file(archivos$url[i], destino, mode = "wb", quiet = TRUE)
}

# Manifiesto: fecha, tamaño y huella MD5 de cada archivo (trazabilidad para la
# sección de Métodos y para la política de datos de la revista).
archivos[, `:=`(
  ruta_local     = file.path(DIR_RAW, archivo),
  bytes          = file.size(file.path(DIR_RAW, archivo)),
  md5            = unname(tools::md5sum(file.path(DIR_RAW, archivo))),
  fecha_descarga = format(file.mtime(file.path(DIR_RAW, archivo)), "%Y-%m-%d")
)]
fwrite(archivos, file.path(DIR_RAW, "manifiesto_descargas.csv"))
print(archivos[, .(archivo, bytes, fecha_descarga)])

if (!dir.exists(file.path(DIR_RAW, "who_excess"))) {
  message(
    "\n[Opcional] Para comparar con la OMS descargue el ZIP 'All files' en:\n",
    "  https://www.who.int/data/sets/global-excess-deaths-associated-with-covid-19-modelled-estimates\n",
    "y descomprímalo en 02_datos/raw/who_excess/"
  )
}
