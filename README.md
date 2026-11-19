# Which deaths count? Mortality shocks from war, famine, and COVID-19 and the attention they receive, 237 countries, 1914–2023

Replication code and derived data for the article:

> Reina Bolaños, C.A., Minotta Díaz, I.L., and Montoya Cardona, J.D. *Which deaths count? Mortality shocks from war, famine, and COVID-19 and the attention they receive, 237 countries, 1914–2023.* Submitted to *Demographic Research*.

The code estimates age-specific excess mortality and losses of period life expectancy (Δe0) for 237 countries (UN World Population Prospects 2024). It then:

1. builds an inventory of national mortality shocks, 1955–2023, and classifies each by cause;
2. compares the First World War, the Spanish Civil War, the Second World War and COVID-19 in 15 Human Mortality Database populations;
3. measures scientific attention (PubMed records) and humanitarian funding (OCHA FTS) per excess death.

## How to reproduce the results

Requirements: R ≥ 4.3 and an internet connection. The packages `data.table`, `ggplot2`, `scales`, `openxlsx` and `jsonlite` are installed automatically if missing.

1. Download or clone this repository and open `which-deaths-count.Rproj` (or set the working directory to the repository folder).
2. **Human Mortality Database (only for script 08).** HMD data cannot be redistributed. Register for free at <https://www.mortality.org>, download the bulk files *Deaths 1x1* and *Exposures 1x1* (all countries), and unzip them anywhere inside `02_datos/raw/hmd/` (see `02_datos/raw/hmd/README_HMD.txt`).
3. Run, from the repository folder:

   ```r
   source("03_codigo/run_all.R")
   ```

   or in a terminal: `Rscript 03_codigo/run_all.R`. The first run downloads about 110 MB of WPP 2024 files from the official `wpp2024` package repository (script 01) and queries PubMed and OCHA FTS (script 09, 5–10 minutes; results are cached). Scripts 08 and 09 are skipped with a warning if HMD files or internet access are missing; the rest still runs.

Outputs are written to `04_resultados/`. The versions included here are those reported in the article.

## Scripts (`03_codigo/`)

| Script | Purpose |
|---|---|
| `00_config.R` | Packages, paths, baseline specifications, WHO standard population, plotting style |
| `01_descarga_datos.R` | Downloads WPP 2024 (package `wpp2024` v1.1-3) and The Economist's excess-death estimates |
| `02_preparar_datos.R` | Country × year × age panel (countries only, no aggregates); rescales deaths to WPP totals |
| `03_exceso_mortalidad.R` | Excess deaths and P-scores, Chinese famine (1959–61) and COVID-19 (2020–21), all baselines |
| `04_esperanza_vida.R` | Life tables and Δe0 by country and region |
| `05_validacion_externa.R` | Validation against The Economist, WHO and the famine literature |
| `06_figuras_tablas.R` | Working figures and tables (Spanish labels) |
| `07_choques_mortalidad_1950_2023.R` | Inventory of national mortality shocks, 1955–2023 |
| `08_guerras_mundiales_hmd.R` | World wars, Spanish Civil War and COVID-19 in 15 HMD populations |
| `09_atencion_cientifica_humanitaria.R` | PubMed records and OCHA FTS funding per excess death, with search sensitivity analyses |
| `10_incertidumbre_montecarlo.R` | Monte Carlo prediction intervals and range across baselines |
| `11_sensibilidad_umbrales_choques.R` | Sensitivity of the shock inventory to the P-score threshold (5%, 10%, 20%) |
| `12_figuras_tablas_ingles.R` | Final figures and tables of the article (English) |
| `run_all.R` | Runs everything in order and writes `sessionInfo.txt` |

Code comments are in Spanish; object and file names follow the article's sections.

## Data

| Folder | Content |
|---|---|
| `02_datos/manual/clasificacion_choques.csv` | Classification of every detected episode (type, event, source citation, verification status) |
| `02_datos/manual/china_hambruna_estimaciones_literatura.csv` | Published estimates of excess deaths in the Chinese famine |
| `02_datos/manual/iso3_codigo_numerico.csv` | UN numeric country codes and ISO3 codes |
| `04_resultados/tablas/` | All derived tables (CSV) |
| `04_resultados/tablas_en/` | Tables 1–4 and A1–A3 of the article (`Tables_DemRes.xlsx` and CSV) |
| `04_resultados/figuras_en/` | Figures 1–4 and A1–A6 of the article (PNG, 300 dpi) |

Raw downloads (`02_datos/raw/`) and intermediate objects (`02_datos/processed/`) are not included; they are recreated by the code.

### Sources

- United Nations, Population Division (2024). *World Population Prospects 2024*, via the R package `wpp2024` v1.1-3 (<https://github.com/PPgp/wpp2024>). CC BY 3.0 IGO.
- Human Mortality Database (<https://www.mortality.org>). Not redistributed here.
- PubMed, NCBI E-utilities (<https://www.ncbi.nlm.nih.gov/books/NBK25497/>).
- OCHA Financial Tracking Service (<https://fts.unocha.org>).
- The Economist's excess-deaths model (<https://github.com/TheEconomist/covid-19-the-economist-global-excess-deaths-model>).

## License

Code: MIT License (see `LICENSE`). Derived tables, classification file and figures: CC BY 4.0. Please cite the article and this repository (see `CITATION.cff`).

## Contact

Carlos Alberto Reina Bolaños — GitHub: [@carlosreina-epi](https://github.com/carlosreina-epi)
