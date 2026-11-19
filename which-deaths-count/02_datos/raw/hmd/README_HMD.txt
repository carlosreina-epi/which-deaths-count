Human Mortality Database files (not included: the HMD user agreement does not allow redistribution).

1. Register for free at https://www.mortality.org
2. Download the bulk files "Deaths 1x1" and "Exposures 1x1" for all countries.
3. Unzip them anywhere inside this folder (02_datos/raw/hmd/).

Script 03_codigo/08_guerras_mundiales_hmd.R reads either the combined files
(Deaths_1x1.txt and Exposures_1x1.txt with a PopName column) or one file per
country (e.g. FRATNP.Deaths_1x1.txt), in any subfolder.

Populations used: FRATNP, GBRTENW, ITA, NLD, BEL, DNK, NOR, SWE, CHE, FIN, ESP, PRT, USA, CAN, AUS.
