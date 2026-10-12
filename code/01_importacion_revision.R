# =========================================================
# SCRIPT 01: IMPORTACIÓN Y REVISIÓN INICIAL DE DATOS
# Proyecto: Concordancia paciente–cuidador en la evaluación de necesidades paliativas mediante SPARC-Sp-Col en pacientes con ACV
# Autor: Julián David León Torres
# Fecha: 11 de octubre de 2026
# =========================================================

library(tidyverse)
library(here)
library(skimr)

# 1. CARGA DE DATOS -------------------------------------------------------

# Importar la base de datos del curso
base_raw <- read.csv(
  here("data", "raw", "base_erc_cohorte.csv"),
  encoding = "UTF-8"
)

# 2. REVISIÓN INICIAL -------------------------------------------------------

# Dimensiones de la base
cat("Participantes:", nrow(base_raw), "\n")
cat("Variables:", ncol(base_raw), "\n")

# Nombres de variables
names(base_raw)

# Estructura de la base
glimpse(base_raw)

# Primeras filas
head(base_raw, 10)

# 3. ESTADÍSTICAS DESCRIPTIVAS RÁPIDAS ------------------------------------

# Resumen completo por variable
skim(base_raw)

# 4. REVISIÓN DE DATOS FALTANTES ------------------------------------------

# Conteo de faltantes por variable
faltantes <- base_raw %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "n_faltantes") %>%
  mutate(pct_faltantes = round(n_faltantes / nrow(base_raw) * 100, 1)) %>%
  filter(n_faltantes > 0) %>%
  arrange(desc(n_faltantes))

print(faltantes)

# 5. REVISIÓN DE VARIABLES CLAVE ------------------------------------------

# Distribución de estadio basal
table(base_raw$estadio_basal)

# Distribución de sexo
table(base_raw$sexo)

# Distribución de etiología
table(base_raw$etiologia_erc)

# Rangos de variables numéricas clave
summary(base_raw[, c("edad", "tfg_ml_min_basal", "creatinina_mg_dl_basal",
                     "hemoglobina_g_dl_basal", "pas_mmhg_basal")])


# 6. NOTAS DE LA REVISIÓN INICIAL -----------------------------------------

# La base contiene 300 participantes y 47 variables.

# Se identificaron 14 variables con valores faltantes, principalmente en las mediciones de seguimiento.

# Al mes 12 se observa una pérdida de seguimiento del 12.3%.
# Al mes 24 se observa una pérdida de seguimiento del 18.0%.

# Se identificaron valores clínicamente improbable:
# - Creatinina basal mínima: 0.08 mg/dL
# - Creatinina basal máxima: 142 mg/dL
# - Peso basal mínimo: 8.5 kg

# Las variables motivo_perdida_m12 y motivo_perdida_m24 contienen espacios sin información que no se contabilizan como NA.

# Se conserva la base de datos original.
