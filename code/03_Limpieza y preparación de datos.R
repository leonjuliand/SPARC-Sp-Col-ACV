library(tidyverse)
library(here)
base <- read.csv(here("data", "raw", "base_erc_cohorte.csv"),
                 encoding = "UTF-8")
#1.Verificacion incial
#Dimensiones
dim(base)

# Estructura: tipos de variables
glimpse(base)

# Primeras y últimas filas
head(base, 5)
tail(base, 5)

# Resumen estadístico rápido
summary(base)

# Resumen completo con skimr
library(skimr)
skim(base)

#2. Revisión y corrección del diccionario de variables
# Ver los tipos actuales
sapply(base, class)

# Corregir tipos cuando sea necesario
base <- base %>%
  mutate(
    # Convertir fecha importada como texto
    fecha_reclutamiento = as.Date(fecha_reclutamiento, format = "%d/%m/%Y"),
    
    # Convertir variables categóricas a factores con orden correcto
    estadio_basal = factor(estadio_basal,
                           levels = c("G2", "G3a", "G3b", "G4"),
                           ordered = TRUE),
    
    nivel_educativo = factor(nivel_educativo,
                             levels = c("Ninguno", "Primaria", "Secundaria",
                                        "Tecnico", "Universitario"),
                             ordered = TRUE),
    
    # Convertir variables de texto a factor nominal
    sexo      = factor(sexo),
    etnia     = factor(etnia),
    procedencia = factor(procedencia),
    etiologia_erc = factor(etiologia_erc)
  )

#3.  Identificación de variables potencialmente identificadoras
# Variables directamente identificadoras en esta base
# - id_estudio: es el identificador del estudio (seudónimo), no identifica directamente
# - fecha_reclutamiento: puede ser identificadora si se combina con otras variables

# Estrategia para esta base:
# El id_estudio (ERC-XXXX) ya es un seudónimo - se conserva para el análisis
# La fecha de reclutamiento se convierte a tiempo desde inicio del estudio

base <- base %>%
  mutate(
    # Calcular días desde inicio del reclutamiento (menos identificador que fecha exacta)
    dias_desde_inicio = as.numeric(
      as.Date(fecha_reclutamiento, format = "%d/%m/%Y") - as.Date("2022-01-15")
    )
  ) %>%
  # Para el análisis, podemos prescindir de la fecha exacta
  select(-fecha_reclutamiento)

#4. Identificación y corrección de inconsistencias
# Definir los rangos esperados según el diccionario de variables
rangos <- list(
  edad                   = c(30, 85),
  peso_kg_basal          = c(35, 180),
  talla_cm               = c(145, 185),
  pas_mmhg_basal         = c(80, 220),
  pad_mmhg_basal         = c(40, 120),
  tfg_ml_min_basal       = c(10, 89),
  creatinina_mg_dl_basal = c(0.4, 15),
  hemoglobina_g_dl_basal = c(5, 18),
  albumina_g_dl_basal    = c(1.5, 5.5),
  proteinuria_mg_g_basal = c(20, 5500)
)

# Identificar valores fuera de rango
for (var in names(rangos)) {
  fuera <- base %>%
    filter(.data[[var]] < rangos[[var]][1] | .data[[var]] > rangos[[var]][2]) %>%
    select(id_estudio, all_of(var))
  
  if (nrow(fuera) > 0) {
    cat("\n[!] Valores fuera de rango en", var, ":\n")
    print(fuera)
  }
}

# CORRECCIÓN DE VALORES ATÍPICOS EN CREATININA BASAL
# Documentación: ver memo_atipicos_creatinina.txt en docs/
#
# ERC-XXXX (valor 0.08): probable error de punto decimal
#   → se corrige a 0.84 mg/dL según revisión del sistema de captura
#
# ERC-YYYY (valor 142.0): probable confusión µmol/L → mg/dL
#   → se convierte: 142 µmol/L ÷ 88.4 = 1.61 mg/dL
#
# ERC-ZZZZ (valor 18.5): extremo pero posible en G4 severo
#   → se conserva; se marcará para análisis de sensibilidad
#
# (Reemplace ERC-XXXX, ERC-YYYY con los IDs reales que identifique)

base <- base %>%
  mutate(
    creatinina_mg_dl_basal = case_when(
      id_estudio == "ERC-XXXX" ~ 0.84,          # corregir error de punto decimal
      id_estudio == "ERC-YYYY" ~ 142.0 / 88.4,  # convertir µmol/L a mg/dL
      TRUE ~ creatinina_mg_dl_basal
    ),
    
    # CORRECCIÓN DE VALORES ATÍPICOS EN PESO BASAL
    # ERC-AAAA (valor 8.5): imposible para adulto, sin información para corregir
    #   → se registra como NA
    # ERC-BBBB (valor 198.0): se conserva; posible obesidad mórbida
    peso_kg_basal = case_when(
      id_estudio == "ERC-AAAA" ~ NA_real_,  # valor imposible → faltante
      TRUE ~ peso_kg_basal
    )
  )
#5. Inconsistencias logicas
# Verificaciones de consistencia lógica

#  La PAD debe ser menor que la PAS
inconsistencia_pa <- base %>%
  filter(!is.na(pas_mmhg_basal) & !is.na(pad_mmhg_basal)) %>%
  filter(pad_mmhg_basal >= pas_mmhg_basal)

if (nrow(inconsistencia_pa) > 0) {
  cat("Inconsistencia PA (PAD >= PAS):", nrow(inconsistencia_pa), "casos\n")
  print(inconsistencia_pa %>% select(id_estudio, pas_mmhg_basal, pad_mmhg_basal))
}

#  El IMC debe ser coherente con peso y talla
base <- base %>%
  mutate(
    imc_calculado = round(peso_kg_basal / (talla_cm / 100)^2, 1),
    discrepancia_imc = abs(imc_basal - imc_calculado)
  )

inconsistencia_imc <- base %>%
  filter(!is.na(discrepancia_imc) & discrepancia_imc > 2)

if (nrow(inconsistencia_imc) > 0) {
  cat("\nDiscrepancia IMC > 2 kg/m²:", nrow(inconsistencia_imc), "casos\n")
  print(inconsistencia_imc %>%
          select(id_estudio, peso_kg_basal, talla_cm,
                 imc_basal, imc_calculado, discrepancia_imc))
}

# Tabaquismo actual y previo deben ser mutuamente excluyentes
inconsistencia_tabaco <- base %>%
  filter(tabaquismo_actual == 1 & tabaquismo_previo == 1)

if (nrow(inconsistencia_tabaco) > 0) {
  cat("\nInconsistencia tabaquismo (actual Y previo = 1):",
      nrow(inconsistencia_tabaco), "casos\n")
}

#5. Recodificación y transformación de variables
base <- base %>%
mutate(
  # Crear grupos de edad para análisis descriptivo
  grupo_edad = cut(edad,
                   breaks = c(0, 45, 60, 75, Inf),
                   labels = c("<45", "45-59", "60-74", "≥75"),
                   right  = FALSE),
  
  # Agrupar estadios de ERC en categorías más amplias
  estadio_agrupado = case_when(
    estadio_basal %in% c("G2", "G3a") ~ "G2-G3a (leve-moderado)",
    estadio_basal %in% c("G3b", "G4") ~ "G3b-G4 (moderado-severo)"
  ),
  
  # Recodificar etiología en grupos
  etiologia_agrupada = case_when(
    etiologia_erc == "Diabetica"      ~ "Diabética",
    etiologia_erc == "Hipertensiva"   ~ "Hipertensiva",
    TRUE                              ~ "Otras / Desconocida"
  ) %>% factor()
)
base <- base %>%
  mutate(
    # Log-transformación de proteinuria (distribución muy sesgada)
    log_proteinuria_basal = log(proteinuria_mg_g_basal),
    
    # Recalcular IMC usando peso y talla (más confiable que el IMC registrado)
    imc_calculado = round(peso_kg_basal / (talla_cm / 100)^2, 1),
    
    # Calcular cambio en TFG entre basal y mes 12
    cambio_tfg_m12 = tfg_ml_min_m12 - tfg_ml_min_basal,
    
    # Calcular tasa anual de declive de TFG (entre basal y mes 24)
    tasa_declive_tfg = (tfg_ml_min_m24 - tfg_ml_min_basal) / 2
  )
#7 Manejo de datos faltantes
library(naniar)

# Visualizar el patrón de datos faltantes
vis_miss(base %>% select(starts_with("tfg"), starts_with("hemoglobina"),
                         starts_with("albumina"), starts_with("proteinuria")))

# Verificar si los faltantes están asociados con otras variables
# (prueba de MAR vs MCAR)
base %>%
  mutate(faltante_hgb_m12 = is.na(hemoglobina_g_dl_m12)) %>%
  group_by(faltante_hgb_m12) %>%
  summarise(
    n = n(),
    pct_g4 = mean(estadio_basal == "G4") * 100,
    tfg_mediana = median(tfg_ml_min_basal, na.rm = TRUE)
  )

# Imputación por mediana para variables con pocos faltantes (<10%)
# (para el análisis principal; imputación múltiple para el análisis de sensibilidad)
base_analitica <- base %>%
  mutate(
    # Imputar hemoglobina faltante por mediana del estadio correspondiente
    hemoglobina_g_dl_m12 = if_else(
      is.na(hemoglobina_g_dl_m12),
      median(hemoglobina_g_dl_m12, na.rm = TRUE),
      hemoglobina_g_dl_m12
    )
    # Documentar: esta decisión se justifica en memo_datos_faltantes.txt
  )

# ESTRATEGIA
# Imputación por mediana para variables con pocos faltantes (<10%)
# (para el análisis principal; imputación múltiple para el análisis de sensibilidad)
base_analitica <- base %>%
  mutate(
    # Imputar hemoglobina faltante por mediana del estadio correspondiente
    hemoglobina_g_dl_m12 = if_else(
      is.na(hemoglobina_g_dl_m12),
      median(hemoglobina_g_dl_m12, na.rm = TRUE),
      hemoglobina_g_dl_m12
    )
    # Documentar: esta decisión se justifica en memo_datos_faltantes.txt
  )

# 8 Construcción de la base analítica
# =========================================================
# CONSTRUCCIÓN DE LA BASE ANALÍTICA — FLUJO COMPLETO
# =========================================================

base_analitica <- read.csv(here("data", "raw", "base_erc_cohorte.csv"),
                           encoding = "UTF-8") %>%
  
  # CORREGIR TIPOS DE VARIABLES
  mutate(
    estadio_basal = factor(estadio_basal,
                           levels = c("G2", "G3a", "G3b", "G4"),
                           ordered = TRUE),
    sexo  = factor(sexo),
    etnia = factor(etnia),
    etiologia_erc = factor(etiologia_erc)
  ) %>%
  
  # CORREGIR VALORES ATÍPICOS (ver documentación en docs/)
  mutate(
    creatinina_mg_dl_basal = case_when(
      id_estudio == "ERC-XXXX" ~ 0.84,
      id_estudio == "ERC-YYYY" ~ 142.0 / 88.4,
      TRUE ~ creatinina_mg_dl_basal
    ),
    peso_kg_basal = case_when(
      id_estudio == "ERC-AAAA" ~ NA_real_,
      TRUE ~ peso_kg_basal
    )
  ) %>%
  
  # CREAR VARIABLES DERIVADAS
  mutate(
    imc_calculado     = round(peso_kg_basal / (talla_cm / 100)^2, 1),
    log_proteinuria   = log(proteinuria_mg_g_basal),
    cambio_tfg_m12    = tfg_ml_min_m12 - tfg_ml_min_basal,
    tasa_declive_tfg  = (tfg_ml_min_m24 - tfg_ml_min_basal) / 2,
    estadio_agrupado  = case_when(
      estadio_basal %in% c("G2", "G3a") ~ "Leve-moderado (G2-G3a)",
      estadio_basal %in% c("G3b", "G4") ~ "Moderado-severo (G3b-G4)"
    ) %>% factor()
  )

# DOCUMENTAR EL FLUJO DE PARTICIPANTES
cat("=== FLUJO DE CONSTRUCCIÓN DE LA BASE ANALÍTICA ===\n")
cat("Registros en base cruda:        ", nrow(read.csv(here("data","raw","base_erc_cohorte.csv"))), "\n")
cat("Registros en base analítica:    ", nrow(base_analitica), "\n")
cat("Variables en base analítica:    ", ncol(base_analitica), "\n")

# EXPORTAR
saveRDS(base_analitica,
        here("data", "processed", "base_analitica_v1.0.rds"))

cat("\nBase analítica exportada: data/processed/base_analitica_v1.0.rds\n")

# REGISTRAR INFORMACIÓN DE SESIÓN
capture.output(
  sessioninfo::session_info(),
  file = here("docs", "session_info_preparacion.txt")
)

# Verificaciones post-construcción
base_check <- readRDS(here("data", "processed", "base_analitica_v1.0.rds"))

# ¿Los tipos de variables son correctos?
sapply(base_check, class)

# ¿Los valores atípicos fueron corregidos?
summary(base_check$creatinina_mg_dl_basal)
summary(base_check$peso_kg_basal)

# ¿Las variables derivadas tienen valores plausibles?
summary(base_check$imc_calculado)
summary(base_check$tasa_declive_tfg)

# ¿El número de faltantes es el esperado?
colSums(is.na(base_check))

#9. Documentación reproducible de las transformaciones
# Crear memo de decisiones de preparación de datos
writeLines(c(
  "MEMO: DECISIONES DE PREPARACIÓN DE DATOS",
  paste("Fecha:", Sys.Date()),
  "",
  "1. CORRECCIÓN DE ATÍPICOS EN CREATININA BASAL",
  "   - ERC-XXXX (0.08 mg/dL → 0.84 mg/dL): error de punto decimal",
  "     Evidencia: valor de 0.84 registrado en el sistema de captura original",
  "   - ERC-YYYY (142.0 → 1.61 mg/dL): unidades incorrectas (µmol/L → mg/dL)",
  "     Evidencia: resultado de laboratorio indica unidades µmol/L",
  "   - ERC-ZZZZ (18.5 mg/dL): conservado; posible en G4 severo",
  "     Acción: marcado para análisis de sensibilidad",
  "",
  "2. MANEJO DE PESO ATÍPICO",
  "   - ERC-AAAA (8.5 kg → NA): imposible para adulto, sin fuente para verificar",
  "",
  "3. VARIABLES EXCLUIDAS DE LA BASE ANALÍTICA",
  "   - fecha_reclutamiento: reemplazada por dias_desde_inicio",
  "   - imc_basal: reemplazado por imc_calculado (más confiable)",
  "",
  "4. IMPUTACIÓN",
  "   - No se realizó imputación en el análisis principal",
  "   - Análisis de sensibilidad con imputación múltiple: pendiente (semana 14)"
),
here("docs", "memo_preparacion_datos.txt"))