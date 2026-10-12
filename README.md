# Concordancia paciente–cuidador en la evaluación de necesidades paliativas mediante SPARC-Sp-Col en pacientes con ACV

## Descripción

Estudio observacional, analítico, transversal, cuyo objetivo es evaluar la concordancia entre las respuestas de pacientes sobrevivientes de accidente cerebrovascular (ACV) y sus cuidadores principales al instrumento SPARC-Sp-Col. Se busca identificar los ítems y dominios con mayor o menor concordancia, describir los patrones de desacuerdo y explorar las características asociadas con la discordancia. El análisis estadístico se realizará mediante R.

## Fuentes de datos

Los datos se recolectarán prospectivamente en la consulta ambulatoria de neurología del Hospital Militar Central, Bogotá, mediante REDCap.

- **Población:** Pacientes adultos con antecedente de ACV isquémico o hemorrágico y sus cuidadores principales.
- **Muestra objetivo:** 255 parejas paciente–cuidador analizables.
- **Muestreo:** No probabilístico, consecutivo por conveniencia operativa.
- **Variables:** Características sociodemográficas, características clínicas del ACV, dependencia funcional, características del cuidador y respuestas al SPARC-Sp-Col.
- **Análisis:** Estadística descriptiva, acuerdo observado, Kappa de Cohen, Kappa ponderado, intervalos de confianza del 95 % y análisis exploratorios de discordancia.

## Estructura del repositorio

```text
SPARC-Sp-Col-ACV/
│
├── code/
│   └── 01_importacion_revision.R
│
├── data/
│   ├── raw/
│   │   └── base_erc_cohorte.csv
│   └── processed/
│
├── docs/
│   ├── diccionario_variables.R
│   ├── diccionario_variables.csv
│   └── fuentes_datos.md
│
├── .gitignore
└── README.md
```

### Descripción de las carpetas

- `data/raw/`: Datos originales exportados de REDCap.
- `data/processed/`: Datos depurados y preparados para el análisis.
- `code/`: Scripts de R para limpieza, análisis descriptivo, concordancia y análisis exploratorios.
- `output/figures/`: Gráficos y visualizaciones generados.
- `output/tables/`: Tablas de resultados estadísticos.
- `docs/`: Documentación metodológica y diccionario de variables.

Los archivos `.gitkeep` permiten conservar las carpetas vacías en GitHub.

## Análisis estadístico

El análisis se realizará en R, siguiendo el plan estadístico del protocolo. Se utilizarán medidas descriptivas, proporciones de acuerdo observado, Kappa ponderado lineal para variables ordinales y Kappa de Cohen para variables dicotómicas. Se realizarán análisis de sensibilidad con ponderación cuadrática y análisis exploratorios de factores asociados con la discordancia.

## Confidencialidad

Este repositorio está destinado al control de versiones, documentación y reproducibilidad del análisis estadístico. No se publicarán bases de datos clínicas, información identificable ni archivos sensibles de los participantes. Los datos originales y procesados estarán excluidos del seguimiento de Git mediante `.gitignore`.

## Requisitos

Este proyecto fue desarrollado con R versión 4.5.2. Los paquetes requeridos son:

- tidyverse
- rmarkdown
- here
- skimr

## Cómo reproducir el análisis

1. Clone este repositorio
2. Coloque los datos en la carpeta `data/raw/` (solicitar acceso al equipo de investigación)
3. Abra el archivo `.Rproj` en RStudio
4. Ejecute los scripts en el orden indicado por su numeración

## Contacto

Julián David León Torres — leonjuliand@javeriana.edu.co

