# ============================================================
# CONFIGURACIÓN DEL ENTORNO Y CARGA DE LIBRERÍAS
# ============================================================

# ------------------------------
# Definición de paquetes necesarios
# ------------------------------

paquetes <- c(
  "shiny", "readxl", "tidyverse", "dplyr", "remotes", "bslib",
  "plotly", "writexl", "DT", "viridis", "ggridges","openxlsx"
  )

# ------------------------------
# Instalación de paquetes faltantes
# ------------------------------

instalar <- paquetes %in% installed.packages()[, "Package"]

if (any(!instalar)) {
  install.packages(paquetes[!instalar])
}

# ------------------------------
# Carga de librerías
# ------------------------------

lapply(paquetes, library, character.only = TRUE)


# ============================================================
# CARGA DE DATOS DESDE PAQUETE EXTERNO
# ============================================================

# ------------------------------
# Instalación del paquete de datos (si no está disponible)
# ------------------------------

if (!requireNamespace("basketarg", quietly = TRUE)) {
  remotes::install_github("gallonico/basketarg")
}

# ------------------------------
# Importación de datasets
# ------------------------------

equipos <- basketarg::equipos
jugadores <- basketarg::jugadores


# ============================================================
# DICCIONARIOS DE VARIABLES Y FILTROS
# ============================================================

# ------------------------------
# Diccionario de nombres de variables
# ------------------------------

nombres_var <- c(
  "2Con" = "Dobles convertidos",
  "2Int" = "Dobles intentados",
  "3Con" = "Triples convertidos",
  "3Int" = "Triples intentados",
  "FG%" = "Efectividad en tiros de campo",
  "1Con" = "Libres convertidos",
  "1Int" = "Libres intentados",
  "1%" = "Efectividad en libres",
  "Ro"   = "Rebotes ofensivos",
  "Rd"   = "Rebotes defensivos",
  "Reb"  = "Rebotes totales",
  "Ast"  = "Asistencias",
  "Bp"   = "Pérdidas",
  "Br"   = "Recuperaciones",
  "Tap"  = "Tapas",
  "Fa"   = "Faltas personales",
  "Pts"  = "Puntos convertidos",
  "PtsRec" = "Puntos recibidos",
  "Dif"  = "Diferencia de puntos"
)

# ------------------------------
# Diccionario de nombres de filtros
# ------------------------------

nombres_filtros <- c(
  "Temp"      = "Temporada",
  "Condicion" = "Condición",
  "Resultado" = "Resultado"
)
