# CARGAR DATOS
library(readxl)
library(tidyverse)
library(dplyr)
library(remotes)

# Instalar el paquete de datos desde GitHub si aun no esta instalado
if (!requireNamespace("basketarg", quietly = TRUE)) {
  remotes::install_github("gallonico/basketarg")
}

# Cargar datos desde el paquete
equipos <- basketarg::equipos
jugadores <- basketarg::jugadores

# Diccionario de nombres de variables
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

# Diccionario de nombres de filtros
nombres_filtros <- c(
  "Temp"      = "Temporada",
  "Condicion" = "Condición",
  "Resultado" = "Resultado"
)