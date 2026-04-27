# ============================================================
# INICIALIZACIÓN Y EJECUCIÓN DE LA APLICACIÓN SHINY
# ============================================================

# ------------------------------
# Carga de archivos principales de la app
# ------------------------------

source("global.R")
source("ui.R")
source("server.R")

# ------------------------------
# Ejecución de la aplicación
# ------------------------------

shinyApp(ui = ui, server = server)
