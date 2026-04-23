# 📊 Análisis de la Liga Nacional de Básquet — App Shiny

Esta aplicación interactiva fue desarrollada en R utilizando el paquete Shiny con el objetivo de analizar estadísticas de partidos de la Liga Nacional de Básquet de Argentina.

La app permite explorar métricas de rendimiento por equipo mediante visualizaciones interactivas y filtros dinámicos, facilitando el análisis comparativo entre temporadas, condiciones de juego y resultados.

## 🌐 Aplicación online

La aplicación se encuentra disponible en el siguiente enlace:

https://gallonico.shinyapps.io/app-shiny-basketarg/

## 💻 Ejecutar la app localmente

La aplicación puede ejecutarse localmente en R mediante el siguiente código:

```r
# Instalar paquetes necesarios (solo si no están instalados)
install.packages(c("shiny", "devtools"))

# Ejecutar la aplicación
devtools::runGitHub("app-shiny-basketarg", "gallonico")