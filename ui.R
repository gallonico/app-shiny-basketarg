# Creacion de la interfaz de usuario

page_sidebar(
  # Definición del título más la imagen
  title = tagList(
    "Estadísticas de la Liga Nacional de Básquet Argentina",
    tags$img(src = "i3.png", height = "50px")
  ),
  
  sidebar = sidebar(
    # Filtros condicionales: solo se muestran en las pestañas de caja y dispersión
    conditionalPanel(
      condition = "input.Paneldevisualizaciones == 'Rendimiento por variable'",
      selectInput(
        "var_uni",
        "Variable:",
        choices = c("Seleccione..." = "", setNames(names(nombres_var), nombres_var))
      )
    ),
    conditionalPanel(
      condition = "input.Paneldevisualizaciones == 'Patrones entre variables'",
      selectInput(
        "var1",
        "Variable 1:",
        choices = c("Seleccione..." = "", setNames(names(nombres_var), nombres_var))
      ),
      selectInput(
        "var2",
        "Variable 2:",
        choices = c("Seleccione..." = "", setNames(names(nombres_var), nombres_var))
      )
    ),
    
    # Filtro principal
    conditionalPanel(
      condition = "input.Paneldevisualizaciones == 'Rendimiento por variable'",
      selectInput(
        "filtro_comparar",
        "Comparar por filtro principal:",
        choices = c("Ninguno", "Temporada" = "Temp", "Condición" = "Condicion", "Resultado"),
        selected = "Ninguno"
      )
    ),
    conditionalPanel(
      condition = "input.Paneldevisualizaciones == 'Patrones entre variables'",
      selectInput(
        "filtro_comparar2",
        "Comparar por filtro principal:",
        choices = c("Ninguno", "Temporada" = "Temp", "Condición" = "Condicion", "Resultado"),
        selected = "Ninguno"
      )
    ),
    
    # Filtro secundario dinámico
    conditionalPanel(
      condition = "input.Paneldevisualizaciones == 'Rendimiento por variable'",
      uiOutput("filtro_secundario_ui")
    ),
    conditionalPanel(
      condition = "input.Paneldevisualizaciones == 'Patrones entre variables'",
      uiOutput("filtro_secundario2_ui")
    ),
    
    # Filtro terciario dinámico (solo dispersión)
    conditionalPanel(
      condition = "input.Paneldevisualizaciones == 'Patrones entre variables'",
      uiOutput("filtro_terciario2_ui")
    ),
    
    # Filtros de temporada, condición y resultado
    uiOutput("checkboxes_filtros_ui")
  ),
  
  
  # Definición de las pestañas de visualizaciones
  navset_card_underline(
    id = "Paneldevisualizaciones",
    title = "Panel de visualizaciones",
    
    nav_panel(
      "Rendimiento por variable",
      plotlyOutput("graf1")
    ),
    nav_panel(
      "Patrones entre variables",
      plotlyOutput("graf2")
    ),
    # Nueva pestaña combinada de mapas
    nav_panel(
      "Desempeño y relaciones",
      fluidRow(
        # Columna del Mapa de calor
        column(
          width = 6,
          plotlyOutput("graf3"),
          p(
            "Muestra cómo se comparan los equipos en cada variable, destacando con colores más intensos a los equipos cuyos valores se alejan más de la media: rojo para valores negativos y azul para positivos.",
            style = "font-size:14px;"
          )
        ),
        # Columna del Correlograma
        column(
          width = 6,
          plotlyOutput("graf4"),
          p(
            "Muestra la correlación entre las variables. Los tonos más intensos indican correlaciones más fuertes: azul para correlaciones positivas y rojo para negativas.",
            style = "font-size:14px;"
          )
        )
      )
    ),
    
    nav_panel(
      "Exportar datos",
      radioButtons(
        "tipo_datos",
        "Dataset sobre:",
        choices = c("Equipos" = "equipos", "Jugadores" = "jugadores"),
        selected = "equipos",
        inline = TRUE
      ),
      selectInput(
        "formato_descarga",
        "Formato de archivo:",
        choices = c("CSV" = "csv", "Excel" = "xlsx")
      ),
      selectizeInput(
        "equipos_descarga",
        "Seleccionar equipos:",
        choices = sort(unique(equipos$Equipo)),
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = 'Todos los equipos')
      ),
      downloadButton("descargar_datos", "Descargar archivo"),
      br(), br(),
      DT::dataTableOutput("tabla_preview")
    ),
    
    nav_panel(
      "Sobre la app",
      h4("Información general"),
      p("El objetivo principal de esta aplicación es proporcionar al usuario una herramienta interactiva para explorar y analizar el rendimiento de los equipos de la Liga Nacional de Básquet Argentina. La misma permite realizar comparaciones útiles de diversas variables entre los equipos teniendo en cuenta el desempeño por temporada, condición y resultado."),
      br(),
      h4("Fuente de datos"),
      p("Los datos utilizados fueron obtenidos mediante técnicas de web scraping de la página ",
        tags$a(href = "https://www.proballers.com/es", "https://www.proballers.com/es", target = "_blank"),
        " y corresponden a la fase regular de las temporadas 2021-2022, 2022-2023 y 2023-2024 de la Liga Nacional de Básquet Argentina."
      ),
      br(),
      h4("Descripción de variables"),
      tags$ul(
        tags$li(strong("id:"), " Identificador del partido (variable de soporte)."),
        tags$li(strong("source:"), " Código de referencia utilizado durante el web scraping (variable de soporte)."),
        tags$li(strong("Equipo:"), " Equipo que disputó el partido."),
        tags$li(strong("Min:"), " Minutos jugados por el equipo en el partido."),
        tags$li(strong("2Con:"), " Dobles convertidos por el equipo en el partido."),
        tags$li(strong("2Int:"), " Dobles intentados por el equipo en el partido."),
        tags$li(strong("3Con:"), " Triples convertidos por el equipo en el partido."),
        tags$li(strong("3Int:"), " Triples intentados por el equipo en el partido."),
        tags$li(strong("FG%:"), " Porcentaje de tiros de campo convertidos por el equipo en el partido."),
        tags$li(strong("1Con:"), " Tiros libres convertidos por el equipo en el partido."),
        tags$li(strong("1Int:"), " Tiros libres intentados por el equipo en el partido."),
        tags$li(strong("1%:"), " Porcentaje de tiros libres convertidos por el equipo en el partido."),
        tags$li(strong("Ro:"), " Rebotes ofensivos ganados por el equipo en el partido."),
        tags$li(strong("Rd:"), " Rebotes defensivos ganados por el equipo en el partido."),
        tags$li(strong("Reb:"), " Rebotes totales ganados por el equipo en el partido."),
        tags$li(strong("Ast:"), " Asistencias realizadas por el equipo en el partido."),
        tags$li(strong("Bp:"), " Pérdidas de balón cometidas por el equipo en el partido."),
        tags$li(strong("Br:"), " Recuperaciones de balón realizadas por el equipo en el partido."),
        tags$li(strong("Tap:"), " Tapas realizadas por el equipo en el partido."),
        tags$li(strong("Fa:"), " Faltas personales cometidas por el equipo en el partido."),
        tags$li(strong("Pts:"), " Puntos convertidos por el equipo en el partido."),
        tags$li(strong("Val:"), " Valoración asignada al equipo en el partido por la página web de la que se obtienen los datos."),
        tags$li(strong("Temp:"), " Temporada a la que corresponde el partido."),
        tags$li(strong("df:"), " Código de referencia utilizado durante el web scraping (variable de soporte)."),
        tags$li(strong("Condicion:"), " Condición en la que el equipo disputó el partido."),
        tags$li(strong("Rival:"), " Rival del equipo que disputó el partido."),
        tags$li(strong("PtsRec:"), " Puntos recibidos por el equipo en el partido."),
        tags$li(strong("Resultado:"), " Resultado que obtuvo el equipo en el partido."),
        tags$li(strong("Dif:"), " Diferencia de puntos en el partido entre el equipo y su rival."),
        tags$li(strong("Estado:"), " Estado y disponibilidad de los datos del partido.")
      ),
      br(),
      h4("Autor"),
      p("Esta aplicación fue desarrollada por Nicolás Gallo como parte del trabajo de tesina de la carrera Licenciatura en Estadística en la Universidad Nacional de Rosario. Para la misma se utilizó el lenguaje de programación R y el paquete Shiny.")
    )
  )
)
