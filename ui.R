# Creacion de la interfaz de usuario

page_navbar(
  # Título + imagen
  title = tagList(
    "Estadísticas de la Liga Nacional de Básquet Argentina",
    tags$img(src = "i3.png", height = "50px", style = "margin-left: 15px;")
  ),
  theme = bs_theme(
    bootswatch = "cosmo",   # base limpia
    primary = "#CD3333",     # color protagonista
    secondary = "#4F4F4F",   # gris oscuro elegante
    body_bg = "#F5F5F5",     # gris claro (mejor que blanco puro)
    body_color = "#1A1A1A",  # texto oscuro
    base_font = font_google("Roboto")
  ),
  
  tags$head(
    tags$style(HTML("
    .navbar-nav {
      margin-left: 30px;
    }
    
    .navbar-brand {
      font-size: 25px;
    }
    
    .radio-inline {
    margin-right: 25px;
  }
  "))
  ),
  
  # -------------------------------
  # PESTAÑA INICIO
  # -------------------------------
  
  nav_panel(
    "Inicio",
    
    # Imagen
    div( 
      style = "text-align: center; max-width: 2000px; margin: auto; margin-top:5px;",
      
      div(
        style = "
        display: flex;
        justify-content: center;
        flex-wrap: wrap;
        margin-top:20px;
        gap: 10px;
      ",
        img(src = "bas1.jpg", height = "120px"),
        img(src = "bas2.jpg", height = "120px"),
        img(src = "bas3.jpg", height = "120px"),
        img(src = "bas4.jpg", height = "120px"),
        img(src = "bas5.jpg", height = "120px")
      ),
      
      br(),
      
      h1("Visualización de datos para el análisis del rendimiento deportivo",
         style = "margin-bottom:15px; color:#CD3333;"),
      
      div(
        style = "text-align: center; line-height: 1.4; color:#4F4F4F;",
        
        p(
          "Aplicación interactiva para el análisis de datos de partidos de la Liga Nacional de Básquet Argentina.",
          style = "margin-bottom: 5px;"
        ),
        
        p(
          "Facilita la exploración de la información y el estudio del rendimiento de los equipos a partir de diferentes enfoques visuales.",
          style = "margin-top: 0px;"
        )
      ),
      
      br(),
      
      layout_columns(
        col_widths = c(4, 4, 4),
        
        bslib::card(
          style = "
          border-radius: 12px;
          border-top: 4px solid #CD3333;
          background-color: white;",
          bslib::card_body(
            h5("🔍 Exploración de datos"),
            p("Visualización de estadísticas de partidos con navegación dinámica de la información.")
          )
        ),
        
        bslib::card(
          style = "
          border-radius: 12px;
          border-top: 4px solid #CD3333;
          background-color: white;",
          bslib::card_body(
            h5("📈 Análisis de rendimiento"),
            p("Evaluación del desempeño de los equipos e identificación de patrones de juego.")
          )
        ),
        
        bslib::card(
          style = "
          border-radius: 12px;
          border-top: 4px solid #CD3333;
          background-color: white;",
          bslib::card_body(
            h5("⬇️ Exportación de datos"),
            p("Descarga de datos filtrados para su análisis en herramientas externas.")
          )
        )
      )
    )
  ),
  
  # -------------------------------
  # PESTAÑA CON SIDEBAR
  # -------------------------------
  
  nav_panel(
    "Panel de visualizaciones",
    
    layout_sidebar(
      
      sidebar = sidebar(
        width = 265,
        h6("Filtros", style = "margin-bottom: 1px; font-weight: bold;"),
        
        # ------------------
        # VARIABLES
        # ------------------
        conditionalPanel(
          condition = "input.Paneldevisualizaciones == 'Rendimiento por variable'",
          
          div(
            style = "background-color:#f8f9fa; padding:12px; border-radius:10px; margin-bottom:1px; border:1px solid #e0e0e0;",
            
            h6("Variables", style = "margin-bottom:10px; font-weight:600; color:#CD3333;"),
            
            selectInput(
              "var_uni",
              NULL,
              choices = c("Seleccionar..." = "", setNames(names(nombres_var), nombres_var)),
              width = "100%"
            )
          )
        ),
        
        conditionalPanel(
          condition = "input.Paneldevisualizaciones == 'Patrones entre variables'",
          
          div(
            style = "background-color:#f8f9fa; padding:12px; border-radius:10px; margin-bottom:1px; border:1px solid #e0e0e0;",
            
            h6("Variables", style = "margin-bottom:10px; font-weight:600; color:#CD3333;"),
            
            selectInput(
              "var1",
              NULL,
              choices = c("Seleccionar..." = "", setNames(names(nombres_var), nombres_var)),
              width = "100%"
            ),
            
            div(style="margin-bottom:10px;"),
            
            selectInput(
              "var2",
              NULL,
              choices = c("Seleccionar..." = "", setNames(names(nombres_var), nombres_var)),
              width = "100%"
            )
          )
        ),
        
        # ------------------
        # COMPARACIÓN + FILTROS ESPECÍFICOS
        # ------------------
        
        conditionalPanel(
          condition = "input.Paneldevisualizaciones == 'Rendimiento por variable'",
          
          div(
            style = "background-color:#f8f9fa; padding:12px; border-radius:10px; margin-bottom:1px; border:1px solid #e0e0e0;",
            
            h6("Segmentar por", style = "margin-bottom:10px; font-weight:600; color:#CD3333;"),
            
            selectInput(
              "filtro_comparar",
              NULL,
              choices = c("Ninguno", "Temporada" = "Temp", "Condición" = "Condicion", "Resultado"),
              selected = "Ninguno",
              width = "100%"
            ),
            
            # Filtros específicos (solo si corresponde)
            conditionalPanel(
              condition = "input.filtro_comparar != 'Ninguno'",
              
              div(style="margin-top:10px;"),  # espacio
              
              uiOutput("filtro_secundario_ui")
            )
          )
        ),
        
        conditionalPanel(
          condition = "input.Paneldevisualizaciones == 'Patrones entre variables'",
          
          div(
            style = "background-color:#f8f9fa; padding:12px; border-radius:10px; margin-bottom:1px; border:1px solid #e0e0e0;",
            
            h6("Segmentar por", style = "margin-bottom:10px; font-weight:600; color:#CD3333;"),
            
            selectInput(
              "filtro_comparar2",
              NULL,
              choices = c("Ninguno", "Temporada" = "Temp", "Condición" = "Condicion", "Resultado"),
              selected = "Ninguno",
              width = "100%"
            ),
            
            # Filtros específicos
            conditionalPanel(
              condition = "input.filtro_comparar2 != 'Ninguno'",
              
              div(style="margin-top:10px;"),
              
              uiOutput("filtro_secundario2_ui"),
              uiOutput("filtro_terciario2_ui")
            )
          )
        ),
        
        # ------------------
        # FILTROS GENERALES (SIEMPRE VISIBLES)
        # ------------------
          uiOutput("checkboxes_filtros_ui")
      ),
      
      # CONTENIDO
      navset_card_underline(
        id = "Paneldevisualizaciones",
        
        nav_panel(
          "Rendimiento por variable",
          card(
          card_header("Distribución de la variable"),
          full_screen = T,
          plotlyOutput("graf1")
        )),
        
        nav_panel(
          "Patrones entre variables",
          card(
          card_header("Relación entre variables"),
          full_screen = T,
          plotlyOutput("graf2")
        )),
        
        nav_panel(
          "Desempeño y relaciones",
          fluidRow(
            column(
              width = 6,
              card(
              card_header("Mapa de calor por equipo y variable"),
              full_screen = T,
              plotlyOutput("graf3"),
              p(
                "Muestra cómo se comparan los equipos en cada variable, destacando con colores más intensos a los equipos cuyos valores se alejan más de la media: blanco representa al promedio, rojo por debajo del promedio y azul, por encima.",
                style = "font-size:14px;"
              )
            )),
            column(
              width = 6,
              card(
              card_header("Correlograma"),
              full_screen = T,
              plotlyOutput("graf4",),
              p(
                "Muestra la correlación entre las variables. Los tonos más intensos indican correlaciones más fuertes: azul para correlaciones positivas y rojo para negativas.",
                style = "font-size:14px;"
              )
            )
          )
        )),
        
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
            "Formato del archivo:",
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
          div(
            style = "margin-top: 15px;",
            downloadButton(
              "descargar_datos",
              "Descargar datos",
              class = "btn-primary btn-lg w-100"
            )
          ),
          br(), br(),
          DT::dataTableOutput("tabla_preview")
        )
      )
    )
  ),
  
  # -------------------------------
  # PESTAÑA SIN SIDEBAR
  # -------------------------------
  
  nav_panel(
    "Sobre la app",
    
    fluidPage(
      h4("Información general", style = "color:#CD3333;"),
      p("El objetivo principal de esta aplicación es proporcionar al usuario una herramienta interactiva para explorar y analizar el rendimiento de los equipos de la Liga Nacional de Básquet Argentina.
        La misma permite realizar comparaciones útiles de diversas variables entre los equipos teniendo en cuenta el desempeño por temporada, condición y resultado."),
      br(),
      h4("Fuente de datos", style = "color:#CD3333;"),
      p("Los datos utilizados fueron obtenidos mediante técnicas de web scraping de la página ",
        tags$a(href = "https://www.proballers.com/es", "https://www.proballers.com/es", target = "_blank"),
        " y corresponden a la fase regular de las temporadas 2021-2022, 2022-2023 y 2023-2024 de la Liga Nacional de Básquet Argentina."
      ),
      br(),
      h4("Descripción de variables", style = "color:#CD3333;"),
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
      h4("Autor", style = "color:#CD3333;"),
      p("Esta aplicación fue desarrollada por Nicolás Gallo como parte del trabajo de tesina de la carrera Licenciatura en Estadística en la Universidad Nacional de Rosario.
        Para la misma se utilizó el lenguaje de programación R y el paquete Shiny.")
    )
  )
)