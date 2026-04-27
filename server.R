# ============================================================
# CREACIÓN DEL SERVIDOR
# ============================================================

function(input, output, session) {
  
# ------------------------------
# Datos filtrados (sin variables)
# ------------------------------

  datos_filtrados_sin_var <- reactive({
    temp_filter <- if (length(input$temp) > 0) filter(equipos, Temp %in% input$temp) else equipos
    cond_filter <- if (length(input$cond) > 0) filter(temp_filter, Condicion %in% input$cond) else temp_filter
    res_filter <- if (length(input$res) > 0) filter(cond_filter, Resultado %in% input$res) else cond_filter
    
    res_filter
  })
  
# ------------------------------
# Datos filtrados para descarga
# ------------------------------
  
  datos_filtrados_descarga <- reactive({
    datos <- if (input$tipo_datos == "equipos") equipos else jugadores
    
    # Filtros principales
    if (!is.null(input$temp) && length(input$temp) > 0) {
      datos <- datos %>% filter(Temp %in% input$temp)
    }
    if (!is.null(input$cond) && length(input$cond) > 0) {
      datos <- datos %>% filter(Condicion %in% input$cond)
    }
    if (!is.null(input$res) && length(input$res) > 0) {
      datos <- datos %>% filter(Resultado %in% input$res)
    }
    
    # Filtro de equipos
    if (!is.null(input$equipos_descarga) && length(input$equipos_descarga) > 0) {
      datos <- datos %>% filter(Equipo %in% input$equipos_descarga)
    }
    
    datos
  })
  
# ------------------------------
# Filtros dinámicos para gráficos
# ------------------------------
  
  # Filtro secundario (gráfico 1)
  output$filtro_secundario_ui <- renderUI({
    req(input$filtro_comparar)
    
    if (input$filtro_comparar == "Ninguno") return(NULL)
    
    secundarios <- setdiff(c("Temp", "Condicion", "Resultado"), input$filtro_comparar)
    
    selectInput("filtro_secundario",
                NULL,
                choices = c("Ninguno" = "Ninguno",
                            setNames(secundarios, nombres_filtros[secundarios])),
                selected = "Ninguno"
                )
              })
  
  # Filtro secundario (gráfico 2)
  output$filtro_secundario2_ui <- renderUI({
    req(input$filtro_comparar2)
    
    if (input$filtro_comparar2 == "Ninguno") return(NULL)
    
    secundarios <- setdiff(c("Temp", "Condicion", "Resultado"), input$filtro_comparar2)
    
    selectInput("filtro_secundario2",
                NULL,
                choices = c("Ninguno" = "Ninguno",
                            setNames(secundarios, nombres_filtros[secundarios])),
                selected = "Ninguno"
                )
              })
  
  # Filtro terciario (gráfico 2)
  output$filtro_terciario2_ui <- renderUI({
    req(input$filtro_comparar2, input$filtro_secundario2)
    
    if (input$filtro_comparar2 == "Ninguno" ||
       is.null(input$filtro_secundario2) ||
       input$filtro_secundario2 == "Ninguno") {
      return(NULL)
    }
    
    # Determinar la variable restante
    resto <- setdiff(
      c("Temp", "Condicion", "Resultado"),
      c(input$filtro_comparar2, input$filtro_secundario2)
      )
    
    selectInput(
      "filtro_terciario2",
      NULL,
      choices = c("Ninguno" = "Ninguno",
                  setNames(resto, nombres_filtros[resto])),
      selected = "Ninguno"
      )
    })

# ------------------------------
# Checkboxes de filtros (UI dinámica)
# ------------------------------  
  
  output$checkboxes_filtros_ui <- renderUI({
    
    if (input$Paneldevisualizaciones %in% c("Rendimiento por variable", "Patrones entre variables")) {
      
      filtro_princ <- if (input$Paneldevisualizaciones == "Rendimiento por variable") input$filtro_comparar else input$filtro_comparar2
      filtro_sec <- if (input$Paneldevisualizaciones == "Rendimiento por variable") input$filtro_secundario else input$filtro_secundario2
      filtro_terc <- if (input$Paneldevisualizaciones == "Rendimiento por variable") NULL else input$filtro_terciario2
      
      filtros_a_ocultar <- c()
      
      if (is.null(filtro_princ) || filtro_princ == "Ninguno") {
        filtros_a_ocultar <- character(0)
      } else {
        filtros_a_ocultar <- c(filtro_princ)
        
        if (!is.null(filtro_sec) && filtro_sec != "Ninguno") {
          filtros_a_ocultar <- c(filtros_a_ocultar, filtro_sec)
          
          if (!is.null(filtro_terc) && filtro_terc != "Ninguno") {
            filtros_a_ocultar <- c(filtros_a_ocultar, filtro_terc)
          }
        }
      }
      
    } else {
      filtros_a_ocultar <- character(0)
    }
    
    tagList(
      
      # Temporada
      if (!("Temp" %in% filtros_a_ocultar)) {
        div(
          style = "margin-bottom:5px;",
          checkboxGroupInput(
            "temp",
            "Temporada:",
            choices = c("2021 / 2022" = "21/22",
                        "2022 / 2023" = "22/23",
                        "2023 / 2024" = "23/24"),
            selected = c("21/22","22/23","23/24")
        ))
      },
      
      # Condición
      if (!("Condicion" %in% filtros_a_ocultar)) {
        div(
          style = "margin-bottom:5px;",
          checkboxGroupInput(
            "cond",
            "Condición:",
            choices = c("Local" = "Local",
                        "Visitante" = "Visit"),
            selected = c("Local","Visit")
        ))
      },
      
      # Resultado
      if (!("Resultado" %in% filtros_a_ocultar)) {
        div(
          style = "margin-bottom:5px;",
          checkboxGroupInput(
            "res",
            "Resultado:",
            choices = c("Triunfo" = "G",
                        "Derrota" = "P"),
            selected = c("G","P")
        ))
      }
    )
  })
  
# ------------------------------
# Resumen por equipo
# ------------------------------
  
  resumen_por_equipo <- reactive({
    req(input$var1 != "", input$var2 != "")
    
    datos_filtrados_sin_var() %>%
      group_by(Equipo) %>%
      summarise(
        var1 = mean(.data[[input$var1]], na.rm = TRUE),
        var2 = mean(.data[[input$var2]], na.rm = TRUE),
        partidos_var1 = sum(!is.na(.data[[input$var1]])),
        partidos_var2 = sum(!is.na(.data[[input$var2]])),
        .groups = "drop"
      )
  })
  
# ------------------------------
# Diccionarios de etiquetas
# ------------------------------

  cond_labels <- c("Local" = "Local",
                   "Visit" = "Visitante")
  
  res_labels <- c("G" = "Triunfo",
                  "P" = "Derrota")
  
# ------------------------------
# Subtítulo con filtros aplicados
# ------------------------------ 

  subtitulo_filtros <- reactive({
    
    todas_temp <- c("21/22", "22/23", "23/24")
    todas_cond <- c("Local", "Visit")
    todas_res <- c("G", "P")
    
    temp_str <- if (length(input$temp) == 0 || all(sort(input$temp) == sort(todas_temp))) "Todas" else paste(gsub("(\\d{2})/(\\d{2})", "20\\1/20\\2", input$temp), collapse = ", ")
    cond_str <- if (length(input$cond) == 0 || all(sort(input$cond) == sort(todas_cond))) "Todas" else paste(recode(input$cond, Local = "Local", Visit = "Visitante"), collapse = ", ")
    res_str <- if (length(input$res) == 0 || all(sort(input$res) == sort(todas_res))) "Todos" else paste(recode(input$res, G = "Triunfo", P = "Derrota"), collapse = ", ")
    
    paste0(
      "<span style='font-size:12px; color:gray;'>",
      "Temporada: ", temp_str,
      " | Condición: ", cond_str,
      " | Resultado: ", res_str,
      "</span>"
      )
  })

# ------------------------------
# Observers (control de variables)
# ------------------------------
  
  observe({
    todas_vars <- names(nombres_var)
    
    # Actualizar var1 (excluyendo var2)
    opciones_var1 <- setdiff(todas_vars, input$var2)
    
    updateSelectInput(
      session,
      "var1",
      choices = c("Seleccionar..." = "",
                  setNames(opciones_var1, nombres_var[opciones_var1])),
      selected = if (input$var1 %in% opciones_var1) input$var1 else ""
      )
    
    # Actualizar var2 (excluyendo var1)
    opciones_var2 <- setdiff(todas_vars, input$var1)
    
    updateSelectInput(
      session,
      "var2",
      choices = c("Seleccionar..." = "",
                  setNames(opciones_var2, nombres_var[opciones_var2])),
      selected = if (input$var2 %in% opciones_var2) input$var2 else ""
      )
  })

  
# ============================================================
# GRÁFICO DE BOXPLOT
# ============================================================  

  output$graf1 <- renderPlotly({
    
    validate(
      need(input$var_uni != "", "📊 Seleccionar una variable para visualizar el gráfico.")
      )
    
    datos <- datos_filtrados_sin_var() %>%
      filter(!is.na(.data[[input$var_uni]]))
    
    validate(
      need(nrow(datos) > 1, "⚠️ No hay suficientes datos para graficar.")
      )
    
# ------------------------------
# Caso sin comparación
# ------------------------------
    
    if (input$filtro_comparar == "Ninguno") {
      
      datos <- datos %>%
        mutate(Equipo = forcats::fct_reorder(
          Equipo,
          .data[[input$var_uni]],
          .fun = median,
          .desc = TRUE
          ))
      
      p <- ggplot(
        datos,
        aes(x = Equipo,
            y = .data[[input$var_uni]],
            fill = Equipo,
            text = paste0(
              "Equipo: ", Equipo,
              "<br>Valor: ", round(.data[[input$var_uni]], 2)
              )
            )
        ) +
        geom_boxplot(
          alpha = 0.6,
          outlier.colour = "red",
          outlier.alpha = 0.6
          ) +
        theme_minimal() +
        labs(
          title = NULL,
          x = NULL,
          y = nombres_var[input$var_uni]
          ) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none"
          )
      
# ------------------------------
# Caso con comparación
# ------------------------------  
      
    } else {
      
      datos <- datos %>%
        mutate(
          Temp = recode(Temp, "21/22" = "2021 / 2022", "22/23" = "2022 / 2023", "23/24" = "2023 / 2024"),
          Condicion = recode(Condicion, Local = "Local", Visit = "Visitante"),
          Resultado = recode(Resultado, G = "Triunfo", P = "Derrota")
        )
      
      filtro_princ <- input$filtro_comparar
      
      filtro_sec <- if (!is.null(input$filtro_secundario) &&
                       input$filtro_secundario != "Ninguno") input$filtro_secundario else NULL
      
# ------------------------------
# Sin filtro secundario
# ------------------------------ 
      
      if (is.null(filtro_sec)) {
        
        facet_var <- filtro_princ
        
        if (facet_var == "Temp") datos$Temp <- factor(datos$Temp, levels = c("2021 / 2022", "2022 / 2023", "2023 / 2024"))
        if (facet_var == "Condicion") datos$Condicion <- factor(datos$Condicion, levels = c("Local", "Visitante"))
        if (facet_var == "Resultado") datos$Resultado <- factor(datos$Resultado, levels = c("Triunfo", "Derrota"))
        
        facet_params <- list()
        
# ------------------------------
# Con filtro secundario
# ------------------------------
        
      } else {
        
        if (filtro_princ == "Temp" || filtro_sec == "Temp") {
          
          temp_var <- "Temp"
          otro_var <- if (filtro_princ == "Temp") filtro_sec else filtro_princ
          
          niveles_temp <- c("2021 / 2022", "2022 / 2023", "2023 / 2024")
          
          niveles_otro <- switch(
            otro_var,
            "Condicion" = c("Local", "Visitante"),
            "Resultado" = c("Triunfo", "Derrota")
            )
          
          niveles_facet <- c()
          
          for (f in niveles_otro) {
            for (t in niveles_temp) {
              niveles_facet <- c(
                niveles_facet,
                paste0(
                  nombres_filtros[otro_var], ": ", f,
                  " | ",
                  nombres_filtros[temp_var], ": ", t
                )
              )
            }
          }
          
          datos$Facet <- paste0(
            nombres_filtros[otro_var], ": ", datos[[otro_var]], 
            " | ",
            nombres_filtros[temp_var], ": ", datos[[temp_var]]
            )
          
          datos$Facet <- factor(datos$Facet, levels = niveles_facet)
          
          facet_var <- "Facet"
          facet_params <- list(nrow = length(niveles_otro))
          
        } else {
          
          datos$Facet <- paste0(
            nombres_filtros[filtro_princ], ": ", datos[[filtro_princ]],
            " | ",
            nombres_filtros[filtro_sec], ": ", datos[[filtro_sec]]
          )
          
          datos$Facet <- factor(datos$Facet, levels = unique(datos$Facet))
          
          facet_var <- "Facet"
          facet_params <- list()
        }
      }
      
# ------------------------------
# Reordenamiento por mediana
# ------------------------------
      
      datos <- datos %>%
        group_by(Equipo) %>%
        mutate(Mediana = median(.data[[input$var_uni]], na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(Equipo = forcats::fct_reorder(Equipo, Mediana, .desc = TRUE))
      
# ------------------------------
# Construcción del gráfico
# ------------------------------
      
      p <- ggplot(
        datos,
        aes(
          x = Equipo,
          y = .data[[input$var_uni]],
          fill = Equipo,
          text = paste0(
            "Equipo: ", Equipo,
            "<br>", nombres_var[input$var_uni], ": ", round(.data[[input$var_uni]], 2),
            if (is.null(filtro_sec)) "" else paste0(
              "<br>", nombres_filtros[filtro_princ], ": ", .data[[filtro_princ]],
              "<br>", nombres_filtros[filtro_sec], ": ", .data[[filtro_sec]]
              )
            )
          )
        ) +
        geom_boxplot(
          alpha = 0.6,
          outlier.colour = "red",
          outlier.alpha = 0.6
          ) +
        do.call(
          facet_wrap,
          c(as.formula(paste("~", facet_var)), facet_params)
          ) +
        theme_minimal() +
        labs(
          title = NULL,
          x = NULL,
          y = nombres_var[input$var_uni]
          ) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none"
        )
    }
    
# ------------------------------
# Conversión a plotly
# ------------------------------
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        title = list(
          text = if (input$filtro_comparar == "Ninguno") {
            paste0(subtitulo_filtros())
          } else {
            NULL
          },
          x = 0.5
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  

# ============================================================
# GRÁFICO DE DISPERSIÓN
# ============================================================
  
  output$graf2 <- renderPlotly({
    
    validate(
      need(input$var1 != "" && input$var2 != "",
           "📊 Seleccionar dos variables para visualizar el gráfico")
    )
    
# ------------------------------
# Preparación de datos
# ------------------------------
    
    datos <- datos_filtrados_sin_var() %>%
      mutate(
        Temp = recode(Temp, "21/22" = "2021 / 2022", "22/23" = "2022 / 2023", "23/24" = "2023 / 2024"),
        Condicion = recode(Condicion, Local = "Local", Visit = "Visitante"),
        Resultado = recode(Resultado, G = "Triunfo", P = "Derrota")
      )
    
# ------------------------------
# Definición de filtros
# ------------------------------
    
    filtro_princ <- input$filtro_comparar2
    
    if (filtro_princ == "Ninguno") {
      
      filtro_sec <- NULL
      filtro_terc <- NULL
      
    } else {
      
      filtro_sec <- if (!is.null(input$filtro_secundario2) && input$filtro_secundario2 != "Ninguno") input$filtro_secundario2 else NULL
      
      if (is.null(filtro_sec)) {
        
        filtro_terc <- NULL
        
      } else {
        
        filtro_terc <- if (!is.null(input$filtro_terciario2) && input$filtro_terciario2 != "Ninguno") input$filtro_terciario2 else NULL
      }
    }
    
# ------------------------------
# Construcción del resumen
# ------------------------------
    
    group_vars <- c("Equipo")
    
    if (filtro_princ != "Ninguno") group_vars <- c(group_vars, filtro_princ)
    if (!is.null(filtro_sec)) group_vars <- c(group_vars, filtro_sec)
    if (!is.null(filtro_terc)) group_vars <- c(group_vars, filtro_terc)
    
    resumen <- datos %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(
        var1 = mean(.data[[input$var1]], na.rm = TRUE),
        var2 = mean(.data[[input$var2]], na.rm = TRUE),
        .groups = "drop"
      )
    
# ------------------------------
# Configuración de facets
# ------------------------------

    facet_var <- NULL
    facet_params <- list()
    
    if (!is.null(filtro_sec) &&
        (filtro_princ == "Temp" || filtro_sec == "Temp")) {
      
      temp_var <- "Temp"
      otro_var <- if (filtro_princ == "Temp") filtro_sec else filtro_princ
      
      niveles_temp <- c("2021 / 2022", "2022 / 2023", "2023 / 2024")
      
      niveles_otro <- switch(
        otro_var,
        "Condicion" = c("Local", "Visitante"),
        "Resultado" = c("Triunfo", "Derrota")
      )
      
      niveles_facet <- c()
      
      for (f in niveles_otro) {
        for (t in niveles_temp) {
          niveles_facet <- c(
            niveles_facet,
            paste0(
              nombres_filtros[otro_var], ": ", f,
              " | ",
              nombres_filtros[temp_var], ": ", t
            )
          )
        }
      }
      
      resumen$Facet <- paste0(
        nombres_filtros[otro_var], ": ", resumen[[otro_var]],
        " | ",
        nombres_filtros[temp_var], ": ", resumen[[temp_var]]
      )
      
      resumen$Facet <- factor(resumen$Facet, levels = niveles_facet)
      
      facet_var <- "Facet"
      facet_params$nrow <- length(niveles_otro)
      
    } else if (!is.null(filtro_sec)) {
      
      resumen$Facet <- paste0(
        nombres_filtros[filtro_princ], ": ", resumen[[filtro_princ]],
        " | ",
        nombres_filtros[filtro_sec], ": ", resumen[[filtro_sec]]
      )
      
      resumen$Facet <- factor(resumen$Facet, levels = unique(resumen$Facet))
      
      facet_var <- "Facet"
      
    } else if (filtro_princ != "Ninguno") {
      
      facet_var <- filtro_princ
    }
    
# ------------------------------
# Construcción del gráfico
# ------------------------------
    
    if (!is.null(filtro_terc)) {
      
      resumen[[filtro_terc]] <- factor(resumen[[filtro_terc]])
      
      p <- ggplot(
        resumen,
        aes(
          x = var1,
          y = var2,
          color = .data[[filtro_terc]],
          text = paste0(
            "Equipo: ", Equipo,
            "<br>", nombres_var[input$var1], ": ", round(var1,2),
            "<br>", nombres_var[input$var2], ": ", round(var2,2),
            "<br>", nombres_filtros[filtro_terc], ": ", resumen[[filtro_terc]]
          )
        )
      ) +
        geom_point(size = 4, alpha = 0.8) +
        labs(color = nombres_filtros[filtro_terc])
      
    } else {
      
      p <- ggplot(
        resumen,
        aes(
          x = var1,
          y = var2,
          text = paste0(
            "Equipo: ", Equipo,
            "<br>", nombres_var[input$var1], ": ", round(var1,2),
            "<br>", nombres_var[input$var2], ": ", round(var2,2)
          )
        )
      ) +
        geom_point(size = 4, alpha = 0.8)
    }
    
# ------------------------------
# Estética general
# ------------------------------   
    
    p <- p +
      theme_minimal() +
      labs(
        x = nombres_var[input$var1],
        y = nombres_var[input$var2]
      )
    
# ------------------------------
# Aplicación de facets
# ------------------------------
    
    if (!is.null(facet_var)) {
      
      if (length(facet_params) > 0) {
        
        p <- p + do.call(
          facet_wrap,
          c(list(as.formula(paste("~", facet_var))), facet_params)
        )
        
      } else {
        
        p <- p + facet_wrap(as.formula(paste("~", facet_var)))
      }
    }
    
# ------------------------------
# Título
# ------------------------------
    
    if (input$filtro_comparar2 == "Ninguno") {
      p <- p + labs(title = paste0(subtitulo_filtros()))
    } else {
      NULL
    }
    
# ------------------------------
# Conversión a plotly
# ------------------------------   
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        title = NULL,
        margin = list(t = 60)
      ) %>%
      config(displayModeBar = FALSE)
  })
  

# ============================================================
# MAPA DE CALOR
# ============================================================
  
  output$graf3 <- renderPlotly({
    
# ------------------------------
# Preparación de datos
# ------------------------------
    
    datos <- datos_filtrados_sin_var()
    
    # Resumen por equipo (medias)
    resumen <- datos %>%
      group_by(Equipo) %>%
      summarise(
        across(all_of(names(nombres_var)), ~ mean(.x, na.rm = TRUE)),
        .groups = "drop"
      )
    
# ------------------------------
# Limpieza de datos
# ------------------------------
    
    # Reemplazo de NA por la media de cada variable
    mat_datos <- resumen %>%
      column_to_rownames("Equipo") %>%
      as.matrix()
    
    for (j in 1:ncol(mat_datos)) mat_datos[is.na(mat_datos[, j])] <- mean(mat_datos[, j], na.rm = TRUE)
    
# ------------------------------
# Normalización (Z-score)
# ------------------------------
    
    mat_datos <- scale(mat_datos)
    
# ------------------------------
# Clustering de equipos (filas)
# ------------------------------
    
    dist_equipos <- dist(mat_datos)
    clust_equipos <- hclust(dist_equipos)
    orden_equipos <- rownames(mat_datos)[clust_equipos$order]
    
# ------------------------------
# Clustering de variables (columnas)
# ------------------------------
    
    dist_vars <- dist(t(mat_datos))
    clust_vars <- hclust(dist_vars)
    orden_vars <- colnames(mat_datos)[clust_vars$order]
    
# ------------------------------
# Transformación a formato largo
# ------------------------------
    
    resumen_long <- resumen %>%
      pivot_longer(
        -Equipo,
        names_to = "Variable",
        values_to = "Valor"
        ) %>%
      group_by(Variable) %>%
      mutate(
        Zscore = (Valor - mean(Valor, na.rm = TRUE)) / sd(Valor, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      mutate(
        Equipo = factor(Equipo, levels = orden_equipos),
        Variable = factor(nombres_var[Variable], levels = nombres_var[orden_vars])
      )
    
# ------------------------------
# Construcción del gráfico
# ------------------------------

    p <- ggplot(
      resumen_long,
      aes(
        x = Variable,
        y = Equipo,
        fill = Zscore,
        text = paste0(
          "Equipo: ", Equipo,
          "<br>Variable: ", Variable,
          "<br>Valor: ", round(Valor, 2),
          "<br>Z-score: ", round(Zscore, 2)
        )
      )
    ) +
      geom_tile() +
      scale_fill_gradient2(
        low = "red",
        mid = "white",
        high = "blue",
        midpoint = 0,
        name = "Z-score"
      ) +
      theme_minimal() +
      labs(
        title = paste0(subtitulo_filtros()),
        x = "Variable",
        y = "Equipo"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()
      )
    
# ------------------------------
# Conversión a plotly
# ------------------------------

    ggplotly(p, tooltip = "text") %>%
      layout(
        title = NULL,
        margin = list(t = 60)
        ) %>%
      config(displayModeBar = FALSE)
  })
  
  
# ============================================================
# CORRELOGRAMA (SPEARMAN)
# ============================================================
  
  output$graf4 <- renderPlotly({
    
# ------------------------------
# Preparación de datos
# ------------------------------
    
    datos <- datos_filtrados_sin_var()
    
    # Resumen por equipo (medias)
    resumen <- datos %>%
      group_by(Equipo) %>%
      summarise(
        across(all_of(names(nombres_var)), ~ mean(.x, na.rm = TRUE)),
        .groups = "drop"
      )
    
# ------------------------------
# Matriz de correlaciones (Spearman)
# ------------------------------

    datos_cor <- resumen %>%
      select(all_of(names(nombres_var)))
    
    cor_mat <- cor(
      datos_cor,
      use = "complete.obs",
      method = "spearman"
    )
    
    orden_vars <- nombres_var[colnames(cor_mat)]
    
# ------------------------------
# Transformación a formato largo
# ------------------------------
    
    cor_long <- as.data.frame(as.table(cor_mat)) %>%
      rename(
        var1 = Var1,
        var2 = Var2,
        correlacion = Freq
      ) %>%
      filter(
        match(var1, colnames(cor_mat)) <= match(var2, colnames(cor_mat))
      ) %>%
      mutate(
        Var1 = nombres_var[as.character(var1)],
        Var2 = nombres_var[as.character(var2)],
        Var1 = factor(Var1, levels = orden_vars),
        Var2 = factor(Var2, levels = rev(orden_vars))
      )
    
# ------------------------------
# Construcción del gráfico
# ------------------------------

    p <- ggplot(
      cor_long,
      aes(
        x = Var1,
        y = Var2,
        fill = correlacion,
        text = paste0(
          "Correlación entre ", Var1,
          " y ", Var2,
          ": ", round(correlacion, 2)
        )
      )
    ) +
      geom_tile() +
      scale_fill_gradient2(
        low = "red",
        mid = "white",
        high = "blue",
        midpoint = 0,
        limits = c(-1, 1),
        name = "Correlación"
      ) +
      theme_minimal() +
      labs(
        title = paste0(subtitulo_filtros()),
        x = NULL,
        y = NULL
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()
      )
    
# ------------------------------
# Conversión a plotly
# ------------------------------

    ggplotly(p, tooltip = "text") %>%
      layout(
        title = NULL,
        margin = list(t = 60)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
# ============================================================
# EXPORTACIÓN DE DATOS
# ============================================================
  
# ------------------------------
# Vista previa de datos
# ------------------------------
  
  output$tabla_preview <- DT::renderDataTable({
    
    datos <- datos_filtrados_descarga()
    
    DT::datatable(
      datos,
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 20, 50, 100)
      )
    ) %>%
      DT::formatRound(
        columns = c("FG%", "1%"),
        digits = 3
      )
  })
  
# ------------------------------
# Descarga de datos
# ------------------------------

  output$descargar_datos <- downloadHandler(
    
    filename = function() {
      paste0("datos_", input$tipo_datos, ".", input$formato_descarga)
    },
    
    content = function(file) {
      
      datos <- datos_filtrados_descarga()
      
      if (input$formato_descarga == "csv") {
        write.csv(datos, file, row.names = FALSE)
      } else {
        writexl::write_xlsx(datos, file)
      }
    }
  )
}
