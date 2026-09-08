library(shiny)
library(tidyverse)
library(ggiraph)   
library(bslib)     
library(stats)
library(ggcorrplot) 
library(GGally)     
library(ggridges)  
library(e1071)      
library(plotly)

# Carga de datos 
if (file.exists("datos_app.RData")) {
  load("datos_app.RData")
} else {
  stop("Error: No se encuentra 'datos_app.RData'. Asegúrate de generarlo en tu Rmd.")
}

# INTERFAZ DE USUARIO (UI)
ui <- page_navbar(
  # Integración del cursor personalizado
  header = tags$head(
    tags$style(HTML("
      /* Forzar el cursor en todos los elementos */
      html, body, * { 
        cursor: url('my_local_files/whale.png') 16 16, auto !important; 
      }
      
      /* Forzar en elementos interactivos */
      a, button, .btn, .selectize-input, .nav-link, .slider-handle, .interactive_point { 
        cursor: url('my_local_files/whale.png') 16 16, pointer !important; 
      }
    "))
  ),
  
  theme = bs_theme(version = 5, bootswatch = "lux", primary = "#2c3e50"),
  title = "Whale's Behaviour",
  
  sidebar = sidebar(
    title = "Filtros de Datos",
    selectInput("whale_select", "ID de la Ballena:", 
                choices = c("Todas", unique(as.character(df_por_segundos$whale_id)))),
    checkboxGroupInput("behav_select", "Comportamientos:",
                       choices = unique(df_por_segundos$behavior),
                       selected = unique(df_por_segundos$behavior)),
    hr(),
    h5("Configuración Histograma"),
    selectInput("hist_var", "Variable Eje X:", 
                choices = c("Aceleración Z" = "AccZ_mean", 
                            "Profundidad" = "Depth_mean", 
                            "Velocidad Máxima" = "Speed_max", 
                            "Vigor Actividad" = "Activity_vigor"), 
                selected = "Depth_mean"),
    sliderInput("hist_bins", "Tamaño de barras (Bins):", 
                min = 5, max = 100, value = 50)
  ),
  
  # PESTAÑA 1: ANÁLISIS DESCRIPTIVO 
  nav_panel("Análisis Descriptivo",
            navset_card_underline(
              nav_panel("Nivel de Actividad Dinámica",
                        card(
                          card_header("Distribución por Comportamiento"),
                          plotOutput("plot_ridge"),
                          card_footer("Conclusión: 'Rest' y 'Travel' muestran movimientos constantes y suaves. 'Feed' representa las embestidas bruscas para capturar presas. Por último, 'Unknown' sospechamos que la ballena se esta desplazando.")
                        )
              ),
              nav_panel("Profundidad",
                        card(
                          card_header("Según el Estado Biológico"),
                          plotOutput("plot_box"),
                          card_footer("Conclusión: Las ballenas son animales de superficie (0-50m). Los puntos rojos en 'Feed' confirman inmersiones profundas (300-400m) biológicamente clave.")
                        )
              ),
              nav_panel("Profundida Global",
                        card(
                          card_header("Distribución de Profundidad de la Población"),
                          plotOutput("plot_hist"),
                          card_footer("Conclusión: Existe una asimetría positiva extrema. La ballena prefiere la superficie, pero su capacidad de buceo profundo genera la mayor variabilidad del estudio.")
                        )
              ),
              # NUEVO: PERFIL DE INMERSIÓN
              nav_panel("Perfil de Inmersión",
                        card(
                          card_header("Estimación de Trayectoria 2D"),
                          plotOutput("plot_perfil"),
                          card_footer("Conclusión: La ballena alterna nado superficial eficiente con inmersiones profundas en forma de V o U específicamente durante las fases de alimentación.")
                        )
              ),
              # NUEVO: ESPACIO DE ESTADOS 3D
              nav_panel("Espacio de Estados 3D",
                        card(
                          card_header("Vigor, Profundidad y Aceleración Z"),
                          plotlyOutput("plot_3d_estados"),
                          card_footer("Conclusión: Los estados de descanso y viaje se agrupan en baja profundidad y energía, mientras que la alimentación ocupa las zonas más dinámicas del espacio.")
                        )
              ),
              # NUEVO: ESPACIO DE ACELERACIÓN
              nav_panel("Postura 3D",
                        card(
                          card_header("Aceleración Triaxial (X, Y, Z)"),
                          plotlyOutput("plot_3d_postura"),
                          card_footer("Conclusión: Durante el descanso la postura es rígida y concentrada, mientras que la alimentación implica una gran dispersión acrobática en todos los ejes.")
                        )
              ),
              # NUEVO: TRAYECTORIA 3D
              nav_panel("Trayectoria Real 3D",
                        card(
                          card_header("Reconstrucción por Doble Integración"),
                          plotlyOutput("plot_3d_trayectoria"),
                          card_footer("Conclusión: Se visualiza el rastro tridimensional; la alimentación implica giros y cambios de profundidad mucho más agresivos que los tránsitos lineales.")
                        )
              ),
              nav_panel("Matriz de Correlación",
                        card(
                          card_header("Interdependencia Lineal entre Sensores"),
                          plotOutput("plot_corr"),
                          card_footer("Conclusión: Las correlaciones son débiles-moderadas, lo que indica que cada sensor aporta información nueva y no hay redundancia.")
                        )
              ),
              nav_panel("Dispersión Generalizada",
                        card(
                          card_header("Relaciones Cruzadas y Densidades"),
                          plotOutput("plot_pairs"),
                          card_footer("Conclusión: La separación de curvas en la diagonal (especialmente en vigor para 'Feed') valida la capacidad de los sensores para distinguir estados.")
                        )
              ),
              nav_panel("Esfuerzo vs Profundidad",
                        card(
                          card_header("Análisis Detallado por Estado Biológico"),
                          girafeOutput("plot_biv"),
                          card_footer("Conclusión: La relación vigor-profundidad no es universal; cambia según el estado para maximizar la eficiencia energética.")
                        )
              )
            )
  ),
  
  # PESTAÑA 2: PCA 
  nav_panel("PCA",
            card(
              full_screen = TRUE,
              card_header("Espacio Latente del Comportamiento"),
              girafeOutput("plot_pca"),
              card_footer("Conclusión: El PCA resume el 65% de la conducta en dos ejes: Intensidad Energética (PC1) y Perfil de Inmersión (PC2), separando nítidamente el descanso de la caza.")
            )
  ),
  
  # PESTAÑA 3: ANÁLISIS CLUSTER
  nav_panel("Análisis Cluster",
            layout_column_wrap(
              width = 1/2,
              card(
                card_header("Grupos Naturales (K-means)"),
                plotOutput("plot_cluster")
              ),
              card(
                card_header("Naturaleza de los Datos 'Unknown'"),
                tableOutput("table_cluster"),
                card_footer("Conclusión: El 66% de los datos 'Unknown' se agrupan con 'Rest' y 'Travel', confirmando que los periodos sin etiqueta son mayoritariamente tránsitos superficiales.")
              )
            )
  ),
  
  # PESTAÑA 4: ANÁLISIS DISCRIMINANTE 
  nav_panel("Análisis Discriminante",
            layout_column_wrap(
              width = 1/2,
              card(
                card_header("¿Qué sensores son más importantes?"),
                plotOutput("plot_lda_imp"),
                card_footer("Conclusión: El Vigor de Actividad es el principal 'chivato' para el modelo, seguido de la velocidad.")
              ),
              card(
                card_header("Precisión del Etograma (65.58%)"),
                tableOutput("table_lda"),
                card_footer("Conclusión: El modelo identifica con éxito el 'Feed', pero confunde los estados de superficie por su similitud física.")
              )
            )
  ),
  
  # PESTAÑA 5: SVM
  nav_panel("SVM",
            navset_card_underline(
              nav_panel("Lineal",
                        card(
                          card_header("Modelo SVM Lineal (Acierto: 65.58%)"),
                          plotOutput("plot_svm_lin"),
                          card_footer("Conclusión: La frontera recta es insuficiente para capturar la nube de datos. Se observa una rigidez que impide separar correctamente el descanso del viaje superficial, limitando la precisión.")
                        )
              ),
              nav_panel("Radial",
                        card(
                          card_header("Modelo SVM Radial (Acierto: 78.15%)"),
                          plotOutput("plot_svm_rad"),
                          card_footer("Conclusión: El Kernel Radial permite 'rodear' los comportamientos de alimentación y buceo profundo con fronteras curvas, logrando rescatar un 13% más de precisión frente al modelo lineal.")
                        )
              )
            )
  )
)


# LÓGICA DEL SERVIDOR (SERVER)
server <- function(input, output) {
  
  datos_r <- reactive({
    res <- df_por_segundos %>% filter(behavior %in% input$behav_select)
    if (input$whale_select != "Todas") res <- res %>% filter(whale_id == input$whale_select)
    res %>% slice_sample(n = min(2500, nrow(res))) 
  })
  
  output$plot_ridge <- renderPlot({
    ggplot(datos_r(), aes(x = Activity_vigor, y = behavior, fill = behavior)) +
      ggridges::geom_density_ridges(alpha = 0.7) +
      scale_fill_viridis_d(option = "plasma") + theme_minimal()
  })
  
  output$plot_box <- renderPlot({
    ggplot(datos_r(), aes(x = behavior, y = Depth_mean, fill = behavior)) +
      geom_boxplot(outlier.color = "red", outlier.alpha = 0.3) +
      theme_minimal() + scale_fill_brewer(palette = "Pastel1")
  })
  
  output$plot_hist <- renderPlot({
    ggplot(datos_r(), aes(x = .data[[input$hist_var]])) +
      geom_histogram(aes(y = ..density..), bins = input$hist_bins, fill = "steelblue", color = "white") +
      geom_density(color = "red", size = 1) + theme_minimal() +
      labs(x = input$hist_var, title = paste("Distribución Global de", input$hist_var))
  })
  
  # LÓGICA: PERFIL DE INMERSIÓN
  output$plot_perfil <- renderPlot({
    df_p <- datos_r() %>% 
      mutate(dist_h = cumsum(Speed_max))
    ggplot(df_p, aes(x = dist_h, y = -Depth_mean, color = behavior)) +
      geom_path(linewidth = 1) + scale_color_viridis_d(option = "plasma") +
      theme_minimal() + labs(x = "Distancia Horizontal Estimada", y = "Profundidad")
  })
  
  # LÓGICA: 3D ESTADOS
  output$plot_3d_estados <- renderPlotly({
    df_3d <- datos_r()
    plot_ly(df_3d, x = ~Activity_vigor, y = ~Depth_mean, z = ~AccZ_mean, 
            color = ~behavior, type = 'scatter3d', mode = 'markers',
            marker = list(size = 3, opacity = 0.6))
  })
  
  # LÓGICA: 3D POSTURA
  output$plot_3d_postura <- renderPlotly({
    df_3d <- datos_r()
    plot_ly(df_3d, x = ~AccX_mean, y = ~AccY_mean, z = ~AccZ_mean, 
            color = ~behavior, type = 'scatter3d', mode = 'markers',
            marker = list(size = 3, opacity = 0.5))
  })
  
  # LÓGICA: 3D TRAYECTORIA
  output$plot_3d_trayectoria <- renderPlotly({
    df_m <- datos_r() %>% arrange(row_number()) %>%
      mutate(vX = cumsum(AccX_mean - mean(AccX_mean)),
             vY = cumsum(AccY_mean - mean(AccY_mean)),
             vZ = cumsum(AccZ_mean - mean(AccZ_mean)),
             pX = cumsum(vX), pY = cumsum(vY), pZ = cumsum(vZ))
    plot_ly(df_m, x = ~pX, y = ~pY, z = ~pZ, color = ~behavior, 
            type = 'scatter3d', mode = 'lines', line = list(width = 4))
  })
  
  output$plot_corr <- renderPlot({
    columnas <- c("AccZ_mean", "Depth_mean", "Speed_max", "Activity_vigor")
    matriz_cor <- cor(df_por_segundos[, columnas], use = "complete.obs")
    ggcorrplot(matriz_cor, hc.order = TRUE, type = "lower", lab = TRUE,
               colors = c("#E46726", "white", "#6D9EC1"))
  })
  
  output$plot_pairs <- renderPlot({
    columnas <- c("AccZ_mean", "Depth_mean", "Speed_max", "Activity_vigor")
    ggpairs(df_por_segundos %>% slice_sample(n = 1000), columns = columnas,
            aes(color = behavior, alpha = 0.5)) + theme_minimal()
  })
  
  output$plot_biv <- renderGirafe({
    p <- ggplot(datos_r(), aes(x = Depth_mean, y = Activity_vigor, color = behavior)) +
      geom_point_interactive(aes(tooltip = whale_id, data_id = whale_id), alpha = 0.5) +
      geom_smooth(method = "lm", se = FALSE) + theme_minimal()
    girafe(ggobj = p)
  })
  
  output$plot_pca <- renderGirafe({
    p <- ggplot(datos_r(), aes(x = PC1, y = PC2, color = behavior)) +
      geom_point_interactive(aes(tooltip = behavior), alpha = 0.6) +
      scale_color_viridis_d(option = "D") + theme_minimal()
    girafe(ggobj = p)
  })
  
  output$plot_cluster <- renderPlot({
    ggplot(datos_r(), aes(x = PC1, y = PC2, color = cluster)) +
      geom_point(alpha = 0.5) + theme_minimal() + scale_color_viridis_d(option = "turbo")
  })
  
  output$table_cluster <- renderTable({ table(df_por_segundos$behavior, df_por_segundos$cluster) }, rownames = TRUE)
  
  output$plot_lda_imp <- renderPlot({
    ggplot(importancia, aes(x = reorder(Variable, Coeficiente), y = Coeficiente, fill = Coeficiente)) +
      geom_bar(stat = "identity") + coord_flip() + theme_minimal()
  })
  
  output$table_lda <- renderTable({ table(Original = df_por_segundos$behavior, Predicho = predict(modelo_lda)$class) }, rownames = TRUE)
  
  grid_reactivo <- reactive({
    rango_depth <- range(df_por_segundos$Depth_mean)
    rango_vigor <- range(df_por_segundos$Activity_vigor)
    expand.grid(
      Depth_mean = seq(rango_depth[1], rango_depth[2], length = 100),
      Activity_vigor = seq(rango_vigor[1], rango_vigor[2], length = 100),
      AccZ_mean = mean(df_por_segundos$AccZ_mean),
      Speed_max = mean(df_por_segundos$Speed_max)
    )
  })
  
  output$plot_svm_lin <- renderPlot({
    grid <- grid_reactivo()
    grid$behavior <- predict(modelo_svm_lin, grid)
    pesos <- t(modelo_svm_lin$coefs) %*% modelo_svm_lin$SV
    w_depth <- pesos[2]; w_vigor <- pesos[4]; intercepto <- modelo_svm_lin$rho
    ggplot() +
      geom_point(data = grid, aes(x = Depth_mean, y = Activity_vigor, color = behavior), 
                 size = 0.8, alpha = 0.1) +
      geom_point(data = datos_r(), aes(x = Depth_mean, y = Activity_vigor, color = behavior), size = 1.2) +
      geom_abline(intercept = intercepto/w_vigor, slope = -w_depth/w_vigor, linewidth = 1) +
      scale_color_viridis_d(option = "plasma") + theme_minimal() +
      labs(x = "Profundidad Media (m)", y = "Vigor de Actividad")
  })
  
  output$plot_svm_rad <- renderPlot({
    grid <- grid_reactivo()
    grid$behavior <- predict(modelo_svm_rad, grid)
    ggplot() +
      geom_point(data = grid, aes(x = Depth_mean, y = Activity_vigor, color = behavior), 
                 size = 0.8, alpha = 0.1) +
      geom_point(data = datos_r(), aes(x = Depth_mean, y = Activity_vigor, color = behavior), size = 1.2) +
      scale_color_viridis_d(option = "plasma") + theme_minimal() +
      labs(x = "Profundidad Media (m)", y = "Vigor de Actividad")
  })
}

shinyApp(ui, server)

