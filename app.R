# ==========================================
# APLICACIÓN SHINY - ANÁLISIS MULTIVARIANTE
# ==========================================

# 1. Cargar librerías necesarias
library(shiny)
library(tidyverse)
library(ggbiplot)
library(ggridges)
library(bslib) # Para darle un tema profesional y bonito

# 2. Cargar los datos preprocesados
# Asegúrate de que 'datos_app.RData' esté en la misma carpeta que este script
load("datos_app.RData")

# Variables numéricas disponibles para elegir
variables_num <- c("AccZ_mean", "Depth_mean", "Speed_max", "Activity_vigor")

# ==========================================
# INTERFAZ DE USUARIO (UI)
# ==========================================
ui <- fluidPage(
  # Tema visual moderno (suma puntos en "personalización")
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#2C3E50"),
  
  titlePanel("Dashboard Interactivo: Etograma de Megaptera novaeangliae"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Controles Globales"),
      
      # Selección del tipo de análisis (Requisito: distintos modos de visualización)
      radioButtons("tipo_analisis", "Selecciona el Modo de Visualización:",
                   choices = c("Descriptivo Univariante", 
                               "Descriptivo Bivariante", 
                               "Resultados PCA (Multivariante)"),
                   selected = "Descriptivo Bivariante"),
      
      hr(),
      
      # Controles Dinámicos (solo aparecen según lo que selecciones arriba)
      conditionalPanel(
        condition = "input.tipo_analisis != 'Resultados PCA (Multivariante)'",
        selectInput("var_x", "Seleccionar Variable X:", choices = variables_num, selected = "Depth_mean")
      ),
      
      conditionalPanel(
        condition = "input.tipo_analisis == 'Descriptivo Bivariante'",
        selectInput("var_y", "Seleccionar Variable Y:", choices = variables_num, selected = "Activity_vigor")
      ),
      
      hr(),
      
      # Requisito: Opciones de selección de parámetros por el usuario
      selectInput("filtro_comportamiento", "Filtrar por Comportamiento:",
                  choices = c("Todos", unique(as.character(df_por_segundos$behavior))),
                  selected = "Todos"),
      
      helpText("Nota: El filtro de comportamiento afecta tanto a la gráfica (excepto PCA) como al resumen numérico.")
    ),
    
    mainPanel(
      width = 9,
      # Requisito: Visualización gráfica y numérica
      tabsetPanel(
        tabPanel("📊 Visualización Gráfica", 
                 br(),
                 plotOutput("grafico_principal", height = "550px")
        ),
        tabPanel("🔢 Resumen Numérico", 
                 br(),
                 h4("Estadísticos Descriptivos de la Selección"),
                 tableOutput("tabla_resumen")
        )
      )
    )
  )
)

# ==========================================
# LÓGICA DEL SERVIDOR (SERVER)
# ==========================================
server <- function(input, output, session) {
  
  # 1. Objeto Reactivo: Filtra los datos según el comportamiento seleccionado
  datos_reactivos <- reactive({
    datos <- df_por_segundos
    if (input$filtro_comportamiento != "Todos") {
      datos <- datos %>% filter(behavior == input$filtro_comportamiento)
    }
    return(datos)
  })
  
  # 2. Renderizar el Gráfico Principal
  output$grafico_principal <- renderPlot({
    
    datos <- datos_reactivos()
    
    # MODO 1: Univariante
    if (input$tipo_analisis == "Descriptivo Univariante") {
      ggplot(datos, aes_string(x = input$var_x, fill = "behavior")) +
        geom_density(alpha = 0.7) +
        scale_fill_viridis_d(option = "plasma") +
        theme_minimal(base_size = 14) +
        labs(title = paste("Distribución de", input$var_x),
             y = "Densidad", x = input$var_x)
      
      # MODO 2: Bivariante
    } else if (input$tipo_analisis == "Descriptivo Bivariante") {
      ggplot(datos, aes_string(x = input$var_x, y = input$var_y, color = "behavior")) +
        geom_point(alpha = 0.5) +
        geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
        scale_color_viridis_d(option = "plasma") +
        theme_minimal(base_size = 14) +
        labs(title = paste("Relación entre", input$var_x, "y", input$var_y))
      
      # MODO 3: PCA (Multivariante)
    } else if (input$tipo_analisis == "Resultados PCA (Multivariante)") {
      # Nota: El Biplot usa el modelo PCA original, por lo que muestra todos los datos
      ggbiplot(pca_ballenas, 
               obs.scale = 1, var.scale = 1,
               groups = df_por_segundos$behavior, 
               ellipse = TRUE, circle = FALSE, alpha = 0.4) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") + 
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
        scale_color_viridis_d(option = "D") + 
        theme_minimal(base_size = 14) +
        labs(title = "Proyección de Componentes Principales (Biplot)",
             subtitle = "Nota: El modelo PCA se calcula sobre el dataset completo")
    }
  })
  
  # 3. Renderizar la Tabla de Resumen (Visualización Numérica)
  output$tabla_resumen <- renderTable({
    datos_reactivos() %>%
      group_by(behavior) %>%
      summarise(
        Casos = n(),
        Media_X = mean(get(input$var_x), na.rm = TRUE),
        DesvTip_X = sd(get(input$var_x), na.rm = TRUE)
      ) %>%
      rename(
        !!paste("Media", input$var_x) := Media_X,
        !!paste("DesvTip", input$var_x) := DesvTip_X
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", digits = 3)
}

# 3. Lanzar la app
shinyApp(ui = ui, server = server)
