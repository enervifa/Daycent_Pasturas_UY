library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(utils)
library(zip)
library(RColorBrewer)
#install.packages("viridisLite")
library(viridisLite) 
library(viridis) 
library(DT)
library(tidyr)
library(viridis)
library(readr)
library(readxl)

# Obtener la paleta de colores 'viridis' con 11 colores
mi_paleta_viridis <- viridis(11)
#https://gl-li.github.io/study-notes/shiny_click_on_a_map.html
        # This aims to allow the visualziation of thE analysis of the three micro-catchments at the same time. 
        #
        library(lubridate)
        library(shiny)
        library(ggplot2)
        library(plotly)
        library(readr)
        library(sf)
        library(leaflet)
library(dplyr)
library(ggplot2)
library(glue)
library(leaflet)
library(plotly)
library(sass)
library(shiny)
library(shiny.fluent)
library(shiny.react)
#library(rhino)
#rhino::init("RhinoApplication")
#remotes::install_github("Appsilon/shiny.react")
#remotes::install_github("Appsilon/shiny.fluent")
library(shinydashboard)
library(leaflet)

# header <- dashboardHeader(
#   title = "Modelación de biomasa de pasturas por región agroecologica, Uruguay"
# )

header <-dashboardHeader(
  title = div(
    HTML('<strong><span style="font-size:15px; color:white;">Modelación de biomasa de pasturas por región agroecologica,<br>Uruguay</span></strong>')
  ))


sidebar <- dashboardSidebar(
  
  width = 200,
 # sidebarUserPanel("FPTA 358",
                   #subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")#,
                   # Image file should be in www/ subdir
                   #image = "userimage.png"
                   sidebarMenu(
                     menuItem("Seleccionar region", tabName = "selectRegion", icon = icon("map")),
                     menuItem("Tabla de datos", tabName = "dataTables", icon = icon("table")),
                     menuItem("Performance del modelo", tabName = "analisis", icon = icon("chart-line"))))
# sidebarLayout(
#   sidebarPanel(
#     
#  
# 
# helpText("En Uruguay existe una buena experiencia en la aplicación de modelos de simulación dinámicos (“modelo Century”), y una versión mejorada más detallada del mismo (“modelo Daycent”),
#                para estudiar la productividad de diferentes tipos de pasturas en diferentes tipos de suelos.Los modelos simulan la dinámica de la materia orgánica y los nutrientes de los suelos y 
#                la productividad de pasturas, cultivos anuales y árboles. Son particularmente sensibles a la capacidad de
#                almacenamiento de agua de los suelos, al tipo de pasturas, a las condiciones climáticas y a la tecnología empleada. 
#                
#                Aqui se observa solamente la producción de pasturas
#                por zona agroecológica. Estos resultados son parte una amplia recopilación de datos y experiencia en el modelo (aqui las citas)"),
# helpText(strong("Para obtener una visualización de los resultados simulados y datos observados, haga click en el mapa en la región agroecológica que desea explorar")), 
# helpText("y observe los resultados de lo simulado vs datos observados estacionales (1917-2023) por zona agroeceológica"),
# helpText("Ambos modelos logran simular valores de biomasa seca de pasturas muy cercanos a los observados, siendo el Daycent el que presenta una menor variabilidad. Los datos observados fueron recopilados por el informe de FAO elaborado por BERRETA (cita) y otros datos observados (citas)")
#   ))

body <- dashboardBody(
  
  tabItems(
    tabItem(
      tabName = "selectRegion",
      fluidRow(
        box(
          title = HTML('<span style="font-size:24px;">Resultados de modelación de biomasa de pasturas por región agroecológica en Uruguay</span>'),
          width = NULL, 
          solidHeader = TRUE,
          leafletOutput("map") 
        )
      ),
      helpText(
        HTML('<strong><span style="font-size:15px; color:black;">Para obtener una visualización de los resultados simulados y datos observados,</span></strong>')
      ), 
      helpText(
        HTML('<strong><span style="font-size:15px; color:blue;">haga click en una región agroecológica del mapa y seleccione el resultado de análisis que desea explorar</span></strong>')
      ),
      tabsetPanel(
        tabPanel(
          title = "Percentiles Estacionales 1917-2022",
          fluidRow(
            column(12, 
                   div(style = 'width:1000px; overflow-x: scroll; height:1000px; overflow-y: scroll; padding-top: 100px',
                       plotlyOutput("plot2", width = 950, height = 1000))
            )
          )
        ),
        tabPanel(
          title = "Mensual",
          fluidRow(
            column(12,
                   div(style = 'width:1000px; overflow-x: scroll; height:1000px; overflow-y: scroll; padding-top: 100px',
                       plotlyOutput("plot3", width = 950, height = 900))
            )
          )
        )
      ),
      tabsetPanel(
        helpText("En Uruguay existe una buena experiencia en la aplicación de modelos de simulación dinámicos (“modelo Century”), y una versión mejorada más detallada del mismo (“modelo Daycent”),
                       para estudiar la productividad de diferentes tipos de pasturas en diferentes tipos de suelos.Los modelos simulan la dinámica de la materia orgánica y los nutrientes de los suelos y
                       la productividad de pasturas, cultivos anuales y árboles. Son particularmente sensibles a la capacidad de
                       almacenamiento de agua de los suelos, al tipo de pasturas, a las condiciones climáticas y a la tecnología empleada.

                       Aqui se observa solamente la producción de pasturas en base
                       por zona agroecológica. Estos resultados son parte una amplia recopilación de datos y experiencia en el modelo (aqui las citas). Este trabajo fue liderado y realizado por Walter Baethgen, IRI, Climate School, Columbia University, New York. INIA, FPTA 358, Uruguay"),
        helpText("El modelo Daycent calibrado para Uruguay logra simular valores de biomasa seca de pasturas muy cercanos a los observados. Los datos observados fueron recopilados por el informe de FAO elaborado por BERRETA (agregar cita) y otros datos observados (otras citas?)")
      )
    ),
    
    tabItem(
      tabName = "dataTables",
      helpText(HTML('<strong><span style="font-size:20px; color:blue;">Tabla de datos para filtrar por nombre y ordenar por año.</span></strong>')),
      helpText("Resultados de simulación por estación y datos observados utilizados para el análisis. Escriba el nombre de una Zona Agroeclógica y ordene por año. Los datos observados tienen año 9999 por lo que se ordenarán primero en forma descendente."),
      helpText(HTML('<strong><span style="; color:black;">Zonas Agroecológicas: Areniscas, Basalto Profundo, Basalto Superficial, Colinas y Lomadas del Este, Cristalino Centro, Litoral, Llanense del este, Noreste, Sedimento Sur, Sierras del este</span></strong>')),
      tabPanel(
        "Tabla de datos",
        DT::dataTableOutput("tabla2"),
        textOutput("descripcion_tabla2")
      )
    ),
    
    tabItem(
      tabName = "analisis",
      # Descriptive Information Outside Tabs
      helpText(
        HTML('<strong><span style="font-size:20px; color:blue;">Modelos biogeoquímicos: DAYCENT</span></strong>')
      ),
      helpText("El modelo DAYCENT fue creado por William J. Parton (Colorado State UniÍersity). Es la versión que permite obtener resultados a paso diariodel modelo biogeoquímico CENTURY.
                 DAYCENT simula flujos de C y N entre la atmósfera,
                 la vegetación y el suelo. Considera el
                 el contenido de agua y la temperatura del suelo por capa, 
                 la producción vegetal y la asignación de la producción primaria
                 neta (PPN), la descomposición de la hojarasca, entre otros. Los flujos de C y N 
                 entre los diferentes depósitos de materia orgánica del suelo
                 están controlados por el tamaño de los depósitos, 
                 la relación C/N y el contenido de lignina del material, y
                 los factores abióticos agua/temperatura. 
                 La producción vegetal es función del potencial genético, 
                 la fenología, la disponibilidad de nutrientes, 
                 el estrés hídrico/temperatura y la radiación solar. 
                 La NPP se asigna a los componentes de la planta
                 (p. ej., raíces versus brotes) según el tipo de vegetación, 
                 la fenología y el estrés hídrico/nutricional."),
      helpText(HTML('<strong><span style="font-size:15px; color:blue;">Doble click en la etiqueta del gráfico para visualizar solo una categoría</span></strong>')),
      helpText("Al observar el visualizador y los percentiles asociados, se puede determinar,
                 por ejemplo, conocer la mediana de la producción de biomasa seca de pasturas en kg/ha
                 (Percentil 50), y al mismo tiempo entender la variabilidad y la probabilidad asociada
                 en base al histórico para esa región. Por ejemplo, al planificar basándose en el percentil 75,
                 se consideraría un escenario donde se tiene una probabilidad del 75% de alcanzar o superar
                 cierta cantidad de materia seca. Este enfoque ayuda a gestionar riesgos y expectativas,
                 permitiendo al productor planificar con mayor precisión"),
      helpText(
        strong(
          "Más información sobre el modelo DAYCENT en ", 
          a("https://www.quantitative-plant.org/model/DayCent", 
            href = "https://www.quantitative-plant.org/model/DayCent", 
            target = "_blank"
          )
        )
      ),
      
      # Tabbed Panel for Plots
      tabsetPanel(
        tabPanel(
          "Global por mes",
          fluidRow(
            column(
              12,
              div(
                style = 'width:1200px; overflow-x: scroll; height:800px; overflow-y: scroll; padding-top: 100px;',
                plotlyOutput("plot4", width = 1000, height = 750)
              )
            )
          )
        ),
        tabPanel(
          "Global por estacion & Coneat",
          fluidRow(
            column(
              12,
              div(
                style = 'width:1100px; overflow-x: scroll; height:700px; overflow-y: scroll;',
                plotlyOutput("plot5", width = 1000, height = 600)
              )
            ),
            column(
              12,
              div(
                style = 'width:1100x; overflow-x: scroll; height:400px; overflow-y: scroll;',
                plotlyOutput("plot6", width = 1000, height = 400)
              )
            )
          ),
          helpText("Valores promedio, desvío estándar (stand.dev) y mediana del índice CONEAT para la Zona Agroecológica determinado a partir del mapa CONEAT.*Los valores de CONEAT extraídos de Mapa de índice CONEAT en algunos casos exceden el valore de 100.")
        )
      )
    )
  )
)


# body <- dashboardBody(
#   tabItems(
#     tabItem(tabName = "Seleccionar region",
#   fluidRow(
#     column(width = 9,
#            box(width = NULL, solidHeader = TRUE,
#                leafletOutput("map", height = 500)
#            ))),
#            
#            tabsetPanel(
#                # tabPanel("Percentiles anuales",
#                #          textOutput("selectedShape"),
#                #          textOutput("descripcion"),
#                #          plotlyOutput("plot2", width = "120%")
#                #          #textOutput("descripcion_plot2")
#                # ),
#                tabPanel(title = "Percentiles anuales",
#                         fluidRow(
#                           column(6,div(style='width:1000px; overflow-x: scroll;height:500px;overflow-y: scroll;', #h3("Percentiles de Biomasa seca de pasturas kg/ha por estación simulada y observada")
#                                        plotlyOutput("plot2",width = 950,height = 600))))),
#                tabPanel("Mensual",
#                         fluidRow(
#                           column(6,div(style='width:1000px; overflow-x: scroll;height:500px;overflow-y: scroll;', #h3("Percentiles de Biomasa seca de pasturas kg/ha por estación simulada y observada")
#                                        plotlyOutput("plot3",width = 950,height = 600)))))
#                
#                # textOutput("selectedShape"),
#                #  plotlyOutput("plot3")
#                #textOutput("descripcion_plot3")
#              )),
#   tabItem(tabName = "Tabla de datos",  
#              navlistPanel(
#                
#                "Todas las regiones",
#                tabPanel("Tabla de datos",
#                         DT::dataTableOutput("tabla2"),
#                         textOutput("descripcion_tabla2")))),
#   tabItem(tabName = "Performance del modelo",
#                tabPanel("Simulado vs observado estacional",
#                         fluidRow(
#                           column(6,div(style='width:1000px; overflow-x: scroll;height:500px;overflow-y: scroll;', #h3("Percentiles de Biomasa seca de pasturas kg/ha por estación simulada y observada")
#                                        plotlyOutput("plot4",width = 950,height = 600))))),
#                #plotlyOutput("plot4"),
#                textOutput("descripcion_tabla3")
#              )
#            ))
    

ui <-dashboardPage(
  header,sidebar,
 # dashboardSidebar(disable = FALSE),
  body
)


        # Define server logic
        server <- function(input, output, session) {
          require(gridExtra)
          require(ggplot2)
          require(lubridate)
          require(dplyr)
          require(grid)
          require(plotly)
        
###Mapa para visualizar
 #capa de regiones agroecoloigicas
          uy_nrm_sf <- read_sf("Agroecol por suelos","stu_sub-reg")
          uy_nrm_sf <- sf::st_make_valid(uy_nrm_sf)
          uy_nrm_sf <- st_transform(uy_nrm_sf, crs = 4326)
          # uy_nrm_sf$REG_COD <- ifelse(uy_nrm_sf$REG_COD == "NA", 0, uy_nrm_sf$REG_COD)
          
          # capa de secciones policiales
          uy_nrm_sf2 <- read_sf("SccPol2","secpol")
          uy_nrm_sf2 <- sf::st_make_valid(uy_nrm_sf2)
          uy_nrm_sf2 <- st_transform(uy_nrm_sf2, crs = 4326)
          # plot(uy_nrm_sf2)
          
          labels <- sprintf(
            "<strong>%s</strong><br/>%s</strong><br/>%s Num Zona Agroecologica ",
            uy_nrm_sf$Clase_Mapa,uy_nrm_sf$REGION_N, uy_nrm_sf$REG_COD
          ) %>% lapply(htmltools::HTML)
          #Define bins for color classification
          bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
          
          # Create a color palette based on the bins
         # pal <- colorBin("YlOrRd", domain = uy_nrm_sf$ZONA_AGROE, bins = bins)
          
          uy_nrm_sf <- sf::st_make_valid(uy_nrm_sf)
          
          # # Generate a palette of colors
          # #n_regions <- length(unique(uy_nrm_sf$REGION_N))
          # pal <- brewer.pal(12, "Set3")
          # # Create the plot of polygons
          
         # output$map <- renderPlot({
            # #map<-plot(uy_nrm_sf["REGION_N"])
            # # Generate a palette of colors with more values
            # n_regions <- length(unique(uy_nrm_sf$REGION_N))
            # pal <- viridis_pal(option = "turbo")(n_regions)  # Using 'viridis' palette
            # 
            # Convert the sf object to a data frame
           # uy_nrm_df <- as.data.frame(uy_nrm_sf)
            
          #   # Create a ggplot using geom_sf()
          # Plotmap<- ggplot() +
          #     geom_sf(data = uy_nrm_sf, aes(fill = REGION_N), color = "black", size = 1) +
          #     scale_fill_manual(values = pal) +
          #     labs(fill = "Zona Agroe") +
          #     theme_minimal()
          # 
          # ggplotly(Plotmap)
          # 
          # })
          
###Much better map in leflet but cannot save click coordinates yet
          # Define a color palette based on unique labels
          #pal <- leaflet::colorFactor(palette = "Spectral", domain = uy_nrm_sf$REGION_N)
          custom_palette <- c("grey", turbo(10))
          pal <- leaflet::colorFactor(
            palette = custom_palette,  # Adjust the number of colors as needed
            domain = uy_nrm_sf$REGION_COD
          )
          output$map = renderLeaflet({
            map <- leaflet(uy_nrm_sf) %>%
              addTiles(
                urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
                options = tileOptions(minZoom = 0, maxZoom = 18) # Adjust zoom levels as needed
              ) %>%
              addPolygons(
                data = uy_nrm_sf2,
                fillColor = "transparent",
                weight = 1,
                opacity = 1,
                color = "grey",
                dashArray = "",
                fillOpacity = 0.2
              )

# # Display the map with the second polygon as the base map
            map %>%
              addPolygons(
                data = uy_nrm_sf,
                fillColor = ~pal(Clase_Mapa),
                weight = 2,
                opacity = 1,
                color = "black",
                dashArray = "3",
                fillOpacity = 0.5,
                label = labels,
                layerId = ~REG_COD,
                highlightOptions = highlightOptions(
                  weight = 5,
                  color = "#666",
                  dashArray = "",
                  fillOpacity = 0.2,
                  bringToFront = TRUE
                )
              ) %>%
              addLegend(
                data=uy_nrm_sf,
                pal = pal,
                values = ~Clase_Mapa,
                opacity = 0.7,
                title = "Zona Agroecológica",
                position = "bottomright")
            
              # %>%
              # addLegend(
              #   data=uy_nrm_sf,
              #   pal = pal,
              #   values = ~REG_COD,
              #   opacity = 0.7,
              #   title = "Código Zona Agroecológica",
              #   position = "bottomright"
              # )
          })
          
          # # Function to observe click event and update selected input
          # observeEvent(input$map_shape_click, {
          #   click <- input$map_shape_click
          #   selected_shape <- uy_nrm_sf[uy_nrm_sf$REGION_N == click$id, ]
          #   output$selectedShape <- renderText({
          #     paste("Selected Shape ID:", click$id, "\n")
          #   })
          #   
          # })
          
          # observeEvent(input$map_shape_click, {
          #   click <- input$map_shape_click
          #   
          #   # Check if the clicked ID exists in the dataset
          #   if (click$id %in% uy_nrm_sf$REGION_N) {
          #     selected_shape <- uy_nrm_sf[uy_nrm_sf$REGION_N == click$id, ]
          #     output$selectedShape <- renderText({
          #       paste("Selected Shape ID:", click$id, "\n")
          #     })
          #     # Update selected input here (e.g., updateSelectInput, updateTextInput, etc.)
          #     # For example: updateSelectInput(session, "selected_input_id", selected_shape$column_to_update)
          #   } else {
          #     output$selectedShape <- renderText({
          #       "No region found"
          #     })
          #   }
          # })
          
          clickedRegion <- reactiveVal(NULL)
          
          observeEvent(input$map_shape_click, {
            click <- input$map_shape_click
            # Check if click$id exists and is not NULL
            if (!is.null(click) && !is.null(click$id) && click$id %in% uy_nrm_sf$REG_COD) {
              selected_shape <- uy_nrm_sf[uy_nrm_sf$REG_COD == click$id, ]
              output$selectedShape <- renderText({
                paste("ID Zona Agroecologica seleccionada:", click$id, "\n")
              })
              
              
              clickedRegion(click$id)
              # Update selected input here (e.g., updateSelectInput, updateTextInput, etc.)
              # For example: updateSelectInput(session, "selected_input_id", selected_shape$column_to_update)
            } else {
              output$selectedShape <- renderText({
                "No se encontró una región seleccionada"
              })
              clickedRegion(NULL)
            }
        
          })
#             
          #output$descripcion_plot2<-output$selectedShape
#Luego de hacer click
#Le pide al usuario que haga click y guarda lat y long
# plot after click
          #archivo con datos por mes
        #   daycentmeses <- read_csv("daycentmeses.csv")
           #archivo con datos de modelacion por estacion
          #Le pide al usuario que haga click y guarda lat y long
          # plot after click
          #archivo con datos por mes
          #daycentmeses <- read_csv("daycentmeses.csv")
          #archivo con datos de modelacion por estacion
        #  model_results <- read_csv("model_results2.csv")
          observed_results <- read_csv("observed.csv")
          #instead of reading I am calculating this from daycent_data and century_data where I put all the reuslts together
          #this is 09022024 The reason i did this mannualy is I was provided an excel with daycent ancd centruy data togethers
          #this makes it very hard to read automatically in R, so I tidy them togetehr torread. The output is one value epr month every year.
          
          ##read csv files with all data
         # setwd("C:/Users/Lenovo/Downloads/Shiny_todos/Carbono_WB4_readingfromoutpu/Walter files")
          data_day<- read.csv("daycent_data.csv", stringsAsFactors = FALSE)
          #data_cen <- read.csv("../Walter files/century_data.csv", stringsAsFactors = FALSE)
          #
          #           ### group by month by year to add trimester MS/MES
          data_day_tri <-  data_day%>%
            mutate(Month_Number = case_when(
              month == "ENE" ~ 1,
              month == "FEB" ~ 2,
              month == "MAR" ~ 3,
              month == "ABR" ~ 4,
              month == "MAY" ~ 5,
              month == "JUN" ~ 6,
              month == "JUL" ~ 7,
              month == "AGO" ~ 8,
              month == "SET" ~ 9,
              month == "OCT" ~ 10,
              month == "NOV" ~ 11,
              month == "DIC" ~ 12))%>%#
            slice(-1,)%>%
            mutate( #Adjust year for December
              Year_adj = ifelse(Month_Number == 12, year + 1, year),
              Trimester = case_when(
                Month_Number %in% c(3, 4, 5) ~ "OTONO",    # March, April, May
                Month_Number %in% c(6, 7, 8) ~ "INVIERNO", # June, July, August
                Month_Number %in% c(9, 10, 11) ~ "PRIMAVERA" , # September, October, November
                Month_Number %in% c(12, 1, 2) ~ "VERANO"  # December, January, February
                
              )) %>%
            group_by(ZAgrEc, modelo, Year_adj, Trimester) %>%
            summarise(Total_MS = sum(MS.MES, na.rm = FALSE))%>%
            ungroup()
          #           ##Jjust pointing out the the first year values start in winter as first month DEC is 0, so not adding onlty Jan and Feb here.
          #
          #           # Calculate the percentiles
          percentiles <-  data_day_tri %>%
            filter(Year_adj >= 1917) %>%  ### watch here!
            group_by(ZAgrEc, modelo, Trimester) %>%
            summarise(
              Perc_005 = quantile(Total_MS, 0.05),
              Perc_01 = quantile(Total_MS, 0.10),
              Perc_025 = quantile(Total_MS, 0.25),
              Perc_05 = quantile(Total_MS, 0.50),
              Perc_075 = quantile(Total_MS, 0.75),
              Perc_09 = quantile(Total_MS, 0.90),
              Perc_095 = quantile(Total_MS, 0.95)
            ) %>%
            ungroup()%>%
            rename(Estacion = Trimester,Modelo=modelo)%>%
            select(ZAgrEc, Modelo, Estacion, Perc_005, Perc_01, Perc_025, Perc_05, Perc_075, Perc_09, Perc_095)%>%
            mutate(calculo="dicenefebr")
          #
          # # ### OTO = APR MAY JUN , INV = jul ago set (COMO HICE CON CENTURY)
          # data_day_tri <-  data_day%>%
          #   mutate(Month_Number = case_when(
          #     month == "ENE" ~ 1,
          #     month == "FEB" ~ 2,
          #     month == "MAR" ~ 3,
          #     month == "ABR" ~ 4,
          #     month == "MAY" ~ 5,
          #     month == "JUN" ~ 6,
          #     month == "JUL" ~ 7,
          #     month == "AGO" ~ 8,
          #     month == "SET" ~ 9,
          #     month == "OCT" ~ 10,
          #     month == "NOV" ~ 11,
          #     month == "DIC" ~ 12
          #   ),# Adjust year for December
          #   # Year_adj = ifelse(Month_Number == 12, year + 1, year),
          #   Trimester = case_when(
          #     Month_Number %in% c(1, 2, 3) ~ "VERANO",  # December, January, February
          #     Month_Number %in% c(4, 5, 6) ~ "OTONO",    # March, April, May
          #     Month_Number %in% c(7, 8, 9) ~ "INVIERNO", # June, July, August
          #     Month_Number %in% c(10, 11, 12) ~ "PRIMAVERA"  # September, October, November
          #   )) %>%
          #   slice(-1,)%>%
          #   group_by(ZAgrEc, modelo, year, Trimester) %>%
          #   summarise(Total_MS = sum(MS.MES, na.rm = TRUE))
          # #           ##Jjust pointing out the the first year values start in winter as first month DEC is 0, so not adding onlty Jan and Feb here.
          # #
          # #           # Calculate the percentiles
          # percentiles2 <-  data_day_tri %>%
          #   filter(year >= 1917) %>%  ### watch here!
          #   group_by(ZAgrEc, modelo, Trimester) %>%
          #   summarise(
          #     Perc_005 = quantile(Total_MS, 0.05),
          #     Perc_01 = quantile(Total_MS, 0.10),
          #     Perc_025 = quantile(Total_MS, 0.25),
          #     Perc_05 = quantile(Total_MS, 0.50),
          #     Perc_075 = quantile(Total_MS, 0.75),
          #     Perc_09 = quantile(Total_MS, 0.90),
          #     Perc_095 = quantile(Total_MS, 0.95)
          #   ) %>%
          #   ungroup()%>%
          #   rename(Estacion = Trimester,Modelo=modelo)%>%
          #   select(ZAgrEc, Modelo, Estacion, Perc_005, Perc_01, Perc_025, Perc_05, Perc_075, Perc_09, Perc_095)%>%
          #   mutate(calculo="enfebmar")
          # #
          # 
          # Save the percentiles dataframe to a CSV file
          
       #   write.csv(file ="percentiles_triinyear.csv", percentiles2)
       #   write.csv(file ="percentiles_decenfeb.csv", percentiles)
          ##compare with model_results2.csv that was made using Walter summary tables
          
         # model_results <- read.csv("../model_results2.csv")%>%
        #    mutate(calculo="planillaWB")%>%
         #   select(ZAgrEc, Modelo, Estacion, Perc_005, Perc_01, Perc_025, Perc_05, Perc_075, Perc_09, Perc_095, calculo)
          # Right join percentiles with model_results
         # two_join_1 <-  bind_rows(percentiles, model_results)
         # # Right join percentiles2 with the result of the first join
         # three_final <- bind_rows(percentiles2, two_join_1)#%>%
          #select(-c(X,X.1,X.2,X.3,X.4,X.5,X.6))
          #
          #
          # # Convert Percentiles from wide to long format
          # model_results_region_long <- tidyr::pivot_longer(three_final ,
          #                                                  cols = starts_with("Perc_"),
          #                                                  names_to = "Percentile",
          #                                                  values_to = "Values")%>%
          #   mutate(Combined = paste(Modelo, calculo, sep = "_"))
          
          #
          # # Assuming observed results are also in the same dataframe, you'll extract them similarly
          # # This step might differ depending on your observed data's structure
          #
          #
          # # Create a dataset for the medians
          # model_medians <- model_results_region_long %>%
          #   filter(Percentile == 'Perc_05') %>%
          #   mutate(Median_Values = Values, Median_ID = Combined)
          # #
          # observed_results <- read_csv("observed.csv")
          # # ## left join   observed_results
          # #
          # # # Create a dataset for the medians
          # observed_results <- observed_results%>%
          #   mutate(Combined="observed")
          # #
          # # ### Plot
          # #
          # #
          # #
          # # # Create the plot using ggplot2
          # ## add  legend_position = "bottom"
          # g11 <- ggplot() +
          #   geom_point(data = model_results_region_long %>%
          #                filter(Modelo %in% c("Daycent", "daycent")),
          #              aes(x = Estacion, y = Values, color = Combined),alpha=0.3) + #shape = Percentile
          #   geom_line(data = model_medians%>%
          #               filter(Modelo %in% c("Daycent", "daycent")),
          #             aes(x = Estacion, y = Median_Values, color = Combined, group = Combined), size = 0.5) +
          #   geom_line(data = observed_results,
          #             aes(x = Estacion, y = OBS, color = Modelo,  group = Modelo), size = 0.5) +
          #   scale_x_discrete(labels = c("Otoño", "Invierno", "Primavera", "Verano")) +
          #   scale_color_discrete(name = "Modelo") +
          #   # scale_shape_manual(name = "Percentile", values = c(16, 17, 18, 19, 20, 21, 22)) +
          #   scale_linetype_manual(name = "Median", values = "dashed", labels = "Median") +
          #   facet_wrap(~ ZAgrEc) +
          #   labs(x = "Estacion", y = "Biomasa seca kg/ha",
          #        title = "Percentiles de Biomasa seca de pasturas kg/ha por estación simulada y observada en ZonasAgEc",
          #   ) +
          #   theme_bw( ) +
          #   theme(legend.position = "bottom")
          # 
          # 
          # 
          # 
          # print(g11)
          
          # ggplotly(g11)
          # De este grafico, me quedo con diciembrenerofebrero como verano para daycent!!!!
          
          model_results<-read.csv("percentiles_decenfeb.csv")
          data_day<- read.csv("daycent_data.csv", stringsAsFactors = FALSE)
          trimester_year<-data_day%>%
              mutate(Month_Number = case_when(
              month == "ENE" ~ 1,
              month == "FEB" ~ 2,
              month == "MAR" ~ 3,
              month == "ABR" ~ 4,
              month == "MAY" ~ 5,
              month == "JUN" ~ 6,
              month == "JUL" ~ 7,
              month == "AGO" ~ 8,
              month == "SET" ~ 9,
              month == "OCT" ~ 10,
              month == "NOV" ~ 11,
              month == "DIC" ~ 12))%>%#
            slice(-1,)%>%
            mutate( #Adjust year for December
              Year_adj = ifelse(Month_Number == 12, year + 1, year),
              Trimester = case_when(
                Month_Number %in% c(3, 4, 5) ~ "OTONO",    # March, April, May
                Month_Number %in% c(6, 7, 8) ~ "INVIERNO", # June, July, August
                Month_Number %in% c(9, 10, 11) ~ "PRIMAVERA" , # September, October, November
                Month_Number %in% c(12, 1, 2) ~ "VERANO"  # December, January, February
                
              )) %>%
            group_by(ZAgrEc, modelo, Year_adj, Trimester) %>%
            summarise(Total_MS = sum(MS.MES, na.rm = FALSE))%>%
            ungroup()
          
          data_day_month <-  data_day %>%
            filter(year >= 1917) %>%  ### watch here!
            group_by(ZAgrEc, modelo, month)%>%
            filter(ZAgrEc =="Areniscas") %>% ##add mesN
            mutate(mesN = match(month, c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SET", "OCT", "NOV", "DIC")))
                    
     
          percentiles2month <-  data_day %>%
                        filter(year >= 1917) %>%  ### watch here!
                        group_by(ZAgrEc, modelo, month) %>%
                        summarise(
                          Perc_005 = quantile(MS.MES, 0.05),
                          Perc_01 = quantile(MS.MES, 0.10),
                          Perc_025 = quantile(MS.MES, 0.25),
                          Perc_05 = quantile(MS.MES, 0.50),
                          Perc_075 = quantile(MS.MES, 0.75),
                          Perc_09 = quantile(MS.MES, 0.90),
                          Perc_095 = quantile(MS.MES, 0.95)
                        ) %>%
                        ungroup()%>%##to numbers ENE is 1, FEB is 2
                        mutate(mesN = case_when(
                          month == "ENE" ~ 1,
                          month == "FEB" ~ 2,
                          month == "MAR" ~ 3,
                          month == "ABR" ~ 4,
                          month == "MAY" ~ 5,
                          month == "JUN" ~ 6,
                          month == "JUL" ~ 7,
                          month == "AGO" ~ 8,
                          month == "SET" ~ 9,
                          month == "OCT" ~ 10,
                          month == "NOV" ~ 11,
                          month == "DIC" ~ 12
                        ))%>%
                        rename(Mes=month)%>%
                        select(ZAgrEc, mesN, Mes, Perc_005, Perc_01, Perc_025, Perc_05, Perc_075, Perc_09, Perc_095)
                   
           ##changeing percentile plot
          daycentmeses<- percentiles2month
          #daycentmeses_region_long <- tidyr::pivot_longer(daycentmeses_region, cols = starts_with("Perc"), names_to = "Percentile", values_to = "Values")
          
            output$descripcion <- renderText({
             req(clickedRegion())  # Ensure clickedRegion has a value
             
             clickedID <- clickedRegion()
             
             if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 1) {
               paste("Areniscas")
             }
               else if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 2) {
                 paste("Basalto Superficial")
               }
              else if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 3) {
               paste("Cristalino Centro")
             } else if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 4) {
               paste("Sedimentos Sur")
             }
               else if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 5) {
               paste("Litoral promedio Norte y Sur")
               }
             else if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 6) {
               paste("Llanuras")
             }
             else if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 7) {
               paste("Noreste")
               
             }
             else if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 8) {
               paste("Sierras del este")
               
             } else if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 9) {
               paste("Basalto Profundo")
             
            }
             else if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 10) {
               paste("Col y Lom de E")
             }
      
            })
           
           output$plot2 <- renderPlotly({
             req(clickedRegion())
             clickedID <- clickedRegion()
             if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 1) {
               daycentmeses_region <-daycentmeses%>%
                 filter(ZAgrEc =="Areniscas") 
               
               daycentmeses_region_long <- tidyr::pivot_longer(daycentmeses_region, cols = starts_with("Perc"), names_to = "Percentile", values_to = "Values")
               
               model_results_region <-model_results%>%
                   filter(ZAgrEc =="Areniscas")%>%
                 mutate(Estacion_Num = case_when(
                   Estacion == "OTONO" ~ 1,
                   Estacion == "INVIERNO" ~ 2,
                   Estacion == "PRIMAVERA" ~ 3,
                   Estacion == "VERANO" ~ 4,
                #   TRUE ~ as.numeric(Estacion)  # For other cases, you can assign other numerical values
                 ))
               
               observed_results_region <-observed_results%>%
                 filter(ZAgrEc =="Areniscas")%>%
                 mutate(Estacion_Num = case_when(
                   Estacion == "OTONO" ~ 1,
                   Estacion == "INVIERNO" ~ 2,
                   Estacion == "PRIMAVERA" ~ 3,
                   Estacion == "VERANO" ~ 4,
                 #  TRUE ~ as.numeric(Estacion)  # For other cases, you can assign other numerical values
                 ))
               
              # model_results_region_long <- tidyr::pivot_longer(model_results_region, cols = 4:8, names_to = "Percentile", values_to = "Values")
               model_results_region_long  <- tidyr::pivot_longer(model_results_region ,
                                                                    cols = starts_with("Perc_"),
                                                                    names_to = "Percentile",
                                                                    values_to = "Values")
               
                observed_results_region_long <- tidyr::pivot_longer(observed_results_region, cols = 4, names_to = "Data", values_to = "Values")
               
                 model_medians <- model_results_region_long %>%
                 group_by(Estacion_Num, Modelo) %>%
                  filter(Percentile == 'Perc_05') %>%
                   mutate(Median_Values = Values, Median_ID = Modelo)
               
               g11<-ggplot() +
                 geom_point(data = model_results_region_long, aes(x = Estacion_Num, y = Values, color = Modelo, cat=Percentile)) +
                 geom_line(data = observed_results_region_long, aes(x = Estacion_Num, y = Values, color = Modelo), size = 1) +
                 geom_line(data = model_medians, aes(x = Estacion_Num, y = Median_Values, color = Modelo, linetype = "Median"), size = 1) +
                 labs(x = "Estacion", y = "Values") +
                 scale_x_continuous(labels = c("Otoño", "Invierno", "Primavera", "Verano"), breaks = 1:4) +
                 scale_color_discrete(name = "Modelo") +
                 scale_linetype_manual(name = "Median", values = "dashed", labels = "Median") +
                 labs(x = "Estacion", y = "Biomasa seca kg/ha", title = "Percentiles de Biomasa seca de pasturas kg/ha por estación simulada y observada en Areniscas") +
                 theme_minimal()
               
               all<-full_join(model_results_region_long,observed_results_region_long)
               
               g13 <- ggplot(all, aes(x = Modelo, y = Values, fill = Modelo)) +
                 geom_boxplot() +
                 labs(x = "Medio anual", y = "Biomasa seca kg/ha",caption = "Medio anual") +
                 scale_fill_discrete(name = "Modelo") +
                 theme_minimal()+
                 stat_summary(fun.y = mean, geom = "point",
                              shape = 18, size = 2.5, color = "#FC4E07")
                 
               
                 g12<-ggplot(daycentmeses_region_long , aes(x = mesN, y = Values, color = Percentile)) +
                 geom_line() +
                 labs(x = "Simulado por mes", y = "Biomasa seca simulada kg/ha",title = "Percentiles") +
                 theme_minimal()+
                 scale_x_continuous(breaks = 1:12)
                
                 plotly::subplot(g11,g13,nrows = 2,margin = 0.1)
               # ggplotly(g11)
                 # ggplotly(g12)
                # ggplotly(g13)
              
             } 
             
             else if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 3) {
               req(clickedRegion())
                 daycentmeses_region <-daycentmeses%>%
                   filter(ZAgrEc =="CristalinoCentro")
                 
                 daycentmeses_region_long <- tidyr::pivot_longer(daycentmeses_region, cols = starts_with("Perc"), names_to = "Percentile", values_to = "Values")
                 
                 model_results_region <-model_results%>%
                   filter(ZAgrEc =="CristalinoCentro")%>%
                   mutate(Estacion_Num = case_when(
                     Estacion == "OTONO" ~ 1,
                     Estacion == "INVIERNO" ~ 2,
                     Estacion == "PRIMAVERA" ~ 3,
                     Estacion == "VERANO" ~ 4,
                    # TRUE ~ as.numeric(Estacion)  # For other cases, you can assign other numerical values
                   ))
                 
                 observed_results_region <-observed_results%>%
                   filter(ZAgrEc =="CristalinoCentro")%>%
                   mutate(Estacion_Num = case_when(
                     Estacion == "OTONO" ~ 1,
                     Estacion == "INVIERNO" ~ 2,
                     Estacion == "PRIMAVERA" ~ 3,
                     Estacion == "VERANO" ~ 4,
                    # TRUE ~ as.numeric(Estacion)  # For other cases, you can assign other numerical values
                   ))
                 
                 #model_results_region_long <- tidyr::pivot_longer(model_results_region, cols = 4:8, names_to = "Percentile", values_to = "Values")
                 
                 model_results_region_long  <- tidyr::pivot_longer(model_results_region ,
                                                                   cols = starts_with("Perc_"),
                                                                   names_to = "Percentile",
                                                                   values_to = "Values")
                 
                 observed_results_region_long <- tidyr::pivot_longer(observed_results_region, cols = 4, names_to = "Data", values_to = "Values")
                 
                 model_medians <- model_results_region_long %>%
                   group_by(Estacion_Num, Modelo) %>%
                   filter(Percentile == 'Perc_05') %>%
                   mutate(Median_Values = Values, Median_ID = Modelo)
                 
                 g11<-ggplot() +
                   geom_point(data = model_results_region_long, aes(x = Estacion_Num, y = Values, color = Modelo, cat=Percentile)) +
                   geom_line(data = observed_results_region_long, aes(x = Estacion_Num, y = Values, color = Modelo), size = 1) +
                   geom_line(data = model_medians, aes(x = Estacion_Num, y = Median_Values, color = Modelo, linetype = "Median"), size = 1) +
                   labs(x = "Estacion", y = "Values") +
                   scale_x_continuous(labels = c("Otoño", "Invierno", "Primavera", "Verano"), breaks = 1:4) +
                   scale_color_discrete(name = "Modelo") +
                   scale_linetype_manual(name = "Median", values = "dashed", labels = "Median") +
                   labs(x = "Estacion", y = "Biomasa seca kg/ha", title = "Percentiles de Biomasa seca de pasturas kg/ha por estación simulada y observada Cristalino Centro") +
                   theme_minimal()
                
                 all<-full_join(model_results_region_long,observed_results_region_long)
                 g13 <- ggplot(all, aes(x = Modelo, y = Values, fill = Modelo)) +
                   geom_boxplot() +
                   labs(x = "Medio anual", y = "Biomasa seca kg/ha",caption = "Medio anual") +
                   scale_fill_discrete(name = "Modelo") +
                   theme_minimal()+
                   stat_summary(fun.y = mean, geom = "point",
                                shape = 18, size = 2.5, color = "#FC4E07")
                 
    
                 # ggplotly(g11)
                 # ggplotly(g12)
                 # ggplotly(g13)
                 plotly::subplot(g11,g13,nrows = 2,margin = 0.1)
             }
             
             else if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 9) {
               req(clickedRegion())
               daycentmeses_region <-daycentmeses%>%
                 filter(ZAgrEc =="BasaltoProfundo")
               
               daycentmeses_region_long <- tidyr::pivot_longer(daycentmeses_region, cols = starts_with("Perc"), names_to = "Percentile", values_to = "Values")
               
               model_results_region <-model_results%>%
                 filter(ZAgrEc =="BasaltoProfundo")%>%
                 mutate(Estacion_Num = case_when(
                   Estacion == "OTONO" ~ 1,
                   Estacion == "INVIERNO" ~ 2,
                   Estacion == "PRIMAVERA" ~ 3,
                   Estacion == "VERANO" ~ 4,
                 #  TRUE ~ as.numeric(Estacion)  # For other cases, you can assign other numerical values
                 ))
               
               observed_results_region <-observed_results%>%
                 filter(ZAgrEc =="BasaltoProfundo")%>%
                 mutate(Estacion_Num = case_when(
                   Estacion == "OTONO" ~ 1,
                   Estacion == "INVIERNO" ~ 2,
                   Estacion == "PRIMAVERA" ~ 3,
                   Estacion == "VERANO" ~ 4,
                 #  TRUE ~ as.numeric(Estacion)  # For other cases, you can assign other numerical values
                 ))
               
               #model_results_region_long <- tidyr::pivot_longer(model_results_region, cols = 4:8, names_to = "Percentile", values_to = "Values")
              
               model_results_region_long  <- tidyr::pivot_longer(model_results_region ,
                                                                 cols = starts_with("Perc_"),
                                                                 names_to = "Percentile",
                                                                 values_to = "Values")
               observed_results_region_long <- tidyr::pivot_longer(observed_results_region, cols = 4, names_to = "Data", values_to = "Values")
               
               model_medians <- model_results_region_long %>%
                 group_by(Estacion_Num, Modelo) %>%
                 filter(Percentile == 'Perc_05') %>%
                 mutate(Median_Values = Values, Median_ID = Modelo)
               
               g11<-ggplot() +
                 geom_point(data = model_results_region_long, aes(x = Estacion_Num, y = Values, color = Modelo, cat=Percentile)) +
                 geom_line(data = observed_results_region_long, aes(x = Estacion_Num, y = Values, color = Modelo), size = 1) +
                 geom_line(data = model_medians, aes(x = Estacion_Num, y = Median_Values, color = Modelo, linetype = "Median"), size = 1) +
                 labs(x = "Estacion", y = "Values") +
                 scale_x_continuous(labels = c("Otoño", "Invierno", "Primavera", "Verano"), breaks = 1:4) +
                 scale_color_discrete(name = "Modelo") +
                 scale_linetype_manual(name = "Median", values = "dashed", labels = "Median") +
                 labs(x = "Estacion", y = "Biomasa seca kg/ha", title = "Percentiles de Biomasa seca de pasturas kg/ha por estación simulada y observada Basalto Profundo") +
                 theme_minimal()
               
               all<-full_join(model_results_region_long,observed_results_region_long)
               g13 <- ggplot(all, aes(x = Modelo, y = Values, fill = Modelo)) +
                 geom_boxplot() +
                 labs(x = "Medio anual", y = "Biomasa seca kg/ha",caption = "Medio anual") +
                 scale_fill_discrete(name = "Modelo") +
                 theme_minimal()+
                 stat_summary(fun.y = mean, geom = "point",
                              shape = 18, size = 2.5, color = "#FC4E07")
               
               
               # ggplotly(g11)
               # ggplotly(g12)
               # ggplotly(g13)
               plotly::subplot(g11,g13,nrows = 2,margin = 0.1)
             }
             
             
             else if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 2) {
               req(clickedRegion())
               daycentmeses_region <-daycentmeses%>%
                 filter(ZAgrEc =="BasaltoSuperficial")
               
               daycentmeses_region_long <- tidyr::pivot_longer(daycentmeses_region, cols = starts_with("Perc"), names_to = "Percentile", values_to = "Values")
               
               model_results_region <-model_results%>%
                 filter(ZAgrEc =="BasaltoSuperficial")%>%
                 mutate(Estacion_Num = case_when(
                   Estacion == "OTONO" ~ 1,
                   Estacion == "INVIERNO" ~ 2,
                   Estacion == "PRIMAVERA" ~ 3,
                   Estacion == "VERANO" ~ 4,
                  # TRUE ~ as.numeric(Estacion)  # For other cases, you can assign other numerical values
                 ))
               
               observed_results_region <-observed_results%>%
                 filter(ZAgrEc =="BasaltoSuperficial")%>%
                 mutate(Estacion_Num = case_when(
                   Estacion == "OTONO" ~ 1,
                   Estacion == "INVIERNO" ~ 2,
                   Estacion == "PRIMAVERA" ~ 3,
                   Estacion == "VERANO" ~ 4,
                #   TRUE ~ as.numeric(Estacion)  # For other cases, you can assign other numerical values
                 ))
               
               #model_results_region_long <- tidyr::pivot_longer(model_results_region, cols = 4:8, names_to = "Percentile", values_to = "Values")
               
               model_results_region_long  <- tidyr::pivot_longer(model_results_region ,
                                                                 cols = starts_with("Perc_"),
                                                                 names_to = "Percentile",
                                                                 values_to = "Values")
               
               observed_results_region_long <- tidyr::pivot_longer(observed_results_region, cols = 4, names_to = "Data", values_to = "Values")
               
               model_medians <- model_results_region_long %>%
                 group_by(Estacion_Num, Modelo) %>%
                 filter(Percentile == 'Perc_05') %>%
                 mutate(Median_Values = Values, Median_ID = Modelo)
               g11<-ggplot() +
                 geom_point(data = model_results_region_long, aes(x = Estacion_Num, y = Values, color = Modelo, cat=Percentile)) +
                 geom_line(data = observed_results_region_long, aes(x = Estacion_Num, y = Values, color = Modelo), size = 1) +
                 geom_line(data = model_medians, aes(x = Estacion_Num, y = Median_Values, color = Modelo, linetype = "Median"), size = 1) +
                 labs(x = "Estacion", y = "Values") +
                 scale_x_continuous(labels = c("Otoño", "Invierno", "Primavera", "Verano"), breaks = 1:4) +
                 scale_color_discrete(name = "Modelo") +
                 scale_linetype_manual(name = "Median", values = "dashed", labels = "Median") +
                 labs(x = "Estacion", y = "Biomasa seca kg/ha", title = "Percentiles de Biomasa seca de pasturas kg/ha por estación simulada y observada Basalto Superficial") +
                 theme_minimal()
               
               all<-full_join(model_results_region_long,observed_results_region_long)
               g13 <- ggplot(all, aes(x = Modelo, y = Values, fill = Modelo)) +
                 geom_boxplot() +
                 labs(x = "Medio anual", y = "Biomasa seca kg/ha",caption = "Medio anual") +
                 scale_fill_discrete(name = "Modelo") +
                 theme_minimal()+
                 stat_summary(fun.y = mean, geom = "point",
                              shape = 18, size = 2.5, color = "#FC4E07")
               
               
               # ggplotly(g11)
               # ggplotly(g12)
               # ggplotly(g13)
               plotly::subplot(g11,g13,nrows = 2,margin = 0.1)
             }
             
             else if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 5) {
               req(clickedRegion())
               daycentmeses_region <-daycentmeses%>%
                 filter(ZAgrEc =="Litoral")
               
               daycentmeses_region_long <- tidyr::pivot_longer(daycentmeses_region, cols = starts_with("Perc"), names_to = "Percentile", values_to = "Values")
               
               model_results_region <-model_results%>%
                 filter(ZAgrEc =="Litoral")%>%
                 mutate(Estacion_Num = case_when(
                   Estacion == "OTONO" ~ 1,
                   Estacion == "INVIERNO" ~ 2,
                   Estacion == "PRIMAVERA" ~ 3,
                   Estacion == "VERANO" ~ 4,
                #   TRUE ~ as.numeric(Estacion)  # For other cases, you can assign other numerical values
                 ))
               
               observed_results_region <-observed_results%>%
                 filter(ZAgrEc =="Litoral")%>%
                 mutate(Estacion_Num = case_when(
                   Estacion == "OTONO" ~ 1,
                   Estacion == "INVIERNO" ~ 2,
                   Estacion == "PRIMAVERA" ~ 3,
                   Estacion == "VERANO" ~ 4,
                 #  TRUE ~ as.numeric(Estacion)  # For other cases, you can assign other numerical values
                 ))
               
              # model_results_region_long <- tidyr::pivot_longer(model_results_region, cols = 4:8, names_to = "Percentile", values_to = "Values")
              
               
               model_results_region_long  <- tidyr::pivot_longer(model_results_region ,
                                                                 cols = starts_with("Perc_"),
                                                                 names_to = "Percentile",
                                                                 values_to = "Values")
               observed_results_region_long <- tidyr::pivot_longer(observed_results_region, cols = 4, names_to = "Data", values_to = "Values")
               
               model_medians <- model_results_region_long %>%
                 group_by(Estacion_Num, Modelo) %>%
                 filter(Percentile == 'Perc_05') %>%
                 mutate(Median_Values = Values, Median_ID = Modelo)
               g11<-ggplot() +
                 geom_point(data = model_results_region_long, aes(x = Estacion_Num, y = Values, color = Modelo, cat=Percentile)) +
                 geom_line(data = observed_results_region_long, aes(x = Estacion_Num, y = Values, color = Modelo), size = 1) +
                 geom_line(data = model_medians, aes(x = Estacion_Num, y = Median_Values, color = Modelo, linetype = "Median"), size = 1) +
                 labs(x = "Estacion", y = "Values") +
                 scale_x_continuous(labels = c("Otoño", "Invierno", "Primavera", "Verano"), breaks = 1:4) +
                 scale_color_discrete(name = "Modelo") +
                 scale_linetype_manual(name = "Median", values = "dashed", labels = "Median") +
                 labs(x = "Estacion", y = "Biomasa seca kg/ha", title = "Percentiles de Biomasa seca de pasturas kg/ha por estación simulada y observada Litoral") +
                 theme_minimal()
               
               all<-full_join(model_results_region_long,observed_results_region_long)
               g13 <- ggplot(all, aes(x = Modelo, y = Values, fill = Modelo)) +
                 geom_boxplot() +
                 labs(x = "Medio anual", y = "Biomasa seca kg/ha",caption = "Medio anual") + #"Boxplots Biomasa seca de pasturas kg/ha por estación simulada"
                 scale_fill_discrete(name = "Modelo") +
                 theme_minimal()+
                 stat_summary(fun.y = mean, geom = "point",
                              shape = 18, size = 2.5, color = "#FC4E07")
               
               
               # ggplotly(g11)
               # ggplotly(g12)
               # ggplotly(g13)
               plotly::subplot(g11,g13,nrows = 2,margin = 0.1)
             }
             
             else if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 6) {
               req(clickedRegion())
               daycentmeses_region <-daycentmeses%>%
                 filter(ZAgrEc =="Llanuras")
               
               daycentmeses_region_long <- tidyr::pivot_longer(daycentmeses_region, cols = starts_with("Perc"), names_to = "Percentile", values_to = "Values")
               
               model_results_region <-model_results%>%
                 filter(ZAgrEc =="Llanensedeleste")%>%
                 mutate(Estacion_Num = case_when(
                   Estacion == "OTONO" ~ 1,
                   Estacion == "INVIERNO" ~ 2,
                   Estacion == "PRIMAVERA" ~ 3,
                   Estacion == "VERANO" ~ 4,
                #   TRUE ~ as.numeric(Estacion)  # For other cases, you can assign other numerical values
                 ))
               
               observed_results_region <-observed_results%>%
                 filter(ZAgrEc =="Llanensedeleste")%>%
                 mutate(Estacion_Num = case_when(
                   Estacion == "OTONO" ~ 1,
                   Estacion == "INVIERNO" ~ 2,
                   Estacion == "PRIMAVERA" ~ 3,
                   Estacion == "VERANO" ~ 4,
               #    TRUE ~ as.numeric(Estacion)  # For other cases, you can assign other numerical values
                 ))
               
              # model_results_region_long <- tidyr::pivot_longer(model_results_region, cols = 4:8, names_to = "Percentile", values_to = "Values")
              
               model_results_region_long  <- tidyr::pivot_longer(model_results_region ,
                                                                 cols = starts_with("Perc_"),
                                                                 names_to = "Percentile",
                                                                 values_to = "Values")
                observed_results_region_long <- tidyr::pivot_longer(observed_results_region, cols = 4, names_to = "Data", values_to = "Values")
               
               model_medians <- model_results_region_long %>%
                 group_by(Estacion_Num, Modelo) %>%
                 filter(Percentile == 'Perc_05') %>%
                 mutate(Median_Values = Values, Median_ID = Modelo)
               
               g11<-ggplot() +
                 geom_point(data = model_results_region_long, aes(x = Estacion_Num, y = Values, color = Modelo, cat=Percentile)) +
                 geom_line(data = observed_results_region_long, aes(x = Estacion_Num, y = Values, color = Modelo), size = 1) +
                 geom_line(data = model_medians, aes(x = Estacion_Num, y = Median_Values, color = Modelo, linetype = "Median"), size = 1) +
                 labs(x = "Estacion", y = "Values") +
                 scale_x_continuous(labels = c("Otoño", "Invierno", "Primavera", "Verano"), breaks = 1:4) +
                 scale_color_discrete(name = "Modelo") +
                 scale_linetype_manual(name = "Median", values = "dashed", labels = "Median") +
                 labs(x = "Estacion", y = "Biomasa seca kg/ha", title = "Percentiles de Biomasa seca de pasturas kg/ha por estación simulada y observada Llanuras") +
                 theme_minimal()
               
               all<-full_join(model_results_region_long,observed_results_region_long)
               g13 <- ggplot(all, aes(x = Modelo, y = Values, fill = Modelo)) +
                 geom_boxplot() +
                 labs(x = "Medio anual", y = "Biomasa seca kg/ha",caption = "Medio anual") +
                 scale_fill_discrete(name = "Modelo") +
                 theme_minimal()+
                 stat_summary(fun.y = mean, geom = "point",
                              shape = 18, size = 2.5, color = "#FC4E07")
               
               
               # ggplotly(g11)
               # ggplotly(g12)
               # ggplotly(g13)
               plotly::subplot(g11,g13,nrows = 2,margin = 0.1)
             }
             
             
             else if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 7) {
               req(clickedRegion())
               daycentmeses_region <-daycentmeses%>%
                 filter(ZAgrEc =="Noreste")
               
               daycentmeses_region_long <- tidyr::pivot_longer(daycentmeses_region, cols = starts_with("Perc"), names_to = "Percentile", values_to = "Values")
               
               model_results_region <-model_results%>%
                 filter(ZAgrEc =="Noreste")%>%
                 mutate(Estacion_Num = case_when(
                   Estacion == "OTONO" ~ 1,
                   Estacion == "INVIERNO" ~ 2,
                   Estacion == "PRIMAVERA" ~ 3,
                   Estacion == "VERANO" ~ 4,
                  # TRUE ~ as.numeric(Estacion)  # For other cases, you can assign other numerical values
                 ))
               
               observed_results_region <-observed_results%>%
                 filter(ZAgrEc =="Noreste")%>%
                 mutate(Estacion_Num = case_when(
                   Estacion == "OTONO" ~ 1,
                   Estacion == "INVIERNO" ~ 2,
                   Estacion == "PRIMAVERA" ~ 3,
                   Estacion == "VERANO" ~ 4,
                  # TRUE ~ as.numeric(Estacion)  # For other cases, you can assign other numerical values
                 ))
               
               #model_results_region_long <- tidyr::pivot_longer(model_results_region, cols = 4:8, names_to = "Percentile", values_to = "Values")
               model_results_region_long  <- tidyr::pivot_longer(model_results_region ,
                                                                 cols = starts_with("Perc_"),
                                                                 names_to = "Percentile",
                                                                 values_to = "Values")
               
                observed_results_region_long <- tidyr::pivot_longer(observed_results_region, cols = 4, names_to = "Data", values_to = "Values")
               
               model_medians <- model_results_region_long %>%
                 group_by(Estacion_Num, Modelo) %>%
                 filter(Percentile == 'Perc_05') %>%
                 mutate(Median_Values = Values, Median_ID = Modelo)
               
               g11<-ggplot() +
                 geom_point(data = model_results_region_long, aes(x = Estacion_Num, y = Values, color = Modelo, cat=Percentile)) +
                 geom_line(data = observed_results_region_long, aes(x = Estacion_Num, y = Values, color = Modelo), size = 1) +
                 geom_line(data = model_medians, aes(x = Estacion_Num, y = Median_Values, color = Modelo, linetype = "Median"), size = 1) +
                 labs(x = "Estacion", y = "Values") +
                 scale_x_continuous(labels = c("Otoño", "Invierno", "Primavera", "Verano"), breaks = 1:4) +
                 scale_color_discrete(name = "Modelo") +
                 scale_linetype_manual(name = "Median", values = "dashed", labels = "Median") +
                 labs(x = "Estacion", y = "Biomasa seca kg/ha", title = "Percentiles de Biomasa seca de pasturas kg/ha por estación simulada y observada Noreste") +
                 theme_minimal()
               
               all<-full_join(model_results_region_long,observed_results_region_long)
               g13 <- ggplot(all, aes(x = Modelo, y = Values, fill = Modelo)) +
                 geom_boxplot() +
                 labs(x = "Medio anual", y = "Biomasa seca kg/ha",caption = "Medio anual") +
                 scale_fill_discrete(name = "Modelo") +
                 theme_minimal()+
                 stat_summary(fun.y = mean, geom = "point",
                              shape = 18, size = 2.5, color = "#FC4E07")
               
               
               # ggplotly(g11)
               # ggplotly(g12)
               # ggplotly(g13)
               plotly::subplot(g11,g13,nrows = 2,margin = 0.1)
             }
             
             else if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 4) {
               req(clickedRegion())
               daycentmeses_region <-daycentmeses%>%
                 filter(ZAgrEc =="SedimentoSur")
               
               daycentmeses_region_long <- tidyr::pivot_longer(daycentmeses_region, cols = starts_with("Perc"), names_to = "Percentile", values_to = "Values")
               
               model_results_region <-model_results%>%
                 filter(ZAgrEc =="SedimentoSur")%>%
                 mutate(Estacion_Num = case_when(
                   Estacion == "OTONO" ~ 1,
                   Estacion == "INVIERNO" ~ 2,
                   Estacion == "PRIMAVERA" ~ 3,
                   Estacion == "VERANO" ~ 4,
                 #  TRUE ~ as.numeric(Estacion)  # For other cases, you can assign other numerical values
                 ))
               
               observed_results_region <-observed_results%>%
                 filter(ZAgrEc =="SedimentoSur")%>%
                 mutate(Estacion_Num = case_when(
                   Estacion == "OTONO" ~ 1,
                   Estacion == "INVIERNO" ~ 2,
                   Estacion == "PRIMAVERA" ~ 3,
                   Estacion == "VERANO" ~ 4,
                 #  TRUE ~ as.numeric(Estacion)  # For other cases, you can assign other numerical values
                 ))
               
              # model_results_region_long <- tidyr::pivot_longer(model_results_region, cols = 4:8, names_to = "Percentile", values_to = "Values")
               
               model_results_region_long  <- tidyr::pivot_longer(model_results_region ,
                                                                 cols = starts_with("Perc_"),
                                                                 names_to = "Percentile",
                                                                 values_to = "Values")
               observed_results_region_long <- tidyr::pivot_longer(observed_results_region, cols = 4, names_to = "Data", values_to = "Values")
               
               model_medians <- model_results_region_long %>%
                 group_by(Estacion_Num, Modelo) %>%
                 filter(Percentile == 'Perc_05') %>%
                 mutate(Median_Values = Values, Median_ID = Modelo)
               
               g11<-ggplot() +
                 geom_point(data = model_results_region_long, aes(x = Estacion_Num, y = Values, color = Modelo, cat=Percentile)) +
                 geom_line(data = observed_results_region_long, aes(x = Estacion_Num, y = Values, color = Modelo), size = 1) +
                 geom_line(data = model_medians, aes(x = Estacion_Num, y = Median_Values, color = Modelo, linetype = "Median"), size = 1) +
                 labs(x = "Estacion", y = "Values") +
                 scale_x_continuous(labels = c("Otoño", "Invierno", "Primavera", "Verano"), breaks = 1:4) +
                 scale_color_discrete(name = "Modelo") +
                 scale_linetype_manual(name = "Median", values = "dashed", labels = "Median") +
                 labs(x = "Estacion", y = "Biomasa seca kg/ha", title = "Percentiles de Biomasa seca de pasturas kg/ha por estación simulada y observada Sedimentos Sur") +
                 theme_minimal()
               
               all<-full_join(model_results_region_long,observed_results_region_long)
               g13 <- ggplot(all, aes(x = Modelo, y = Values, fill = Modelo)) +
                 geom_boxplot() +
                 labs(x = "Medio anual", y = "Biomasa seca kg/ha",caption = "Medio anual") +
                 scale_fill_discrete(name = "Modelo") +
                 theme_minimal()+
                 stat_summary(fun.y = mean, geom = "point",
                              shape = 18, size = 2.5, color = "#FC4E07")
               
               
               # ggplotly(g11)
               # ggplotly(g12)
               # ggplotly(g13)
               plotly::subplot(g11,g13,nrows = 2,margin = 0.1)
             }
             
             else if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 8) {
               req(clickedRegion())
               daycentmeses_region <-daycentmeses%>%
                 filter(ZAgrEc =="Sierrasdeleste")
               
               daycentmeses_region_long <- tidyr::pivot_longer(daycentmeses_region, cols = starts_with("Perc"), names_to = "Percentile", values_to = "Values")
               
               model_results_region <-model_results%>%
                 filter(ZAgrEc =="Sierrasdeleste")%>%
                 mutate(Estacion_Num = case_when(
                   Estacion == "OTONO" ~ 1,
                   Estacion == "INVIERNO" ~ 2,
                   Estacion == "PRIMAVERA" ~ 3,
                   Estacion == "VERANO" ~ 4,
               #    TRUE ~ as.numeric(Estacion)  # For other cases, you can assign other numerical values
                 ))
               
               observed_results_region <-observed_results%>%
                 filter(ZAgrEc =="Sierrasdeleste")%>%
                 mutate(Estacion_Num = case_when(
                   Estacion == "OTONO" ~ 1,
                   Estacion == "INVIERNO" ~ 2,
                   Estacion == "PRIMAVERA" ~ 3,
                   Estacion == "VERANO" ~ 4,
               #    TRUE ~ as.numeric(Estacion)  # For other cases, you can assign other numerical values
                 ))
               
               #model_results_region_long <- tidyr::pivot_longer(model_results_region, cols = 4:8, names_to = "Percentile", values_to = "Values")
               
               model_results_region_long  <- tidyr::pivot_longer(model_results_region ,
                                                                 cols = starts_with("Perc_"),
                                                                 names_to = "Percentile",
                                                                 values_to = "Values")
               observed_results_region_long <- tidyr::pivot_longer(observed_results_region, cols = 4, names_to = "Data", values_to = "Values")
               
               model_medians <- model_results_region_long %>%
                 group_by(Estacion_Num, Modelo) %>%
                 filter(Percentile == 'Perc_05') %>%
                 mutate(Median_Values = Values, Median_ID = Modelo)
               
               g11<-ggplot() +
                 geom_point(data = model_results_region_long, aes(x = Estacion_Num, y = Values, color = Modelo, cat=Percentile)) +
                 geom_line(data = observed_results_region_long, aes(x = Estacion_Num, y = Values, color = Modelo), size = 1) +
                 geom_line(data = model_medians, aes(x = Estacion_Num, y = Median_Values, color = Modelo, linetype = "Median"), size = 1) +
                 labs(x = "Estacion", y = "Values") +
                 scale_x_continuous(labels = c("Otoño", "Invierno", "Primavera", "Verano"), breaks = 1:4) +
                 scale_color_discrete(name = "Modelo") +
                 scale_linetype_manual(name = "Median", values = "dashed", labels = "Median") +
                 labs(x = "Estacion", y = "Biomasa seca kg/ha", title = "Percentiles de Biomasa seca de pasturas kg/ha por estación simulada y observada Sierras del Este") +
                 theme_minimal()
               
               all<-full_join(model_results_region_long,observed_results_region_long)
               g13 <- ggplot(all, aes(x = Modelo, y = Values, fill = Modelo)) +
                 geom_boxplot() +
                 labs(x = "Medio anual", y = "Biomasa seca kg/ha",caption = "Medio anual") +
                 scale_fill_discrete(name = "Modelo") +
                 theme_minimal()+
                 stat_summary(fun.y = mean, geom = "point",
                              shape = 18, size = 2.5, color = "#FC4E07")
               
               
               # ggplotly(g11)
               # ggplotly(g12)
               # ggplotly(g13)
               plotly::subplot(g11,g13,nrows = 2,margin = 0.1)
             }
             
             else if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 10) {
               req(clickedRegion())
               daycentmeses_region <-daycentmeses%>%
                 filter(ZAgrEc =="ColyLomdeE")
               
               daycentmeses_region_long <- tidyr::pivot_longer(daycentmeses_region, cols = starts_with("Perc"), names_to = "Percentile", values_to = "Values")
               
               model_results_region <-model_results%>%
                 filter(ZAgrEc =="ColyLomdeE")%>%
                 mutate(Estacion_Num = case_when(
                   Estacion == "OTONO" ~ 1,
                   Estacion == "INVIERNO" ~ 2,
                   Estacion == "PRIMAVERA" ~ 3,
                   Estacion == "VERANO" ~ 4,
                 #  TRUE ~ as.numeric(Estacion)  # For other cases, you can assign other numerical values
                 ))
               
               observed_results_region <-observed_results%>%
                 filter(ZAgrEc =="ColyLomdeE")%>%
                 mutate(Estacion_Num = case_when(
                   Estacion == "OTONO" ~ 1,
                   Estacion == "INVIERNO" ~ 2,
                   Estacion == "PRIMAVERA" ~ 3,
                   Estacion == "VERANO" ~ 4,
                #   TRUE ~ as.numeric(Estacion)  # For other cases, you can assign other numerical values
                 ))
               
               #model_results_region_long <- tidyr::pivot_longer(model_results_region, cols = 4:8, names_to = "Percentile", values_to = "Values")
              
               model_results_region_long  <- tidyr::pivot_longer(model_results_region ,
                                                                 cols = starts_with("Perc_"),
                                                                 names_to = "Percentile",
                                                                 values_to = "Values")
                observed_results_region_long <- tidyr::pivot_longer(observed_results_region, cols = 4, names_to = "Data", values_to = "Values")
               
               model_medians <- model_results_region_long %>%
                 group_by(Estacion_Num, Modelo) %>%
                 filter(Percentile == 'Perc_05') %>%
                 mutate(Median_Values = Values, Median_ID = Modelo)
               
               g11<-ggplot() +
                 geom_point(data = model_results_region_long, aes(x = Estacion_Num, y = Values, color = Modelo, cat=Percentile)) +
                 geom_line(data = observed_results_region_long, aes(x = Estacion_Num, y = Values, color = Modelo), size = 1) +
                 geom_line(data = model_medians, aes(x = Estacion_Num, y = Median_Values, color = Modelo, linetype = "Median"), size = 1) +
                 labs(x = "Estacion", y = "Values") +
                 scale_x_continuous(labels = c("Otoño", "Invierno", "Primavera", "Verano"), breaks = 1:4) +
                 scale_color_discrete(name = "Modelo") +
                 scale_linetype_manual(name = "Median", values = "dashed", labels = "Median") +
                 labs(x = "Estacion", y = "Biomasa seca kg/ha", title = "Percentiles de Biomasa seca de pasturas kg/ha por estación simulada y observada Col y Lom de E") +
                 theme_minimal()
               
               all<-full_join(model_results_region_long,observed_results_region_long)
               g13 <- ggplot(all, aes(x = Modelo, y = Values, fill = Modelo)) +
                 geom_boxplot() +
                 labs(x = "Medio anual", y = "Biomasa seca kg/ha",caption = "Medio anual") +
                 scale_fill_discrete(name = "Modelo") +
                 theme_minimal()+
                 stat_summary(fun.y = mean, geom = "point",
                              shape = 18, size = 2.5, color = "#FC4E07")
               
               
               # ggplotly(g11)
               # ggplotly(g12)
               # ggplotly(g13)
               plotly::subplot(g11,g13,nrows = 2,margin = 0.1)
             }
          })
           
           custom_palette <- RColorBrewer::brewer.pal(length(unique(model_results$ZAgrEc)), "Set3")        ##################################   
           ################################
           
           output$plot3 <- renderPlotly({
             req(clickedRegion())
             clickedID <- clickedRegion()
             if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 1) {
               daycentmeses_region <-daycentmeses%>%
                 filter(ZAgrEc =="Areniscas")
               
               daycentmeses_region_long <- tidyr::pivot_longer(daycentmeses_region, cols = starts_with("Perc"), names_to = "Percentile", values_to = "Values")
               color_palette1 <- setNames(custom_palette, unique(daycentmeses_region_long$Percentile))
               
               model_results_region <-model_results%>%
                 filter(ZAgrEc =="Areniscas")%>%
                 mutate(Estacion_Num = case_when(
                   Estacion == "OTONO" ~ 1,
                   Estacion == "INVIERNO" ~ 2,
                   Estacion == "PRIMAVERA" ~ 3,
                   Estacion == "VERANO" ~ 4,
               #    TRUE ~ as.numeric(Estacion)  # For other cases, you can assign other numerical values
                 ))
               
               observed_results_region <-observed_results%>%
                 filter(ZAgrEc =="Areniscas")%>%
                 mutate(Estacion_Num = case_when(
                   Estacion == "OTONO" ~ 1,
                   Estacion == "INVIERNO" ~ 2,
                   Estacion == "PRIMAVERA" ~ 3,
                   Estacion == "VERANO" ~ 4,
               #    TRUE ~ as.numeric(Estacion)  # For other cases, you can assign other numerical values
                 ))
               
              # model_results_region_long <- tidyr::pivot_longer(model_results_region, cols = 4:8, names_to = "Percentile", values_to = "Values")
               
               model_results_region_long  <- tidyr::pivot_longer(model_results_region ,
                                                                 cols = starts_with("Perc_"),
                                                                 names_to = "Percentile",
                                                                 values_to = "Values")
               
               observed_results_region_long <- tidyr::pivot_longer(observed_results_region, cols = 4, names_to = "Data", values_to = "Values")
               
               model_medians <- model_results_region_long %>%
                 group_by(Estacion_Num, Modelo) %>%
                 filter(Percentile == 'Perc_05') %>%
                 mutate(Median_Values = Values, Median_ID = Modelo)
               
            
               # Usar la paleta de colores 'viridis' en tu gráfico
              #  g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
              #    geom_line(show.legend = FALSE) +
              #    labs(x = "Simulado por mes", y = "Biomasa seca simulada kg/ha") +
              #    labs(x = "Estacion", y = "Biomasa seca kg/ha", title = "Biomasa seca kg/ha simulada simulada por mes y valor medio en punto, debajo percentiles") +
              #    theme_minimal() +
              #    scale_x_continuous(breaks = 1:12) +
              #    scale_color_manual(values = mi_paleta_viridis)
              #  
              # 
              #  g22 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, group = Mes,colour=Mes)) +
              #    geom_boxplot(show.legend = FALSE) +
              #    labs(x = "Mes", y = "Biomasa seca kg/ha") +
              #    theme_minimal() +
              #    stat_summary(fun.y = mean, geom = "point", shape = 18, size = 2.5) +
              #    labs(caption = "Los puntos corresponden a la media simulada del mes") +
              #    scale_x_discrete(labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))
              #  
              # plotly::subplot(g22,g12,nrows = 2, margin = 0.1)
               mean_stats <- data_day_month %>%
                 group_by(mesN) %>%
                 summarise(mean_MS.MES = mean(MS.MES, na.rm = TRUE))
               y_min <- min(data_day_month$MS.MES, na.rm = TRUE)
               y_max <- max(data_day_month$MS.MES, na.rm = TRUE)
               
               # Create the box plot with mean points
               
               box_plot <- plot_ly(
                 data = data_day_month,
                 x = ~mesN,
                 y = ~MS.MES,
                 type = 'box',
                 color = ~'Mes',  # Use mesN to differentiate colors, if desired
                 showlegend = TRUE
               ) %>%
                 add_trace(
                   data = mean_stats,
                   x = ~mesN,
                   y = ~mean_MS.MES,
                   mode = 'markers',
                   type = 'scatter',
                   marker = list(color = 'blue', size = 7, symbol = 'circle'),
                   name = 'Promedio'
                 ) %>%
                 layout(
                   title = list(
                     text = "Distribución de Biomasa seca kg/ha simulada por mes en Areniscas",
                     font = list(size = 14)
                   ),
                   xaxis = list(
                     title = list(text = "Mes", font = list(size = 12)),
                     tickvals = 1:12,
                     ticktext = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"),
                     tickfont = list(size = 10)
                   ),
                   yaxis = list(
                     title = list(text = "Biomasa seca kg/ha", font = list(size = 12)),
                     range = c(y_min, y_max),
                     tickfont = list(size = 10)
                   ),
                   hovermode = "closest",
                   annotations = list(
                     list(
                       text = "Cuartiles calculados por el método lineal, q1 es percentil 25, y q3 es percentil 75",
                       x = 0.5,
                       y = -0.1,
                       showarrow = FALSE,
                       font = list(size = 12),
                       xref = 'paper',
                       yref = 'paper',
                       align = 'center'
                     )
                   )
                 )
               
               
               # g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
               #   geom_line() +
               #   labs(x = "Mes", y = "Biomasa seca kg/ha", title = "Simulated Biomass by Month and Percentile") +
               #   theme_minimal() +
               #   scale_color_manual(values = color_palette, name = "Percentile") +
               #   scale_x_continuous(breaks = 1:12, labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
               #   scale_y_continuous(limits = c(y_min, y_max))+
               #   
               color_palette1 <- setNames(custom_palette, unique(daycentmeses_region_long$Percentile))
               # Create the ggplot
               g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
                 geom_line() +
                 geom_text(
                   data = daycentmeses_region_long %>%
                     group_by(Percentile) %>%
                     filter(mesN == max(mesN)) %>%
                     ungroup(),
                   aes(label = Percentile),
                   vjust = -0.5,
                   hjust = 1.1,
                   color = "darkgrey",
                   size = 2
                 ) +
                 labs(
                   x = "Mes", 
                   y = "Biomasa seca kg/ha", 
                   title = "Distribución de Biomasa seca kg/ha simulada por mes en Areniscas",
                 ) +
                 theme_minimal() +
                 scale_color_manual(values = color_palette1, name = "Percentile") +
                 scale_x_continuous(
                   breaks = 1:12, 
                   labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
                 ) +
                 scale_y_continuous(limits = c(y_min, y_max)) +
                 theme(
                   plot.title = element_text(size = 14),
                   axis.title.x = element_text(size = 12),
                   axis.title.y = element_text(size = 12),
                   axis.text.x = element_text(size = 10),
                   axis.text.y = element_text(size = 10),
                   legend.title = element_text(size = 12),
                   legend.text = element_text(size = 10)
                 )
               
               # Convert to interactive plotly object
               g12_plotly <- ggplotly(g12) %>%
                 layout(
                   title = list(
                     text = "Distribución de Biomasa seca kg/ha simulada por mes en Areniscas",
                     font = list(size = 14)
                   ),
                   xaxis = list(
                     title = list(text = "Mes", font = list(size = 12)),
                     tickfont = list(size = 10)
                   ),
                   yaxis = list(
                     title = list(text = "Biomasa seca kg/ha", font = list(size = 12)),
                     tickfont = list(size = 10)
                   ),
                   legend = list(
                     title = list(text = "Percentile", font = list(size = 12)),
                     font = list(size = 10)
                   ),
                   annotations = list(
                     list(
                       text = "Percentiles para cada mes",
                       x = 0.5,
                       y = -0.1,
                       showarrow = FALSE,
                       font = list(size = 12),
                       xref = 'paper',
                       yref = 'paper',
                       align = 'center'
                     )
                   )
                 )
               
               combined_plot <- subplot(
                 box_plot,  # Plotly box plot
                 g12_plotly, # ggplot2 plot converted to plotly
                 nrows = 2,  # Arrange plots in 2 rows
                 shareX = TRUE,
                 shareY = TRUE
               )
               
               # Display the combined plot
               combined_plot
               
             } 
             
             else if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 9) {
               req(clickedRegion())
               daycentmeses_region <-daycentmeses%>%
                 filter(ZAgrEc =="BasaltoProfundo")
               # 
                daycentmeses_region_long <- tidyr::pivot_longer(daycentmeses_region, cols = starts_with("Perc"), names_to = "Percentile", values_to = "Values")
               # 
               # 
               # # Usar la paleta de colores 'viridis' en tu gráfico
               # g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
               #   geom_line(show.legend = FALSE) +
               #   labs(x = "Estacion", y = "Biomasa seca kg/ha", title = "Biomasa seca kg/ha simulada simulada por mes y valor medio en punto, debajo percentiles") +
               #   theme_minimal() +
               #   scale_x_continuous(breaks = 1:12) +
               #   scale_color_manual(values = mi_paleta_viridis)
               # 
               # 
               # g22 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, group = Mes,colour=Mes)) +
               #   geom_boxplot(show.legend = FALSE) +
               #   labs(x = "Mes", y = "Biomasa seca kg/ha") +
               #   theme_minimal() +
               #   stat_summary(fun.y = mean, geom = "point", shape = 18, size = 2.5) +
               #   labs(caption = "Los puntos corresponden a la media simulada del mes") +
               #   scale_x_discrete(labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))
               # 
               # plotly::subplot(g22,g12,nrows = 2, margin = 0.1)
               
               ## I am using the compllete data to calulate boxplot 
              
               data_day_month <-  data_day %>%
                 filter(year >= 1917) %>%  ### watch here!
                 group_by(ZAgrEc, modelo, month)%>%
                 filter(ZAgrEc =="BasaltoProfundo") %>% ##add mesN
                 mutate(mesN = match(month, c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SET", "OCT", "NOV", "DIC")))%>%
                 mutate(mes_col= as.factor(mesN))
               
                 # summary_stats <- data_day_month %>%
                 # group_by(ZAgrEc, modelo, month) %>%
                 # summarise(
                 #   Median = median(MS.MES, na.rm = TRUE),
                 #   Mean = mean(MS.MES, na.rm = TRUE),
                 #   P05 = quantile(MS.MES, 0.05, na.rm = TRUE),
                 #   P95 = quantile(MS.MES, 0.95, na.rm = TRUE))
                   
                   # Plot
                 # Create the box plot
               mean_stats <- data_day_month %>%
                 group_by(mesN) %>%
                 summarise(mean_MS.MES = mean(MS.MES, na.rm = TRUE))
               y_min <- min(data_day_month$MS.MES, na.rm = TRUE)
               y_max <- max(data_day_month$MS.MES, na.rm = TRUE)
               
               # Create the box plot with mean points
               
               box_plot <- plot_ly(
                 data = data_day_month,
                 x = ~mesN,
                 y = ~MS.MES,
                 type = 'box',
                 color = ~'Mes',  # Use mesN to differentiate colors, if desired
                 showlegend = TRUE
               ) %>%
                 add_trace(
                   data = mean_stats,
                   x = ~mesN,
                   y = ~mean_MS.MES,
                   mode = 'markers',
                   type = 'scatter',
                   marker = list(color = 'blue', size = 7, symbol = 'circle'),
                   name = 'Promedio'
                 ) %>%
                 layout(
                   title = list(
                     text = "Distribución de Biomasa seca kg/ha por mes",
                     font = list(size = 14)
                   ),
                   xaxis = list(
                     title = list(text = "Mes", font = list(size = 12)),
                     tickvals = 1:12,
                     ticktext = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"),
                     tickfont = list(size = 10)
                   ),
                   yaxis = list(
                     title = list(text = "Biomasa seca kg/ha", font = list(size = 12)),
                     range = c(y_min, y_max),
                     tickfont = list(size = 10)
                   ),
                   hovermode = "closest",
                   annotations = list(
                     list(
                       text = "Cuartiles calculados por el método lineal, q1 es percentil 25, y q3 es percentil 75",
                       x = 0.5,
                       y = -0.1,
                       showarrow = FALSE,
                       font = list(size = 12),
                       xref = 'paper',
                       yref = 'paper',
                       align = 'center'
                     )
                   )
                 )
               
               
               # g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
               #   geom_line() +
               #   labs(x = "Mes", y = "Biomasa seca kg/ha", title = "Simulated Biomass by Month and Percentile") +
               #   theme_minimal() +
               #   scale_color_manual(values = color_palette, name = "Percentile") +
               #   scale_x_continuous(breaks = 1:12, labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
               #   scale_y_continuous(limits = c(y_min, y_max))+
               #   
               color_palette1 <- setNames(custom_palette, unique(daycentmeses_region_long$Percentile))
               # Create the ggplot
               g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
                 geom_line() +
                 geom_text(
                   data = daycentmeses_region_long %>%
                     group_by(Percentile) %>%
                     filter(mesN == max(mesN)) %>%
                     ungroup(),
                   aes(label = Percentile),
                   vjust = -0.5,
                   hjust = 1.1,
                   color = "darkgrey",
                   size = 2
                 ) +
                 labs(
                   x = "Mes", 
                   y = "Biomasa seca kg/ha", 
                   title = "Distribución de Biomasa seca kg/ha por mes",
                 ) +
                 theme_minimal() +
                 scale_color_manual(values = color_palette1, name = "Percentile") +
                 scale_x_continuous(
                   breaks = 1:12, 
                   labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
                 ) +
                 scale_y_continuous(limits = c(y_min, y_max)) +
                 theme(
                   plot.title = element_text(size = 14),
                   axis.title.x = element_text(size = 12),
                   axis.title.y = element_text(size = 12),
                   axis.text.x = element_text(size = 10),
                   axis.text.y = element_text(size = 10),
                   legend.title = element_text(size = 12),
                   legend.text = element_text(size = 10)
                 )
               
               # Convert to interactive plotly object
               g12_plotly <- ggplotly(g12) %>%
                 layout(
                   title = list(
                     text = "Distribución de Biomasa seca kg/ha simulada por mes en Basalto Profundo",
                     font = list(size = 14)
                   ),
                   xaxis = list(
                     title = list(text = "Mes", font = list(size = 12)),
                     tickfont = list(size = 10)
                   ),
                   yaxis = list(
                     title = list(text = "Biomasa seca kg/ha", font = list(size = 12)),
                     tickfont = list(size = 10)
                   ),
                   legend = list(
                     title = list(text = "Percentile", font = list(size = 12)),
                     font = list(size = 10)
                   ),
                   annotations = list(
                     list(
                       text = "Percentiles para cada mes",
                       x = 0.5,
                       y = -0.1,
                       showarrow = FALSE,
                       font = list(size = 12),
                       xref = 'paper',
                       yref = 'paper',
                       align = 'center'
                     )
                   )
                 )
               
               combined_plot <- subplot(
                 box_plot,  # Plotly box plot
                 g12_plotly, # ggplot2 plot converted to plotly
                 nrows = 2,  # Arrange plots in 2 rows
                 shareX = TRUE,
                 shareY = TRUE
               )
               
               # Display the combined plot
               combined_plot
             }
             
             else if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 3) {
               req(clickedRegion())
               daycentmeses_region <-daycentmeses%>%
                 filter(ZAgrEc =="CristalinoCentro")
               
                daycentmeses_region_long <- tidyr::pivot_longer(daycentmeses_region, cols = starts_with("Perc"), names_to = "Percentile", values_to = "Values")
               # 
               # 
               # # Usar la paleta de colores 'viridis' en tu gráfico
               # g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
               #   geom_line(show.legend = FALSE) +
               #   labs(x = "Estacion", y = "Biomasa seca kg/ha", title = "Biomasa seca kg/ha simulada simulada por mes y valor medio en punto, debajo percentiles") +
               #   theme_minimal() +
               #   scale_x_continuous(breaks = 1:12) +
               #   scale_color_manual(values = mi_paleta_viridis)
               # 
               # 
               # g22 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, group = Mes,colour=Mes)) +
               #   geom_boxplot(show.legend = FALSE) +
               #   labs(x = "Mes", y = "Biomasa seca kg/ha") +
               #   theme_minimal() +
               #   stat_summary(fun.y = mean, geom = "point", shape = 18, size = 2.5) +
               #   labs(caption = "Los puntos corresponden a la media simulada del mes") +
               #   scale_x_discrete(labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))
               # 
               # plotly::subplot(g22,g12,nrows = 2, margin = 0.1)
               data_day_month <-  data_day %>%
                 filter(year >= 1917) %>%  ### watch here!
                 group_by(ZAgrEc, modelo, month)%>%
                 filter(ZAgrEc =="CristalinoCentro") %>% ##add mesN
                 mutate(mesN = match(month, c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SET", "OCT", "NOV", "DIC")))%>%
                 mutate(mes_col= as.factor(mesN))
               
               # summary_stats <- data_day_month %>%
               # group_by(ZAgrEc, modelo, month) %>%
               # summarise(
               #   Median = median(MS.MES, na.rm = TRUE),
               #   Mean = mean(MS.MES, na.rm = TRUE),
               #   P05 = quantile(MS.MES, 0.05, na.rm = TRUE),
               #   P95 = quantile(MS.MES, 0.95, na.rm = TRUE))
               
               # Plot
               # Create the box plot
               mean_stats <- data_day_month %>%
                 group_by(mesN) %>%
                 summarise(mean_MS.MES = mean(MS.MES, na.rm = TRUE))
               y_min <- min(data_day_month$MS.MES, na.rm = TRUE)
               y_max <- max(data_day_month$MS.MES, na.rm = TRUE)
               
               # Create the box plot with mean points
               
               box_plot <- plot_ly(
                 data = data_day_month,
                 x = ~mesN,
                 y = ~MS.MES,
                 type = 'box',
                 color = ~'Mes',  # Use mesN to differentiate colors, if desired
                 showlegend = TRUE
               ) %>%
                 add_trace(
                   data = mean_stats,
                   x = ~mesN,
                   y = ~mean_MS.MES,
                   mode = 'markers',
                   type = 'scatter',
                   marker = list(color = 'blue', size = 7, symbol = 'circle'),
                   name = 'Promedio'
                 ) %>%
                 layout(
                   title = list(
                     text = "Distribución de Biomasa seca kg/ha por mes",
                     font = list(size = 14)
                   ),
                   xaxis = list(
                     title = list(text = "Mes", font = list(size = 12)),
                     tickvals = 1:12,
                     ticktext = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"),
                     tickfont = list(size = 10)
                   ),
                   yaxis = list(
                     title = list(text = "Biomasa seca kg/ha", font = list(size = 12)),
                     range = c(y_min, y_max),
                     tickfont = list(size = 10)
                   ),
                   hovermode = "closest",
                   annotations = list(
                     list(
                       text = "Cuartiles calculados por el método lineal, q1 es percentil 25, y q3 es percentil 75",
                       x = 0.5,
                       y = -0.1,
                       showarrow = FALSE,
                       font = list(size = 12),
                       xref = 'paper',
                       yref = 'paper',
                       align = 'center'
                     )
                   )
                 )
               
               
               # g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
               #   geom_line() +
               #   labs(x = "Mes", y = "Biomasa seca kg/ha", title = "Simulated Biomass by Month and Percentile") +
               #   theme_minimal() +
               #   scale_color_manual(values = color_palette, name = "Percentile") +
               #   scale_x_continuous(breaks = 1:12, labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
               #   scale_y_continuous(limits = c(y_min, y_max))+
               #   
               color_palette1 <- setNames(custom_palette, unique(daycentmeses_region_long$Percentile))
               # Create the ggplot
               g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
                 geom_line() +
                 geom_text(
                   data = daycentmeses_region_long %>%
                     group_by(Percentile) %>%
                     filter(mesN == max(mesN)) %>%
                     ungroup(),
                   aes(label = Percentile),
                   vjust = -0.5,
                   hjust = 1.1,
                   color = "darkgrey",
                   size = 2
                 ) +
                 labs(
                   x = "Mes", 
                   y = "Biomasa seca kg/ha", 
                   title = "Distribución de Biomasa seca kg/ha por mes",
                 ) +
                 theme_minimal() +
                 scale_color_manual(values = color_palette1, name = "Percentile") +
                 scale_x_continuous(
                   breaks = 1:12, 
                   labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
                 ) +
                 scale_y_continuous(limits = c(y_min, y_max)) +
                 theme(
                   plot.title = element_text(size = 14),
                   axis.title.x = element_text(size = 12),
                   axis.title.y = element_text(size = 12),
                   axis.text.x = element_text(size = 10),
                   axis.text.y = element_text(size = 10),
                   legend.title = element_text(size = 12),
                   legend.text = element_text(size = 10)
                 )
               
               # Convert to interactive plotly object
               g12_plotly <- ggplotly(g12) %>%
                 layout(
                   title = list(
                     text = "Distribución de Biomasa seca kg/ha simulada por mes en Cristalino Centro",
                     font = list(size = 14)
                   ),
                   xaxis = list(
                     title = list(text = "Mes", font = list(size = 12)),
                     tickfont = list(size = 10)
                   ),
                   yaxis = list(
                     title = list(text = "Biomasa seca kg/ha", font = list(size = 12)),
                     tickfont = list(size = 10)
                   ),
                   legend = list(
                     title = list(text = "Percentile", font = list(size = 12)),
                     font = list(size = 10)
                   ),
                   annotations = list(
                     list(
                       text = "Percentiles para cada mes",
                       x = 0.5,
                       y = -0.1,
                       showarrow = FALSE,
                       font = list(size = 12),
                       xref = 'paper',
                       yref = 'paper',
                       align = 'center'
                     )
                   )
                 )
               
               combined_plot <- subplot(
                 box_plot,  # Plotly box plot
                 g12_plotly, # ggplot2 plot converted to plotly
                 nrows = 2,  # Arrange plots in 2 rows
                 shareX = TRUE,
                 shareY = TRUE
               )
               
               # Display the combined plot
               combined_plot
               
             }
             
             else if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 2) {
               req(clickedRegion())
               daycentmeses_region <-daycentmeses%>%
                 filter(ZAgrEc =="BasaltoSuperficial")
               
               daycentmeses_region_long <- tidyr::pivot_longer(daycentmeses_region, cols = starts_with("Perc"), names_to = "Percentile", values_to = "Values")
               # 
               # 
               # # Usar la paleta de colores 'viridis' en tu gráfico
               # g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
               #   geom_line(show.legend = FALSE) +
               #   labs(x = "Estacion", y = "Biomasa seca kg/ha", title = "Biomasa seca kg/ha simulada simulada por mes y valor medio en punto, debajo percentiles") +
               #   theme_minimal() +
               #   scale_x_continuous(breaks = 1:12) +
               #   scale_color_manual(values = mi_paleta_viridis)
               # 
               # 
               # g22 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, group = Mes,colour=Mes)) +
               #   geom_boxplot(show.legend = FALSE) +
               #   labs(x = "Mes", y = "Biomasa seca kg/ha") +
               #   theme_minimal() +
               #   stat_summary(fun.y = mean, geom = "point", shape = 18, size = 2.5) +
               #   labs(caption = "Los puntos corresponden a la media simulada del mes") +
               #   scale_x_discrete(labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))
               # 
               # plotly::subplot(g22,g12,nrows = 2, margin = 0.1)
               
               data_day_month <-  data_day %>%
                 filter(year >= 1917) %>%  ### watch here!
                 group_by(ZAgrEc, modelo, month)%>%
                 filter(ZAgrEc =="BasaltoSuperficial") %>% ##add mesN
                 mutate(mesN = match(month, c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SET", "OCT", "NOV", "DIC")))%>%
                 mutate(mes_col= as.factor(mesN))
               
               # summary_stats <- data_day_month %>%
               # group_by(ZAgrEc, modelo, month) %>%
               # summarise(
               #   Median = median(MS.MES, na.rm = TRUE),
               #   Mean = mean(MS.MES, na.rm = TRUE),
               #   P05 = quantile(MS.MES, 0.05, na.rm = TRUE),
               #   P95 = quantile(MS.MES, 0.95, na.rm = TRUE))
               
               # Plot
               # Create the box plot
               mean_stats <- data_day_month %>%
                 group_by(mesN) %>%
                 summarise(mean_MS.MES = mean(MS.MES, na.rm = TRUE))
               y_min <- min(data_day_month$MS.MES, na.rm = TRUE)
               y_max <- max(data_day_month$MS.MES, na.rm = TRUE)
               
               # Create the box plot with mean points
               
               box_plot <- plot_ly(
                 data = data_day_month,
                 x = ~mesN,
                 y = ~MS.MES,
                 type = 'box',
                 color = ~'Mes',  # Use mesN to differentiate colors, if desired
                 showlegend = TRUE
               ) %>%
                 add_trace(
                   data = mean_stats,
                   x = ~mesN,
                   y = ~mean_MS.MES,
                   mode = 'markers',
                   type = 'scatter',
                   marker = list(color = 'blue', size = 7, symbol = 'circle'),
                   name = 'Promedio'
                 ) %>%
                 layout(
                   title = list(
                     text = "Distribución de Biomasa seca kg/ha por mes",
                     font = list(size = 14)
                   ),
                   xaxis = list(
                     title = list(text = "Mes", font = list(size = 12)),
                     tickvals = 1:12,
                     ticktext = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"),
                     tickfont = list(size = 10)
                   ),
                   yaxis = list(
                     title = list(text = "Biomasa seca kg/ha", font = list(size = 12)),
                     range = c(y_min, y_max),
                     tickfont = list(size = 10)
                   ),
                   hovermode = "closest",
                   annotations = list(
                     list(
                       text = "Cuartiles calculados por el método lineal, q1 es percentil 25, y q3 es percentil 75",
                       x = 0.5,
                       y = -0.1,
                       showarrow = FALSE,
                       font = list(size = 12),
                       xref = 'paper',
                       yref = 'paper',
                       align = 'center'
                     )
                   )
                 )
               
               
               # g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
               #   geom_line() +
               #   labs(x = "Mes", y = "Biomasa seca kg/ha", title = "Simulated Biomass by Month and Percentile") +
               #   theme_minimal() +
               #   scale_color_manual(values = color_palette, name = "Percentile") +
               #   scale_x_continuous(breaks = 1:12, labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
               #   scale_y_continuous(limits = c(y_min, y_max))+
               #   
               color_palette1 <- setNames(custom_palette, unique(daycentmeses_region_long$Percentile))
               # Create the ggplot
               g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
                 geom_line() +
                 geom_text(
                   data = daycentmeses_region_long %>%
                     group_by(Percentile) %>%
                     filter(mesN == max(mesN)) %>%
                     ungroup(),
                   aes(label = Percentile),
                   vjust = -0.5,
                   hjust = 1.1,
                   color = "darkgrey",
                   size = 2
                 ) +
                 labs(
                   x = "Mes", 
                   y = "Biomasa seca kg/ha", 
                   title = "Distribución de Biomasa seca simualda kg/ha por mes en Basalto Superficial",
                 ) +
                 theme_minimal() +
                 scale_color_manual(values = color_palette1, name = "Percentile") +
                 scale_x_continuous(
                   breaks = 1:12, 
                   labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
                 ) +
                 scale_y_continuous(limits = c(y_min, y_max)) +
                 theme(
                   plot.title = element_text(size = 14),
                   axis.title.x = element_text(size = 12),
                   axis.title.y = element_text(size = 12),
                   axis.text.x = element_text(size = 10),
                   axis.text.y = element_text(size = 10),
                   legend.title = element_text(size = 12),
                   legend.text = element_text(size = 10)
                 )
               
               # Convert to interactive plotly object
               g12_plotly <- ggplotly(g12) %>%
                 layout(
                   title = list(
                     text = "Distribución de Biomasa seca simualda kg/ha por mes en Basalto Superficial",
                     font = list(size = 14)
                   ),
                   xaxis = list(
                     title = list(text = "Mes", font = list(size = 12)),
                     tickfont = list(size = 10)
                   ),
                   yaxis = list(
                     title = list(text = "Biomasa seca kg/ha", font = list(size = 12)),
                     tickfont = list(size = 10)
                   ),
                   legend = list(
                     title = list(text = "Percentile", font = list(size = 12)),
                     font = list(size = 10)
                   ),
                   annotations = list(
                     list(
                       text = "Percentiles para cada mes",
                       x = 0.5,
                       y = -0.1,
                       showarrow = FALSE,
                       font = list(size = 12),
                       xref = 'paper',
                       yref = 'paper',
                       align = 'center'
                     )
                   )
                 )
               
               combined_plot <- subplot(
                 box_plot,  # Plotly box plot
                 g12_plotly, # ggplot2 plot converted to plotly
                 nrows = 2,  # Arrange plots in 2 rows
                 shareX = TRUE,
                 shareY = TRUE
               )
               
               # Display the combined plot
               combined_plot
             }
             
             else if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 5) {
               req(clickedRegion())
               daycentmeses_region <-daycentmeses%>%
                 filter(ZAgrEc =="Litoral")
               
                daycentmeses_region_long <- tidyr::pivot_longer(daycentmeses_region, cols = starts_with("Perc"), names_to = "Percentile", values_to = "Values")
             #   
             #   
             #   # Usar la paleta de colores 'viridis' en tu gráfico
             #   g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
             #     geom_line(show.legend = FALSE) +
             #     labs(x = "Estacion", y = "Biomasa seca kg/ha", title = "Biomasa seca kg/ha simulada simulada por mes y valor medio en punto, debajo percentiles") +
             #     theme_minimal() +
             #     scale_x_continuous(breaks = 1:12) +
             #     scale_color_manual(values = mi_paleta_viridis)
             #   
             #   
             #   g22 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, group = Mes,colour=Mes)) +
             #     geom_boxplot(show.legend = FALSE) +
             #     labs(x = "Mes", y = "Biomasa seca kg/ha") +
             #     theme_minimal() +
             #     stat_summary(fun.y = mean, geom = "point", shape = 18, size = 2.5) +
             #     labs(caption = "Los puntos corresponden a la media simulada del mes") +
             #     scale_x_discrete(labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))
             #   
             #   plotly::subplot(g22,g12,nrows = 2, margin = 0.1)
             # }
             # 
             # else if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 6) {
             #   req(clickedRegion())
             #   daycentmeses_region <-daycentmeses%>%
             #     filter(ZAgrEc =="Llanensedeleste")
             #   
             #   daycentmeses_region_long <- tidyr::pivot_longer(daycentmeses_region, cols = starts_with("Perc"), names_to = "Percentile", values_to = "Values")
             #   
             #   
             #   # Usar la paleta de colores 'viridis' en tu gráfico
             #   g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
             #     geom_line(show.legend = FALSE) +
             #     labs(x = "Estacion", y = "Biomasa seca kg/ha", title = "Biomasa seca kg/ha simulada simulada por mes y valor medio en punto, debajo percentiles") +
             #     theme_minimal() +
             #     scale_x_continuous(breaks = 1:12) +
             #     scale_color_manual(values = mi_paleta_viridis)
             #   
             #   
             #   g22 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, group = Mes,colour=Mes)) +
             #     geom_boxplot(show.legend = FALSE) +
             #     labs(x = "Mes", y = "Biomasa seca kg/ha") +
             #     theme_minimal() +
             #     stat_summary(fun.y = mean, geom = "point", shape = 18, size = 2.5) +
             #     labs(caption = "Los puntos corresponden a la media simulada del mes") +
             #     scale_x_discrete(labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))
             #   
             #   plotly::subplot(g22,g12,nrows = 2, margin = 0.1)
               
               data_day_month <-  data_day %>%
                 filter(year >= 1917) %>%  ### watch here!
                 group_by(ZAgrEc, modelo, month)%>%
                 filter(ZAgrEc =="Litoral") %>% ##add mesN
                 mutate(mesN = match(month, c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SET", "OCT", "NOV", "DIC")))%>%
                 mutate(mes_col= as.factor(mesN))
               
               # summary_stats <- data_day_month %>%
               # group_by(ZAgrEc, modelo, month) %>%
               # summarise(
               #   Median = median(MS.MES, na.rm = TRUE),
               #   Mean = mean(MS.MES, na.rm = TRUE),
               #   P05 = quantile(MS.MES, 0.05, na.rm = TRUE),
               #   P95 = quantile(MS.MES, 0.95, na.rm = TRUE))
               
               # Plot
               # Create the box plot
               mean_stats <- data_day_month %>%
                 group_by(mesN) %>%
                 summarise(mean_MS.MES = mean(MS.MES, na.rm = TRUE))
               y_min <- min(data_day_month$MS.MES, na.rm = TRUE)
               y_max <- max(data_day_month$MS.MES, na.rm = TRUE)
               
               # Create the box plot with mean points
               
               box_plot <- plot_ly(
                 data = data_day_month,
                 x = ~mesN,
                 y = ~MS.MES,
                 type = 'box',
                 color = ~'Mes',  # Use mesN to differentiate colors, if desired
                 showlegend = TRUE
               ) %>%
                 add_trace(
                   data = mean_stats,
                   x = ~mesN,
                   y = ~mean_MS.MES,
                   mode = 'markers',
                   type = 'scatter',
                   marker = list(color = 'blue', size = 7, symbol = 'circle'),
                   name = 'Promedio'
                 ) %>%
                 layout(
                   title = list(
                     text = "Distribución de Biomasa seca kg/ha por mes",
                     font = list(size = 14)
                   ),
                   xaxis = list(
                     title = list(text = "Mes", font = list(size = 12)),
                     tickvals = 1:12,
                     ticktext = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"),
                     tickfont = list(size = 10)
                   ),
                   yaxis = list(
                     title = list(text = "Biomasa seca kg/ha", font = list(size = 12)),
                     range = c(y_min, y_max),
                     tickfont = list(size = 10)
                   ),
                   hovermode = "closest",
                   annotations = list(
                     list(
                       text = "Cuartiles calculados por el método lineal, q1 es percentil 25, y q3 es percentil 75",
                       x = 0.5,
                       y = -0.1,
                       showarrow = FALSE,
                       font = list(size = 12),
                       xref = 'paper',
                       yref = 'paper',
                       align = 'center'
                     )
                   )
                 )
               
               
               # g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
               #   geom_line() +
               #   labs(x = "Mes", y = "Biomasa seca kg/ha", title = "Simulated Biomass by Month and Percentile") +
               #   theme_minimal() +
               #   scale_color_manual(values = color_palette, name = "Percentile") +
               #   scale_x_continuous(breaks = 1:12, labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
               #   scale_y_continuous(limits = c(y_min, y_max))+
               #   
               color_palette1 <- setNames(custom_palette, unique(daycentmeses_region_long$Percentile))
               # Create the ggplot
               g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
                 geom_line() +
                 geom_text(
                   data = daycentmeses_region_long %>%
                     group_by(Percentile) %>%
                     filter(mesN == max(mesN)) %>%
                     ungroup(),
                   aes(label = Percentile),
                   vjust = -0.5,
                   hjust = 1.1,
                   color = "darkgrey",
                   size = 2
                 ) +
                 labs(
                   x = "Mes", 
                   y = "Biomasa seca kg/ha", 
                   title = "Distribución de Biomasa seca simulada kg/ha por mes en Litoral",
                 ) +
                 theme_minimal() +
                 scale_color_manual(values = color_palette1, name = "Percentile") +
                 scale_x_continuous(
                   breaks = 1:12, 
                   labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
                 ) +
                 scale_y_continuous(limits = c(y_min, y_max)) +
                 theme(
                   plot.title = element_text(size = 14),
                   axis.title.x = element_text(size = 12),
                   axis.title.y = element_text(size = 12),
                   axis.text.x = element_text(size = 10),
                   axis.text.y = element_text(size = 10),
                   legend.title = element_text(size = 12),
                   legend.text = element_text(size = 10)
                 )
               
               # Convert to interactive plotly object
               g12_plotly <- ggplotly(g12) %>%
                 layout(
                   title = list(
                     text = "Distribución de Biomasa seca simulada kg/ha por mes en Litoral",
                     font = list(size = 14)
                   ),
                   xaxis = list(
                     title = list(text = "Mes", font = list(size = 12)),
                     tickfont = list(size = 10)
                   ),
                   yaxis = list(
                     title = list(text = "Biomasa seca kg/ha", font = list(size = 12)),
                     tickfont = list(size = 10)
                   ),
                   legend = list(
                     title = list(text = "Percentile", font = list(size = 12)),
                     font = list(size = 10)
                   ),
                   annotations = list(
                     list(
                       text = "Percentiles para cada mes",
                       x = 0.5,
                       y = -0.1,
                       showarrow = FALSE,
                       font = list(size = 12),
                       xref = 'paper',
                       yref = 'paper',
                       align = 'center'
                     )
                   )
                 )
               
               combined_plot <- subplot(
                 box_plot,  # Plotly box plot
                 g12_plotly, # ggplot2 plot converted to plotly
                 nrows = 2,  # Arrange plots in 2 rows
                 shareX = TRUE,
                 shareY = TRUE
               )
               
               # Display the combined plot
               combined_plot
             }
             
             else if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 7) {
               req(clickedRegion())
               daycentmeses_region <-daycentmeses%>%
                 filter(ZAgrEc =="Noreste")
               
                daycentmeses_region_long <- tidyr::pivot_longer(daycentmeses_region, cols = starts_with("Perc"), names_to = "Percentile", values_to = "Values")
               # 
               # 
               # # Usar la paleta de colores 'viridis' en tu gráfico
               # g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
               #   geom_line(show.legend = FALSE) +
               #   labs(x = "Estacion", y = "Biomasa seca kg/ha", title = "Biomasa seca kg/ha simulada simulada por mes y valor medio en punto, debajo percentiles") +
               #   theme_minimal() +
               #   scale_x_continuous(breaks = 1:12) +
               #   scale_color_manual(values = mi_paleta_viridis)
               # 
               # 
               # g22 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, group = Mes,colour=Mes)) +
               #   geom_boxplot(show.legend = FALSE) +
               #   labs(x = "Mes", y = "Biomasa seca kg/ha") +
               #   theme_minimal() +
               #   stat_summary(fun.y = mean, geom = "point", shape = 18, size = 2.5) +
               #   labs(caption = "Los puntos corresponden a la media simulada del mes") +
               #   scale_x_discrete(labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))
               # 
               # plotly::subplot(g22,g12,nrows = 2, margin = 0.1)
               data_day_month <-  data_day %>%
                 filter(year >= 1917) %>%  ### watch here!
                 group_by(ZAgrEc, modelo, month)%>%
                 filter(ZAgrEc =="Noreste") %>% ##add mesN
                 mutate(mesN = match(month, c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SET", "OCT", "NOV", "DIC")))%>%
                 mutate(mes_col= as.factor(mesN))
               
               # summary_stats <- data_day_month %>%
               # group_by(ZAgrEc, modelo, month) %>%
               # summarise(
               #   Median = median(MS.MES, na.rm = TRUE),
               #   Mean = mean(MS.MES, na.rm = TRUE),
               #   P05 = quantile(MS.MES, 0.05, na.rm = TRUE),
               #   P95 = quantile(MS.MES, 0.95, na.rm = TRUE))
               
               # Plot
               # Create the box plot
               mean_stats <- data_day_month %>%
                 group_by(mesN) %>%
                 summarise(mean_MS.MES = mean(MS.MES, na.rm = TRUE))
               y_min <- min(data_day_month$MS.MES, na.rm = TRUE)
               y_max <- max(data_day_month$MS.MES, na.rm = TRUE)
               
               # Create the box plot with mean points
               
               box_plot <- plot_ly(
                 data = data_day_month,
                 x = ~mesN,
                 y = ~MS.MES,
                 type = 'box',
                 color = ~'Mes',  # Use mesN to differentiate colors, if desired
                 showlegend = TRUE
               ) %>%
                 add_trace(
                   data = mean_stats,
                   x = ~mesN,
                   y = ~mean_MS.MES,
                   mode = 'markers',
                   type = 'scatter',
                   marker = list(color = 'blue', size = 7, symbol = 'circle'),
                   name = 'Promedio'
                 ) %>%
                 layout(
                   title = list(
                     text = "Distribución de Biomasa seca kg/ha por mes",
                     font = list(size = 14)
                   ),
                   xaxis = list(
                     title = list(text = "Mes", font = list(size = 12)),
                     tickvals = 1:12,
                     ticktext = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"),
                     tickfont = list(size = 10)
                   ),
                   yaxis = list(
                     title = list(text = "Biomasa seca kg/ha", font = list(size = 12)),
                     range = c(y_min, y_max),
                     tickfont = list(size = 10)
                   ),
                   hovermode = "closest",
                   annotations = list(
                     list(
                       text = "Cuartiles calculados por el método lineal, q1 es percentil 25, y q3 es percentil 75",
                       x = 0.5,
                       y = -0.1,
                       showarrow = FALSE,
                       font = list(size = 12),
                       xref = 'paper',
                       yref = 'paper',
                       align = 'center'
                     )
                   )
                 )
               
               
               # g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
               #   geom_line() +
               #   labs(x = "Mes", y = "Biomasa seca kg/ha", title = "Simulated Biomass by Month and Percentile") +
               #   theme_minimal() +
               #   scale_color_manual(values = color_palette, name = "Percentile") +
               #   scale_x_continuous(breaks = 1:12, labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
               #   scale_y_continuous(limits = c(y_min, y_max))+
               #   
               color_palette1 <- setNames(custom_palette, unique(daycentmeses_region_long$Percentile))
               # Create the ggplot
               g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
                 geom_line() +
                 geom_text(
                   data = daycentmeses_region_long %>%
                     group_by(Percentile) %>%
                     filter(mesN == max(mesN)) %>%
                     ungroup(),
                   aes(label = Percentile),
                   vjust = -0.5,
                   hjust = 1.1,
                   color = "darkgrey",
                   size = 2
                 ) +
                 labs(
                   x = "Mes", 
                   y = "Biomasa seca kg/ha", 
                   title = "Distribución de Biomasa seca simualda kg/ha por mes en Noreste",
                 ) +
                 theme_minimal() +
                 scale_color_manual(values = color_palette1, name = "Percentile") +
                 scale_x_continuous(
                   breaks = 1:12, 
                   labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
                 ) +
                 scale_y_continuous(limits = c(y_min, y_max)) +
                 theme(
                   plot.title = element_text(size = 14),
                   axis.title.x = element_text(size = 12),
                   axis.title.y = element_text(size = 12),
                   axis.text.x = element_text(size = 10),
                   axis.text.y = element_text(size = 10),
                   legend.title = element_text(size = 12),
                   legend.text = element_text(size = 10)
                 )
               
               # Convert to interactive plotly object
               g12_plotly <- ggplotly(g12) %>%
                 layout(
                   title = list(
                     text = "Distribución de Biomasa seca simulada kg/ha por mes en Noreste",
                     font = list(size = 14)
                   ),
                   xaxis = list(
                     title = list(text = "Mes", font = list(size = 12)),
                     tickfont = list(size = 10)
                   ),
                   yaxis = list(
                     title = list(text = "Biomasa seca kg/ha", font = list(size = 12)),
                     tickfont = list(size = 10)
                   ),
                   legend = list(
                     title = list(text = "Percentile", font = list(size = 12)),
                     font = list(size = 10)
                   ),
                   annotations = list(
                     list(
                       text = "Percentiles para cada mes",
                       x = 0.5,
                       y = -0.1,
                       showarrow = FALSE,
                       font = list(size = 12),
                       xref = 'paper',
                       yref = 'paper',
                       align = 'center'
                     )
                   )
                 )
               
               combined_plot <- subplot(
                 box_plot,  # Plotly box plot
                 g12_plotly, # ggplot2 plot converted to plotly
                 nrows = 2,  # Arrange plots in 2 rows
                 shareX = TRUE,
                 shareY = TRUE
               )
               
               # Display the combined plot
               combined_plot
             }
             else if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 4) {
               req(clickedRegion())
               daycentmeses_region <-daycentmeses%>%
                 filter(ZAgrEc =="SedimentoSur")
               
                daycentmeses_region_long <- tidyr::pivot_longer(daycentmeses_region, cols = starts_with("Perc"), names_to = "Percentile", values_to = "Values")
               # 
               # 
               # # Usar la paleta de colores 'viridis' en tu gráfico
               # g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
               #   geom_line(show.legend = FALSE) +
               #   labs(x = "Estacion", y = "Biomasa seca kg/ha", title = "Biomasa seca kg/ha simulada simulada por mes y valor medio en punto, debajo percentiles") +
               #   theme_minimal() +
               #   scale_x_continuous(breaks = 1:12) +
               #   scale_color_manual(values = mi_paleta_viridis)
               # 
               # 
               # g22 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, group = Mes,colour=Mes)) +
               #   geom_boxplot(show.legend = FALSE) +
               #   labs(x = "Mes", y = "Biomasa seca kg/ha") +
               #   theme_minimal() +
               #   stat_summary(fun.y = mean, geom = "point", shape = 18, size = 2.5) +
               #   labs(caption = "Los puntos corresponden a la media simulada del mes") +
               #   scale_x_discrete(labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))
               # 
               # plotly::subplot(g22,g12,nrows = 2, margin = 0.1)
               
               data_day_month <-  data_day %>%
                 filter(year >= 1917) %>%  ### watch here!
                 group_by(ZAgrEc, modelo, month)%>%
                 filter(ZAgrEc =="SedimentoSur") %>% ##add mesN
                 mutate(mesN = match(month, c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SET", "OCT", "NOV", "DIC")))%>%
                 mutate(mes_col= as.factor(mesN))
               
               # summary_stats <- data_day_month %>%
               # group_by(ZAgrEc, modelo, month) %>%
               # summarise(
               #   Median = median(MS.MES, na.rm = TRUE),
               #   Mean = mean(MS.MES, na.rm = TRUE),
               #   P05 = quantile(MS.MES, 0.05, na.rm = TRUE),
               #   P95 = quantile(MS.MES, 0.95, na.rm = TRUE))
               
               # Plot
               # Create the box plot
               mean_stats <- data_day_month %>%
                 group_by(mesN) %>%
                 summarise(mean_MS.MES = mean(MS.MES, na.rm = TRUE))
               y_min <- min(data_day_month$MS.MES, na.rm = TRUE)
               y_max <- max(data_day_month$MS.MES, na.rm = TRUE)
               
               # Create the box plot with mean points
               
               box_plot <- plot_ly(
                 data = data_day_month,
                 x = ~mesN,
                 y = ~MS.MES,
                 type = 'box',
                 color = ~'Mes',  # Use mesN to differentiate colors, if desired
                 showlegend = TRUE
               ) %>%
                 add_trace(
                   data = mean_stats,
                   x = ~mesN,
                   y = ~mean_MS.MES,
                   mode = 'markers',
                   type = 'scatter',
                   marker = list(color = 'blue', size = 7, symbol = 'circle'),
                   name = 'Promedio'
                 ) %>%
                 layout(
                   title = list(
                     text = "Distribución de Biomasa seca kg/ha por mes",
                     font = list(size = 14)
                   ),
                   xaxis = list(
                     title = list(text = "Mes", font = list(size = 12)),
                     tickvals = 1:12,
                     ticktext = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"),
                     tickfont = list(size = 10)
                   ),
                   yaxis = list(
                     title = list(text = "Biomasa seca kg/ha", font = list(size = 12)),
                     range = c(y_min, y_max),
                     tickfont = list(size = 10)
                   ),
                   hovermode = "closest",
                   annotations = list(
                     list(
                       text = "Cuartiles calculados por el método lineal, q1 es percentil 25, y q3 es percentil 75",
                       x = 0.5,
                       y = -0.1,
                       showarrow = FALSE,
                       font = list(size = 12),
                       xref = 'paper',
                       yref = 'paper',
                       align = 'center'
                     )
                   )
                 )
               
               
               # g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
               #   geom_line() +
               #   labs(x = "Mes", y = "Biomasa seca kg/ha", title = "Simulated Biomass by Month and Percentile") +
               #   theme_minimal() +
               #   scale_color_manual(values = color_palette, name = "Percentile") +
               #   scale_x_continuous(breaks = 1:12, labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
               #   scale_y_continuous(limits = c(y_min, y_max))+
               #   
               color_palette1 <- setNames(custom_palette, unique(daycentmeses_region_long$Percentile))
               # Create the ggplot
               g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
                 geom_line() +
                 geom_text(
                   data = daycentmeses_region_long %>%
                     group_by(Percentile) %>%
                     filter(mesN == max(mesN)) %>%
                     ungroup(),
                   aes(label = Percentile),
                   vjust = -0.5,
                   hjust = 1.1,
                   color = "darkgrey",
                   size = 2
                 ) +
                 labs(
                   x = "Mes", 
                   y = "Biomasa seca kg/ha", 
                   title = "Distribución de Biomasa seca simulada kg/ha por mes en Sedimento Sur",
                 ) +
                 theme_minimal() +
                 scale_color_manual(values = color_palette1, name = "Percentile") +
                 scale_x_continuous(
                   breaks = 1:12, 
                   labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
                 ) +
                 scale_y_continuous(limits = c(y_min, y_max)) +
                 theme(
                   plot.title = element_text(size = 14),
                   axis.title.x = element_text(size = 12),
                   axis.title.y = element_text(size = 12),
                   axis.text.x = element_text(size = 10),
                   axis.text.y = element_text(size = 10),
                   legend.title = element_text(size = 12),
                   legend.text = element_text(size = 10)
                 )
               
               # Convert to interactive plotly object
               g12_plotly <- ggplotly(g12) %>%
                 layout(
                   title = list(
                     text = "Distribución de Biomasa seca kg/ha simulada por mes en Sedimento Sur",
                     font = list(size = 14)
                   ),
                   xaxis = list(
                     title = list(text = "Mes", font = list(size = 12)),
                     tickfont = list(size = 10)
                   ),
                   yaxis = list(
                     title = list(text = "Biomasa seca kg/ha", font = list(size = 12)),
                     tickfont = list(size = 10)
                   ),
                   legend = list(
                     title = list(text = "Percentile", font = list(size = 12)),
                     font = list(size = 10)
                   ),
                   annotations = list(
                     list(
                       text = "Percentiles para cada mes",
                       x = 0.5,
                       y = -0.1,
                       showarrow = FALSE,
                       font = list(size = 12),
                       xref = 'paper',
                       yref = 'paper',
                       align = 'center'
                     )
                   )
                 )
               
               combined_plot <- subplot(
                 box_plot,  # Plotly box plot
                 g12_plotly, # ggplot2 plot converted to plotly
                 nrows = 2,  # Arrange plots in 2 rows
                 shareX = TRUE,
                 shareY = TRUE
               )
               
               # Display the combined plot
               combined_plot
             }
             
             else if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 8) {
               req(clickedRegion())
               daycentmeses_region <-daycentmeses%>%
                 filter(ZAgrEc =="Sierrasdeleste")
               
                daycentmeses_region_long <- tidyr::pivot_longer(daycentmeses_region, cols = starts_with("Perc"), names_to = "Percentile", values_to = "Values")
               # 
               # 
               # # Usar la paleta de colores 'viridis' en tu gráfico
               # g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
               #   geom_line(show.legend = FALSE) +
               #   labs(x = "Estacion", y = "Biomasa seca kg/ha", title = "Biomasa seca kg/ha simulada simulada por mes y valor medio en punto, debajo percentiles") +
               #   theme_minimal() +
               #   scale_x_continuous(breaks = 1:12) +
               #   scale_color_manual(values = mi_paleta_viridis)
               # 
               # 
               # g22 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, group = Mes,colour=Mes)) +
               #   geom_boxplot(show.legend = FALSE) +
               #   labs(x = "Mes", y = "Biomasa seca kg/ha") +
               #   theme_minimal() +
               #   stat_summary(fun.y = mean, geom = "point", shape = 18, size = 2.5) +
               #   labs(caption = "Los puntos corresponden a la media simulada del mes") +
               #   scale_x_discrete(labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))
               # 
               # plotly::subplot(g22,g12,nrows = 2, margin = 0.1)
               
               data_day_month <-  data_day %>%
                 filter(year >= 1917) %>%  ### watch here!
                 group_by(ZAgrEc, modelo, month)%>%
                 filter(ZAgrEc =="Sierrasdeleste") %>% ##add mesN
                 mutate(mesN = match(month, c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SET", "OCT", "NOV", "DIC")))%>%
                 mutate(mes_col= as.factor(mesN))
               
               # summary_stats <- data_day_month %>%
               # group_by(ZAgrEc, modelo, month) %>%
               # summarise(
               #   Median = median(MS.MES, na.rm = TRUE),
               #   Mean = mean(MS.MES, na.rm = TRUE),
               #   P05 = quantile(MS.MES, 0.05, na.rm = TRUE),
               #   P95 = quantile(MS.MES, 0.95, na.rm = TRUE))
               
               # Plot
               # Create the box plot
               mean_stats <- data_day_month %>%
                 group_by(mesN) %>%
                 summarise(mean_MS.MES = mean(MS.MES, na.rm = TRUE))
              
               y_min <- min(data_day_month$MS.MES, na.rm = TRUE)
               y_max <- max(data_day_month$MS.MES, na.rm = TRUE)
               
               # Create the box plot with mean points
               
               box_plot <- plot_ly(
                 data = data_day_month,
                 x = ~mesN,
                 y = ~MS.MES,
                 type = 'box',
                 color = ~'Mes',  # Use mesN to differentiate colors, if desired
                 showlegend = TRUE
               ) %>%
                 add_trace(
                   data = mean_stats,
                   x = ~mesN,
                   y = ~mean_MS.MES,
                   mode = 'markers',
                   type = 'scatter',
                   marker = list(color = 'blue', size = 7, symbol = 'circle'),
                   name = 'Promedio'
                 ) %>%
                 layout(
                   title = list(
                     text = "Distribución de Biomasa seca kg/ha por mes",
                     font = list(size = 14)
                   ),
                   xaxis = list(
                     title = list(text = "Mes", font = list(size = 12)),
                     tickvals = 1:12,
                     ticktext = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"),
                     tickfont = list(size = 10)
                   ),
                   yaxis = list(
                     title = list(text = "Biomasa seca kg/ha", font = list(size = 12)),
                     range = c(y_min, y_max),
                     tickfont = list(size = 10)
                   ),
                   hovermode = "closest",
                   annotations = list(
                     list(
                       text = "Cuartiles calculados por el método lineal, q1 es percentil 25, y q3 es percentil 75",
                       x = 0.5,
                       y = -0.1,
                       showarrow = FALSE,
                       font = list(size = 12),
                       xref = 'paper',
                       yref = 'paper',
                       align = 'center'
                     )
                   )
                 )
               
               
               # g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
               #   geom_line() +
               #   labs(x = "Mes", y = "Biomasa seca kg/ha", title = "Simulated Biomass by Month and Percentile") +
               #   theme_minimal() +
               #   scale_color_manual(values = color_palette, name = "Percentile") +
               #   scale_x_continuous(breaks = 1:12, labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
               #   scale_y_continuous(limits = c(y_min, y_max))+
               #   
               color_palette1 <- setNames(custom_palette, unique(daycentmeses_region_long$Percentile))
               # Create the ggplot
               g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
                 geom_line() +
                 geom_text(
                   data = daycentmeses_region_long %>%
                     group_by(Percentile) %>%
                     filter(mesN == max(mesN)) %>%
                     ungroup(),
                   aes(label = Percentile),
                   vjust = -0.5,
                   hjust = 1.1,
                   color = "darkgrey",
                   size = 2
                 ) +
                 labs(
                   x = "Mes", 
                   y = "Biomasa seca kg/ha", 
                   title = "Distribución de Biomasa seca simulada kg/ha por mes en Sierras del este",
                 ) +
                 theme_minimal() +
                 scale_color_manual(values = color_palette1, name = "Percentile") +
                 scale_x_continuous(
                   breaks = 1:12, 
                   labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
                 ) +
                 scale_y_continuous(limits = c(y_min, y_max)) +
                 theme(
                   plot.title = element_text(size = 14),
                   axis.title.x = element_text(size = 12),
                   axis.title.y = element_text(size = 12),
                   axis.text.x = element_text(size = 10),
                   axis.text.y = element_text(size = 10),
                   legend.title = element_text(size = 12),
                   legend.text = element_text(size = 10)
                 )
               
               # Convert to interactive plotly object
               g12_plotly <- ggplotly(g12) %>%
                 layout(
                   title = list(
                     text = "Distribución de Biomasa seca simulada kg/ha por mes en Sierras del este",
                     font = list(size = 14)
                   ),
                   xaxis = list(
                     title = list(text = "Mes", font = list(size = 12)),
                     tickfont = list(size = 10)
                   ),
                   yaxis = list(
                     title = list(text = "Biomasa seca kg/ha", font = list(size = 12)),
                     tickfont = list(size = 10)
                   ),
                   legend = list(
                     title = list(text = "Percentile", font = list(size = 12)),
                     font = list(size = 10)
                   ),
                   annotations = list(
                     list(
                       text = "Percentiles para cada mes",
                       x = 0.5,
                       y = -0.1,
                       showarrow = FALSE,
                       font = list(size = 12),
                       xref = 'paper',
                       yref = 'paper',
                       align = 'center'
                     )
                   )
                 )
               
               combined_plot <- subplot(
                 box_plot,  # Plotly box plot
                 g12_plotly, # ggplot2 plot converted to plotly
                 nrows = 2,  # Arrange plots in 2 rows
                 shareX = TRUE,
                 shareY = TRUE
               )
               
               # Display the combined plot
               combined_plot
             }
             else if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 10) {
               req(clickedRegion())
               daycentmeses_region <-daycentmeses%>%
                 filter(ZAgrEc =="ColyLomdeE")
               # 
                daycentmeses_region_long <- tidyr::pivot_longer(daycentmeses_region, cols = starts_with("Perc"), names_to = "Percentile", values_to = "Values")
               # 
               # 
               # # Usar la paleta de colores 'viridis' en tu gráfico
               # g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
               #   geom_line(show.legend = FALSE) +
               #   labs(x = "Estacion", y = "Biomasa seca kg/ha", title = "Biomasa seca kg/ha simulada simulada por mes y valor medio en punto, debajo percentiles") +
               #   theme_minimal() +
               #   scale_x_continuous(breaks = 1:12) +
               #   scale_color_manual(values = mi_paleta_viridis)
               # 
               # 
               # g22 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, group = Mes,colour=Mes)) +
               #   geom_boxplot(show.legend = FALSE) +
               #   labs(x = "Mes", y = "Biomasa seca kg/ha") +
               #   theme_minimal() +
               #   stat_summary(fun.y = mean, geom = "point", shape = 18, size = 2.5) +
               #   labs(caption = "Los puntos corresponden a la media simulada del mes") +
               #   scale_x_discrete(labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))
               # 
               # plotly::subplot(g22,g12,nrows = 2, margin = 0.1)
               
               data_day_month <-  data_day %>%
                 filter(year >= 1917) %>%  ### watch here!
                 group_by(ZAgrEc, modelo, month)%>%
                 filter(ZAgrEc =="ColyLomdeE") %>% ##add mesN
                 mutate(mesN = match(month, c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SET", "OCT", "NOV", "DIC")))%>%
                 mutate(mes_col= as.factor(mesN))
               
               # summary_stats <- data_day_month %>%
               # group_by(ZAgrEc, modelo, month) %>%
               # summarise(
               #   Median = median(MS.MES, na.rm = TRUE),
               #   Mean = mean(MS.MES, na.rm = TRUE),
               #   P05 = quantile(MS.MES, 0.05, na.rm = TRUE),
               #   P95 = quantile(MS.MES, 0.95, na.rm = TRUE))
               
               # Plot
               # Create the box plot
               mean_stats <- data_day_month %>%
                 group_by(mesN) %>%
                 summarise(mean_MS.MES = mean(MS.MES, na.rm = TRUE))
               
               y_min <- min(data_day_month$MS.MES, na.rm = TRUE)
               y_max <- max(data_day_month$MS.MES, na.rm = TRUE)
               
               # Create the box plot with mean points
               
               box_plot <- plot_ly(
                 data = data_day_month,
                 x = ~mesN,
                 y = ~MS.MES,
                 type = 'box',
                 color = ~'Mes',  # Use mesN to differentiate colors, if desired
                 showlegend = TRUE
               ) %>%
                 add_trace(
                   data = mean_stats,
                   x = ~mesN,
                   y = ~mean_MS.MES,
                   mode = 'markers',
                   type = 'scatter',
                   marker = list(color = 'blue', size = 7, symbol = 'circle'),
                   name = 'Promedio'
                 ) %>%
                 layout(
                   title = list(
                     text = "Distribución de Biomasa seca kg/ha por mes",
                     font = list(size = 14)
                   ),
                   xaxis = list(
                     title = list(text = "Mes", font = list(size = 12)),
                     tickvals = 1:12,
                     ticktext = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"),
                     tickfont = list(size = 10)
                   ),
                   yaxis = list(
                     title = list(text = "Biomasa seca kg/ha", font = list(size = 12)),
                     range = c(y_min, y_max),
                     tickfont = list(size = 10)
                   ),
                   hovermode = "closest",
                   annotations = list(
                     list(
                       text = "Cuartiles calculados por el método lineal, q1 es percentil 25, y q3 es percentil 75",
                       x = 0.5,
                       y = -0.1,
                       showarrow = FALSE,
                       font = list(size = 12),
                       xref = 'paper',
                       yref = 'paper',
                       align = 'center'
                     )
                   )
                 )
               
               
               # g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
               #   geom_line() +
               #   labs(x = "Mes", y = "Biomasa seca kg/ha", title = "Simulated Biomass by Month and Percentile") +
               #   theme_minimal() +
               #   scale_color_manual(values = color_palette, name = "Percentile") +
               #   scale_x_continuous(breaks = 1:12, labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
               #   scale_y_continuous(limits = c(y_min, y_max))+
               #   
               color_palette1 <- setNames(custom_palette, unique(daycentmeses_region_long$Percentile))
               # Create the ggplot
               g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
                 geom_line() +
                 geom_text(
                   data = daycentmeses_region_long %>%
                     group_by(Percentile) %>%
                     filter(mesN == max(mesN)) %>%
                     ungroup(),
                   aes(label = Percentile),
                   vjust = -0.5,
                   hjust = 1.1,
                   color = "darkgrey",
                   size = 2
                 ) +
                 labs(
                   x = "Mes", 
                   y = "Biomasa seca kg/ha", 
                   title = "Distribución de Biomasa seca kg/ha por mes",
                 ) +
                 theme_minimal() +
                 scale_color_manual(values = color_palette1, name = "Percentile") +
                 scale_x_continuous(
                   breaks = 1:12, 
                   labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
                 ) +
                 scale_y_continuous(limits = c(y_min, y_max)) +
                 theme(
                   plot.title = element_text(size = 14),
                   axis.title.x = element_text(size = 12),
                   axis.title.y = element_text(size = 12),
                   axis.text.x = element_text(size = 10),
                   axis.text.y = element_text(size = 10),
                   legend.title = element_text(size = 12),
                   legend.text = element_text(size = 10)
                 )
               
               # Convert to interactive plotly object
               g12_plotly <- ggplotly(g12) %>%
                 layout(
                   title = list(
                     text = "Distribución de Biomasa seca simulada kg/ha por mes en Colinas y Lomadas del Este",
                     font = list(size = 14)
                   ),
                   xaxis = list(
                     title = list(text = "Mes", font = list(size = 12)),
                     tickfont = list(size = 10)
                   ),
                   yaxis = list(
                     title = list(text = "Biomasa seca kg/ha", font = list(size = 12)),
                     tickfont = list(size = 10)
                   ),
                   legend = list(
                     title = list(text = "Percentile", font = list(size = 12)),
                     font = list(size = 10)
                   ),
                   annotations = list(
                     list(
                       text = "Percentiles para cada mes",
                       x = 0.5,
                       y = -0.1,
                       showarrow = FALSE,
                       font = list(size = 12),
                       xref = 'paper',
                       yref = 'paper',
                       align = 'center'
                     )
                   )
                 )
               
               combined_plot <- subplot(
                 box_plot,  # Plotly box plot
                 g12_plotly, # ggplot2 plot converted to plotly
                 nrows = 2,  # Arrange plots in 2 rows
                 shareX = TRUE,
                 shareY = TRUE
               )
               
               # Display the combined plot
               combined_plot
               
             }
             else if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 10) {
               req(clickedRegion())
               daycentmeses_region <-daycentmeses%>%
                 filter(ZAgrEc =="ColyLomdeE")
               # 
               daycentmeses_region_long <- tidyr::pivot_longer(daycentmeses_region, cols = starts_with("Perc"), names_to = "Percentile", values_to = "Values")
               # 
               # 
               # # Usar la paleta de colores 'viridis' en tu gráfico
               # g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
               #   geom_line(show.legend = FALSE) +
               #   labs(x = "Estacion", y = "Biomasa seca kg/ha", title = "Biomasa seca kg/ha simulada simulada por mes y valor medio en punto, debajo percentiles") +
               #   theme_minimal() +
               #   scale_x_continuous(breaks = 1:12) +
               #   scale_color_manual(values = mi_paleta_viridis)
               # 
               # 
               # g22 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, group = Mes,colour=Mes)) +
               #   geom_boxplot(show.legend = FALSE) +
               #   labs(x = "Mes", y = "Biomasa seca kg/ha") +
               #   theme_minimal() +
               #   stat_summary(fun.y = mean, geom = "point", shape = 18, size = 2.5) +
               #   labs(caption = "Los puntos corresponden a la media simulada del mes") +
               #   scale_x_discrete(labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))
               # 
               # plotly::subplot(g22,g12,nrows = 2, margin = 0.1)
               
               data_day_month <-  data_day %>%
                 filter(year >= 1917) %>%  ### watch here!
                 group_by(ZAgrEc, modelo, month)%>%
                 filter(ZAgrEc =="ColyLomdeE") %>% ##add mesN
                 mutate(mesN = match(month, c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SET", "OCT", "NOV", "DIC")))%>%
                 mutate(mes_col= as.factor(mesN))
               
               # summary_stats <- data_day_month %>%
               # group_by(ZAgrEc, modelo, month) %>%
               # summarise(
               #   Median = median(MS.MES, na.rm = TRUE),
               #   Mean = mean(MS.MES, na.rm = TRUE),
               #   P05 = quantile(MS.MES, 0.05, na.rm = TRUE),
               #   P95 = quantile(MS.MES, 0.95, na.rm = TRUE))
               
               # Plot
               # Create the box plot
               mean_stats <- data_day_month %>%
                 group_by(mesN) %>%
                 summarise(mean_MS.MES = mean(MS.MES, na.rm = TRUE))
               
               y_min <- min(data_day_month$MS.MES, na.rm = TRUE)
               y_max <- max(data_day_month$MS.MES, na.rm = TRUE)
               
               # Create the box plot with mean points
               
               box_plot <- plot_ly(
                 data = data_day_month,
                 x = ~mesN,
                 y = ~MS.MES,
                 type = 'box',
                 color = ~'Mes',  # Use mesN to differentiate colors, if desired
                 showlegend = TRUE
               ) %>%
                 add_trace(
                   data = mean_stats,
                   x = ~mesN,
                   y = ~mean_MS.MES,
                   mode = 'markers',
                   type = 'scatter',
                   marker = list(color = 'blue', size = 7, symbol = 'circle'),
                   name = 'Promedio'
                 ) %>%
                 layout(
                   title = list(
                     text = "Distribución de Biomasa seca kg/ha por mes",
                     font = list(size = 14)
                   ),
                   xaxis = list(
                     title = list(text = "Mes", font = list(size = 12)),
                     tickvals = 1:12,
                     ticktext = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"),
                     tickfont = list(size = 10)
                   ),
                   yaxis = list(
                     title = list(text = "Biomasa seca kg/ha", font = list(size = 12)),
                     range = c(y_min, y_max),
                     tickfont = list(size = 10)
                   ),
                   hovermode = "closest",
                   annotations = list(
                     list(
                       text = "Cuartiles calculados por el método lineal, q1 es percentil 25, y q3 es percentil 75",
                       x = 0.5,
                       y = -0.1,
                       showarrow = FALSE,
                       font = list(size = 12),
                       xref = 'paper',
                       yref = 'paper',
                       align = 'center'
                     )
                   )
                 )
               
               
               # g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
               #   geom_line() +
               #   labs(x = "Mes", y = "Biomasa seca kg/ha", title = "Simulated Biomass by Month and Percentile") +
               #   theme_minimal() +
               #   scale_color_manual(values = color_palette, name = "Percentile") +
               #   scale_x_continuous(breaks = 1:12, labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
               #   scale_y_continuous(limits = c(y_min, y_max))+
               #   
               color_palette1 <- setNames(custom_palette, unique(daycentmeses_region_long$Percentile))
               # Create the ggplot
               g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
                 geom_line() +
                 geom_text(
                   data = daycentmeses_region_long %>%
                     group_by(Percentile) %>%
                     filter(mesN == max(mesN)) %>%
                     ungroup(),
                   aes(label = Percentile),
                   vjust = -0.5,
                   hjust = 1.1,
                   color = "darkgrey",
                   size = 2
                 ) +
                 labs(
                   x = "Mes", 
                   y = "Biomasa seca kg/ha", 
                   title = "Distribución de Biomasa seca kg/ha por mes",
                 ) +
                 theme_minimal() +
                 scale_color_manual(values = color_palette1, name = "Percentile") +
                 scale_x_continuous(
                   breaks = 1:12, 
                   labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
                 ) +
                 scale_y_continuous(limits = c(y_min, y_max)) +
                 theme(
                   plot.title = element_text(size = 14),
                   axis.title.x = element_text(size = 12),
                   axis.title.y = element_text(size = 12),
                   axis.text.x = element_text(size = 10),
                   axis.text.y = element_text(size = 10),
                   legend.title = element_text(size = 12),
                   legend.text = element_text(size = 10)
                 )
               
               # Convert to interactive plotly object
               g12_plotly <- ggplotly(g12) %>%
                 layout(
                   title = list(
                     text = "Distribución de Biomasa seca simulada kg/ha por mes en Colinas y Lomadas del Este",
                     font = list(size = 14)
                   ),
                   xaxis = list(
                     title = list(text = "Mes", font = list(size = 12)),
                     tickfont = list(size = 10)
                   ),
                   yaxis = list(
                     title = list(text = "Biomasa seca kg/ha", font = list(size = 12)),
                     tickfont = list(size = 10)
                   ),
                   legend = list(
                     title = list(text = "Percentile", font = list(size = 12)),
                     font = list(size = 10)
                   ),
                   annotations = list(
                     list(
                       text = "Percentiles para cada mes",
                       x = 0.5,
                       y = -0.1,
                       showarrow = FALSE,
                       font = list(size = 12),
                       xref = 'paper',
                       yref = 'paper',
                       align = 'center'
                     )
                   )
                 )
               
               combined_plot <- subplot(
                 box_plot,  # Plotly box plot
                 g12_plotly, # ggplot2 plot converted to plotly
                 nrows = 2,  # Arrange plots in 2 rows
                 shareX = TRUE,
                 shareY = TRUE
               )
               
               # Display the combined plot
               combined_plot
               
             }
             else if (!is.null(clickedID) && is.numeric(clickedID) && clickedID == 6) {
               req(clickedRegion())
               daycentmeses_region <-daycentmeses%>%
                 filter(ZAgrEc =="Llanensedeleste")
               # 
               daycentmeses_region_long <- tidyr::pivot_longer(daycentmeses_region, cols = starts_with("Perc"), names_to = "Percentile", values_to = "Values")
               # 
               # 
               # # Usar la paleta de colores 'viridis' en tu gráfico
               # g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
               #   geom_line(show.legend = FALSE) +
               #   labs(x = "Estacion", y = "Biomasa seca kg/ha", title = "Biomasa seca kg/ha simulada simulada por mes y valor medio en punto, debajo percentiles") +
               #   theme_minimal() +
               #   scale_x_continuous(breaks = 1:12) +
               #   scale_color_manual(values = mi_paleta_viridis)
               # 
               # 
               # g22 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, group = Mes,colour=Mes)) +
               #   geom_boxplot(show.legend = FALSE) +
               #   labs(x = "Mes", y = "Biomasa seca kg/ha") +
               #   theme_minimal() +
               #   stat_summary(fun.y = mean, geom = "point", shape = 18, size = 2.5) +
               #   labs(caption = "Los puntos corresponden a la media simulada del mes") +
               #   scale_x_discrete(labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))
               # 
               # plotly::subplot(g22,g12,nrows = 2, margin = 0.1)
               
               data_day_month <-  data_day %>%
                 filter(year >= 1917) %>%  ### watch here!
                 group_by(ZAgrEc, modelo, month)%>%
                 filter(ZAgrEc =="Llanensedeleste") %>% ##add mesN
                 mutate(mesN = match(month, c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SET", "OCT", "NOV", "DIC")))%>%
                 mutate(mes_col= as.factor(mesN))
               
               # summary_stats <- data_day_month %>%
               # group_by(ZAgrEc, modelo, month) %>%
               # summarise(
               #   Median = median(MS.MES, na.rm = TRUE),
               #   Mean = mean(MS.MES, na.rm = TRUE),
               #   P05 = quantile(MS.MES, 0.05, na.rm = TRUE),
               #   P95 = quantile(MS.MES, 0.95, na.rm = TRUE))
               
               # Plot
               # Create the box plot
               mean_stats <- data_day_month %>%
                 group_by(mesN) %>%
                 summarise(mean_MS.MES = mean(MS.MES, na.rm = TRUE))
               
               y_min <- min(data_day_month$MS.MES, na.rm = TRUE)
               y_max <- max(data_day_month$MS.MES, na.rm = TRUE)
               
               # Create the box plot with mean points
               
               box_plot <- plot_ly(
                 data = data_day_month,
                 x = ~mesN,
                 y = ~MS.MES,
                 type = 'box',
                 color = ~'Mes',  # Use mesN to differentiate colors, if desired
                 showlegend = TRUE
               ) %>%
                 add_trace(
                   data = mean_stats,
                   x = ~mesN,
                   y = ~mean_MS.MES,
                   mode = 'markers',
                   type = 'scatter',
                   marker = list(color = 'blue', size = 7, symbol = 'circle'),
                   name = 'Promedio'
                 ) %>%
                 layout(
                   title = list(
                     text = "Distribución de Biomasa seca kg/ha por mes",
                     font = list(size = 14)
                   ),
                   xaxis = list(
                     title = list(text = "Mes", font = list(size = 12)),
                     tickvals = 1:12,
                     ticktext = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"),
                     tickfont = list(size = 10)
                   ),
                   yaxis = list(
                     title = list(text = "Biomasa seca kg/ha", font = list(size = 12)),
                     range = c(y_min, y_max),
                     tickfont = list(size = 10)
                   ),
                   hovermode = "closest",
                   annotations = list(
                     list(
                       text = "Cuartiles calculados por el método lineal, q1 es percentil 25, y q3 es percentil 75",
                       x = 0.5,
                       y = -0.1,
                       showarrow = FALSE,
                       font = list(size = 12),
                       xref = 'paper',
                       yref = 'paper',
                       align = 'center'
                     )
                   )
                 )
               
               
               # g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
               #   geom_line() +
               #   labs(x = "Mes", y = "Biomasa seca kg/ha", title = "Simulated Biomass by Month and Percentile") +
               #   theme_minimal() +
               #   scale_color_manual(values = color_palette, name = "Percentile") +
               #   scale_x_continuous(breaks = 1:12, labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))+
               #   scale_y_continuous(limits = c(y_min, y_max))+
               #   
               color_palette1 <- setNames(custom_palette, unique(daycentmeses_region_long$Percentile))
               # Create the ggplot
               g12 <- ggplot(daycentmeses_region_long, aes(x = mesN, y = Values, color = Percentile)) +
                 geom_line() +
                 geom_text(
                   data = daycentmeses_region_long %>%
                     group_by(Percentile) %>%
                     filter(mesN == max(mesN)) %>%
                     ungroup(),
                   aes(label = Percentile),
                   vjust = -0.5,
                   hjust = 1.1,
                   color = "darkgrey",
                   size = 2
                 ) +
                 labs(
                   x = "Mes", 
                   y = "Biomasa seca kg/ha", 
                   title = "Distribución de Biomasa seca kg/ha por mes",
                 ) +
                 theme_minimal() +
                 scale_color_manual(values = color_palette1, name = "Percentile") +
                 scale_x_continuous(
                   breaks = 1:12, 
                   labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
                 ) +
                 scale_y_continuous(limits = c(y_min, y_max)) +
                 theme(
                   plot.title = element_text(size = 14),
                   axis.title.x = element_text(size = 12),
                   axis.title.y = element_text(size = 12),
                   axis.text.x = element_text(size = 10),
                   axis.text.y = element_text(size = 10),
                   legend.title = element_text(size = 12),
                   legend.text = element_text(size = 10)
                 )
               
               # Convert to interactive plotly object
               g12_plotly <- ggplotly(g12) %>%
                 layout(
                   title = list(
                     text = "Distribución de Biomasa seca simulada kg/ha por mes en Llanense del este",
                     font = list(size = 14)
                   ),
                   xaxis = list(
                     title = list(text = "Mes", font = list(size = 12)),
                     tickfont = list(size = 10)
                   ),
                   yaxis = list(
                     title = list(text = "Biomasa seca kg/ha", font = list(size = 12)),
                     tickfont = list(size = 10)
                   ),
                   legend = list(
                     title = list(text = "Percentile", font = list(size = 12)),
                     font = list(size = 10)
                   ),
                   annotations = list(
                     list(
                       text = "Percentiles para cada mes",
                       x = 0.5,
                       y = -0.1,
                       showarrow = FALSE,
                       font = list(size = 12),
                       xref = 'paper',
                       yref = 'paper',
                       align = 'center'
                     )
                   )
                 )
               
               combined_plot <- subplot(
                 box_plot,  # Plotly box plot
                 g12_plotly, # ggplot2 plot converted to plotly
                 nrows = 2,  # Arrange plots in 2 rows
                 shareX = TRUE,
                 shareY = TRUE
               )
               
               # Display the combined plot
               combined_plot
               
             }
           })
           
         #  browser()
           

           model_results_lp<-model_results%>%
             mutate(Estacion_Num = case_when(
               Estacion == "OTONO" ~ 1,
               Estacion == "INVIERNO" ~ 2,
               Estacion == "PRIMAVERA" ~ 3,
               Estacion == "VERANO" ~ 4,
               #   TRUE ~ as.numeric(Estacion)  # For other cases, you can assign other numerical values
             ))
           
           observed_results_t<- observed_results%>%
             mutate(Trimester=Estacion,Total_MS=OBS, year=9999, modelo=Modelo)%>%
             select(-c(Estacion,OBS,Modelo))
             
           #browser()
           #https://shiny.posit.co/r/articles/build/datatables/
            output$tabla2 <- DT::renderDataTable({
           tabla_todos<- bind_rows(trimester_year[-1,], observed_results_t )%>%
             mutate(ZAgrEc=case_when(
               ZAgrEc == "Areniscas" ~ "Areniscas",
               ZAgrEc == "BasaltoProfundo" ~ "Basalto Profundo",
               ZAgrEc == "BasaltoSuperficial" ~ "Basalto Superficial",
               ZAgrEc == "ColyLomdeE" ~ "Colinas y Lomadas del Este",
               ZAgrEc == "CristalinoCentro" ~ "Cristalino Centro",
               ZAgrEc == "Litoral" ~ "Litoral",
               ZAgrEc == "Llanensedeleste" ~ "Llanense del este",
               ZAgrEc == "Noreste" ~ "Noreste",
               ZAgrEc == "SedimentoSur" ~ "Sedimento Sur",
               ZAgrEc == "Sierrasdeleste" ~ "Sierras del este",
               TRUE ~ "Otro"
             ))%>%
             rename(Zona_Agroecologica=ZAgrEc)%>%
             arrange( Zona_Agroecologica, desc(year))   # Arrange by 'year' in descending order
           
           #  arrange(c("ZAgrEc" ,  "Modelo" ,  "Estacion","OBS", "Perc_005" ,"Perc_01" , "Perc_025" ,"Perc_05",  "Perc_075", "Perc_09" , "Perc_095" ))
           datatable(tabla_todos,rownames=FALSE, filter = "top", 
                     options = list(lengthMenu = c(5, 30, 50),pageLength = 50),class = 'cell-border stripe')%>%
             
             formatStyle(
               'modelo',
               backgroundColor = styleEqual(
                 unique(tabla_todos$modelo),
                 c('grey', 'lightblue', 'yellow','cyan','cyan3')  # Define colors based on unique values
               )
             )
           
            })
            
              output$descripcion_tabla2 <- renderText({
                paste("Biomasa seca kg/ha por estación simulada para cada año y dato observado para cada estación")
              })
              
              output$descripcion_tabla1 <- renderText({
                paste("Diagrama de cajas para simulaciones 1917-2022 de biomasa seca kg/ha de pasturas simulada en Daycent y observada (OBS)")
              })
              
              model_results_lp<-model_results%>%
                mutate(Estacion_Num = case_when(
                  Estacion == "OTONO" ~ 1,
                  Estacion == "INVIERNO" ~ 2,
                  Estacion == "PRIMAVERA" ~ 3,
                  Estacion == "VERANO" ~ 4,
                  #   TRUE ~ as.numeric(Estacion)  # For other cases, you can assign other numerical values
                ))
              
              model_results_region_longall  <- tidyr::pivot_longer(model_results_lp ,
                                                               cols = starts_with("Perc_"),
                                                               names_to = "Percentile",
                                                               values_to = "Values")
#browser()
              #model_results_region_longall <- tidyr::pivot_longer(model_results_lp, cols = 4:8, names_to = "Percentile", values_to = "Values")
              
observed_results_table<-observed_results%>%
  #filter(ZAgrEc =="Areniscas")%>%
  mutate(Estacion_Num = case_when(
    Estacion == "OTONO" ~ 1,
    Estacion == "INVIERNO" ~ 2,
    Estacion == "PRIMAVERA" ~ 3,
    Estacion == "VERANO" ~ 4,
    #  TRUE ~ as.numeric(Estacion)  # For other cases, you can assign other numerical values
  ))

observed_results_region_longall <- tidyr::pivot_longer(observed_results_table, cols = 4, names_to = "Data", values_to = "Values")
              
              all2<-full_join(model_results_region_longall[,-1],observed_results_region_longall)
              
              allsummary <- all2%>%
                group_by(ZAgrEc,Modelo,Estacion) %>%
                summarise(
                  sd = sd(Values, na.rm = TRUE),
                  len = mean(Values)
                )
              
              pivoted_data <- pivot_wider(allsummary, names_from = Modelo, values_from = c(sd, len))
             
                output$plot4 <- renderPlotly({
                #   Plot obs data vs sim data by trimester and see correlation
                # gg <- ggplot(pivoted_data, aes(x = len_Observed, y = len_daycent)) +
                #   geom_point(aes(color =Estacion , cat= ZAgrEc), size = 1.5) +
                #   geom_errorbar(
                #     aes(ymin = len_daycent - sd_daycent, ymax = len_daycent + sd_daycent),
                #     width = 0.2,alpha=0.2, size=0.3,
                #     position = position_dodge(width = 0.5)) +
                #   #geom_line(aes(group = 1)) +
                #   # geom_errorbar(
                #   #   aes(ymin = len_Daycent - sd_Daycent, ymax = len_Daycent + sd_Daycent),
                #   #   width = 0.2,
                #   #   position = position_dodge(width = 0.5)
                #   # )+
                #   scale_color_brewer(palette = "Set1") +
                #   labs(
                #     x = "Dato observado",
                #     y = "Media estacional simulada en Daycent",
                #     #title = "Daycent vs Observed across seasons"
                #   ) +
                #   theme_minimal()+ylim(0,3000)+xlim(0,3000)
                
                # # Calculating linear regression coefficients
                # fit <- lm(len_Daycent ~ len_Observed, data = pivoted_data)
                # coef <- round(coef(fit), 4)
                # r_squared <- format(summary(fit)$r.squared, digits = 4)
                # 
                # # Formatting equation and R-squared value
                # equation <- paste("y == ", coef[2], " *x + ", coef[1], sep = "")
                # r_squared_text <- paste("R² =", r_squared)
                # 
                # gg_with_trend <- gg + geom_smooth(method = "lm", se = FALSE, color = "black",size=0.3)
                
                  ### plot boxplot with biomass per mesN for all ZgAgro group by group=ZAgrEc
                  interactive_plot <- plot_ly(
                    data = data_day,
                    x = ~month,
                    y = ~MS.MES,
                    type = "box",
                    color = ~ZAgrEc,
                    boxpoints = "all",  # Show all points
                    jitter = 0.2,       # Add some jitter for better visibility of points
                    pointpos = -1.8,    # Position points relative to the box
                    opacity = 0.5,      # Set transparency
                    marker = list(size = 1)
                  ) %>%
                    layout(
                      title = "Biomasa seca kg/ha por mes para cada Zona Agroecológica 1917-2022",
                      xaxis = list(title = "Mes"),
                      yaxis = list(title = "Biomasa seca kg/ha"),
                      boxmode = "group",  # Group boxes by month
                      legend = list(title = list(text = "Grupo ZAgrEc")),
                      titlefont = list(size = 14),
                      font = list(size = 10),
                      tickfont = list(size = 12)
                    )
              #  interactive_plot 
                })
                
                
                # Manually add annotations to the interactive plot
                # equation_text <- paste("Equation: y =", coef[2], "x +", coef[1])
                #line_text <- "This is the regression line"
                # plot0 <- ggplot(na.omit(data_day_tri), aes(x = ZAgrEc, y = Total_MS, color = Trimester, fill = Trimester)) +
                #   geom_boxplot(position = position_dodge(width = 0.6), lwd = 0.5, alpha = 0.1) +  # Ensure modelo_colors is defined
                #   labs(
                #     title = "Distribución de Biomasa seca por Zona Agroecológica",
                #     x = "Zona Agroecológica",
                #     y = "kg/ha MS/mes"
                #   ) +
                #   theme_bw()
                # 
                plot0 <- ggplot(na.omit(data_day_tri), aes(x = ZAgrEc, y = Total_MS, color = Trimester, fill = Trimester)) +
                  geom_boxplot(position = position_dodge(width = 0.6), lwd = 0.5, alpha = 0.1) +  # Ensure modelo_colors is defined
                  labs(
                    title = "Distribución de Biomasa seca por Zona Agroecológica",
                    x = "Zona Agroecológica",
                    y = "kg/ha MS/mes"
                  ) +
                  theme_minimal()
                
                # Convert to interactive plotly object and customize layout
                plotly_boxplot <- ggplotly(plot0) %>%
                  layout(
                    title = list(
                      text = "Distribución de Biomasa seca por Zona Agroecológica",
                      tickfont = list(size = 12)  # Font size for the plot title
                    ),
                    xaxis = list(
                      title = "Zona Agroecológica",
                      titlefont = list(size = 10),  # Font size for x-axis title
                      tickfont = list(size = 9)    # Font size for x-axis labels
                    ),
                    yaxis = list(
                      title = "kg/ha MS/mes",
                      titlefont = list(size = 10),  # Font size for y-axis title
                      tickfont = list(size = 9)    # Font size for y-axis labels
                    ),
                    legend = list(
                      title = list(text = "Grupo ZAgrEc"),
                      tickfont = list(size = 9)  # Font size for legend text
                    )
                  )
                  
                # correspondencia entre ZAgrEc y Tipo_ZAgrEc
                # mapeo_ZAgrEc_tipo <- c("SedimentoSur" = "SS",
                #                        "Sierrasdeleste" = "SE",
                #                        "Agua" = "AA",
                #                        "ColyLomdeE" = "CL",
                #                        "CristalinoCentro" = "CC",
                #                        "Llanensedeleste" = "LL",
                #                        "BasaltoSuperficial" = "BS",
                #                        "BasaltoProfundo" = "BP",
                #                        "Areniscas" = "AR",
                #                        "Noreste" = "NE",
                #                        "Litoral" = "LT",
                #                        "Dunas" = "DU")
                
                
                # Agrega la nueva columna Tipo_ZAgrEc al dataset
                # model_results <- model_results2 %>%
                #   mutate(ZAgrEc2 = mapeo_ZAgrEc_tipo[ZAgrEc])
                
                ###mapa coneat
               # rounded_breaks <- c(min(heatmap_data$Value), round(mean(heatmap_data$Value)), max(heatmap_data$Value)) # Adjust the 'by' value as needed
                
                overlay_stats_R <- read_excel("overlay_stats_R.xlsx")
                interactive_plot2<- plotly_boxplot 
                overlay_stats_R2 <- overlay_stats_R %>%
                  select(-variance) %>%
                  filter(Clase_Mapa != "Agua", Clase_Mapa != "Dunas")
                
                # Melt the tibble to long format for better plotting
                overlay_stats_long <- tidyr::gather(overlay_stats_R2, key = "Variable", value = "Value", -Clase_Mapa)
                
                # # Create a list to store individual plots
                # plot_list <- list()
                # 
                # # Get unique variables for faceting
                # variables <- unique(overlay_stats_long$Variable)
                # 
                # # Create a bar plot for each variable
                # for (var in variables) {
                #   plot <- overlay_stats_long %>%
                #     filter(Variable == var) %>%
                #     plot_ly(
                #       x = ~Clase_Mapa,
                #       y = ~Value,
                #       type = 'bar',
                #       color = ~Clase_Mapa,
                #       colors = "Viridis",
                #       opacity = 0.6,  # Set the opacity of the bars
                #       text = ~Value,
                #       textposition = 'outside',
                #       marker = list(line = list(width = 1.5, color = 'black')),
                #       name = var
                #     ) %>%
                #     layout(
                #       title = paste("Distribution for", var),
                #       xaxis = list(title = "Zona Agroecológica", tickangle = 0),  # Set tick angle to 0
                #       yaxis = list(title = "IndiProd CONEAT"),
                #       barmode = 'group'
                #     )
                #   plot_list[[var]] <- plot
                # }
                # 
                # # Combine all plots into a subplot
                # combined_plot <- subplot(
                #   plot_list,
                #   nrows = length(plot_list),
                #   shareX = TRUE,
                #   shareY = TRUE
                # )
                # 
                # # Display the combined plot
                # combined_plot2
                
                
                
                ###coneaty mapo
                heatmap_data <- overlay_stats_R2 %>%
                  pivot_longer(cols = -Clase_Mapa, names_to = "Statistic", values_to = "Value")
                palette <- rev(brewer.pal(4, "RdYlBu")) 
                
                # heatmap_plot <- plot_ly(
                #   data = heatmap_data,
                #   x = ~Clase_Mapa,
                #   y = ~Statistic,
                #   z = ~Value,
                #   type = "heatmap",
                #   colors = palette,
                #   colorbar = list(
                #     title = "Value",
                #     tickvals = quantile(heatmap_data$Value, probs = seq(0, 1, length.out = 5))
                #   )) %>%
                #   layout(
                #     title = "Valores CONEAT extraídos de superponer mapa CONEAT con zonas aroecológicas, promedio, desvío y mediana",
                #     xaxis = list(title = "Clase_Mapa", tickangle = 0),  # Ensure labels on x-axis are straight
                #     yaxis = list(title = "Statistic", tickangle = 0),
                #     showlegend = FALSE# Ensure labels on y-axis are straight
                #   )
                
                heatmap_plot <- plot_ly(
                  data = heatmap_data,
                  x = ~Clase_Mapa,
                  y = ~Statistic,
                  z = ~Value,
                  type = "heatmap",
                  colors = palette,
                  colorbar = list(
                    title = "Value",
                    tickvals = quantile(heatmap_data$Value, probs = seq(0, 1, length.out = 5)),
                    ticktext = format(quantile(heatmap_data$Value, probs = seq(0, 1, length.out = 5)), nsmall = 2),
                    tickfont = list(size = 9),
                    titlefont = list(size = 14)
                  )
                ) %>%
                  layout(
                    title = "Indice CONEAT extraído de superponer mapa coneat y zonas aroecológicas, promedio, desvío y mediana",
                    xaxis = list(
                      title = "Zona Agroecológica",
                      tickangle = 0,
                      tickfont = list(size = 9) # Adjust font size
                      
                    ),
                    yaxis = list(
                      title = "",
                      tickangle = 0,
                       tickfont = list(size = 9) 
                    ),
                    margin = list(l = 80, r = 100, b = 60, t = 60),  # Adjust margins
                    colorbar = list(
                      x = 1.1,  # Position colorbar on the right
                      y = 0.5,
                      xanchor = 'left',
                      yanchor = 'middle'
                    )
                  )
                
                # 
                # combined_plot <- subplot(
                #   plotly_boxplot, heatmap_plot,
                #   nrows = 2,  # Place both plots in a single row
                #   margin = 0.05  # Adjust margin to add space between plots
                # ) #%>%
        #           layout(
        #             title = "Distribución de Biomasa seca simualda por Zona Agroecológica y valor de indice CONEAT para la misma",
        # showlegend = FALSE
    #)
                
                
                output$plot5 <- renderPlotly({
               
                plot5<- plotly_boxplot 
                plot5
                })
                
                output$plot6 <- renderPlotly({
                  
                  plot6<-heatmap_plot
                  plot6
                })
                # output$descripcion_heatmap <- renderText({
                #   paste("Valores promedio, desvío estándar (stand.dev) y mediana del ínidce CONEAT para la Zona Agroecológica,
                #         *Los valores de CONEAT extaridos de Mapa de índice CONEAT en algunos casos exceden el valore de 100 ")
                # })
                # Display the heatmap
                
                # Display the interactive plot with annotations
                # interactive_plot2 <- layout(
                #   interactive_plot,
                #   annotations = list(
                #     list(
                #       x = 1,  # Setting x to 1 places the annotation at the rightmost edge
                #       y = 0.05,  # Setting y to 0 places the annotation at the bottom
                #       xref = "paper",  # x reference is the entire plot area
                #       yref = "paper",  # y reference is the entire plot area
                #       text = equation_text, 
                #       showarrow = FALSE,
                #       xanchor = "right",  # Anchoring the text to the right
                #       yanchor = "bottom",  # Anchoring the text to the bottom
                #       align = "right"  # Aligning the text to the right
                #     ),
                #     list(
                #       x = 1,  # Setting x to 1 places the annotation at the rightmost edge
                #       y = 0,  # Setting y to 0 places the annotation at the bottom
                #       xref = "paper",  # x reference is the entire plot area
                #       yref = "paper",  # y reference is the entire plot area
                #       text = r_squared_text, 
                #       showarrow = FALSE,
                #       xanchor = "right",  # Anchoring the text to the right
                # #       yanchor = "bottom",  # Anchoring the text to the bottom
                # #       align = "right"  # Aligning the text to the right
                #     )
                #   )
                # )
                # 
                # interactive_plot2
                #})
                
                # Rename Clase_Mapa to match the ZAgrEc column for joining
                
                # overlay_stats_R2 <- overlay_stats_R2 %>%
                #   rename(ZAgrEc = Clase_Mapa)
                # 
                # # Join the datasets on ZAgrEc
                # # Join the datasets on ZAgrEc
                # combined_data <- data_day_tri %>%
                #   group_by(ZAgrEc, Trimester) %>%
                #   summarize(mean_Total_MS = mean(Total_MS, na.rm = TRUE),
                #             median_Total_MS = median(Total_MS, na.rm = TRUE),
                #             stand.dev_Total_MS = sd(Total_MS, na.rm = TRUE)) %>%
                #   left_join(overlay_stats_R2, by = "ZAgrEc")%>%
                #   ungroup()
                # 
                # # Load required libraries
                # library(dplyr)
                # library(Hmisc)
                # library(corrplot)
                
          
                # Aggregate Total_MS by ZAgrEc if needed
                
                # # Assuming combined_data is your dataset
                # # Select the relevant columns
                # selected_data <- combined_data %>%
                #   select(mean_Total_MS, median_Total_MS,
                #          stand.dev_Total_MS,stand.dev., median, average)
                # selected_data<-na.omit(selected_data)
                # 
                # # Convert to matrix for correlation calculation
                # data_matrix <- as.matrix(aggregated_data %>%
                #                            select(Total_MS, stand.dev., median, average))
                # 
                # # Calculate Spearman correlation and p-values
                # cor_results <- rcorr(data_matrix, type = "spearman")
                # 
                # # Extract correlation matrix and p-values
                # cor_matrix <- cor_results$r
                # p_values <- cor_results$P
                # 
                # # Ensure dimensions match
                # print(dim(cor_matrix)) # Should be 4x4
                # print(dim(p_values))  # Should be 4x4
                # 
                # # Plot correlation matrix with significance
                # corrplot(cor_matrix, 
                #          method = "color", 
                #          col = colorRampPalette(c("red", "white", "blue"))(20),
                #          addCoef.col = "black",  # Color of correlation coefficients
                #          insig = "pch",          # Show insignificant correlations as symbols
                #          p.mat = p_values,       # P-values matrix
                #          sig.level = 0.05,       # Significance level
                #          number.cex = 0.7)       # Font size of coefficients
                # 
          
        }        
    
        
        #shiny::runApp(display.mode="showcase")
shinyApp(ui = ui, server = server)
