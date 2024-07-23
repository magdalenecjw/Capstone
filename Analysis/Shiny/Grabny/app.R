#### Load Packages ####
pacman::p_load(sf, tidyverse, lubridate, spNetwork,
               tmap, RColorBrewer,
               shiny, shinydashboard, shinycssloaders, tools, shinyjs)

#### Load Data ####
mpsz <- read_rds("mpsz_shiny.rds")
pickup_sf <- read_rds("pickup_shiny.rds")
dropoff_sf <- read_rds("dropoff_shiny.rds")
sg_road <- read_rds("sg_road_shiny.rds")

planning_areas <- sort(str_to_title(unique(sg_road$PLN_AREA_N)))

#### Define Functions ####

# Function to filter by location and data type
filter_by_location <- function(location, data_type) {
  sg_road_filtered <- sg_road %>%
    filter(PLN_AREA_N == toupper(location)) %>%
    select(osm_id, name, geometry)
  
  mpsz_filtered <- mpsz %>%
    filter(PLN_AREA_N == toupper(location))
  
  if (data_type == "pickup") {
    events_filtered <- pickup_sf %>%
      st_intersection(st_as_sf(mpsz_filtered))
  } else {
    events_filtered <- dropoff_sf %>%
      st_intersection(st_as_sf(mpsz_filtered))
  }
  
  list(sg_road_filtered = st_as_sf(sg_road_filtered), 
       mpsz_filtered = st_as_sf(mpsz_filtered), 
       events_filtered = events_filtered)
}

# Function to compute optimal bandwidth
compute_optimal_bw <- function(filtered_objects, method) {
  sg_road_filtered <- filtered_objects$sg_road_filtered
  events_filtered <- filtered_objects$events_filtered
  
  bw_list <- bw_cv_likelihood_calc(
    c(100, 1000), 50,
    lines = sg_road_filtered, 
    events = events_filtered,
    w = rep(1, nrow(events_filtered)),
    kernel_name = "quartic", method = tolower(method),
    max_depth = 8,
    digits = 2, tol = 0.1, agg = 5,
    sparse = TRUE, grid_shape = c(1, 1),
    verbose = FALSE, check = TRUE)
  
  # Get corresponding bw value for the lowest cv_score in each column
  get_max_bw <- function(cv_values) {
    sapply(cv_values[-1], function(x) cv_values$bw[which.max(x)])
  }
  
  max_bw_values <- get_max_bw(bw_list)
  max_bw_values
}

# Function to compute densities
compute_densities <- function(location, bw, method, filtered_objects, adaptive, show_basemap) {
  sg_road_filtered <- filtered_objects$sg_road_filtered
  events_filtered <- filtered_objects$events_filtered
  lixel_length <- bw / 5
  
  lixels <- lixelize_lines(sg_road_filtered, lixel_length, mindist = 50)
  samples <- lines_center(lixels)
  
  densities <- nkde(sg_road_filtered, 
                    events = events_filtered,
                    w = rep(1, nrow(events_filtered)),
                    samples = samples,
                    kernel_name = "quartic",
                    bw = bw, div = "bw", adaptive = as.logical(adaptive), trim_bw = if(as.logical(adaptive)) bw * 2 else NA,
                    method = tolower(method), digits = 1, tol = 1,
                    grid_shape = c(1, 1), max_depth = 8,
                    agg = 5, 
                    sparse = TRUE,
                    verbose = FALSE)
  
  if (as.logical(adaptive)) {
    densities <- densities$k
  }
  
  # Rescaling to help the mapping
  samples$density <- densities
  lixels$density <- densities
  samples$density <- samples$density * 1000
  lixels$density <- lixels$density * 1000
  
  colorRamp <- c("skyblue", "gold", "lightpink", "indianred", "firebrick")
  
  # Plot the result
  plot <- if(show_basemap) {
    tm_basemap(leaflet::providers$Esri.WorldTopoMap) + 
      tm_shape(lixels) + 
      tm_lines(col = "density", id = "name", size = 5, style = "kmeans", n=5, palette = colorRamp) + 
      tm_view(set.zoom.limits = c(11, 16), set.view = 14)
  } else {
    tm_shape(lixels) + 
      tm_lines(col = "density", id = "name", size = 5, style = "kmeans", n=5, palette = colorRamp) + 
      tm_view(set.zoom.limits = c(11, 16), set.view = 14)
  }
  
  plot
}

#### UI ####
ui <- dashboardPage(
  dashboardHeader(title = "NKDE Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Pickup", tabName = "pickup", icon = icon("car")),
      menuItem("Dropoff", tabName = "dropoff", icon = icon("car"))
    )
  ),
  dashboardBody(
    useShinyjs(), # Initialize shinyjs
    tabItems(
      tabItem(tabName = "pickup",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    selectInput("location_pickup", "Select Planning Area:", 
                                choices = planning_areas),
                    selectInput("method_pickup", "Method:", 
                                choices = c("Simple" = "simple",
                                            "Continuous" = "continuous",
                                            "Discontinuous" = "discontinuous"),
                                selected = "discontinuous"),
                    radioButtons("adaptive_pickup", "Estimator Type:", choices = c("Fixed" = FALSE, "Adaptive" = TRUE), selected = FALSE),
                    actionButton("compute_bw_pickup", "Compute Optimal Bandwidth"),
                    tags$br(), tags$br(),
                    uiOutput("bw_input_ui_pickup"), # Bandwidth input
                    checkboxInput("show_basemap_pickup", "Show Basemap", value = TRUE),
                    actionButton("plot_pickup", "Generate Plot"),
                    tags$br(), tags$br(),
                    actionButton("reset_pickup", "Reset")
                    , style = "min-width: 250px;"),
                  mainPanel(
                    textOutput("title_pickup"),
                    uiOutput("plotUI_pickup") 
                  )
                )
              )
      ),
      tabItem(tabName = "dropoff",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    selectInput("location_dropoff", "Select Planning Area:", 
                                choices = planning_areas),
                    selectInput("method_dropoff", "Method:", 
                                choices = c("Simple" = "simple",
                                            "Continuous" = "continuous",
                                            "Discontinuous" = "discontinuous"),
                                selected = "discontinuous"),
                    radioButtons("adaptive_dropoff", "Estimator Type:", choices = c("Fixed" = FALSE, "Adaptive" = TRUE), selected = FALSE),
                    actionButton("compute_bw_dropoff", "Compute Optimal Bandwidth"),
                    tags$br(), tags$br(),
                    uiOutput("bw_input_ui_dropoff"), # Bandwidth input
                    checkboxInput("show_basemap_dropoff", "Show Basemap", value = TRUE), 
                    actionButton("plot_dropoff", "Generate Plot"), 
                    tags$br(), tags$br(),
                    actionButton("reset_dropoff", "Reset") 
                    , style = "min-width: 250px;"), 
                  mainPanel(
                    textOutput("title_dropoff"), 
                    uiOutput("plotUI_dropoff") 
                  )
                )
              )
      )
    )
  )
)

#### Server ####
server <- function(input, output, session) {
  # Reactive values to store optimal bandwidths
  optimal_bw_pickup <- reactiveVal(NULL)
  optimal_bw_dropoff <- reactiveVal(NULL)
  
  # Observe event for computing optimal bandwidth for pickup
  observeEvent(input$compute_bw_pickup, {
    filtered_objects <- filter_by_location(input$location_pickup, "pickup")
    
    withProgress(message = "Computing optimal bandwidth values for pickup", {
      max_bw_values <- compute_optimal_bw(filtered_objects, input$method_pickup)
      optimal_bw_pickup(max_bw_values)
      
      output$bw_input_ui_pickup <- renderUI({
        numericInput("bw_pickup", "Bandwidth (bw):", value = max_bw_values[1], min = 50, max = 1000)
      })
      
      shinyjs::show("plot_pickup")
    })
  })
  
  # Observe event for computing optimal bandwidth for dropoff
  observeEvent(input$compute_bw_dropoff, {
    filtered_objects <- filter_by_location(input$location_dropoff, "dropoff")
    
    withProgress(message = "Computing optimal bandwidth values for dropoff", {
      max_bw_values <- compute_optimal_bw(filtered_objects, input$method_dropoff)
      optimal_bw_dropoff(max_bw_values)
      
      output$bw_input_ui_dropoff <- renderUI({
        numericInput("bw_dropoff", "Bandwidth (bw):", value =

 max_bw_values[1], min = 50, max = 1000)
      })
      
      shinyjs::show("plot_dropoff") 
    })
  })
  
  # Reactive values to track if the plot button has been clicked
  plot_generated_pickup <- reactiveVal(FALSE)
  plot_generated_dropoff <- reactiveVal(FALSE)
  
  # Generate plot for pickup
  observeEvent(input$plot_pickup, {
    plot_generated_pickup(TRUE)
    location <- input$location_pickup
    bw <- if (is.null(input$bw_pickup)) 300 else input$bw_pickup
    method <- input$method_pickup
    adaptive <- as.logical(input$adaptive_pickup)
    show_basemap <- input$show_basemap_pickup
    
    output$title_pickup <- renderText({
      paste0("Pickup events density by kilometres in 2019, within a radius of ", bw, "m in ", location, "\n")
    })
    
    output$densityPlot_pickup <- renderTmap({
      tmap_mode("view")
      
      withProgress(message = "Generating pickup density plot", {
        filtered_objects <- filter_by_location(location, "pickup")
        plot <- compute_densities(location, bw, method, filtered_objects, adaptive, show_basemap)
        
        plot
      })
    })
  })
  
  # Generate plot for dropoff
  observeEvent(input$plot_dropoff, {
    plot_generated_dropoff(TRUE)
    location <- input$location_dropoff
    bw <- if (is.null(input$bw_dropoff)) 300 else input$bw_dropoff
    method <- input$method_dropoff
    adaptive <- as.logical(input$adaptive_dropoff)
    show_basemap <- input$show_basemap_dropoff
    
    output$title_dropoff <- renderText({
      paste0("Dropoff events density by kilometres in 2019, within a radius of ", bw, "m in ", location, "\n")
    })
    
    output$densityPlot_dropoff <- renderTmap({
      tmap_mode("view")
      
      withProgress(message = "Generating dropoff density plot", {
        filtered_objects <- filter_by_location(location, "dropoff")
        plot <- compute_densities(location, bw, method, filtered_objects, adaptive, show_basemap)
        
        plot
      })
    })
  })
  
  output$plotUI_pickup <- renderUI({
    if (plot_generated_pickup()) {
      withSpinner(tmapOutput("densityPlot_pickup", height = "600px"))
    }
  })
  
  output$plotUI_dropoff <- renderUI({
    if (plot_generated_dropoff()) {
      withSpinner(tmapOutput("densityPlot_dropoff", height = "600px"))
    }
  })
  
  # Reset functionality for pickup tab
  observeEvent(input$reset_pickup, {
    shinyjs::reset("location_pickup")
    shinyjs::reset("method_pickup")
    shinyjs::reset("compute_bw_pickup")
    shinyjs::reset("adaptive_pickup")
    plot_generated_pickup(FALSE)
  })
  
  # Reset functionality for dropoff tab
  observeEvent(input$reset_dropoff, {
    shinyjs::reset("location_dropoff")
    shinyjs::reset("method_dropoff")
    shinyjs::reset("compute_bw_dropoff")
    shinyjs::reset("adaptive_dropoff")
    plot_generated_dropoff(FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)