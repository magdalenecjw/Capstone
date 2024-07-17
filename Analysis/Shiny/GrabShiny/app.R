#### Load Packages ####
pacman::p_load(sf, tidyverse, lubridate, spNetwork,
               tmap, RColorBrewer,
               shiny, shinydashboard, shinycssloaders, tools, shinyjs)

#### Load Data ####
mpsz <- read_rds("mpsz.rds")
pickup_sf <- read_rds("pickup_sf.rds")
dropoff_sf <- read_rds("dropoff_sf.rds")
sg_road <- read_rds("sg_road.rds")

planning_areas <- sort(str_to_title(unique(sg_road$PLN_AREA_N)))

#### Define Functions ####

# Define a function to filter by location and data type
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

# Define a function to compute optimal bandwidth
compute_optimal_bw <- function(filtered_objects, method) {
  sg_road_filtered <- filtered_objects$sg_road_filtered
  events_filtered <- filtered_objects$events_filtered
  
  bw_list <- bw_cv_likelihood_calc(
    bws = seq(50, 700, 50),
    lines = sg_road_filtered, 
    events = events_filtered,
    w = rep(1, nrow(events_filtered)),
    kernel_name = "quartic", method = tolower(method),
    diggle_correction = FALSE, study_area = NULL,
    max_depth = 8,
    digits = 2, tol = 0.1, agg = 5,
    sparse = TRUE, grid_shape = c(1, 1),
    verbose = FALSE, check = TRUE)
  
  # Function to get the corresponding bw value for the lowest cv_score in each column
  get_max_bw <- function(cv_values) {
    sapply(cv_values[-1], function(x) cv_values$bw[which.max(x)])
  }
  
  # Get the corresponding bw values for the lowest cv_scores
  max_bw_values <- get_max_bw(bw_list)
  max_bw_values
}

# Define a function to compute densities
compute_densities <- function(location, bw, method, filtered_objects) {
  sg_road_filtered <- filtered_objects$sg_road_filtered
  events_filtered <- filtered_objects$events_filtered
  
  lixels <- lixelize_lines(sg_road_filtered, 500, mindist = 100)
  samples <- lines_center(lixels)
  
  densities <- nkde(sg_road_filtered, 
                    events = events_filtered,
                    w = rep(1, nrow(events_filtered)),
                    samples = samples,
                    kernel_name = "quartic",
                    bw = bw, div = "bw", 
                    method = tolower(method), digits = 1, tol = 1,
                    grid_shape = c(1, 1), max_depth = 8,
                    agg = 5, 
                    sparse = TRUE,
                    verbose = FALSE)
  
  samples$density <- densities
  samples$density <- samples$density * 1000
  
  # Merge samples with sg_road_filtered to get name in the samples
  samples_with_name <- samples %>%
    st_join(sg_road_filtered, join = st_nearest_feature)
  
  samples_with_name
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
                    actionButton("compute_bw_pickup", "Compute Optimal Bandwidth"),
                    tags$br(), tags$br(), # Added space
                    uiOutput("bw_input_ui_pickup"), # Dynamic UI for bandwidth input
                    actionButton("plot_pickup", "Generate Plot"), # Always visible
                    tags$br(), tags$br(), # Added space for reset button
                    actionButton("reset_pickup", "Reset") # Added reset button
                    , style = "min-width: 250px;"), # Added style for minimum width
                  mainPanel(
                    textOutput("title_pickup"), # Output for title
                    uiOutput("plotUI_pickup") # Use uiOutput to conditionally show the plot
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
                    actionButton("compute_bw_dropoff", "Compute Optimal Bandwidth"),
                    tags$br(), tags$br(), # Added space
                    uiOutput("bw_input_ui_dropoff"), # Dynamic UI for bandwidth input
                    actionButton("plot_dropoff", "Generate Plot"), # Always visible
                    tags$br(), tags$br(), # Added space for reset button
                    actionButton("reset_dropoff", "Reset") # Added reset button
                    , style = "min-width: 250px;"), # Added style for minimum width
                  mainPanel(
                    textOutput("title_dropoff"), # Output for title
                    uiOutput("plotUI_dropoff") # Use uiOutput to conditionally show the plot
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
      
      shinyjs::show("plot_pickup") # Ensure the plot button is always visible
    })
  })
  
  # Observe event for computing optimal bandwidth for dropoff
  observeEvent(input$compute_bw_dropoff, {
    filtered_objects <- filter_by_location(input$location_dropoff, "dropoff")
    
    withProgress(message = "Computing optimal bandwidth values for dropoff", {
      max_bw_values <- compute_optimal_bw(filtered_objects, input$method_dropoff)
      optimal_bw_dropoff(max_bw_values)
      
      output$bw_input_ui_dropoff <- renderUI({
        numericInput("bw_dropoff", "Bandwidth (bw):", value = max_bw_values[1], min = 50, max = 1000)
      })
      
      shinyjs::show("plot_dropoff") # Ensure the plot button is always visible
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
    
    output$title_pickup <- renderText({
      paste0("Pickup events density by kilometers in 2019, within a radius of ", bw, "m in ", location, "\n")
    })
    
    output$densityPlot_pickup <- renderTmap({
      tmap_mode("view")
      
      withProgress(message = "Generating pickup density plot", {
        filtered_objects <- filter_by_location(location, "pickup")
        samples <- compute_densities(location, bw, method, filtered_objects)
        samples2 <- samples[order(samples$density),]
        
        colorRamp <- brewer.pal(n = 7, name = "Spectral")
        colorRamp <- rev(colorRamp)
        
        plot <- tm_shape(filtered_objects$sg_road_filtered) + 
          tm_lines("black", id = "name") + 
          tm_shape(samples2) + 
          tm_dots("density", id = "name", style = "kmeans", palette = colorRamp, n = 7, size = 0.05) + 
          tm_layout(legend.outside = TRUE) + 
          tm_view(set.zoom.limits = c(11, 15), set.view = 14)
        
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
    
    output$title_dropoff <- renderText({
      paste0("Dropoff events density by kilometers in 2019, within a radius of ", bw, "m in ", location, "\n")
    })
    
    output$densityPlot_dropoff <- renderTmap({
      tmap_mode("view")
      
      withProgress(message = "Generating dropoff density plot", {
        filtered_objects <- filter_by_location(location, "dropoff")
        samples <- compute_densities(location, bw, method, filtered_objects)
        samples2 <- samples[order(samples$density),]
        
        colorRamp <- brewer.pal(n = 7, name = "Spectral")
        colorRamp <- rev(colorRamp)
        
        plot <- tm_shape(filtered_objects$sg_road_filtered) + 
          tm_lines("black", id = "name") + 
          tm_shape(samples2) + 
          tm_dots("density", id = "name", style = "kmeans", palette = colorRamp, n = 7, size = 0.05) + 
          tm_layout(legend.outside = TRUE) + 
          tm_view(set.zoom.limits = c(11, 15), set.view = 14)
        
        plot
      })
    })
  })
  
  output$plotUI_pickup <- renderUI({
    if (plot_generated_pickup()) {
      withSpinner(tmapOutput("densityPlot_pickup"))
    }
  })
  
  output$plotUI_dropoff <- renderUI({
    if (plot_generated_dropoff()) {
      withSpinner(tmapOutput("densityPlot_dropoff"))
    }
  })
  
  # Reset functionality for pickup tab
  observeEvent(input$reset_pickup, {
    shinyjs::reset("location_pickup")
    shinyjs::reset("method_pickup")
    shinyjs::reset("compute_bw_pickup")
    plot_generated_pickup(FALSE)
  })
  
  # Reset functionality for dropoff tab
  observeEvent(input$reset_dropoff, {
    shinyjs::reset("location_dropoff")
    shinyjs::reset("method_dropoff")
    shinyjs::reset("compute_bw_dropoff")
    plot_generated_dropoff(FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)