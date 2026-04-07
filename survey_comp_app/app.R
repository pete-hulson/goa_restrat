# to run this, first un-comment the setwd line, then comment it out again, save, and run (will throw error when trying ot publish)
# setwd('C:/AA - PH Stuff/Projects/goa_restrat/survey_comp_app')

library(tidytable)
library(shiny)
library(stringr)

# 1. Setup - Let's assume your figures are in a folder named 'www/stocks'
# It is best practice to put images in a 'www' subfolder for Shiny
file_list <- list.files(file.path('www', 'stocks'),
                        pattern = "\\.png$|\\.jpg$")

# Extract stock names from filenames
stock_names <- str_remove(file_list, "_subreg_biom*\\.png$|_region*\\.png$") %>% 
  unique() %>% 
  gsub(pattern = "_", 
       replacement = " ")


ui <- fluidPage(
  # The title
  fluidRow(
    column(12, 
           h2("Gulf of Alaska bottom trawl survey design comparison", 
              style = "text-align: center; font-weight: bold;")
    )
  ),
  
  # The explanatory text
  fluidRow(
    column(12, 
           p("Herein contains figures that compare the historical index estimates from the GOA bottom trawl survey to the index estimates from the survey design implemented in 2025.
             Figures are included for each Tier 3 and 5 stock assessed by the AFSC within the GOA.",
             style = "font-size: 14px;")
    )
  ),
  
  # The Dropdown Menu
  fluidRow(
    column(width = 4, offset = 4, # Centers the dropdown (4 columns wide, pushed 4 from left)
           selectInput("stock_select", "Choose a Stock:", 
                       choices = stock_names, 
                       width = "100%")
    )
  ),
  
  hr(style = "border-top: 2px solid #222;"), # Adds a horizontal line for visual separation
  
  # The Plots and Text
  fluidRow(
    column(width = 10, offset = 1, # Keeps the main content slightly narrower for readability
           p(strong('Survey indices'), style = "text-align: center; font-size: 21px;"),
           p('GOA bottom trawl survey biomass (top) and population (bottom) index time series from the original design-based estimates (ORIG) and the post-stratified design-based estimates (PS) under the new survey design.'),
           imageOutput("figure1", height = "auto"),
           
           br(), # Adds extra vertical space
           hr(style = "border-top: 2px solid #222;"), # Adds a horizontal line for visual separation
           
           p(strong('Survey subregion biomass indices'), style = "text-align: center; font-size: 21px;"),
           p('GOA bottom trawl survey biomass time series by sebregion from the original design-based estimates (ORIG) and the post-stratified design-based estimates (PS) under the new survey design.'),
           imageOutput("figure2", height = "auto")
    )
  )
)

server <- function(input, output, session) {
  
  # Function to find and render the image
  output$figure1 <- renderImage({
    # Build path relative to the app.R file
    path <- file.path('www', 'stocks', 
                      paste0(gsub(input$stock_select, pattern = " ", replacement = "_"), 
                             "_region.png"))
    
    list(src = path,
         width = "100%",
         alt = paste("Survey index plot for", input$stock_select))
  }, deleteFile = FALSE)
  
  output$figure2 <- renderImage({
    # Build path relative to the app.R file
    path <- file.path('www', 'stocks', 
                      paste0(gsub(input$stock_select,
                                  pattern = " ", 
                                  replacement = "_"), 
                             "_subreg_biom.png"))
    
    list(src = path,
         width = "100%",
         alt = paste("Subregion biomass plot for", input$stock_select))
  }, deleteFile = FALSE)
  
}

shinyApp(ui, server)
