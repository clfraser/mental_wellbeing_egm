##########################################################
# Mental health EGM
# Original author(s): Catriona Fraser
# Original date: 2023-05-12
# Written/run on RStudio server 2022.7.2.576.12 and R 4.1.2
# Description of content
##########################################################


# Get packages
source("setup.R")

# Get EGM module --------------
source("modules/egm_reactable_mod.R")

# Get egm functions
source("functions/egm_functions.R")

# UI
ui <- 
#secure_app( # For password protection
  
  fluidPage(
tagList(
  tags$html(lang = "en"), # Set the language of the page - important for accessibility
# Specify most recent fontawesome library - change version as needed
tags$style("@import url(https://use.fontawesome.com/releases/v6.2.0/css/all.css);"),
# Add note that dashboard is in development
# div(
#     div("Dashboard is in development. Not final version."),
#     style = "color: white; background-color: red; width: 100%; text-align: center; font-weight: bold;",
# ),

    header = tags$head(          includeCSS("www/css/main.css"),  # Main
                                 includeCSS("www/css/tables.css"),  # tables
                                 includeCSS("www/css/navbar_and_panels.css"), # navbar and notes panel
                                 includeCSS("www/css/buttons.css"), # buttons
                                 includeCSS("www/css/select.css"), # selectors and radio buttons
                                 includeCSS("www/css/popovers.css"), # popovers
                                 includeCSS("www/css/boxes.css"), # boxes
                                 includeCSS("www/css/value_box.css"), # valueBox for headline figures
                                 includeCSS("www/css/info_box.css"), # infoBox for summary page boxes
                                 includeCSS("www/css/js_tree_r.css") # for heirarchical checkboxes
      
    ,  # CSS stylesheet
    tags$link(rel = "shortcut icon", href = "favicon_phs.ico"), # Icon for browser tab
    
),

  titlePanel(h1("Mental wellbeing evidence and gap map"),
             windowTitle = "Mental wellbeing evidence and gap map" # Title for browser tab
             ),
  
  p("This evidence and gap map contains primary-level evidence") %>% 
    tagAppendAttributes(class = 'box'),
  linebreaks(2),

  selectInput("dataset_input", "Select population of interest:",
            choices = c("Adults", "Children and young people")),

  egm_reactable_ui("egm_reactable", dataset = reactive({input$dataset_input}))

) # taglist
) # ui fluidpage
#) # Secure app, for password protection

# Server

server <- function(input, output, session) {

  
  credentials <- readRDS(here("admin/credentials.rds"))

  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  # EGM reactable module server
  egm_reactable_server("egm_reactable", dataset = reactive({input$dataset_input}))
  
  # Keeps the shiny app from timing out quickly 
  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
  })

}

# Run the application
shinyApp(ui=ui, server=server)

### END OF SCRIPT ###
