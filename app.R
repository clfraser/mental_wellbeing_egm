##########################################################
# Mental health EGM
# Original author(s): Catriona Fraser
# Original date: 2023-05-12
# Written/run on RStudio server 2022.7.2.576.12 and R 4.1.2
# Description of content
##########################################################


# Get packages
source("setup.R")

# UI
ui <- 
  secure_app(
  
  fluidPage(
tagList(
# Specify most recent fontawesome library - change version as needed
tags$style("@import url(https://use.fontawesome.com/releases/v6.2.0/css/all.css);"),
navbarPage(
    id = "intabset", # id used for jumping between tabs
    title = div(
        tags$a(img(src = "phs-logo.png", height = 40),
               href = "https://www.publichealthscotland.scot/",
               target = "_blank"), # PHS logo links to PHS website
    style = "position: relative; top: -5px;"),
    windowTitle = "Mental health EGM",# Title for browser tab
    header = tags$head(includeCSS("www/styles.css"),  # CSS stylesheet
    tags$link(rel = "shortcut icon", href = "favicon_phs.ico") # Icon for browser tab
), ##############################################.
# INTRO PAGE ----
##############################################.
tabPanel(title = "Introduction",
    icon = icon_no_warning_fn("circle-info"),
    value = "intro",

    uiOutput("intro_page_ui")

), # tabpanel
##############################################.
# EGM ----
##############################################.
tabPanel(title = "Evidence and gap map",
    # Look at https://fontawesome.com/search?m=free for icons
    icon = icon_no_warning_fn("map"),
    value = "Evidence and gap map",

    h1("Evidence and gap map"),
    uiOutput("page_1_ui")

    ), # tabpanel
##############################################.
# Glossary ----
##############################################.
tabPanel(title = "Glossary",
         # Look at https://fontawesome.com/search?m=free for icons
         icon = icon_no_warning_fn("circle-info"),
         value = "Glossary",
         
         h1("Glossary"),
         uiOutput("glossary_ui")
         
) # tabpanel
         
) # navbar
) # taglist
) # ui fluidpage
) # Secure app, for password protection

# Server

server <- function(input, output, session) {

  
  credentials <- readRDS(here("admin/credentials.rds"))
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
    # Get functions
    source(file.path("functions/core_functions.R"), local = TRUE)$value
    source(file.path("functions/intro_page_functions.R"), local = TRUE)$value
    source(file.path("functions/egm_functions.R"), local = TRUE)$value

    # Get content for individual pages
    source(file.path("pages/intro_page.R"), local = TRUE)$value
    source(file.path("pages/page_1.R"), local = TRUE)$value
    source(file.path("pages/glossary.R"), local = TRUE)$value

}

# Run the application
shinyApp(ui=ui, server=server)

### END OF SCRIPT ###
