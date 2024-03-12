####################### Intro UI #######################

####################### Intro Page #######################


homepageTab <-   tabPanel(
  lang = "en",
  div(
    div(class="fa fa-home", 
        role = "navigation"), "Introduction"), # wrap in div for screenreader / accessibility purposes 
  value = "intro", # tab ID
  titlePanel(h1("Introduction")),
  useShinyjs(),
  
  sidebarLayout(
    # Sidebar with a action buttons
    sidebarPanel(width = 3,
                 column(12),
                 column(12),
                 column(12, actionButton("about", "About", style="width:15vw;margin-bottom:10px;background-color: #9B4393;color: #FFFFFF;")),
                 column(12, actionButton("use", "How to use the EGM", style="width:15vw;margin-bottom:10px;background-color: #9B4393;color: #FFFFFF;")),
                 column(12, actionButton("methodology", "Methodology", style="width:15vw;margin-bottom:10px;background-color: #9B4393;color: #FFFFFF;")),
                 column(12, actionButton("contact", "Contact", style="width:15vw;margin-bottom:10px;background-color: #9B4393;color: #FFFFFF;")),
                 column(12, actionButton("accessibility", "Accessibility", style="width:15vw;margin-bottom:10px;background-color: #9B4393;color: #FFFFFF;"))
    ),
    # Display selected text
    mainPanel(width = 9,
              fluidRow(column(10, htmlOutput("text"))
              )
    )
  )
)