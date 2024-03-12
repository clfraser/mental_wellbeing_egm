####################### Glossary #######################

glossaryTab <- tabPanel(
  lang = "en",
  div(
    div(class="fa fa-circle-info", 
        role = "navigation"), "Glossary"), # wrap in div for screenreader / accessibility purposes 
  value = "glossary", # tab ID
  titlePanel(h1("Glossary")),
  useShinyjs(),
  fluidRow(
    DT::dataTableOutput("glossary_table")
    
  ) #fluidrow
) # tabPanel