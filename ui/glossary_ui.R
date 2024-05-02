####################### Glossary #######################

glossaryTab <- tabPanel(
  lang = "en",
  div(
    div(class="fa fa-circle-info", 
        role = "navigation"), "Glossary"), # wrap in div for screenreader / accessibility purposes 
  value = "glossary", # tab ID
  titlePanel(h1("Glossary")),
  useShinyjs(),
  p("Click on row headers to sort columns."),
  p("Use the search field to search the whole table, or the boxes at the top of each column to search in that column."),
  fluidRow(
    DT::dataTableOutput("glossary_table")
    
  ) #fluidrow
) # tabPanel