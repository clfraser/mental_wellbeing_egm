#####################################.
# egm_filters_mod.R
# This module creates the filters for the EGM tab
#####################################.

#######################################################
## MODULE UI ----
#######################################################

egm_filters_ui <- function(id, dataset){
  ns <- NS(id)
  
  tagList(
    
    sidebarLayout(
      sidebarPanel(width = 3,
                   ## Inputs
                   # Set to use Shinyjs and set ID, so that checkboxes can be automatically reset
                   shinyjs::useShinyjs(),
                   id = ns("filter_panel"),
                   
                   # Action button to update filters
                   actionButton(
                     ns("filter_update_top"),
                     "Apply filters"
                   ),
                   
                   # Action button to select all filter options
                   actionButton(
                     ns("clear_all_filters_top"),
                     "Clear all"
                   ),
                   
                   linebreaks(2),
                   
                   div(id = ns("all_filters"), # Create div for the Cicerone tour
                       
                       # Domains and subdomains
                       # Different for adult and CYP
                       tags$span(
                         tags$label("Select domain and sub-domain"), 
                         actionButton(ns("domains_defs"), "", icon = icon("circle-info", `aria-label` = "Click for more information about domains and subdomains"))),
                       # Domains and sub-domains with jsTreeR
                       jstreeOutput(ns("domains_tree")),
                       
                       # Intervention or exposure. This is a straightforward checkboxGroupInput, since it doesn't need multiple levels.
                       checkboxGroupInput(
                         inputId = ns("intervention_risk_input"),
                         label = tags$span("Select studies looking at interventions or risk/protective factors",
                                           actionButton(ns("int_exposure_defs"), "", icon = icon("circle-info", `aria-label` = "Click for more information about interventions and risk/protective factors"))),
                         choices = c("Interventions", "Risk/protective factors"),
                         selected = NULL
                       )
                       
                   ) # sidebarLayout
      ), # sidebarPanel
      mainPanel()
      
    ) #div
    
  ) # tagList
  
}

#######################################################
## MODULE SERVER ----
#######################################################

egm_filters_server <- function(id, dataset){
  moduleServer(
    id,
    
    function(input, output, session){
      
      # Define ns here since the jsTreeUpdate function below refers to a namespaced output
      ns <- session$ns
      
      # Putting data in the right format for jsTreeR
      
      # Create initial dataframes for trees
      
      domains_subs_for_tree <- reactive({dataset() %>%
          select(domain, subdomain) %>%
          distinct(domain, subdomain) %>%
          arrange(domain, subdomain)
      })
      
      # Turn these into dataframes for creating nodes for the tree
      # Note that for the create_df_for_nodes function, the column names shouldn't be in quotes,
      # but for the create_nodes_from_df function, they should
      
      domains_df <- reactive({create_df_for_nodes(domains_subs_for_tree(), subdomain)})
      
      # Turn the dataframes into nodes
      
      domain_subs_nodes <- reactive({create_nodes_from_df(domains_df(), "domain", "subdomain")})
      
      ## Render tree for jsTreeR inputs
      output$domains_tree <- renderJstree({
        jstree(
          domain_subs_nodes(),
          checkboxes = TRUE
        )
      })
      
      # Reset filters when button clicked
      # Reset from the shinyjs package doesn't reset the tree (hierarchical) checkboxes, so add these separately
      
      observeEvent(input$clear_all_filters_top, {
        shinyjs::reset("filter_panel")
        jstreeUpdate(session, ns("domains_tree"), clear_tree(domains_df(), "domain", "subdomain"))
        
        #chart_data(full_dataframe)
      })
      
    }
  )
}

# Run the app to test

ui <- fluidPage(
  selectInput("dataset_input", "Select population of interest:",
              choices = c("Adults", "Children and young people")),
  
  egm_filters_ui("egm_filters", dataset = selected_dataset)
)

server <- function(input, output, session) {
  
  selected_dataset <- reactive({
    switch(input$dataset_input,
           "Adults" = wemwebs_adult_chart_data,
           "Children and young people" = wemwebs_cyp_chart_data)
  })
  
  egm_filters_server("egm_filters", dataset = selected_dataset)
}

shinyApp(ui, server)
