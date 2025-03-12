#####################################.
# egm_reactable_mod.R
# This module creates the EGM chart using the reactable package
#####################################.

#######################################################
## READ IN DATA ----
#######################################################

## WEMWEBS

# Dataframe for adult plot
wemwebs_adult_chart_data <- read_parquet(here("data/wemwebs_adult_chart_data.parquet"))

# Dataframe for adult table
wemwebs_adult_table_data <- read_parquet(here("data/wemwebs_adult_table_data.parquet"))

# Dataframe for CYP plot
wemwebs_cyp_chart_data <- read_parquet(here("data/wemwebs_cyp_chart_data.parquet"))

# Dataframe for CYP table
wemwebs_cyp_table_data <- read_parquet(here("data/wemwebs_cyp_table_data.parquet"))

#######################################################
## MODULE UI ----
#######################################################

egm_reactable_ui <- function(id, dataset){
  ns <- NS(id)
  
  tagList(
    
    sidebarLayout(
      
      #######################################################
      ## Filters in sidebar ----
      #######################################################
      
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
                         choices = c("Intervention", "Risk/protective factor"),
                         selected = NULL
                       )
                   ) # div
                       
      ), # sidebarPanel
      
      #######################################################
      ## EGM chart in main panel ----
      #######################################################
      
      mainPanel(
        # Show chart_data to debug
        #tableOutput(ns("chart_data_test")),
        
        reactableOutput(ns("egm_chart")))
          
      ) # sidebarLayout
  ) #tagList
}

#######################################################
## MODULE SERVER ----
#######################################################

egm_reactable_server <- function(id, dataset) {
  moduleServer(
    id,
    
    function(input, output, session){
      
      # Define ns here since the jsTreeUpdate function for filters below refers to a namespaced output
      ns <- session$ns
      
      #######################################################
      ## Setting up dataset ----
      #######################################################
      
      # Set the appropriate dataset, depending on the user input
    
      selected_dataset <- reactive({
        switch({dataset()},
               "Adults" = wemwebs_adult_chart_data,
               "Children and young people" = wemwebs_cyp_chart_data)
      })
      
      # Create a reactiveVal for chart_data
      chart_data <- reactiveVal()
      
      # Set chart data to the full dataframe initially, and add a selected column
      full_dataframe <- reactive({selected_dataset() %>%
            mutate(selected = if_else(dummy == 0, 1, 0))})
      
      # Set chart_data to full_dataframe
      observe({chart_data(full_dataframe())})
      
      #######################################################
      ## Filters ----
      #######################################################
      
      # Putting data in the right format for jsTreeR
      
      # Create initial dataframes for trees
      
      domains_subs_for_tree <- reactive({selected_dataset() %>%
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
      
      # Reset filters when Clear Filters button clicked
      # Reset from the shinyjs package doesn't reset the tree (hierarchical) checkboxes, so add these separately
      # Assign full_dataframe to chart_data when Clear Filters button is clicked
      
      observeEvent(input$clear_all_filters_top, {
        shinyjs::reset("filter_panel")
        jstreeUpdate(session, ns("domains_tree"), clear_tree(domains_df(), "domain", "subdomain"))

        # Set chart_data to full_dataframe (remove any filtering on the chart data)
        chart_data(full_dataframe())
      })
      # 
      # Filtered dataframe
      
      observeEvent(input$filter_update_top, {
        chart_data(selected_dataset() %>%
                     mutate(domains_filter = if(is.null(unlist(input$domains_tree_selected))) TRUE else if_else(subdomain %in% unlist(input$domains_tree_selected), TRUE, FALSE),
                            intervention_risk_filter = if(is.null(input$intervention_risk_input)) TRUE else if_else(intervention_exposure_short %in% input$intervention_risk_input, TRUE, FALSE),
                            selected = if_else(domains_filter + intervention_risk_filter
                                               + dummy == 2, 1, 0))) # All of the filter checks are true, but the record isn't a dummy one
        })
      
      
      output$chart_data_test <- renderTable({chart_data()})

      #######################################################
      ## Formatting data for EGM ----
      #######################################################
      
      observe({
      
      # Get data into the right format for the EGM
      count_pivot <- reactive({
        chart_data() %>%
          group_by(domain, subdomain, overall_outcome, intervention_exposure_short) %>%
          summarise(count = length(unique(id_number[selected == 1]))) %>% # Count the unique IDs for studies that have been selected
          ungroup() %>%
          mutate(overall_outcome = gsub(" |-", "_", overall_outcome),
                 intervention_exposure_short = gsub(" |-|/", "_", intervention_exposure_short)) %>% # Replace spaces, slashes and hyphens with underscores for better variable names (hyphens seem to cause problems with grouping columns below)
          pivot_wider(names_from = c(overall_outcome, intervention_exposure_short), values_from = count, names_sep = ".") %>%
          mutate(across(everything(), ~replace_na(., 0))) %>% # Replace NAs with 0
          arrange(domain) %>%
          mutate(padding = "") %>%
          select(padding, domain, subdomain, WEMWEBS.Risk_protective_factor, WEMWEBS.Intervention)
      })

      # Create aggregated EGM table

      egm_aggregated_count <- reactive({
        chart_data() %>%
          group_by(domain, overall_outcome, intervention_exposure_short) %>%
          summarise(count = length(unique(id_number[selected == 1]))) %>% # Count the unique IDs for studies that have been selected
          ungroup() %>%
          mutate(overall_outcome = gsub(" |-", "_", overall_outcome),
                 intervention_exposure_short = gsub(" |-|/", "_", intervention_exposure_short)) %>% # Replace spaces, slashes and hyphens with underscores for better variable names (hyphens seem to cause problems with grouping columns below)
          pivot_wider(names_from = c(overall_outcome, intervention_exposure_short), values_from = count, names_sep = ".") %>%
          mutate(across(everything(), ~replace_na(., 0))) %>% # Replace NAs with 0
          arrange(domain) %>%
          mutate(subdomain = "") %>%
          select(domain, subdomain, WEMWEBS.Risk_protective_factor, WEMWEBS.Intervention)
      })

      # Create table with both aggregated and disaggregated data

      egm_agg_disag <- reactive({
        count_pivot() %>%
          select(-padding) %>%
          rbind(egm_aggregated_count())
      })

      #######################################################
      ## Creating EGM plot ----
      #######################################################

      output$egm_chart <- renderReactable({

        reactable(
          egm_aggregated_count(),
          # Only show shapes in header rows when they are expanded. See buttons.css for the CSS.
          rowClass = JS("function(rowInfo) {if (rowInfo.expanded == true) {
                                    return 'expanded-header'
                                    }}"),
          defaultColDef = colDef(
            align = 'center',
            width = 150),
          bordered = TRUE,
          fullWidth = FALSE,
          sortable = FALSE,
          defaultExpanded = TRUE,
          onClick = get_click_data, # Function defined in core functions
          theme = reactableTheme(backgroundColor = "#BCD9DA"),
          columns = list(
            domain = colDef(name = "Domain"),
            subdomain = colDef(name = "Sub-domain"),
            WEMWEBS.Risk_protective_factor = colDef(name = "Risk/protective factor",
                                                      cell = bubble_grid_modified(
                                                        data = egm_agg_disag(),
                                                        colors = '#83BB26',
                                                        tooltip = TRUE,
                                                        shape = "squares"
                                                      )),
            WEMWEBS.Intervention = colDef(name = "Intervention",
                                            cell = bubble_grid_modified(
                                              data = egm_agg_disag(),
                                              colors = '#3F3685',
                                              tooltip = TRUE
                                            ))
          ),
          details = function(index) {
            data_sub <- count_pivot()[count_pivot()$domain == egm_aggregated_count()$domain[index], ]
            reactable(data_sub,
                      defaultColDef = colDef(
                        align = 'center',
                        width = 150),
                      bordered = TRUE,
                      striped = TRUE,
                      fullWidth = FALSE,
                      sortable = FALSE,
                      class = "hidden-column-headers", # Use custom CSS in tables.css script. See: https://github.com/glin/reactable/issues/102
                      onClick = get_click_data, # Function defined in core functions
                      columns = list(
                        padding = colDef(name = "",
                                         width = 44),
                        domain = colDef(name = "Domain",
                                        width = 150),
                        subdomain = colDef(name = "Sub-domain",
                                           width = 150),
                        WEMWEBS.Risk_protective_factor = colDef(name = "",
                                                                  vAlign = "bottom",
                                                                  cell = bubble_grid_modified(
                                                                    data = egm_agg_disag(),
                                                                    colors = '#83BB26',
                                                                    tooltip = TRUE,
                                                                    shape = "squares"
                                                                  )),
                        WEMWEBS.Intervention = colDef(name = "",
                                                        vAlign = "top",
                                                        cell = bubble_grid_modified(
                                                          data = egm_agg_disag(),
                                                          colors = '#3F3685',
                                                          tooltip = TRUE
                                                        ))
                      ))
          }

        )
      }) # renderReactable
      
      }) # observe
    
    }
  )
}

# Run the app to test

ui <- fluidPage(
  selectInput("dataset_input", "Select population of interest:",
              choices = c("Adults", "Children and young people")),
  
  egm_reactable_ui("egm_reactable", dataset = reactive({input$dataset_input}))
)

server <- function(input, output, session) {
  egm_reactable_server("egm_reactable", dataset = reactive({input$dataset_input}))
}

shinyApp(ui, server)