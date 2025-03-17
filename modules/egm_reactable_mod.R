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
        
        tabsetPanel(type = "tabs",
                    id = ns("tabset_egm"),
                    tabPanel("EGM",
                             linebreaks(1),
                             actionButton(ns("show_egm_numbers"), "Show EGM as text", `aria-label` = "Show EGM as text button. The visual evidence and gap map (EGM) is not accessible via screenreader. Please click this button to access and download a text version of the EGM. You can use the filters to update this."),
                             linebreaks(2),
                             tags$div(withNavySpinner(reactableOutput(ns("egm_chart"))), value = "graph",'aria-label' = "The visual EGM is not accessible by screen reader. Please use the Show EGM as text button above to access an accessible version.")),
                    tabPanel("Included studies",
                             linebreaks(1),
                             textOutput(ns("record_count")) %>% 
                               tagAppendAttributes(class = 'box-info'),
                             linebreaks(1),
                             p("To see more columns, use the scroll bar at the bottom of the table, or click inside the table and use the left and right arrow keys on your keyboard."),
                             linebreaks(1),
                             csvDownloadButton(ns("studies_table"), filename = "egm_studies.csv"), # To download table as a CSV (defined in core functions script)
                             #withNavySpinner(tableOutput(ns("studies_table_not_reactable"))),
                             withNavySpinner(reactableOutput(ns("studies_table"))),
                             value = ns("included_studies")) # For switching tabs on click
    ) # tabsetPanel
      ) # mainPanel
          
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
      
      # Reset filters when Clear Filters button clicked OR when the dataset is changed (via the dataset_input outside the module)
      # Reset from the shinyjs package doesn't reset the tree (hierarchical) checkboxes, so add these separately
      # Assign full_dataframe to chart_data
      
      observeEvent({input$clear_all_filters_top
                    dataset()}, {
        shinyjs::reset("filter_panel")
        jstreeUpdate(session, ns("domains_tree"), clear_tree(domains_df(), "domain", "subdomain"))

        # Set chart_data to full_dataframe (remove any filtering on the chart data)
        chart_data(full_dataframe())
      })

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
      
      ###############################################################################
      ## Table with numbers for the EGM, rather than shapes (for accessibility) ----
      ###############################################################################
      
      output$egm_numbers <- renderReactable({
        count_pivot() %>%
          select(-padding) %>% # Remove padding column that's used for nested EGM tables
          reactable(
            defaultColDef = colDef(
              align = 'center',
              width = 150),
            defaultExpanded = TRUE,
            bordered = TRUE,
            striped = TRUE,
            fullWidth = FALSE,
            columns = list(
              domain = colDef(name = "Domain",
                              width = 150),
              subdomain = colDef(name = "Sub-domain",
                                 width = 150),
              WEMWEBS.Risk_protective_factor = colDef(name = "Association study"),
              WEMWEBS.Intervention = colDef(name = "Intervention study")
            ),
            columnGroups = list(
              colGroup(name = "Mental wellbeing", columns = c("WEMWEBS.Risk_protective_factor", "WEMWEBS.Intervention"))
            )
          )
      })
      
      # Show numeric table on click
      
      observeEvent(input$show_egm_numbers, {
        showModal(modalDialog(
          id = "show_egm_numbers_modal",
          title = "Evidence and gap map counts",
          p("This table can be filtered by using the filters on the main page. Change the filters and click back on 'Show EGM as text' to get an updated table."),
          linebreaks(1),
          csvDownloadButton(ns("egm_numbers"), filename = "egm_counts.csv"), # To download table as a CSV (defined in core functions script)
          linebreaks(2),
          reactableOutput(ns("egm_numbers")),
          easyClose = TRUE,
          size = "l"))
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
      
      #######################################################
      ## Table of included studies ----
      #######################################################
      
      # Set the appropriate table dataset, depending on the user input
      
      selected_table_dataset <- reactive({
        switch({dataset()},
               "Adults" = wemwebs_adult_table_data,
               "Children and young people" = wemwebs_cyp_table_data)
      })
      
      # Create dataframe to show in table
      
      table_data <- reactive({
        
        only_selected <-
          chart_data() %>%
          filter(selected == 1)
        
        selected_table_dataset() %>%
          filter(id_number %in% only_selected$id_number) %>%
          dplyr::select(-id_number) %>%
          arrange(study_id)
      })
      
      output$studies_table_not_reactable <- renderTable(table_data())
      
      output$studies_table <- renderReactable({
        
        table_no_dups <- unique(table_data())
        
        table_no_dups %>%
          reactable(
            searchable = TRUE,
            resizable = TRUE,
            filterable = TRUE,
            showPageSizeOptions = TRUE,
            defaultColDef = colDef(
              minWidth = 200),
            columns = list(
              study_id = colDef(name = "Author and date",
                                sticky = "left",
                                html = TRUE, cell = function(value, index) {
                                  sprintf('<a href="%s" target="_blank">%s</a>', table_no_dups$DOI[index], value)
                                }),
              title = colDef(name = "Title",
                             sticky = "left",
                             # Add a right border style to visually distinguish the sticky column
                             style = list(borderRight = "1px solid #eee"),
                             headerStyle = list(borderRight = "1px solid #eee")),
              aim = colDef(name = "Aim of study",
                                    minWidth = 300),
              intervention_exposure_short = colDef(name = "Intervention or risk/protective factor"),
              domain = colDef(name = "Domain"),
              subdomain = colDef(name = "Subdomain"),
              subdomain_topic = colDef(name = "Subdomain description"),
              DOI = colDef(name = "DOI",
                           cell = function(value) {
                             htmltools::tags$a(href = value, target = "_blank", value)
                           })
            )
          )
      })
      
      # Output the number of unique records
      
      output$record_count <- renderText({
        paste("Number of unique reviews:", nrow(table_data()))
      })
      
      # Switch tabset panel on click
      
      observeEvent(input$click_details, {
        # use tabsetPanel 'id' argument to change tabs
        updateTabsetPanel(session, "tabset_egm", selected = "included_studies")
      })
      
      }) # observe
    
    }
  )
}

# Run the app to test

ui <- fluidPage(tagList(
  header = tags$head(          includeCSS("../www/css/main.css"),  # Main
                               includeCSS("../www/css/tables.css"),  # tables
                               includeCSS("../www/css/navbar_and_panels.css"), # navbar and notes panel
                               includeCSS("../www/css/buttons.css"), # buttons
                               includeCSS("../www/css/select.css"), # selectors and radio buttons
                               includeCSS("../www/css/popovers.css"), # popovers
                               includeCSS("../www/css/boxes.css"), # boxes
                               includeCSS("../www/css/value_box.css"), # valueBox for headline figures
                               includeCSS("../www/css/info_box.css"), # infoBox for summary page boxes
                               includeCSS("../www/css/js_tree_r.css") # for heirarchical checkboxes
                               
                               )  # CSS stylesheet
), #tagList
  
  selectInput("dataset_input", "Select population of interest:",
              choices = c("Adults", "Children and young people")),
  
  egm_reactable_ui("egm_reactable", dataset = reactive({input$dataset_input}))
)

server <- function(input, output, session) {
  egm_reactable_server("egm_reactable", dataset = reactive({input$dataset_input}))
}

shinyApp(ui, server)