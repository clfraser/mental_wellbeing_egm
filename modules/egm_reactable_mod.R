#####################################.
# egm_reactable_mod.R
# This module creates the EGM chart using the reactable package
#####################################.

#######################################################
## MODULE UI ----
#######################################################

egm_reactable_ui <- function(id, dataset){
  ns <- NS(id)
  
  tagList(
    
    #######################################################
    ## Guided tour ----
    #######################################################
    
    fluidPage(
      use_cicerone(), # Include Cicerone to give a guide of the page
      actionButton(ns("egm_guide_button"), "Click here for a guided tour of the page"),
      linebreaks(2),
      # Set checkbox colour
      tags$head(tags$style("input[type=checkbox] { accent-color: DodgerBlue; }")),
    
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
    ) # fluidPage
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
      ## Guided tour ----
      #######################################################
      
      # Set up steps for guide
      
      egm_guide <- Cicerone$
        new()$
        step(
          el = ns("egm_chart"),
          position = "top",
          title = "Evidence and gap map",
          description = "The size of the shapes correspond to the number of studies. Hover over each shape to see the number of studies.
    Click the shape to see a table with study details."
        )$
        step(
          ns("show_egm_numbers"),
          "See EGM as a table",
          "See a table of the number of studies included in the EGM. This takes account of any filters applied. You can download the table as a CSV. The visual EGM (below) is not screenreader accessible, so please use this button for an accessible version of the table."
        )$
        step(
          ns("all_filters"),
          "Filters",
          "Change the filters to find studies relevant to you."
        )$
        step(
          ns("filter_update_top"),
          "Update filters",
          "Click the update filters button to update the EGM and the table of studies."
        )$
        step(
          ns("clear_all_filters_top"),
          "Clear filters",
          "Clear all selected filters."
        )
      
      # # initialise the Cicerone guide
      egm_guide$init()
      
      observeEvent(input$egm_guide_button, {
        # Switch to the EGM panel to show walkthrough
        updateTabsetPanel(session, "tabset_egm", selected = ns("EGM"))
        # Use 'delay' to only start the walkthrough once on the EGM panel has been selected
        delay(5, egm_guide$start())
      })
      
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
      ## Filter definition buttons ----
      #######################################################
      
      # Domains and subdomains information
      observeEvent(input$domains_defs, {
        if(dataset() == "Children and young people"){
        
        domain_defs_overall <- glossary_list %>%
          filter(Topic == "Domains and subdomains - overall") %>%
          select(-Topic, -Population)
        
        domain_defs_individual <- glossary_list %>%
          filter(Topic == "Domains and subdomains - individual", Population == "CYP") %>%
          select(-Topic, -Population)
        
        domain_defs_family <- glossary_list %>%
          filter(Topic == "Domains and subdomains - family and friends", Population == "CYP") %>%
          select(-Topic, -Population)
        
        domain_defs_learning <- glossary_list %>%
          filter(Topic == "Domains and subdomains - learning environment", Population == "CYP") %>%
          select(-Topic, -Population)
        
        domain_defs_community <- glossary_list %>%
          filter(Topic == "Domains and subdomains - community", Population == "CYP") %>%
          select(-Topic, -Population)
        
        domain_defs_structural <- glossary_list %>%
          filter(Topic == "Domains and subdomains - structural", Population == "CYP") %>%
          select(-Topic, -Population)
        
        output$domain_defs_overall_table <- renderTable(domain_defs_overall)
        output$domain_defs_individual_table <- renderTable(domain_defs_individual)
        output$domain_defs_family_table <- renderTable(domain_defs_family)
        output$domain_defs_learning_table <- renderTable(domain_defs_learning)
        output$domain_defs_community_table <- renderTable(domain_defs_community)
        output$domain_defs_structural_table <- renderTable(domain_defs_structural)
        
        showModal(modalDialog(
          title = "Domain and subdomain definitions: Children and young people",
          "Determinants of mental health are a range of factors that influence our mental health and wellbeing. The EGM uses the Public Health Scotland mental health indicator framework to map how the circumstances in which children and young people live shape their mental health outcomes. The determinants are grouped according to domains and subdomains.",
          linebreaks(2),
          tags$b("Overall definitions"),
          tableOutput(ns("domain_defs_overall_table")),
          tags$b("Individual subdomain definitions"),
          tableOutput(ns("domain_defs_individual_table")),
          tags$b("Family and friends subdomain definitions"),
          tableOutput(ns("domain_defs_family_table")),
          tags$b("Learning environment subdomain definitions"),
          tableOutput(ns("domain_defs_learning_table")),
          tags$b("Community subdomain definitions"),
          tableOutput(ns("domain_defs_community_table")),
          tags$b("Structural subdomain definitions"),
          tableOutput(ns("domain_defs_structural_table")),
          easyClose = TRUE))
        
        } else{
          
          domain_defs_overall <- glossary_list %>%
            filter(Topic == "Domains and subdomains - overall") %>%
            select(-Topic, -Population)
          
          domain_defs_individual <- glossary_list %>%
            filter(Topic == "Domains and subdomains - individual", Population == "Adults") %>%
            select(-Topic, -Population)
          
          domain_defs_community <- glossary_list %>%
            filter(Topic == "Domains and subdomains - community", Population == "Adults") %>%
            select(-Topic, -Population)
          
          domain_defs_structural <- glossary_list %>%
            filter(Topic == "Domains and subdomains - structural", Population == "Adults") %>%
            select(-Topic, -Population)
          
          output$domain_defs_overall_table <- renderTable(domain_defs_overall)
          output$domain_defs_individual_table <- renderTable(domain_defs_individual)
          output$domain_defs_community_table <- renderTable(domain_defs_community)
          output$domain_defs_structural_table <- renderTable(domain_defs_structural)
          
          showModal(modalDialog(
            title = "Domain and subdomain definitions: Adults",
            "Determinants of mental health are a range of factors that influence our mental health and wellbeing. The EGM uses the Public Health Scotland mental health indicator framework to map how the circumstances in which people live shape their mental health outcomes. The determinants are grouped according to domains and subdomains.",
            linebreaks(2),
            tags$b("Overall definitions"),
            tableOutput(ns("domain_defs_overall_table")),
            tags$b("Individual subdomain definitions"),
            tableOutput(ns("domain_defs_individual_table")),
            tags$b("Community subdomain definitions"),
            tableOutput(ns("domain_defs_community_table")),
            tags$b("Structural subdomain definitions"),
            tableOutput(ns("domain_defs_structural_table")),
            easyClose = TRUE))
          
        }
      })
      
      # Interventions and risk factors information
      
      observeEvent(input$int_exposure_defs, {
        defs_filtered <- glossary_list %>%
          filter(Topic == "Interventions and risk/protective factors") %>%
          select(-Topic, - Population)
        
        output$defs_table <- renderTable(defs_filtered)
        
        showModal(modalDialog(
          title = paste("Interventions and risk/protective factors definitions"),
          tableOutput(ns("defs_table")),
          easyClose = TRUE
        ))
      })
      
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
      
      # Create JavaScript function to store details when the user clicks the EGM
      # First, create click_details_name, which is a namespaced input
      click_details_name <- ns("click_details")
      
      # Then, use paste0 to include the click_details_name variable in the JavaScript function
      get_click_data <- JS(paste0("function(rowInfo, column) {
        // Don't handle click events in the domain or subdomain columns
    if (column.id === 'domain' || column.id === 'subdomain') {
      return
    }
    // Send the click event to Shiny, which will be available in input$click_details
    if (window.Shiny) {
      Shiny.setInputValue('", click_details_name,"', { domain: rowInfo.values.domain, subdomain: rowInfo.values.subdomain, outcome_and_type: column.id }, { priority: 'event' })
    }
  }"))

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
                      onClick = get_click_data, # Function defined above
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
      ## Responding to user click on EGM ----
      #######################################################
      
      # Get click details from map
      
      outcome_click <- reactive({sub("_", "-", sub("\\..*", "", input$click_details$outcome_and_type))})
      type_click <- reactive({sub("Risk_protective_factor", "Risk/protective factor", sub(".*\\.", "", input$click_details$outcome_and_type))})

      # When the map is clicked, select the relevant filters and then filter the dataframe

      observeEvent(input$click_details, {
        # Update domains tree regardless of what's in the current filter
        jstreeUpdate(session,
                     ns("domains_tree"),
                     domains_df() %>%
                       mutate(selected = case_when(input$click_details$subdomain == "" & input$click_details$domain == domain ~ TRUE,
                                                   input$click_details$subdomain == subdomain ~ TRUE,
                                                   .default = FALSE)) %>%
                       create_nodes_from_df("domain", "subdomain"))
        # Only update intervention/risk input if nothing is currently selected in the filter
        if(is.null(input$intervention_risk_input)){
          updateCheckboxGroupInput(
            inputId = "intervention_risk_input",
            selected = type_click()
          )
        }
        # Use delay to give filters time to update before dataframe is updated
        delay(400,
              chart_data(selected_dataset() %>%
                           mutate(domains_filter = if(is.null(unlist(input$domains_tree_selected))) TRUE else if_else(subdomain %in% unlist(input$domains_tree_selected), TRUE, FALSE),
                                  intervention_risk_filter = if(is.null(input$intervention_risk_input)) TRUE else if_else(intervention_exposure_short %in% input$intervention_risk_input, TRUE, FALSE),
                                  selected = if_else(domains_filter + intervention_risk_filter
                                                     + dummy == 2, 1, 0))) # All of the filter checks are true, but the record isn't a dummy one
        )
       })

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
        paste("Number of unique studies:", nrow(table_data()))
      })
      
      # Switch tabset panel on click
      
      observeEvent(input$click_details, {
        # use tabsetPanel 'id' argument to change tabs
        updateTabsetPanel(session, "tabset_egm", selected = ns("included_studies"))
      })
      
      }) # observe
    
    }
  )
}