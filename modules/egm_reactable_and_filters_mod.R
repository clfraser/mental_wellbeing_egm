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
          reactableOutput(ns("egm_chart"))
        )
}

#######################################################
## MODULE SERVER ----
#######################################################

egm_reactable_server <- function(id, dataset) {
  moduleServer(
    id,
    
    function(input, output, session){
      
      # Set the appropriate dataset, depending on the user input
    
      selected_dataset <- reactive({
        switch({dataset()},
               "Adults" = wemwebs_adult_chart_data,
               "Children and young people" = wemwebs_cyp_chart_data)
      })
      
      # Set chart data to the full dataframe initially, and add a selected column
      full_dataframe <- reactive({selected_dataset() %>%
            mutate(selected = if_else(dummy == 0, 1, 0))})
      
      # Set chart_data to full_dataframe
      chart_data <- reactive({full_dataframe()})
       
      # Get data into the right format for the EGM
      count_pivot <- reactive({
        chart_data() %>%
          group_by(domain, subdomain, overall_outcome, intervention_exposure_short) %>%
          summarise(count = length(unique(id_number[selected == 1]))) %>% # Count the covidence numbers (unique IDs) for studies that have been selected
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
          summarise(count = length(unique(id_number[selected == 1]))) %>% # Count the ID numbers (unique IDs) for studies that have been selected
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

      # Create EGM plot

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