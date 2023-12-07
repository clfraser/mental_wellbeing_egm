####################### EGM functions #######################

#### function for creating egm plot --------------------------------------------

# Reset filters when button clicked
# Reset from the shinyjs package doesn't reset the tree (nested) checkboxes, so add these separately

observeEvent(input$select_all_filters_top, {
  shinyjs::reset("filter_panel")
  updateTreeInput(inputId = "pop_characteristics",
                  selected = c(unique(sub_population$sub_population), "General population"))
  updateTreeInput(inputId = "intervention_exposure",
                  selected = c(unique(intervention_exposure$intervention_classification), "Risk/protective factor"))
})

# Filtered dataframe
# Create when the app starts (using the ignoreNULL = FALSE argument), and then only update when the Update filter button is pressed

filtered <- eventReactive(list(input$filter_update_top, input$filter_update_bottom), {
  reviews_chart %>%
    mutate(selected = 0,
           selected = if_else(  dummy == 0 &
                                  outcome_definition %in% input$outcome_def &
                                  age %in% input$pop_age &
                                  (sub_population %in% input$pop_characteristics | ("General population" %in% input$pop_characteristics & is.na(sub_population))) &
                                  study_setting %in% input$study_setting_input &
                                  (("Risk/protective factor" %in% input$intervention_exposure & intervention_exposure_short == "Risk/protective factor") |
                                     (intervention_classification %in% input$intervention_exposure & intervention_exposure_short == "Intervention")) &
                                  type_of_review %in% input$synth_type_input &
                                  (input$qual_appraisal_input == "No" | (input$qual_appraisal_input == "Yes" & quality_appraisal == "Yes")) &
                                  (input$pre_reg_input == "No" | (input$pre_reg_input == "Yes" & pre_registered_protocol == "Yes")) &
                                  design_of_reviewed_studies %in% input$study_design_input,
                                1,
                                0))
}, ignoreNULL = FALSE)

# Create a reactive value to keep the map selection
map_selection <- reactiveVal()

output$egm <- renderReactable({
  ## create EGM plot    
  grouped <- filtered() %>%
    group_by(domain, subdomain, overall_outcome, intervention_exposure_short) %>%
    summarise(count = length(unique(study_id[selected == 1]))) %>% # Count the study IDs for studies that have been selected
    ungroup()
  
  ## Create a reactable
  
  # Pivot data into the right format
  count_pivot <- grouped %>%
    mutate(overall_outcome = gsub(" |-", "_", overall_outcome),
           intervention_exposure_short = gsub(" |-|/", "_", intervention_exposure_short)) %>% # Replace spaces, slashes and hyphens with underscores for better variable names (hyphens seem to cause problems with grouping columns below)
    pivot_wider(names_from = c(overall_outcome, intervention_exposure_short), values_from = count, names_sep = ".") %>%
    mutate(across(Self_harm.Risk_protective_factor:Outcome_category_3.Risk_protective_factor, ~replace_na(., 0))) %>% # Replace NAs with 0
    select(-Self_harm.NA)
  
  count_pivot %>%
    reactable(
      defaultColDef = colDef(
        align = 'center',
        maxWidth = 150),
      groupBy = "domain",
      defaultExpanded = TRUE,
      bordered = TRUE,
      striped = TRUE,
      onClick = JS("function(rowInfo, column) {
        // Don't handle click events in the domain or subdomain columns
    if (column.id === 'domain' || column.id === 'subdomain') {
      return
    }
    // Send the click event to Shiny, which will be available in input$click_details
    if (window.Shiny) {
      Shiny.setInputValue('click_details', { subdomain: rowInfo.values.subdomain, outcome_and_type: column.id }, { priority: 'event' })
    }
  }"),
      columns = list(
        domain = colDef(name = "Domain",
                        maxWidth = 150,
                        # Render grouped cells without the row count
                        grouped = JS("function(cellInfo) {
                          return cellInfo.value
                          }")),
        subdomain = colDef(name = "Sub-domain",
                           maxWidth = 150),
        Self_harm.Risk_protective_factor = colDef(name = "",
                                    vAlign = "top",
                                    cell = bubble_grid_modified(
                                      data = .,
                                      colors = '#1b9e77',
                                      tooltip = TRUE
                                    )),
        Self_harm.Intervention = colDef(name = "",
                                        vAlign = "bottom",
                                        cell = bubble_grid_modified(
                                          data = .,
                                          colors = '#d95f02',
                                          tooltip = TRUE,
                                          shape = "squares"
                                        )),
        Outcome_category_2.Risk_protective_factor = colDef(name = "",
                                             vAlign = "top",
                                             cell = bubble_grid_modified(
                                               data = .,
                                               colors = '#1b9e77',
                                               tooltip = TRUE
                                             )),
        Outcome_category_2.Intervention = colDef(name = "",
                                                 vAlign = "bottom",
                                                 cell = bubble_grid_modified(
                                                   data = .,
                                                   colors = '#d95f02',
                                                   tooltip = TRUE,
                                                   shape = "squares"
                                                 )),
        Outcome_category_3.Risk_protective_factor = colDef(name = "",
                                             vAlign = "top",
                                             cell = bubble_grid_modified(
                                               data = .,
                                               colors = '#7570b3',
                                               tooltip = TRUE,
                                               shape = "triangles"
                                             )),
        Outcome_category_3.Intervention = colDef(name = "",
                                                 vAlign = "bottom",
                                                 cell = bubble_grid_modified(
                                                   data = .,
                                                   colors = '#d95f02',
                                                   tooltip = TRUE,
                                                   shape = "squares"
                                                 ))
      ),
      columnGroups = list(
        colGroup(name = "Self-harm", columns = c("Self_harm.Risk_protective_factor", "Self_harm.Intervention")),
        colGroup(name = "Outcome category 2", columns = c("Outcome_category_2.Risk_protective_factor", "Outcome_category_2.Intervention")),
        colGroup(name = "Outcome category 3", columns = c("Outcome_category_3.Risk_protective_factor", "Outcome_category_3.Intervention"))
      )
    )
})

observe({
  if(!is.null(input$click_details)){
  map_selection(input$click_details)
  }
})


table_data <- reactive({
  
  outcome_extract <- sub("\\..*", "", map_selection()$outcome_and_type)
  outcome_click <- sub("_", "-", outcome_extract) # The outcome has an underscore in the original but we need it to have a hyphen for filtering
  type_click <- sub(".*\\.", "", map_selection()$outcome_and_type)
  
  only_selected <-
    filtered() %>%
    filter(selected == 1)
  
  if(is.null(map_selection()) | is.na(is.null(map_selection()))){
    return(reviews_table %>%
             filter(study_id %in% only_selected$study_id) %>%
             dplyr::select(study_id, title, aim_of_study, author_conclusions = summary, overall_outcome, outcome_definition, age, overall_population, sub_population, intervention_or_exposure, intervention_classification, study_setting, overall_domain, subdomain, type_of_review, design_of_reviewed_studies, number_of_primary_studies, quality_appraisal, pre_registered_protocol, empty_review, DOI) %>%
             arrange(study_id))
  }
  
  return(reviews_table %>%
           filter(study_id %in% only_selected$study_id) %>%
           arrange(study_id) %>%
           filter(str_detect(subdomain, input$click_details$subdomain) &
                    overall_outcome == outcome_click &
                    str_detect(intervention_or_exposure, type_click)) %>%
           dplyr::select(study_id, title, aim_of_study, author_conclusions = summary, overall_outcome, outcome_definition, age, overall_population, sub_population, intervention_or_exposure, intervention_classification, study_setting, overall_domain, subdomain, type_of_review, design_of_reviewed_studies, number_of_primary_studies, quality_appraisal, pre_registered_protocol, empty_review, DOI)
  )
})

# Output

output$print_click_details <- reactive({
  outcome_extract <- sub("\\..*", "", map_selection()$outcome_and_type)
  outcome_click <- sub("_", "-", outcome_extract) # The outcome has an underscore in the original but we need it to have a hyphen for filtering
  type_click <- sub(".*\\.", "", map_selecion()$outcome_and_type)
  
})


output$data <- renderReactable({
  
  table_no_dups <- unique(table_data())
  
  table_no_dups %>%
    reactable(
      searchable = TRUE,
      resizable = TRUE,
      filterable = TRUE,
      defaultColDef = colDef(
        minWidth = 200),
      rowStyle = function(index) {
        if (table_no_dups[index, "empty_review"] == "Yes") {
          list(background = "rgba(0, 0, 0, 0.05)")
        }
      },
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
        aim_of_study = colDef(name = "Aim of study",
                              minWidth = 300),
        author_conclusions = colDef(minWidth = 400,
                                    name = "Author conclusions"),
        overall_outcome = colDef(name = "Overall outcome"),
        outcome_definition = colDef(name = "Outcome definition"),
        age = colDef(name = "Age of children or young people"),
        overall_population = colDef(name = "Population category"),
        sub_population = colDef(name = "Sub-population of children or young people"),
        intervention_or_exposure = colDef(name = "Intervention or risk/protective factor"),
        intervention_classification = colDef(name = "Type of intervention"),
        study_setting = colDef(name = "Study setting"),
        overall_domain = colDef(name = "Domain"),
        subdomain = colDef(name = "Subdomain"),
        type_of_review = colDef(name = "Type of review"),
        design_of_reviewed_studies = colDef(name = "Design of reviewed studies"),
        number_of_primary_studies = colDef(name = "Number of primary studies"),
        quality_appraisal = colDef(name = "Does review assess quality of reviewed studies?"),
        pre_registered_protocol = colDef(name = "Does review have a pre-registered protocol?"),
        empty_review = colDef(name = "Empty review"),
        DOI = colDef(name = "DOI",
                     cell = function(value) {
                       htmltools::tags$a(href = value, target = "_blank", value)
                     })
      )
    )
})


# Note: Rewrite code to only get click details once and use in multiple expressions
output$print_click_details <- renderUI({
  outcome_extract <- sub("\\..*", "", map_selection()$outcome_and_type)
  outcome_click <- sub("_", "-", outcome_extract) # The outcome has an underscore in the original but we need it to have a hyphen for filtering
  type_click <- sub(".*\\.", "", map_selection()$outcome_and_type)
  
  if(!length(map_selection())){
    return("No selection from EGM")
  }
  return(HTML(paste0("From the EGM, you have selected:", "<br/>", "  Subdomain: ", map_selection()$subdomain, "<br/>", "  Outcome: ", outcome_click, "<br/>", "  Type: ", type_click)))
  
})

# Switch tabset panel on click

observeEvent(input$click_details, {
  # use tabsetPanel 'id' argument to change tabs
  updateTabsetPanel(session, "tabset", selected = "table")
})

# Clear map selection when button pressed
observeEvent(input$reset_map_selection, map_selection(NULL))
