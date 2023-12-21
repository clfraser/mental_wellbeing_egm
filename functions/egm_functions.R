####################### EGM functions #######################

#### function for creating egm plot --------------------------------------------

# Reset filters when button clicked
# Reset from the shinyjs package doesn't reset the tree (nested) checkboxes, so add these separately

observeEvent(input$clear_all_filters_top, {
  shinyjs::reset("filter_panel")
  updateTreeInput(inputId = "pop_characteristics",
                  selected = character(0))
  updateTreeInput(inputId = "intervention_exposure",
                  selected = character(0))
})

## Show modals with definitions for filter options

# Write a function to create appropriate modals
defs_topic_modal <- function(topic){
  defs_filtered <- glossary_list %>%
    filter(Topic == topic) %>%
    select(-Topic)
  
  output$defs_table <- renderTable(defs_filtered)
  
  showModal(modalDialog(
    title = paste(topic, "definitions"),
    tableOutput("defs_table"),
    easyClose = TRUE
  ))
}

# Create observe events for clicking each info button
# Some information is different from the list of definitions in the glossary

# Outcomes information
observeEvent(input$outcome_defs, {
             outcome_defs_filtered <- glossary_list %>%
               filter(Topic == "Outcomes") %>%
               select(-Topic)
             
             output$outcome_defs_table <- renderTable(outcome_defs_filtered)
             
             showModal(modalDialog(
               title = "Outcome definitions",
               "All reviews have a synthesis for some form of self-harm. Selecting 'exclusively non-suicidal self-harm', for example, returns reviews where the results only consider self-harm with the specific measure of non-suicidal self-injury.",
               linebreaks(2),
               tableOutput("outcome_defs_table"),
               easyClose = TRUE))
})

# Population age information
observeEvent(input$pop_age_defs,
             {showModal(modalDialog(
               title = "Population age information",
               "All reviews have some kind of synthesis that applies to young people under 18yrs, but by selecting 'exclusively 0-18 years' you can see reviews where ALL the results are relevant to under 18 years.",
               easyClose = TRUE))})

# All other definitions
observeEvent(input$pop_characteristics_defs, {defs_topic_modal("Population characteristics")})
observeEvent(input$study_setting_defs, {defs_topic_modal("Study setting")})
observeEvent(input$int_exposure_defs, {defs_topic_modal("Interventions and risk/protective factors")})
observeEvent(input$synth_type_defs, {defs_topic_modal("Type of synthesis")})
observeEvent(input$quality_appraisal_defs, {defs_topic_modal("Quality appraisal")})
observeEvent(input$pre_reg_defs, {defs_topic_modal("Pre-registration")})
observeEvent(input$study_design_defs, {defs_topic_modal("Study Design (of reviewed literature)")})

# Filtered dataframe
# Create when the app starts (using the ignoreNULL = FALSE argument), and then only update when the Update filter button is pressed

filtered <- eventReactive(list(input$filter_update_top, input$filter_update_bottom), {
  reviews_chart %>%
    mutate(outcomes_filter = if(is.null(input$outcome)) TRUE else if_else(outcome_definition %in% input$outcome, TRUE, FALSE),
           age_filter = if(is.null(input$pop_age)) TRUE else if_else(age %in% input$pop_age, TRUE, FALSE),
           sub_pop_filter = if(is.null(input$pop_characteristics)) TRUE else if_else(sub_population %in% input$pop_characteristics | ("General population" %in% input$pop_characteristics & is.na(sub_population)), TRUE, FALSE),
           study_setting_filter = if(is.null(input$study_setting_input)) TRUE else if_else(study_setting %in% input$study_setting_input, TRUE, FALSE),
           int_exposure_filter = if(is.null(input$intervention_exposure)) TRUE else if_else(("Risk/protective factor" %in% input$intervention_exposure & intervention_exposure_short == "Risk/protective factor") | (intervention_classification %in% input$intervention_exposure & intervention_exposure_short == "Intervention"), TRUE, FALSE),
           synth_type_filter = if(is.null(input$synth_type_input)) TRUE else if_else(type_of_review %in% input$synth_type_input, TRUE, FALSE),
           qual_appraisal_filter = if_else(input$qual_appraisal_input == "No" | (input$qual_appraisal_input == "Yes" & quality_appraisal == "Yes"), TRUE, FALSE),
           pre_reg_filter = if_else(input$pre_reg_input == "No" | (input$pre_reg_input == "Yes" & pre_registered_protocol == "Yes"), TRUE, FALSE),
           design_filter = if(is.null(input$study_design_input)) TRUE else if_else(design_of_reviewed_studies %in% input$study_design_input, TRUE, FALSE),
           selected = if_else(outcomes_filter + age_filter + sub_pop_filter + study_setting_filter + int_exposure_filter + synth_type_filter +
                                qual_appraisal_filter + pre_reg_filter + design_filter + dummy == 9, 1, 0)) # All of the filter checks are true, but the record isn't a dummy one
}, ignoreNULL = FALSE)

# Create a reactive value to keep the map selection
map_selection <- reactiveVal()

output$egm <- renderReactable({
  ## create EGM plot    
  grouped <- filtered() %>%
    group_by(domain, subdomain, overall_outcome, intervention_exposure_short) %>%
    summarise(count = length(unique(covidence_number[selected == 1]))) %>% # Count the covidence numbers (unique IDs) for studies that have been selected
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
  type_click <- sub("Risk_protective_factor", "Risk/protective factor", sub(".*\\.", "", map_selection()$outcome_and_type))
  
  only_selected <-
    filtered() %>%
    filter(selected == 1)
  
  if(is.null(map_selection()) | is.na(is.null(map_selection()))){
    return(reviews_table %>%
             filter(covidence_number %in% only_selected$covidence_number) %>%
             dplyr::select(study_id, title, aim_of_study, author_conclusions = summary, overall_outcome, outcome_definition, age, overall_population, sub_population, intervention_or_exposure, intervention_classification, study_setting, overall_domain, subdomain, type_of_review, design_of_reviewed_studies, number_of_primary_studies, quality_appraisal, pre_registered_protocol, empty_review, DOI) %>%
             arrange(study_id))
  }
  
  return(reviews_table %>%
           filter(covidence_number %in% only_selected$covidence_number) %>%
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
  type_click <- sub("Risk_protective_factor", "Risk/protective factor", sub(".*\\.", "", map_selection()$outcome_and_type))
})


output$data <- renderReactable({
  
  table_no_dups <- unique(table_data())
  
  table_no_dups %>%
    reactable(
      searchable = TRUE,
      resizable = TRUE,
      filterable = TRUE,
      height = 5000,
      defaultColDef = colDef(
        minWidth = 200),
      rowStyle = function(index) {
        if (table_no_dups[index, "empty_review"] == "Yes" & !is.na(table_no_dups[index, "empty_review"])) {
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
  type_click <- sub("Risk_protective_factor", "Risk/protective factor", sub(".*\\.", "", map_selection()$outcome_and_type))
  
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
