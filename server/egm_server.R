####################### EGM server #######################

# # initialise the Cicerone guide
egm_guide$init()

observeEvent(input$egm_guide_button, {
  # Switch to the EGM panel to show walkthrough
  updateTabsetPanel(session, "tabset_egm", selected = "EGM")
  # Use 'delay' to only start the walkthrough once on the EGM panel has been selected
  delay(5, egm_guide$start())
})

# Switch tab on video link button click

observeEvent(input$video_link_button, {
  output$text <- renderUI(intro_use_ui)
  updateTabsetPanel(session, "tabset_navbar", selected = "intro")
})

## Render trees for jsTreeR inputs
output$outcome_tree <- renderJstree({
  jstree(
    outcome_nodes,
    checkboxes = TRUE
  )
})

output$domains_tree <- renderJstree({
  jstree(
    domain_subs_nodes,
    checkboxes = TRUE
  )
})

output$age_tree <- renderJstree({
  jstree(
    age_nodes,
    checkboxes = TRUE
  )
})

output$intervention_risk_tree <- renderJstree({
  jstree(
    intervention_risk_nodes,
    checkboxes = TRUE
  )
})

output$sub_pop_tree <- renderJstree({
  jstree(
    sub_pop_nodes,
    checkboxes = TRUE
  )
})

#### Create EGM plot  --------------------------------------------

## Reactive values

# Define full dataframe (can't seem to define filtered dataframe since it's reactive)

full_dataframe <- reviews_chart %>%
                  mutate(selected = if_else(dummy == 0, 1, 0))

# Create a reactive value for chart data and set it as the full dataframe initially

chart_data <- reactiveVal()
chart_data(full_dataframe)

# Reset filters when button clicked
# Reset from the shinyjs package doesn't reset the tree (hierarchical) checkboxes, so add these separately

observeEvent(input$clear_all_filters_top, {
  shinyjs::reset("filter_panel")
  jstreeUpdate(session, "outcome_tree", clear_tree(outcome_df, "first_level", "second_level"))
  jstreeUpdate(session, "domains_tree", clear_tree(domains_df, "domain", "subdomain"))
  jstreeUpdate(session, "age_tree", clear_tree(age_df, "first_level", "second_level"))
  jstreeUpdate(session, "intervention_risk_tree", clear_tree(intervention_risk_df, "intervention_exposure_short", "intervention_classification"))
  jstreeUpdate(session, "sub_pop_tree", clear_tree(sub_pop_df, "overall_population", "sub_population"))
  
  chart_data(full_dataframe)
})

## Show modals with definitions for filter options

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

# Domains and subdomains information
observeEvent(input$domains_defs, {
  domain_defs_overall <- glossary_list %>%
    filter(Topic == "Domains and subdomains - overall") %>%
    select(-Topic)
  
  domain_defs_individual <- glossary_list %>%
    filter(Topic == "Domains and subdomains - individual") %>%
    select(-Topic)
  
  domain_defs_family <- glossary_list %>%
    filter(Topic == "Domains and subdomains - family and friends") %>%
    select(-Topic)
  
  domain_defs_learning <- glossary_list %>%
    filter(Topic == "Domains and subdomains - learning environment") %>%
    select(-Topic)
  
  domain_defs_community <- glossary_list %>%
    filter(Topic == "Domains and subdomains - community") %>%
    select(-Topic)
  
  domain_defs_structural <- glossary_list %>%
    filter(Topic == "Domains and subdomains - structural") %>%
    select(-Topic)
  
  output$domain_defs_overall_table <- renderTable(domain_defs_overall)
  output$domain_defs_individual_table <- renderTable(domain_defs_individual)
  output$domain_defs_family_table <- renderTable(domain_defs_family)
  output$domain_defs_learning_table <- renderTable(domain_defs_learning)
  output$domain_defs_community_table <- renderTable(domain_defs_community)
  output$domain_defs_structural_table <- renderTable(domain_defs_structural)
  
  showModal(modalDialog(
    title = "Domain and subdomain definitions",
    "Determinants of mental health are a range of factors that influence our mental health and wellbeing. The EGM uses the Public Health Scotland mental health indicator framework to map how the circumstances in which children and young people live shape their mental health outcomes. The determinants are grouped according to domains and subdomains.",
    linebreaks(2),
    tags$b("Overall definitions"),
    tableOutput("domain_defs_overall_table"),
    tags$b("Individual subdomain definitions"),
    tableOutput("domain_defs_individual_table"),
    tags$b("Family and friends subdomain definitions"),
    tableOutput("domain_defs_family_table"),
    tags$b("Learning environment subdomain definitions"),
    tableOutput("domain_defs_learning_table"),
    tags$b("Community subdomain definitions"),
    tableOutput("domain_defs_community_table"),
    tags$b("Structural subdomain definitions"),
    tableOutput("domain_defs_structural_table"),
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
observeEvent(input$empty_defs, {defs_topic_modal("Empty review")}) # Empty reviews, on 'Included reviews' tab

# Filtered dataframe

observeEvent(input$filter_update_top, {
  chart_data(reviews_chart %>%
               mutate(outcomes_filter = if(is.null(unlist(input$outcome_tree_selected)) | "Any form of self-injurious thoughts and behaviours" %in% unlist(input$outcome_tree_selected)) TRUE else if_else(outcome_definition %in% unlist(input$outcome_tree_selected), TRUE, FALSE),
                      domains_filter = if(is.null(unlist(input$domains_tree_selected))) TRUE else if_else(subdomain %in% unlist(input$domains_tree_selected), TRUE, FALSE),
                      age_filter = if(is.null(unlist(input$age_tree_selected)) | "All ages" %in% unlist(input$age_tree_selected)) TRUE else if_else(age %in% unlist(input$age_tree_selected), TRUE, FALSE),
                      sub_pop_filter = if(is.null(unlist(input$sub_pop_tree_selected))) TRUE else if_else(sub_population %in% unlist(input$sub_pop_tree_selected) | ("General population" %in% unlist(input$sub_pop_tree_selected) & is.na(sub_population)), TRUE, FALSE),
                      study_setting_filter = if(is.null(input$study_setting_input)) TRUE else if_else(study_setting %in% input$study_setting_input, TRUE, FALSE),
                      int_exposure_filter = if(is.null(unlist(input$intervention_risk_tree_selected))) TRUE else if_else(("Risk/protective factor" %in% unlist(input$intervention_risk_tree_selected) & intervention_exposure_short == "Risk/protective factor") | (intervention_classification %in% unlist(input$intervention_risk_tree_selected) & intervention_exposure_short == "Intervention"), TRUE, FALSE),
                      synth_type_filter = if(is.null(input$synth_type_input)) TRUE else if_else(type_of_review %in% input$synth_type_input, TRUE, FALSE),
                      qual_appraisal_filter = if_else(input$qual_appraisal_input == "No" | (input$qual_appraisal_input == "Yes" & quality_appraisal == "Yes"), TRUE, FALSE),
                      pre_reg_filter = if_else(input$pre_reg_input == "No" | (input$pre_reg_input == "Yes" & pre_registered_protocol == "Yes"), TRUE, FALSE),
                      selected = if_else(outcomes_filter + domains_filter + age_filter + sub_pop_filter + study_setting_filter + int_exposure_filter + synth_type_filter +
                                           qual_appraisal_filter +
                                           pre_reg_filter  + dummy == 9, 1, 0))) # All of the filter checks are true, but the record isn't a dummy one
  })

# Print out chart data for debugging
#output$chart_data <- renderTable(chart_data())

# Get data into the right format for the EGM
count_pivot <- reactive({
  chart_data() %>%
    group_by(domain, subdomain, overall_outcome, intervention_exposure_short) %>%
    summarise(count = length(unique(covidence_number[selected == 1]))) %>% # Count the covidence numbers (unique IDs) for studies that have been selected
    ungroup() %>%
    mutate(overall_outcome = gsub(" |-", "_", overall_outcome),
           intervention_exposure_short = gsub(" |-|/", "_", intervention_exposure_short)) %>% # Replace spaces, slashes and hyphens with underscores for better variable names (hyphens seem to cause problems with grouping columns below)
    pivot_wider(names_from = c(overall_outcome, intervention_exposure_short), values_from = count, names_sep = ".") %>%
    mutate(across(everything(), ~replace_na(., 0))) %>% # Replace NAs with 0
    arrange(domain) %>%
    mutate(padding = "") %>%
    select(padding, domain, subdomain, Self_harm.Risk_protective_factor, Self_harm.Intervention)
})

# Create aggregated EGM table

egm_aggregated_count <- reactive({
  chart_data() %>%
    group_by(domain, overall_outcome, intervention_exposure_short) %>%
    summarise(count = length(unique(covidence_number[selected == 1]))) %>% # Count the covidence numbers (unique IDs) for studies that have been selected
    ungroup() %>%
    mutate(overall_outcome = gsub(" |-", "_", overall_outcome),
           intervention_exposure_short = gsub(" |-|/", "_", intervention_exposure_short)) %>% # Replace spaces, slashes and hyphens with underscores for better variable names (hyphens seem to cause problems with grouping columns below)
    pivot_wider(names_from = c(overall_outcome, intervention_exposure_short), values_from = count, names_sep = ".") %>%
    mutate(across(everything(), ~replace_na(., 0))) %>% # Replace NAs with 0
    arrange(domain) %>%
    mutate(subdomain = "") %>%
    select(domain, subdomain, Self_harm.Risk_protective_factor, Self_harm.Intervention)
})

# Create table with both aggregated and disaggregated data

egm_agg_disag <- reactive({
  count_pivot() %>%
    select(-padding) %>%
    rbind(egm_aggregated_count())
})

# Create EGM plot

output$egm <- renderReactable({

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
    Self_harm.Risk_protective_factor = colDef(name = "Risk/protective factor",
                                    cell = bubble_grid_modified(
                                      data = egm_agg_disag(),
                                      colors = '#83BB26',
                                      tooltip = TRUE,
                                      shape = "squares"
                                    )),
    Self_harm.Intervention = colDef(name = "Intervention",
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
                Self_harm.Risk_protective_factor = colDef(name = "",
                                                vAlign = "bottom",
                                                cell = bubble_grid_modified(
                                                  data = egm_agg_disag(),
                                                  colors = '#83BB26',
                                                  tooltip = TRUE,
                                                  shape = "squares"
                                                )),
                Self_harm.Intervention = colDef(name = "",
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
  
# Get click details from map

outcome_click <- reactive({sub("_", "-", sub("\\..*", "", input$click_details$outcome_and_type))})
type_click <- reactive({sub("Risk_protective_factor", "Risk/protective factor", sub(".*\\.", "", input$click_details$outcome_and_type))})

# When the map is clicked, select the relevant filters and then filter the dataframe

observeEvent(input$click_details, {
  jstreeUpdate(session,
               "outcome_tree",
               outcome_df %>%
                 mutate(selected = TRUE) %>% # When there is more than one outcome, this will need to change
                 create_nodes_from_df("first_level", "second_level")
  )
  jstreeUpdate(session,
               "domains_tree",
               domains_df %>%
                 mutate(selected = case_when(input$click_details$subdomain == "" & input$click_details$domain == domain ~ TRUE,
                                             input$click_details$subdomain == subdomain ~ TRUE,
                                             .default = FALSE)) %>%
                 create_nodes_from_df("domain", "subdomain"))
  jstreeUpdate(session,
               "intervention_risk_tree",
               intervention_risk_df %>%
                 mutate(selected = if_else(intervention_exposure_short == type_click(), TRUE, FALSE)) %>%
                 create_nodes_from_df("intervention_exposure_short", "intervention_classification"))
  chart_data(reviews_chart %>%
               mutate(outcomes_filter = TRUE, # Would need to be more sophisticated if other outcomes
                      domains_filter = 
                        if (input$click_details$subdomain == "")
                                if_else(domain %in% input$click_details$domain, TRUE, FALSE)
                       else
                                if_else(subdomain %in% input$click_details$subdomain, TRUE, FALSE),
                      int_exposure_filter = if_else((type_click() == "Risk/protective factor" & intervention_exposure_short == "Risk/protective factor") | (type_click() == "Intervention" & intervention_exposure_short == "Intervention"), TRUE, FALSE),
                      age_filter = if(is.null(unlist(input$age_tree_selected)) | "All ages" %in% unlist(input$age_tree_selected)) TRUE else if_else(age %in% unlist(input$age_tree_selected), TRUE, FALSE),
                      sub_pop_filter = if(is.null(unlist(input$sub_pop_tree_selected))) TRUE else if_else(sub_population %in% unlist(input$sub_pop_tree_selected) | ("General population" %in% unlist(input$sub_pop_tree_selected) & is.na(sub_population)), TRUE, FALSE),
                      study_setting_filter = if(is.null(input$study_setting_input)) TRUE else if_else(study_setting %in% input$study_setting_input, TRUE, FALSE),
                      synth_type_filter = if(is.null(input$synth_type_input)) TRUE else if_else(type_of_review %in% input$synth_type_input, TRUE, FALSE),
                      qual_appraisal_filter = if_else(input$qual_appraisal_input == "No" | (input$qual_appraisal_input == "Yes" & quality_appraisal == "Yes"), TRUE, FALSE),
                      pre_reg_filter = if_else(input$pre_reg_input == "No" | (input$pre_reg_input == "Yes" & pre_registered_protocol == "Yes"), TRUE, FALSE),
                      selected = if_else(outcomes_filter + domains_filter + age_filter + sub_pop_filter + study_setting_filter + int_exposure_filter + synth_type_filter + qual_appraisal_filter + pre_reg_filter +
                                          dummy == 9, 1, 0))) # All of the filter checks are true, but the record isn't a dummy one
})

table_data <- reactive({
  
  only_selected <-
    chart_data() %>%
    filter(selected == 1)
  
  reviews_table %>%
             filter(covidence_number %in% only_selected$covidence_number) %>%
             dplyr::select(study_id, title, aim_of_study, author_conclusions = summary, overall_outcome, outcome_definition, age, overall_population, sub_population, intervention_or_exposure, intervention_classification, study_setting, overall_domain, subdomain, type_of_review, design_of_reviewed_studies, number_of_primary_studies, quality_appraisal, pre_registered_protocol, empty_review, DOI) %>%
             arrange(study_id)
})

# Table with numbers for the EGM, rather than shapes

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
      #height = 2050,
      columns = list(
        domain = colDef(name = "Domain",
                        width = 150),
        subdomain = colDef(name = "Sub-domain",
                           width = 150),
        Self_harm.Risk_protective_factor = colDef(name = "Risk/protective factor"),
        Self_harm.Intervention = colDef(name = "Intervention")
      ),
      columnGroups = list(
        colGroup(name = "Self-harm", columns = c("Self_harm.Risk_protective_factor", "Self_harm.Intervention"))
      )
    )
})

# Show numeric table on click

observeEvent(input$show_egm_numbers, {
  showModal(modalDialog(
    id = "show_egm_numbers_modal",
    title = "Evidence and gap map counts",
    csvDownloadButton("egm_numbers", filename = "egm_counts.csv"), # To download table as a CSV (defined in core functions script)
    linebreaks(1),
    p("This table can be filtered by using the filters on the main page. Change the filters and click back on 'Show EGM as text' to get an updated table."),
    linebreaks(1),
    reactableOutput("egm_numbers"),
    easyClose = TRUE,
    size = "l"))
})

# Output data for table

output$reviews_table <- renderReactable({
  
  table_no_dups <- unique(table_data())
  
  table_no_dups %>%
    reactable(
      searchable = TRUE,
      resizable = TRUE,
      filterable = TRUE,
      height = 1000,
      defaultColDef = colDef(
        minWidth = 200),
      rowStyle = function(index) {
        if (table_no_dups[index, "empty_review"] == "Yes" & !is.na(table_no_dups[index, "empty_review"])) {
          list(background = "rgba(210, 210, 210, 0.97)")
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

# Output the number of unique records

output$record_count <- renderText({
  paste("Number of unique reviews:", nrow(table_data()))
})

# Switch tabset panel on click

observeEvent(input$click_details, {
  # use tabsetPanel 'id' argument to change tabs
  updateTabsetPanel(session, "tabset_egm", selected = "included_reviews")
})
