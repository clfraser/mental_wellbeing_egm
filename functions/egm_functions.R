####################### EGM functions #######################

#### function for creating egm plot --------------------------------------------

# Filtered dataframe
# Create when the app starts (using the ignoreNULL = FALSE argument), and then only update when the Update filter button is pressed

filtered <- eventReactive(input$filter_update, {
  reviews_chart %>%
    mutate(selected = 0,
           selected = if_else(  dummy == 0 &
                                subdomain %in% input$dom_sub &
                                outcome_definition %in% input$outcome &
                                type_of_review %in% input$review_type_input &
                                intervention_exposure_short %in% input$intervention_exposure &
                                (input$RCT == "No" | input$RCT == "Yes" & design_of_reviewed_studies == "Randomised control trials"),
                              1,
                              0))
}, ignoreNULL = FALSE)

  
output$egm <- renderReactable({
    ## create EGM plot    
    grouped <- filtered() %>%
      group_by(domain, subdomain, overall_outcome, intervention_exposure_short) %>%
      summarise(count = sum(selected)) %>%
      ungroup()
    
    ## Create a reactable
    
    # Pivot data into the right format
    count_pivot <- grouped %>%
      mutate(overall_outcome = gsub(" |-", "_", overall_outcome)) %>% # Replace spaces and hyphens with underscores for better variable names (hyphens seem to cause problems with grouping columns below)
      pivot_wider(names_from = c(overall_outcome, intervention_exposure_short), values_from = count, names_sep = ".") %>%
      mutate(across(Self_harm.Exposure:Outcome_category_3.Exposure, ~replace_na(., 0))) %>% # Replace NAs with 0
      select(-Self_harm.NA) # Remove column for dummy sub-domains
    
    count_pivot %>%
      reactable(
        defaultColDef = colDef(
          align = 'center',
          maxWidth = 60),
        groupBy = "domain",
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
                          maxWidth = 150),
          subdomain = colDef(name = "Sub-domain",
                          maxWidth = 150),
          Self_harm.Exposure = colDef(name = "",
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
          Self_harm.Attitudes = colDef(name = "",
                                       vAlign = "top",
                                       cell = bubble_grid_modified(
                                         data = .,
                                         colors = '#7570b3',
                                         tooltip = TRUE,
                                         shape = "triangles"
                                       )),
          Outcome_category_2.Exposure = colDef(name = "",
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
          Outcome_category_3.Exposure = colDef(name = "",
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
          colGroup(name = "Self-harm", columns = c("Self_harm.Exposure", "Self_harm.Intervention", "Self_harm.Attitudes")),
          colGroup(name = "Outcome category 2", columns = c("Outcome_category_2.Exposure", "Outcome_category_2.Intervention")),
          colGroup(name = "Outcome category 3", columns = c("Outcome_category_3.Exposure", "Outcome_category_3.Intervention"))
        )
      )
  })

  
table_data <- reactive({

    outcome_extract <- sub("\\..*", "", input$click_details$outcome_and_type)
    outcome_click <- sub("_", "-", outcome_extract) # The outcome has an underscore in the original but we need it to have a hyphen for filtering
    type_click <- sub(".*\\.", "", input$click_details$outcome_and_type)

    if (!length(input$click_details)){
      return(filtered() %>%
               select(study_id, title, aim_of_study, "Author conclusions" = summary, overall_outcome, outcome_definition, age, intervention_or_exposure, study_setting, overall_domain, subdomain, type_of_review, design_of_reviewed_studies, number_of_primary_studies) %>%
               arrange(study_id)) #%>%
               #clean_names(., case = "title"))
    }
    return(filtered() %>%
             select(study_id, title, aim_of_study, "Author conclusions" = summary, overall_outcome, outcome_definition, age, intervention_or_exposure, study_setting, overall_domain, subdomain, type_of_review, design_of_reviewed_studies, number_of_primary_studies) %>%
             arrange(study_id) %>%
             filter(str_detect(subdomain, input$click_details$subdomain) &
                      overall_outcome == outcome_click &
                      str_detect(intervention_or_exposure, type_click)
             )) #%>%
             #clean_names(., case = "title"))
    
  })


  output$print_click_details <- reactive({
    outcome_extract <- sub("\\..*", "", input$click_details$outcome_and_type)
    outcome_click <- sub("_", "-", outcome_extract) # The outcome has an underscore in the original but we need it to have a hyphen for filtering
    type_click <- sub(".*\\.", "", input$click_details$outcome_and_type)
    
  })

# Output
output$data <- renderReactable({
  id_and_title <- unique(table_data()[, c("study_id", "title")])
  
  id_and_title %>%
    reactable(searchable = TRUE,
              details = function(index) {
      more_columns <- table_data()[table_data()$study_id == id_and_title$study_id[index], ] %>%
        clean_names(., case = "title")
      htmltools::div(style = "padding: 1rem",
                     reactable(more_columns, outlined = TRUE)
      )
})
})

# Note: Rewrite code to only get click details once and use in multiple expressions
output$print_click_details <- renderText({
  outcome_extract <- sub("\\..*", "", input$click_details$outcome_and_type)
  outcome_click <- sub("_", "-", outcome_extract) # The outcome has an underscore in the original but we need it to have a hyphen for filtering
  type_click <- sub(".*\\.", "", input$click_details$outcome_and_type)
  
   if(!length(input$click_details)){
     return("No selection from EGM")
   }
  return(paste0("You have selected: Subdomain: ", input$click_details$subdomain, ", Outcome: ", outcome_click, ", Type: ", type_click))
  
})
  
# Switch tabset panel on click

observeEvent(input$click_details, {
  # use tabsetPanel 'id' argument to change tabs
    updateTabsetPanel(session, "tabset", selected = "table")
})
