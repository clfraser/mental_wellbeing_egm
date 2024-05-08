####################### Main tab UI #######################

mainTab <- tabPanel(
  lang = "en",
  div(
    div(class="fa fa-map", 
        role = "navigation"), "Evidence and gap map"), # wrap in div for screenreader / accessibility purposes 
  value = "main_page", # tab ID
  titlePanel(h1("Evidence and gap map")),
  useShinyjs(),
  fluidPage(
    use_cicerone(), # Include Cicerone to give a guide of the page
    actionButton("egm_guide_button", "Click here for a guided tour of the page"),
    linebreaks(2),
    actionButton("video_link_button", "Click here to see walkthrough videos"),
    # Set checkbox colour
    tags$head(tags$style("input[type=checkbox] { accent-color: DodgerBlue; }")),
    ## Page titles
    titlePanel(
      "Self-harm in children and young people"),
    ## Inputs
    sidebarLayout(
      sidebarPanel(width = 3,
                   # Set to use Shinyjs and set ID, so that checkboxes can be automatically reset
                   shinyjs::useShinyjs(),
                   id = "filter_panel",
                   
                   # Action button to update filters
                   actionButton(
                     "filter_update_top",
                     "Apply filters"
                   ),
                   
                   # Action button to select all filter options
                     actionButton(
                     "clear_all_filters_top",
                     "Clear all"
                   ),
                   
                   linebreaks(2),
                   
                   # Filters
                  div(id = "all_filters", # Create div for the Cicerone tour
                      
                  ### New tree filter tests
                      
                   # Sub-outcome definition
                  tags$span(
                    tags$label("Select outcome definition"), 
                    actionButton("outcome_defs", "", icon = icon("circle-info", `aria-label` = "Click for more information about outcome definitions"))),
                  jstreeOutput("outcome_tree"),
                  
                  # Label for input
                  tags$span(
                    tags$label("Select domain and sub-domain"), 
                    actionButton("domains_defs", "", icon = icon("circle-info", `aria-label` = "Click for more information about domains and subdomains"))),
                  # Domains and sub-domains with jsTreeR
                  jstreeOutput("domains_tree"),
                   
                   # Intervention or exposure and intervention classification
                  tags$span(
                    tags$label("Select reviews looking at interventions or risk/protective factors, and the intervention classification"), 
                    actionButton("int_exposure_defs", "", icon = icon("circle-info", `aria-label` = "Click for more information about interventions and risk/protective factors"))),
                  jstreeOutput("intervention_risk_tree"),
                   
                   # Population age
                  tags$span(
                    tags$label("Select population age"), 
                    actionButton("pop_age_defs", "", icon = icon("circle-info", `aria-label` = "Click for more information about population age"))),
                  jstreeOutput("age_tree"),
                   
                   # Population characteristics
                  tags$span(
                    tags$label("Select population characteristics"), 
                    actionButton("pop_characteristics_defs", "", icon = icon("circle-info", `aria-label` = "Click for more information about population characteristics"))),
                  jstreeOutput("sub_pop_tree"),
                  
                  linebreaks(1),
                   
                   # Study setting
                   checkboxGroupInput(
                     inputId = "study_setting_input",
                     label = tags$span("Select study setting",
                                       actionButton("study_setting_defs", "", icon = icon("circle-info", `aria-label` = "Click for more information about study setting"))),
                     choices = c("Clinical setting", "Community setting", "Educational establishment", "Online", "Not specific", "Other: Youth detention centres"),
                     selected = NULL
                   ),
                   
                   # Type of synthesis
                   checkboxGroupInput(
                     inputId = "synth_type_input",
                     label = tags$span("Select type of synthesis",
                                       actionButton("synth_type_defs", "", icon = icon("circle-info", `aria-label` = "Click for more information about types of synthesis"))),
                     choices = c("Systematic review with meta-analysis", "Systematic review with narrative synthesis", "Other review with narrative synthesis", "Scoping evidence mapping"),
                     selected = NULL
                   ),
                   
                   # Quality appraisal
                   radioButtons(
                     "qual_appraisal_input",
                     label = tags$span("Only show reviews that assess the quality of reviewed studies?",
                                       actionButton("quality_appraisal_defs", "", icon = icon("circle-info", `aria-label` = "Click for more information about quality appraisal"))),
                     choices = c("Yes","No"),
                     selected = "No"
                   ),
                   
                   # Pre-registered protocol
                   radioButtons(
                     "pre_reg_input",
                     label = tags$span("Only show reviews that have a pre-registered protocol?",
                                       actionButton("pre_reg_defs", "", icon = icon("circle-info", `aria-label` = "Click for more information about pre-registered protocols"))),
                     choices = c("Yes","No"),
                     selected = "No"
                   )
                  ) #div
      ), # sidebar panel
      ## Main panel for displaying outputs ----
      mainPanel(
        tabsetPanel(type = "tabs",
                    id = "tabset_egm",
                    tabPanel("EGM",
                             linebreaks(1),
                             actionButton("show_egm_numbers", "Show EGM as text", `aria-label` = "Show EGM as text button. The visual evidence and gap map (EGM) is not accessible via screenreader. Please click this button to access and download a text version of the EGM. You can use the filters to update this."),
                             linebreaks(2),
                             tags$div(withNavySpinner(reactableOutput("egm")), value = "graph", 'aria-label' = "The visual EGM is not accessible by screen reader. Please use the Show EGM as text button above to access an accessible version."),
                             # This is meant to focus on the show EGM as text modal, but it's not working
                             tags$script(HTML(
                               "Shiny.addCustomMessageHandler('focusModal', function(message) {
      // Set focus to the modal content
      $('#show_egm_numbers_modal').focus();
    });"
                             ))),
                    tabPanel("Included reviews",
                             linebreaks(1),
                             textOutput("record_count") %>% 
                               tagAppendAttributes(class = 'box-info'),
                             linebreaks(1),
                             box("Note: shaded rows indicate empty reviews",
                                 actionButton("empty_defs", "", icon = icon("circle-info", `aria-label` = "Click for a definition of empty reviews"))
                                 ),
                             linebreaks(3),
                             p("To see more columns, use the scroll bar at the bottom of the table, or click inside the table and use the left and right arrow keys on your keyboard."),
                             linebreaks(1),
                             csvDownloadButton("reviews_table", filename = "egm_reviews.csv"), # To download table as a CSV (defined in core functions script)
                             withNavySpinner(reactableOutput("reviews_table")),
                             value = "included_reviews"))) # For switching tabs on click
    )
  )# Sidebar layout
) #Tab panel
