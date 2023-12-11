####################### Page 1 UI #######################

output$page_1_ui <-  renderUI({
  
  fluidPage(
    # Set checkbox colour
    tags$head(tags$style("input[type=checkbox] { accent-color: DodgerBlue; }")),
    ## Page titles
    titlePanel(
      "Common mental health conditions in childhood"),
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
                     "select_all_filters_top",
                     "Select all"
                   ),
                   
                   linebreaks(2),
                   
                   # Filters
                   
                   # Outcome definition
                   checkboxGroupInput(
                     inputId = "outcome_def",
                     label = tags$span(
                       "Select outcome definition", 
                       actionButton("outcome_defs", "", icon = icon("circle-info"))),
                     choices = c("Any form of self-injurous thoughts and behaviours",
                                 "Exclusively non-suicidal self-harm",
                                 "Repetitive, compulsive self-injury"),
                     selected = c("Any form of self-injurous thoughts and behaviours",
                                  "Exclusively non-suicidal self-harm",
                                  "Repetitive, compulsive self-injury")
                   ),
                   
                   # Population age
                   checkboxGroupInput(
                     inputId = "pop_age",
                     label = tags$span("Select population age:",
                                       actionButton("pop_age_defs", "", icon = icon("circle-info"))),
                     choices = c("Exclusively 0-18 years",
                                 "Up to 25 years",
                                 "All ages"),
                     selected = c("Exclusively 0-18 years",
                                  "Up to 25 years",
                                  "All ages")
                   ),
                   
                   # Population characteristics
                   treeInput(
                     inputId = "pop_characteristics",
                     label = tags$span("Select population characteristics:",
                                       actionButton("pop_characteristics_defs", "", icon = icon("circle-info"))),
                     choices = create_tree(sub_population),
                     selected = c(unique(sub_population$sub_population), "General population"),
                     returnValue = "text",
                     closeDepth = 0
                   ),
                   
                   # Study setting
                   checkboxGroupInput(
                     inputId = "study_setting_input",
                     label = tags$span("Select study setting:",
                                       actionButton("study_setting_defs", "", icon = icon("circle-info"))),
                     choices = c("Clinical setting", "Community setting", "Educational establishment", "Online", "Not specific", "Other: Youth detention centres"),
                     selected = c("Clinical setting", "Community setting", "Educational establishment", "Online", "Not specific", "Other: Youth detention centres")
                   ),
                   
                   # Intervention or exposure and intervention classification
                   treeInput(
                     inputId = "intervention_exposure",
                     label = tags$span("Select reviews looking at interventions or risk/protective factos, and the intervention classification:",
                                       actionButton("int_exposure_defs", "", icon = icon("circle-info"))),
                     choices = create_tree(intervention_exposure),
                     selected = c(unique(intervention_exposure$intervention_classification), "Risk/protective factor"),
                     returnValue = "text",
                     closeDepth = 0
                   ),
                   
                   # Type of synthesis
                   checkboxGroupInput(
                     inputId = "synth_type_input",
                     label = tags$span("Select type of synthesis:",
                                       actionButton("synth_type_defs", "", icon = icon("circle-info"))),
                     choices = c("Systematic review with meta-analysis (Quantitative)", "Systematic review with narrative synthesis (Quantitative)", "Other review with narrative synthesis (Quantitative)", "Other: Scoping evidence mapping (Quantitative)"),
                     selected = c("Systematic review with meta-analysis (Quantitative)", "Systematic review with narrative synthesis (Quantitative)", "Other review with narrative synthesis (Quantitative)", "Other: Scoping evidence mapping (Quantitative)")
                   ),
                   
                   # Quality appraisal
                   radioButtons(
                     "qual_appraisal_input",
                     label = tags$span("Only show reviews that assess the quality of reviewed studies?",
                                       actionButton("quality_appraisal_defs", "", icon = icon("circle-info"))),
                     choices = c("Yes","No"),
                     selected = "No"
                   ),
                   
                   # Pre-registered protocol
                   radioButtons(
                     "pre_reg_input",
                     label = tags$span("Only show reviews that have a pre-registered protocol?",
                                       actionButton("pre_reg_defs", "", icon = icon("circle-info"))),
                     choices = c("Yes","No"),
                     selected = "No"
                   ),
                   
                   # Study design of reviewed studies
                   checkboxGroupInput(
                     inputId = "study_design_input",
                     label = tags$span("Select study design of reviewed studies:",
                                       actionButton("study_design_defs", "", icon = icon("circle-info"))),
                     choices = c("Grey literature", "Longitudinal designs", "Non-randomised control trials", "Randomised control trials", "Other quantitative designs", "Reviews", "Qualitative designs", "Other", "Not reported"),
                     selected = c("Grey literature", "Longitudinal designs", "Non-randomised control trials", "Randomised control trials", "Other quantitative designs", "Reviews", "Qualitative designs", "Other", "Not reported")
                   ),
                   
                   # Action button to update filters
                   actionButton(
                     "filter_update_bottom",
                     "Apply filters"
                   )
                   
      ), # sidebar panel
      ## Main panel for displaying outputs ----
      mainPanel(
        # Add in message when data is loading
        tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               top: 50px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #4B999D;
               z-index: 105;
             }
          ")),
        tabsetPanel(type = "tabs",
                    id = "tabset",
                    tabPanel("EGM", reactableOutput("egm", height = 1800, width = 1800), value = "graph"),
                    tabPanel("Table",
                             htmlOutput("print_click_details"),
                             linebreaks(1),
                             # Action button to clear click from map
                             actionButton(
                               "reset_map_selection",
                               "Reset map selection"
                             ),
                             linebreaks(2),
                             p("Note: shaded rows indicate empty reviews"),
                             linebreaks(1),
                             csvDownloadButton("data", filename = "egm_reviews.csv"), # To download table as a CSV (defined in core functions script)
                             reactableOutput("data"),
                             value = "table"))) # For switching tabs on click
    ),
    # Panel to show when map etc. is loading
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div("Loading...",id="loadmessage"))
  )# Sidebar layout
}) # renderUI

