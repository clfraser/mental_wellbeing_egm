####################### Page 1 UI #######################

output$page_1_ui <-  renderUI({

  fluidPage(
    ## Page titles
    titlePanel(
      "Common mental health conditions in childhood"),
    ## Inputs
    sidebarLayout(
      sidebarPanel(width = 3,
          # Filters
          
          # Outcome definition
                checkboxGroupInput(
                inputId = "outcome_def",
                label = "Select outcome definition:",
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
            label = "Select population age:",
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
            label = "Select population characteristics:",
            choices = create_tree(sub_population),
            selected = c(unique(sub_population$sub_population), "General population"),
            returnValue = "text",
            closeDepth = 0
          ),
          
          # Study setting
          checkboxGroupInput(
            inputId = "study_setting_input",
            label = "Select study setting:",
            choices = c("Clinical setting", "Community setting", "Educational establishment", "Online", "Not specific", "Other: Youth detention centres"),
            selected = c("Clinical setting", "Community setting", "Educational establishment", "Online", "Not specific", "Other: Youth detention centres")
          ),
          
          # Intervention or exposure and intervention classification
          treeInput(
            inputId = "intervention_exposure",
            label = "Select reviews looking at interventions or exposures, and the intervention classification:",
            choices = create_tree(intervention_exposure),
            selected = c(unique(intervention_exposure$intervention_classification), "Exposure"),
            returnValue = "text",
            closeDepth = 0
          ),
          
          # Comparator details
          checkboxGroupInput(
            inputId = "comparator_details_input",
            label = "Select intervention comparators:",
            choices = c("Between groups - alternative treatment", "Within group - before/after", "Wait list", "Other: None", "Other: Unclear", "Not applicable (exposure studies)"),
            selected = c("Between groups - alternative treatment", "Within group - before/after", "Wait list", "Other: None", "Other: Unclear", "Not applicable (exposure studies)")
          ),
          
          # Type of synthesis
          checkboxGroupInput(
            inputId = "synth_type_input",
            label = "Select type of synthesis:",
            choices = c("Systematic review with meta-analysis (Quantitative)", "Systematic review with narrative synthesis (Quantitative)", "Other review with narrative synthesis (Quantitative)", "Other: Scoping evidence mapping (Quantitative)"),
            selected = c("Systematic review with meta-analysis (Quantitative)", "Systematic review with narrative synthesis (Quantitative)", "Other review with narrative synthesis (Quantitative)", "Other: Scoping evidence mapping (Quantitative)")
          ),
          
        # Quality appraisal
            radioButtons(
              "qual_appraisal_input",
              label = "Only show reviews that assess the quality of reviewed studies?",
              choices = c("Yes","No"),
              selected = "No"
            ),
        
        # Pre-registered protocol
        radioButtons(
          "pre_reg_input",
          label = "Only show reviews that have a pre-registered protocol?",
          choices = c("Yes","No"),
          selected = "No"
        ),
        
        # Study design of reviewed studies
        checkboxGroupInput(
          inputId = "study_design_input",
          label = "Select study design of reviewed studies:",
          choices = c("Grey literature", "Longitudinal designs", "Non-randomised control trials", "Randomised control trials", "Other quantitative designs", "Reviews", "Qualitative designs", "Other", "Not reported"),
          selected = c("Grey literature", "Longitudinal designs", "Non-randomised control trials", "Randomised control trials", "Other quantitative designs", "Reviews", "Qualitative designs", "Other", "Not reported")
        ),
        
        # Action button to update filters
        actionButton(
          "filter_update",
          "Update filters"
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

