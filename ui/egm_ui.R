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
                  # Label for input
                  tags$b("Select domains and subdomains:"),
                  # Domains and sub-domains with shinyTree
                  shinyTree(
                    "domain_subs_tree",
                    checkbox = TRUE,
                    themeIcons = FALSE,
                    themeDots = FALSE,
                    
                  ),
                  
                  # Sub-outcome definition with virtual select input
                  # virtualSelectInput(
                  #   inputId = "sel2",
                  #   label = "Group choices:",
                  #   choices = prepare_choices(domains_subs_for_tree, subdomain, subdomain, domain),
                  #   multiple = TRUE,
                  #   disableOptionGroupCheckbox = FALSE
                  # ),
                      
                      
                   # Sub-outcome definition
                   treeInput(
                     inputId = "outcome",
                     label = tags$span(
                       "Select outcome definition", 
                       actionButton("outcome_defs", "", icon = icon("circle-info"))),
                     choices = create_tree(sub_outcomes_for_tree),
                     selected = NULL,
                     returnValue = "text",
                     closeDepth = 2
                   ),
                   
                   # Domains and sub-domains
                   treeInput(
                     inputId = "domains",
                     label = tags$span(
                       "Select domain and sub-domain", 
                       actionButton("domains_defs", "", icon = icon("circle-info"))),
                     choices = create_tree(domains_subs_for_tree),
                     selected = NULL,
                     returnValue = "text",
                     closeDepth = 1
                   ),
                   
                   # Intervention or exposure and intervention classification
                   treeInput(
                     inputId = "intervention_exposure",
                     label = tags$span("Select reviews looking at interventions or risk/protective factors, and the intervention classification:",
                                       actionButton("int_exposure_defs", "", icon = icon("circle-info"))),
                     choices = create_tree(intervention_exposure_for_tree),
                     selected = NULL,
                     returnValue = "text",
                     closeDepth = 1
                   ),
                   
                   # Population age
                   treeInput(
                     inputId = "pop_age",
                     label = tags$span("Select population age:",
                                       actionButton("pop_age_defs", "", icon = icon("circle-info"))),
                     choices = create_tree(age_for_tree),
                     selected = NULL,
                     returnValue = "text",
                     closeDepth = 1
                   ),
                   
                   # Population characteristics
                   treeInput(
                     inputId = "pop_characteristics",
                     label = tags$span("Select population characteristics:",
                                       actionButton("pop_characteristics_defs", "", icon = icon("circle-info"))),
                     choices = create_tree(sub_population_for_tree),
                     selected = NULL,
                     returnValue = "text",
                     closeDepth = 1
                   ),
                   
                   # Study setting
                   checkboxGroupInput(
                     inputId = "study_setting_input",
                     label = tags$span("Select study setting:",
                                       actionButton("study_setting_defs", "", icon = icon("circle-info"))),
                     choices = c("Clinical setting", "Community setting", "Educational establishment", "Online", "Not specific", "Other: Youth detention centres"),
                     selected = NULL
                   ),
                   
                   # Type of synthesis
                   checkboxGroupInput(
                     inputId = "synth_type_input",
                     label = tags$span("Select type of synthesis:",
                                       actionButton("synth_type_defs", "", icon = icon("circle-info"))),
                     choices = c("Systematic review with meta-analysis", "Systematic review with narrative synthesis", "Other review with narrative synthesis", "Scoping evidence mapping"),
                     selected = NULL
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
                   )
                  ) #div
      ), # sidebar panel
      ## Main panel for displaying outputs ----
      mainPanel(
        tabsetPanel(type = "tabs",
                    id = "tabset",
                    tabPanel("EGM",
                             linebreaks(1),
                             actionButton("show_egm_numbers", "See EGM as text"),
                             linebreaks(2),
                             withNavySpinner(reactableOutput("egm")), value = "graph"),
                    tabPanel("Table",
                             linebreaks(1),
                             textOutput("record_count"),
                             linebreaks(1),
                             p("Note: shaded rows indicate empty reviews"),
                             linebreaks(1),
                             csvDownloadButton("reviews_table", filename = "egm_reviews.csv"), # To download table as a CSV (defined in core functions script)
                             withNavySpinner(reactableOutput("reviews_table")),
                             value = "table"))) # For switching tabs on click
    )
  )# Sidebar layout
) #Tab panel
