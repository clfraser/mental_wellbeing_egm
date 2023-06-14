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
          
          # Domain and subdomain
                treeInput(
                inputId = "dom_sub",
                label = "Select domain and subdomain:",
                choices = create_tree(domains_subs),
                selected = unique(domains_subs$domain),
                returnValue = "text",
                closeDepth = 0
                   ),
          # Overall outcome and sub-outcome
                treeInput(
                  inputId = "outcome",
                  label = "Select overall outcome and sub-outcome area:",
                  choices = create_tree(outcomes),
                  selected = unique(outcomes$outcome_definition),
                  returnValue = "text",
                  closeDepth = 0
                ),   
                   
        # Review type
              treeInput(
                  inputId = "review_type_input",
                  label = "Select review type and sub-type:",
                  choices = create_tree(review_type),
                  selected = unique(review_type$type_of_review),
                  returnValue = "text",
                  closeDepth = 0
                ),
        # Intervention or exposure
        treeInput(
          inputId = "intervention_exposure",
          label = "Select reviews looking at interventions, exposures or attitudes:",
          choices = create_tree(intervention_exposure),
          selected = unique(intervention_exposure$intervention_classification),
          returnValue = "text",
          closeDepth = 0
        ),
        # RCTs
            radioButtons(
              "RCT",
              label = "Show reviews only containing RCTs?",
              choices = c("Yes","No"),
              selected = "No"
            ),
        # Action button to update filters
        actionButton(
          "filter_update",
          "Update filters"
        )
    ), # sidebar panel
    ## Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("EGM", plotOutput("egm", height = 1800, width = 1800)),
                  tabPanel("Table", tableOutput("data")
      )))
)
) # Sidebar layout
}) # renderUI

