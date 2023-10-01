# First, run lines from reactable_egm_test to import data, packages and bubble_grid_modified function

library(shiny)
library(jsonlite)
library(stringr)

# Set up

# Read in table data

reviews_table <- readRDS(here("data/self-harm_egm_table_data.rds")) %>% ungroup()

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Evidence and Gap Map"),
  reactableOutput("egm"),
  tableOutput("click_data_print"),
  textOutput("outcome_click_print"),
  textOutput("type_click_print"),
  tableOutput("table")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$egm <- renderReactable({
      count_pivot %>%
        reactable(
          defaultColDef = colDef(
            align = 'center'),
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
            Outcome_category_2.Exposure = colDef(name = "", aggregate = "sum"),
            Outcome_category_2.Intervention = colDef(name = "", aggregate = "sum"),
            Outcome_category_3.Exposure = colDef(name = "Exposure", aggregate = "sum"),
            Outcome_category_3.Intervention = colDef(name = "Intervention", aggregate = "sum")
          ),
          columnGroups = list(
            colGroup(name = "Self-harm", columns = c("Self_harm.Exposure", "Self_harm.Intervention", "Self_harm.Attitudes")),
            colGroup(name = "Outcome category 2", columns = c("Outcome_category_2.Exposure", "Outcome_category_2.Intervention")),
            colGroup(name = "Outcome category 3", columns = c("Outcome_category_3.Exposure", "Outcome_category_3.Intervention"))
          )
        )
    })
    
    # Come back to how these reactive expressions should be structured. Do I need to return variables to get it to work?
    
    # outcome_click <- observeEvent(input$click_details,{
    #   sub("\\..*", "", input$click_details$outcome_and_type)
    # })
    # 
    # type_click <- observeEvent(input$click_details, {
    #   sub(".*\\.", "", input$click_details$outcome_and_type)
    # })
    
    table_data <- reactive({
       output$click_data_print <- renderTable({input$click_details})

       outcome_extract <- sub("\\..*", "", input$click_details$outcome_and_type)
       outcome_click <- sub("_", "-", outcome_extract) # The outcome has an underscore in the original but we need it to have a hyphen for filtering
       type_click <- sub(".*\\.", "", input$click_details$outcome_and_type)
       output$outcome_click_print <- renderPrint({outcome_click})
       output$type_click_print <- renderPrint({type_click})
       
      if (!length(input$click_details)){
        return(reviews_table %>%
                 select(study_id, title, aim_of_study, "Author conclusions" = summary, overall_outcome, outcome_definition, age, intervention_or_exposure, study_setting, overall_domain, subdomain, type_of_review, design_of_reviewed_studies, number_of_primary_studies) %>%
                 arrange(study_id) %>%
                 clean_names(., case = "title"))
      }
      return(reviews_table %>%
        select(study_id, title, aim_of_study, "Author conclusions" = summary, overall_outcome, outcome_definition, age, intervention_or_exposure, study_setting, overall_domain, subdomain, type_of_review, design_of_reviewed_studies, number_of_primary_studies) %>%
        arrange(study_id) %>%
        filter(str_detect(subdomain, input$click_details$subdomain) &
               overall_outcome == outcome_click &
               str_detect(intervention_or_exposure, type_click)
               ) %>%
        clean_names(., case = "title"))

    })
    
    output$table <- renderTable({
      table_data()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
