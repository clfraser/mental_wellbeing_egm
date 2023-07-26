# First, run lines from reactable_egm_test to import data, packages and bubble_grid_modified function


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Evidence and Gap Map"),
  reactableOutput("egm")
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
    if (column.id == 'domain' | column.id == 'subdomain') {
      return
    }
    // Display an alert dialog with details for the row
    window.alert('Details for row ' + rowInfo.index + ':\\n' + JSON.stringify(rowInfo.values, null, 2) +
                 '\\n' + 'Details for column' + JSON.stringify(column.id, null, 2))

    // Send the click event to Shiny, which will be available in input$show_details
    // Note that the row index starts at 0 in JavaScript, so we add 1
    if (window.Shiny) {
      Shiny.setInputValue('show_details', { index: rowInfo.index + 1 }, { priority: 'event' })
    }
  }"),
          columns = list(
            Self_harm.Exposure = colDef(name = "", aggregate = "sum",
                                        vAlign = "top",
                                        cell = bubble_grid_modified(
                                          data = .,
                                          colors = '#1b9e77',
                                          tooltip = TRUE
                                        )),
            Self_harm.Intervention = colDef(name = "", aggregate = "sum",
                                            vAlign = "bottom",
                                            cell = bubble_grid_modified(
                                              data = .,
                                              colors = '#d95f02',
                                              tooltip = TRUE,
                                              shape = "squares"
                                            )),
            Self_harm.Attitudes = colDef(name = "Attitudes", aggregate = "sum",
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
}

# Run the application 
shinyApp(ui = ui, server = server)
