####################### Page 1 #######################

output$glossary_ui <-  renderUI({

  div(
	     fluidRow(
            DT::dataTableOutput("glossary_table")

	      ) #fluidrow
   ) # div
}) # renderUI


# Data table example
output$glossary_table <- DT::renderDataTable({
  make_table(glossary_list)
})

output$glossary_table_simple <- renderTable(glossary_list)

# Plotly plot example
output$test_plot <- renderPlotly({
  mtcars_plot(datasets::mtcars)
})
