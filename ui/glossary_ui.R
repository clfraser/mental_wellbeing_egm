####################### Page 1 #######################

output$glossary_ui <-  renderUI({

  div(
	     fluidRow(
            DT::dataTableOutput("glossary_table")

	      ) #fluidrow
   ) # div
}) # renderUI