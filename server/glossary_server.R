###### Server for glossary tab ######

output$glossary_table <- DT::renderDataTable({
  make_table(glossary_list)
})

output$glossary_table_simple <- renderTable(glossary_list)