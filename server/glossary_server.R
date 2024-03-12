###### Server for glossary tab ######

output$glossary_table <- DT::renderDataTable({
  make_table(glossary_list)
})