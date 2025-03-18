####################### Core functions #######################

# Add n linebreaks
linebreaks <- function(n){HTML(strrep(br(), n))}

# Remove warnings from icons 
icon_no_warning_fn = function(icon_name) {
  icon(icon_name, verify_fa=FALSE)
}

# Generic data table
make_table <- function(input_data_table,
                      rows_to_display = 20
){

# Take out underscores in column names for display purposes
table_colnames  <-  gsub("_", " ", colnames(input_data_table))

dt <- DT::datatable(input_data_table, style = 'bootstrap',
                      class = 'table-bordered table-condensed',
                      rownames = FALSE,
                      filter="top",
                      colnames = table_colnames,
                      options = list(pageLength = rows_to_display,
                                     scrollX = FALSE,
                                     scrollY = FALSE,
                                     dom = 'pftl',
                                     autoWidth = TRUE,
                                     # style header
                                     initComplete = htmlwidgets::JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#C5C3DA', 'color': '#3F3685'});",
                                       "$(this.api().table().row().index()).css({'background-color': '#C5C3DA', 'color': '#3F3685'});",
                                       "}")))


  return(dt)
}




################### Create definition modals #########################

# Modal when topic is filtered on
defs_topic_modal <- function(topic){
  defs_filtered <- glossary_list %>%
    filter(Topic == topic) %>%
    select(-Topic)
  
  output$defs_table <- renderTable(defs_filtered)
  
  showModal(modalDialog(
    title = paste(topic, "definitions"),
    tableOutput("defs_table"),
    easyClose = TRUE
  ))
}

# Modal when term is filtered on
defs_term_modal <- function(term){
  defs_filtered <- glossary_list %>%
    filter(Term == term) %>%
    select(-Topic)
  
  output$defs_table <- renderTable(defs_filtered)
  
  showModal(modalDialog(
    title = paste(term, "definition"),
    tableOutput("defs_table"),
    easyClose = TRUE
  ))
}

# Set UI for 'how to use EGM' on intro page
# Name this so it can be used when switching tabs
intro_use_ui <- tagList(h2("How to use the Evidence and Gap Map (EGM)"),
                        p("You can click on the 'Click here for a guided tour of the page' button on the Evidence and gap map tab to get a quick guided tour."),
                        p("If you'd like more details, read the information below or watch our walkthrough and example videos."),
                        h3("Walkthrough video"),
                        p("The video below shows a walkthrough of the EGM"),
                        tags$iframe(width="560", height="315", src="https://youtube.com/embed/uzJZpS_hlyQ", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA),
                        h3("Example video"),
                        p("The video below shows an example of how to use the evidence and gap map to answer the question:"),
                        p("What review-level evidence is available on schools-based interventions that could reduce the risk of self-harming behaviour in children and young people?"),
                        p("It includes more details about the filters available."),
                        tags$iframe(width="560", height="315", src="https://youtube.com/embed/f2ZJGOysgOU", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA),
                        h3("How the EGM is organised"),
                        p("The EGM is organised into a matrix. Reviews are thematically categorised by domains/subdomains into rows and allocated to columns based on whether the review explores association of risk/protective factors with self-harm or interventions for self-harm.
            More information about the domains and subdomains are in ", tags$a(href = "https://publichealthscotland.scot/publications/children-and-young-people-mental-health-indicator-resources/",
                                                                               target = "_blank",
                                                                               tags$b("Public Health Scotland's children and young people mental health indicator resources"))
                        ),
                        p("The intersecting cell in the matrix shows either a coloured shape for that combination of domain/subdomain and review type (green square = risk/protective factor reviews and blue circle = intervention reviews) or an empty cell. The size of the shape gives an indication of the quantity of available reviews and hovering over it provides the exact number of reviews. An empty cell indicates that review-level evidence is lacking."),
                        p("Clicking on the shapes in the cell retrieves the underlying reference(s) and study details in an evidence table."),
                        
                        h3("How the table is organised"),
                        p("The table presents key characteristics of each included review, such as:"),
                        tags$ul(
                          tags$li("author and date"),
                          tags$li("study title and aim"),
                          tags$li("outcome definition"),
                          tags$li("population characteristics"),
                          tags$li("study setting"),
                          tags$li("domain/subdomain"),
                          tags$li("review type and design of primary studies"),
                          tags$li("number of primary studies"),
                          tags$li("inclusion of quality indicators i.e., whether the review authors undertook critical appraisal of primary studies, and whether a preregistered protocol is available or not"),
                          tags$li("whether it is an empty review i.e. the authors of the review searched for but did not find evidence relevant to this self-harm EGM.")
                        ),
                        p("The table also provides a hyperlink to the database record of the review."),
                        p("Shaded rows highlight empty reviews."),
                        p("The table has a free-text search function and the search results or the full table can be downloaded as CSV file."),
                        
                        h3("How the filters work"),
                        p("Using the filters helps to refine the EGM and limit the contents of the matrix to specific types of evidence. The EGM can be filtered by:"),
                        tags$ul(
                          tags$li("outcome definition"),
                          tags$li("domains and subdomains"),
                          tags$li("population age"),
                          tags$li("population characteristics"),
                          tags$li("type of synthesis"),
                          tags$li("specific types of interventions"),
                          tags$li("inclusion of author quality appraisal of primary studies"),
                          tags$li("existence of a pre-registered protocol")
                        ),
                        p("More than one filter can be selected at a time. Applying these filters will impact both the EGM and table. The EGM and table can be reset by clearing all the filters.")
                        
) # tagList



# CSV download button for table

csvDownloadButton <- function(id, filename = "data.csv", label = "Download as CSV") {
  tags$button(
    tagList(icon("download"), label),
    onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')", id, filename)
  )
}

# Show a navy navigation spinner

# Set navy colour

navy <- "#010068"

withNavySpinner <- function(out){
  withSpinner(out, color = navy)
}