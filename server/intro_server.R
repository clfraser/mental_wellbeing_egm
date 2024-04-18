# observeEvents respond if the relevant actionButton is clicked on homepageTab
# The about text is rendered before any button is clicked.
observeEvent(input$about, {
  output$text <- renderUI({
    tagList(h2("What is an evidence and gap map?"),
            p("An evidence and gap map is a new approach to presenting evidence for Public Health Scotland. It provides a visual representation of the evidence base on a specific policy area, highlighting where there is evidence and where there is not."),
            p("EGMs can be used to support decision-making in policy and practice, and to understand where there are gaps in knowledge and where research is needed."),
            h2("Self-harm in children and young people"),
            p("Self-harm is increasingly common among young people and has the potential to do serious harm. It is a strong predictor for future suicide risk. Consultation with a range of stakeholders identified self-harm in children and young people as a key public health priority."),
            p("This evidence and gap map brings together review-level evidence on self-harm in children and young people on:"),
            tags$ul(
            tags$li("what is already known about risks and protective factors that are associated with self-harm, and"),
            tags$li("what interventions or policies can prevent it.")
            ),
            p("This EGM focuses on systematic review-level evidence (glossary) as this is considered to be the most robust source of evidence.")
    ) # tagList
  }) # renderUI
  },
ignoreNULL = FALSE) # so that this text loads initially, before a button is clicked
  
observeEvent(input$use, {
  output$text <- renderUI({
    tagList(h2("How to use the Evidence and Gap Map (EGM)"),
            tags$video(id="overview_video", type = "video/mp4", src = "www/test_video.mp4", width = "1080px", height = "480px", controls = "controls"),
            
            h3("What the self-harm EGM can and can’t do"),
            p("The self-harm EGM", tags$b("does"), ":"),
            tags$ul(
              tags$li("Provide a visual representation of the evidence landscape"),
              tags$li("Tell you where there is and isn’t evidence; however an empty cell can mean that there was no evidence that met inclusion criteria, i.e. an empty cell in an EGM including only review-level evidence may mean that evidence is available at a lower tier (e.g. primary studies)"),
              tags$li("Have an intuitive and interactive format"),
              tags$li("Enable filtering of the evidence through systematic categorisation of reviews"),
              tags$li("Allow regular updates to the included evidence"),
              tags$li("Provide a foundation for more focused synthesis of the included evidence.")
            ),
            p("The self-harm EGM", tags$b("does not"), ":"),
            tags$ul(
              tags$li("Tell you what the evidence says"),
              tags$li("Assess the quality of the included reviews but the filters allow users to search based on quality criteria (i.e. availability of a pre-registered protocol, review author critical appraisal of primary studies)"),
              tags$li("Tell you all of the information you will need to know to make decisions. EGMs work best when considered alongside other data and intelligence."),
            ),
            
            h3("How the EGM is organised"),
            p("The EGM is organised into a matrix. Reviews are thematically categorised by domains/subdomains into rows and allocated to columns based on whether the review explores association of risk/protective factors with self-harm or interventions for self-harm.
            More information about the domains and subdomains are in ", tags$a(href = "https://publichealthscotland.scot/publications/children-and-young-people-mental-health-indicator-resources/",
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
              tags$li("popualation age"),
              tags$li("population characteristics"),
              tags$li("type of synthesis"),
              tags$li("specific types of interventions"),
              tags$li("Inclusion of author quality appraisal of primary studies"),
              tags$li("existence of a pre-registered protocol")
            ),
            p("More than one filter can be selected at a time. Applying these filters will impact both the EGM and table. The EGM and table can be reset by clearing all the filters.")
      
    ) # tagList
  })
})

observeEvent(input$methodology, {
  output$text <- renderUI({
    tagList(
      h2("Methodology"),
      
      h3("Technical report"),
      p("The self-harm EGM has been populated using a comprehensive and systematic methodology. The technical report outlines the research questions, documents the search strategy, sets out the inclusion and exclusion criteria, and outlines relevant processes and software used:"),
      p("[Insert Technical report link] – Self-harm EGM technical report"),
      
      h3("Last updated"),
      p("The search for evidence included in the evidence and gap map was last conducted in March 2023.")
    ) #tagList
  })
  
})  
 
observeEvent(input$contact, {
  output$text <- renderUI({
    tagList(h2("Contact us"),
            p("We would be grateful for any feedback about the evidence and gap map, and how it is being used."),
            p("If you have any questions or comments, please contact us at ", tags$a(href="mailto:phs.egm@phs.scot", "phs.egm@phs.scot"), "."),
    )# tagList
  })
  
  
})
observeEvent(input$accessibility, {
  output$text <- renderText({
    paste("<h2>Accessibility</h2>
      This website is run by
      <a href=\"https://www.publichealthscotland.scot/\" target=\"_blank\"><b>Public Health Scotland</b></a>
      , Scotland's national organisation for public health. 
      Public Health Scotland is committed to making its website accessible, 
      in accordance with the Public Sector Bodies (Websites and Mobile Applications) (No. 2) Accessibility Regulations 2018.
      <br><br><a href=\"https://mcmw.abilitynet.org.uk/\" target=\"_blank\"><b>AbilityNet (external website)</b></a>
            has advice on making your device easier to use if you have a disability.
            <br><h3>How accessible is this website?</h3>
            This site has not yet been evaluated against WCAG 2.1 level AA.
            <br><h3>Reporting any accessibility problems with this website</h3>
            If you wish to contact us about any accessibility issues you encounter on this site, 
            please email <a href=\"mailto:phs.egm@phs.scot\"> <b>phs.egm@phs.scot</b></a>
            <br><h3>Enforcement procedure</h3>
        The Equality and Human Rights Commission (EHRC) is responsible for enforcing the Public Sector Bodies 
        (Websites and Mobile Applications) (No. 2) Accessibility Regulations 2018 (the ‘accessibility regulations’). 
        If you’re not happy with how we respond to your complaint, contact the 
        <a href=\"https://www.equalityadvisoryservice.com/\" target=\"_blank\"><b>Equality Advisory and Support Service (EASS) (external website).</b></a>
")  
  })
  
  
})  
