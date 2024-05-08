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
            p("This EGM focuses on review-level evidence as this is considered to be the most robust source of evidence.",
              actionButton("review_level_defs", "", icon = icon("circle-info", `aria-label` = "Click for more information about outcome definitions"))),
            h2("What the self-harm EGM can and can’t do"),
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
              tags$li("Synthesise the evidence"),
              tags$li("Assess the quality of the included reviews, although the filters allow users to search based on quality criteria (i.e. availability of a pre-registered protocol, review author critical appraisal of primary studies).
                      Links to resources on interpreting and appraising health research can be found on the", tags$a(tags$b("Useful Links"), onclick="customHref('useful_links')", href = "#"), " tab."),
              tags$li("Tell you all of the information you will need to know to make decisions. EGMs work best when considered alongside other data and intelligence."),
            ),
    ) # tagList
  }) # renderUI
  },
ignoreNULL = FALSE) # so that this text loads initially, before a button is clicked

# Show a modal with definition of review-level evidence when information button clicked on 'About' page
observeEvent(input$review_level_defs, {defs_term_modal("Review-level evidence")})

# Since this is switched to when users click the walkthrough button on the EGM tab, intro_use_ui is defined in core functions  
observeEvent(input$use, {
  output$text <- renderUI({
    intro_use_ui
  })
})

observeEvent(input$methodology, {
  output$text <- renderUI({
    tagList(
      h3("Methodology"),
      p("The research questions we sought to answer were:"),
      tags$ul(
        tags$li("What risks and protective factors are associated with self-harm in children?"),
        tags$li("What policies and interventions might be effective in the primary prevention of self-harm in children?")
      ),
      
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
            p("If you have any questions or comments, please contact us at ", tags$a(href="mailto:phs.egm@phs.scot", tags$b("phs.egm@phs.scot"))),
    )# tagList
  })
  
  
})

observeEvent(input$accessibility, {
  output$text <- renderText({
    paste("<h2>Accessibility</h2>
      This website is run by
      <a href=\"https://www.publichealthscotland.scot/\" target=\"_blank\"><b>Public Health Scotland</b></a>, Scotland's national organisation for public health. 
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
