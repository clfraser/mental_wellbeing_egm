############### Home/introduction page server ###################

#Naviagtion buttons on intro page
observeEvent(input$jump_to_egm, {updateTabsetPanel(session, "intabset", selected = "main_page")})
observeEvent(input$jump_to_glossary, {updateTabsetPanel(session, "intabset", selected = "glossary")})
observeEvent(input$jump_to_links, {updateTabsetPanel(session, "intabset", selected = "useful_links")})

output$introduction_about <- renderUI({
  
  tagList(h1("What is an evidence and gap map?"),
           p("An evidence and gap map (EGM) is a new approach to presenting evidence for Public Health Scotland. It provides a visual representation of the evidence base on a specific policy area, highlighting where there is evidence and where there is not."),
           p("EGMs can be used to support decision-making in policy and practice, and to understand where there are gaps in knowledge and where research is needed."),
          br(),
          
          h1("Self-harm in children and young people"),
            p("Self-harm is increasingly common among young people and has the potential to do serious harm. It is a strong predictor for future suicide risk. Consultation with a range of stakeholders identified self-harm in children and young people as a key public health priority."),
            p("This EGM brings together review-level evidence on self-harm in children and young people on:"),
            tags$li("what is already known about risks and protective factors that are associated with self-harm, and"),
            tags$li("what interventions or policies can prevent it."),
          br(),
          p("This EGM focuses on review-level evidence (glossary) as this is considered to be the most robust source of evidence."),
          p("The full methodology for the EGM can be found in the technical report [link]."),
          br(),
          
          fluidRow(
            column(4,tags$div(class = "special_button",
                              actionButton("jump_to_egm", "Evidence and gap map"))),
            column(8, p("This section contains the EGM, and a table with information on the included studies."))),
          br(),
          
          
          fluidRow(
            column(4,tags$div(class = "special_button",
                              actionButton("jump_to_glossary", "Glossary"))),
            column(8, p("A glossary of terms used in the EGM."))),
          br(),
          
          fluidRow(
            column(4,tags$div(class = "special_button",
                              actionButton("jump_to_links", "Useful links"))),
            column(8, p("Useful links about EGMs, and things to consider when using evidence."))),
          br()
          
          
  )# tagList
})

output$introduction_use <- renderUI({
  tagList(
          h2("How the map is organised"),
          p("The EGM is organised into a matrix. Reviews are thematically categorised by domains and subdomains into rows and allocated to columns based on whether the review explores association of risk/protective factors with self-harm or interventions for self-harm. Find out more about domains and subdomains in Public Health Scotland's ",
          a(href = "https://publichealthscotland.scot/publications/children-and-young-people-mental-health-indicator-resources/", "Chidren and Young People Mental Health Indicator resources"),
          "The intersecting cell in the matrix shows a shape for that combination of domain/subdomain and review type (green square = risk/protective factor reviews and blue circle = intervention reviews)."),
          p("Collectively, the size of the shape is an indication of the quantity of available reviews and hovering over it provides the exact number. Clicking on the shapes in the cell retrieves the underlying reference(s) and study details."),
          p("Empty cells indicate review-level evidence is lacking."),
          p("The table tab brings you to a table with key details on each included review."),
          p("Shaded rows indicate empty reviews."),
          p("The table can be downloaded as CSV file."),
          p("Filters can help to refine a search by:"),
          tags$li("Domains and subdomains"),
          tags$li("Outcome definition"),
          tags$li("Population age"),
          tags$li("Population characteristics"),
          tags$li("Type of synthesis"),
          tags$li("Reviews looking at specific types of interventions"),
          tags$li("Reviews that have included quality appraisal"),
          tags$li("Reviews that have a pre-registered protocol."),
          br(),
          p("The map can be accompanied by narrative evidence outputs for stakeholders, such as evidence briefings or scoping reports. [CF note - do we want to leave this out for now if we aren't including any at the moment?]"),
          
          h2("Advantages of EGMs"),
          tags$li("Intiutive and interactive format"),
          tags$li("Easy to update"),
          tags$li("Visual illustration of evidence"),
          h2("Limitations of EGMs"),
          tags$li("Data extraction can be limited"),
          tags$li("Gaps in the cell can mean that there was no evidence that met inclusion criteria, i.e. an empty cell in an EGM including only systematic reviews may mean that evidence is available at a lower tier (e.g. primary studies)"),
          tags$li("The way studies can be systematically categorising into domains and subdomains can vary"),
          tags$li("EGMs will only tell you part of the information you will need to know to make decisions. They work best when considered alongside other data and intelligence."),
  ) #tagList
})

output$introduction_accessibility <- renderUI({
  tagList(h1(tags$b("Accessibility")),
          h1("THIS TEXT IS TAKEN FROM ANOTHER DASHBOARD AND WILL NEED ADAPTED FOR US"),
          p("This website is run by ", tags$a(href="https://www.publichealthscotland.scot/",
                                              "Public Health Scotland", target="_blank"),
            ", Scotland's national organisation for public health. As a new organisation formed
                                   on 01 April 2020, Public Health Scotland is currently reviewing its web estate. Public
                                   Health Scotland is committed to making its website accessible, in accordance with
                                   the Public Sector Bodies (Websites and Mobile Applications) (No. 2) Accessibility
                                   Regulations 2018. This accessibility statement applies to the dashboard that accompanies
                                   the HSMR quarterly publication."),
          p(tags$a(href="https://mcmw.abilitynet.org.uk/", "AbilityNet (external website)", target="_blank"),
            " has advice on making your device easier to use if you have a disability."),
          
          p(tags$b("Compliance status")),
          p("This site has not yet been evaluated against Web Content Accessibility Guidelines
                                   version 2.1 level AA standard. We are currently working towards an evaluation."),
          br(),
          p(tags$b("Accessible features")),
          p("We have taken the following steps to make this dashboard more accessible:"),
          p(tags$li("Colours have been chosen to meet colour contrast standards")),
          p(tags$li("Plot colours have been chosen to be colour blind friendly")),
          p(tags$li("The dashboard can be navigated using a keyboard")),
          p(tags$li("The content is still readable at 400% zoom")),
          p(tags$li("Alternative descriptions of plot contents have been provided via 'Plot description' buttons")),
          p(tags$li("Data tables have been provided alongside plots where possible")),
          p(tags$li("Hover content has been replaced with clickable content where possible")),
          br(),
          p(tags$b("Reporting any accessibility problems with this website")),
          p("If you wish to contact us about any accessibility issues you encounter on this
                                   site, please email ", tags$a(href="mailto:phs.covid19data&analytics@phs.scot", "phs.covid19data&analytics@phs.scot", ".")),
          
          p(tags$b("Enforcement procedure")),
          p("The Equality and Human Rights Commission (EHRC) is responsible for enforcing the
                                   Public Sector Bodies (Websites and Mobile Applications) (No. 2) Accessibility Regulations
                                   2018 (the ‘accessibility regulations’). If you’re not happy with how we respond to your complaint,",
            tags$a(href="https://www.equalityadvisoryservice.com/", "contact the Equality Advisory and Support Service (EASS) (external website).",
                   target = "_blank")),
          
          p(tags$b("Preparation of this accessibility statement")),
          p("This statement was prepared on 29 January 2024. It was last reviewed on 29 January 2024.")
  ) # tagList
})

output$last_updated <- renderUI({
  tagList(
    h2(tags$b("Last updated")),
    p("The search for evidence included in the evidence and gap map was last conducted in March 2023.")
  ) #tagList
})

output$introduction_contact <- renderUI({
  tagList(h2(tags$b("Contact us")),
          p("We would be grateful for any feedback about the evidence and gap map, and how it is being used."),
          p("If you have any questions or comments, please contact us at ", tags$a(href="mailto:phs.egm@phs.scot", "phs.egm@phs.scot"), "."),
  )# tagList
})
