########## Useful links page ############

linksTab <- tabPanel(
  lang = "en",
  div(
    div(class="fa fa-link", 
        role = "navigation"), "Useful links"), # wrap in div for screenreader / accessibility purposes 
  value = "useful_links", # tab ID
  titlePanel(h1("Useful links")),
  useShinyjs(),
fluidRow(
  tagList(
    h2("Other evidence and gap maps"),
    p("The Campbell Collaboration has produced EGMs in several policy areas. The following links provide an overview of EGMs and an example of an EGM:",
      br(),
  tags$a(href = "https://www.campbellcollaboration.org/evidence-gap-maps.html",
    target = "_blank",
    tags$b("Evidence and gap maps - The Campbell Collaboration - overview of EGMs")),
  br(),
  tags$a(href = "https://centreforhomelessnessimpact.github.io/egm/",
    target = "_blank",
    tags$b("Centre for Homelessness Impact – example of an EGM that summarises what is known about interventions to overcome homelessness")),
    ),
  
  h2("Technical report"),
  p("The self-harm EGM has been populated using a comprehensive and systematic methodology. The technical report outlines the research questions, documents the search strategy, sets out the inclusion and exclusion criteria, and outlines relevant processes and software used:"),
  p("[Insert Technical report link] – Self-harm EGM technical report"),
  
  h2("Children and Young People Mental Health Indicator Set"),
  p("The EGM used the domains and subdomains identified in the Public Health Scotland Children and Young People Mental Health Indicator Set as a framework to categorise the evidence.
    An overview of the mental health indicator set and documents that summarise the rationale for the domains and constructs can be found here:",
  br(),
  tags$a(href = "https://publichealthscotland.scot/our-areas-of-work/health-and-wellbeing/prevention-of-mental-ill-health-and-improved-wellbeing/mental-health-indicators/overview/",
         target = "_blank",
         tags$b("Public Health Scotland - Overview - Mental Health Indicators")),
  br(),
  tags$a(href = "https://publichealthscotland.scot/publications/children-and-young-people-mental-health-indicator-resources/",
         target = "_blank",
         tags$b("Public Health Scotland - Children and Young People's Mental Health Indicator resources (indicator set and construct rationales)")),
  ),
  
  h2("Evidence briefing"),
  p("EGMs can be accompanied by narrative evidence outputs for stakeholders, such as evidence briefings or scoping reports. Here is a link to a review of review-level evidence of factors associated with self-harm without suicidal intent in childhood (using a narrower outcome definition of self-harm):",
  br(),
    tags$a(href = "https://publichealthscotland.scot/media/17228/what-factors-are-associated-with-self-harm-in-childhood_jan23.pdf",
         target = "_blank",
         tags$b("What factors are associated with self-harm in childhood? - Public Health Scotland evidence briefing")),
  ),
  
  h2("Interpreting and appraising health research"),
  p("Not all EGM have interpretation or critically appraised evidence. The following tool helps you to review and interpret a published health research paper:",
    br(),
  tags$a(href = "https://www.understandinghealthresearch.org/",
         target = "_blank",
         tags$b("Understanding health research")),
  ),
  
  p("The Maryland Scientific Methods Scale (SMS) can also be used to determine the robustness of research methods used in the evidence presented in the EGM:",
    br(),
  tags$a(href = "https://whatworksgrowth.org/resource-library/the-maryland-scientific-methods-scale-sms/",
         target = "_blank",
         tags$b("The Maryland Scientific Methods Scale"))
  )
) #fluidRow
) # tagsList
) # tabPanel
