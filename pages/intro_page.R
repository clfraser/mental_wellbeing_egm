####################### Intro Page #######################

output$intro_page_ui <-  renderUI({
  
  div(
    fluidRow(
      h1("How to use an Evidence Gap Map (EGM)"),
      h2("What is an EGM?"),
      p("An evidence and gap map is a new approach to presenting evidence for Public Health Scotland. It provides a visual representation of the evidence base on a specific policy area, highlighting where there is evidence and where there is not."),
      p("EGMs can be used to support decision-making in policy and practice, and to understand where there are gaps in knowledge and where research is needed."),
      
      h2("How the map is organised"),
      p("The map is organised into a matrix format. Reviews are thematically categorised by",
        tags$a(href = "https://publichealthscotland.scot/publications/children-and-young-people-mental-health-indicator-resources/", "domains/subdomains"),
        "which are mapped into rows, and the health outcomes of interest which are allocated to columns (such as any form of self-injurious thoughts and behaviours, exclusive non-suicidal self-harm, repetitive compulsive self-injury). The intersecting cell in the matrix shows all reviews for that combination of domain/subdomain and outcome. Within a cell a coloured shape indicates reviews relating to an exposure, intervention, exposure and intervention or attitudes. Collectively, the size of the shapes gives an indication of the quantity of available reviews on an exposure/intervention/attitudes/exposure and intervention and outcome. Clicking on the shapes in the cell retrieves the underlying reference(s) and the underlying study detail. Empty cells indicate review-level evidence is lacking."),
      p("Filters are a feature of EGMs and help to refine a search by:"),
      tags$ul(
        tags$li("Domains and subdomains"), 
        tags$li("Reviews that only contain intervention, exposures, intervention and exposure, attitudes"), 
        tags$li("Reviews that only contain RCTs"),
        tags$li("Different types of evidence ie qualitative or quantitative studies")
      ),
      p("The map can be accompanied by narrative evidence outputs for stakeholders, such as evidence briefings or scoping reports."),
      h2("Advantages of EGMs"),
      tags$ul(
        tags$li("Intuitive and interactive format"), 
        tags$li("Easy to update"), 
        tags$li("Visual illustration of evidence")
      ),
      h2("Limitations of EGMs"),
      tags$ul(
        tags$li("Narrow or broad scope of the research question may mean that the EGM has variable utility"), 
        tags$li("Data extraction can be limited"), 
        tags$li("Gaps in the cell can mean that there was no evidence that met inclusion criteria, i.e. an empty cell in an EGM including only systematic reviews may mean that evidence is available at a lower tier (e.g. primary studies)"),
        tags$li("The way studies can be systematically categorised into domains and subdomains can vary"),
        tags$li("EGMs will only tell you part of the information you will need to know to make decisions. They work best when considered alongside other data and intelligence")
      ),
      linebreaks(2)
    ) #fluidrow
  ) # div
}) # renderUI