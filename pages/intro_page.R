####################### Intro Page #######################

output$intro_page_ui <-  renderUI({
  
  div(
    fluidRow(
      h1("How to use an Evidence and Gap Map (EGM)"),
      h2("What is an EGM?"),
      p("An evidence and gap map is a new approach to presenting evidence for Public Health Scotland. It provides a visual representation of the evidence base on a specific policy area, highlighting where there is evidence and where there is not."),
      p("EGMs can be used to support decision-making in policy and practice, and to understand where there are gaps in knowledge and where research is needed."),
      
      h2("How the map is organised"),
      p("The map is organised into a matrix format. Reviews are thematically categorised by",
        tags$a(href = "https://publichealthscotland.scot/publications/children-and-young-people-mental-health-indicator-resources/", "domains/subdomains"),
        "which are mapped into rows, and the health outcomes of interest which are allocated to columns. The intersecting cell in the matrix shows all reviews for that combination of domain/subdomain and outcome. Within a cell a coloured shape indicates reviews relating to an intervention or risk/protective factor. Collectively, the size of the shapes gives an indication of the quantity of available reviews on an intervention or risk/protective factor and outcome. Clicking on the shapes in the cell retrieves the underlying reference(s) and the underlying study detail. Empty cells indicate review-level evidence is lacking."),
      p("Filters are a feature of EGMs and help to refine a search."),
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
      h1("Last updated"),
      p("The papers included in the EGM were last updated in March 2023."),
      h1("Contact us"),
      p("If you have any questions or comments on the EGM, please email us at",
        tags$a(href = "mailto:email@example.com?subject=Dummy email address&body=This is a dummy email address", "email@example.com"))
    ) #fluidrow
  ) # div
}) # renderUI