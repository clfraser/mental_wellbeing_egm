############### Home/introduction page server ###################

#Naviagtion buttons on intro page
observeEvent(input$jump_to_egm, {updateTabsetPanel(session, "intabset", selected = "main_page")})
observeEvent(input$jump_to_glossary, {updateTabsetPanel(session, "intabset", selected = "glossary")})
observeEvent(input$jump_to_links, {updateTabsetPanel(session, "intabset", selected = "useful_links")})

output$introduction_about <- renderUI({
  
  tagList(h2("What is an evidence and gap map?"),
           p("An evidence and gap map is a new approach to presenting evidence for Public Health Scotland. It provides a visual representation of the evidence base on a specific policy area, highlighting where there is evidence and where there is not."),
           p("EGMs can be used to support decision-making in policy and practice, and to understand where there are gaps in knowledge and where research is needed."),
          br(),
          
          h2("Common mental health conditions in childhood"),
            p("[Add a couple of sentences about the EGM topic]"),
          br(),
          
          fluidRow(
            column(4,tags$div(class = "special_button",
                              actionButton("jump_to_egm", "Evidence and gap map"))),
            column(8, p("This section contains the evidence and gap map, and a table with information on the included studies."))),
          br(),
          
          
          fluidRow(
            column(4,tags$div(class = "special_button",
                              actionButton("jump_to_glossary", "Glossary"))),
            column(8, p("A glossary of terms used in the evidence and gap map."))),
          br(),
          
          fluidRow(
            column(4,tags$div(class = "special_button",
                              actionButton("jump_to_links", "Useful links"))),
            column(8, p("Useful links about evidence and gap maps, and things to consider when using evidence."))),
          br()
          
          
  )# tagList
})

output$introduction_use <- renderUI({
  tagList(h2("How to use the evidence and gap map"),
          p("[Some videos and screenshots to go here]")
  ) #tagList
})

output$introduction_accessibility <- renderUI({
  tagList(h2(tags$b("Accessibility")),
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
