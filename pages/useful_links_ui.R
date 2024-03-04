########## Useful links page ############

output$links_ui <- renderUI({
fluidRow(
  a(href = "https://www.campbellcollaboration.org/evidence-gap-maps.html", "Evidence and gap maps - The Campbell Collaboration"),
  br(),
  a(href = "https://www.understandinghealthresearch.org/", "Understanding Health Research"),
  br(),
  a(href = "https://publichealthscotland.scot/our-areas-of-work/health-and-wellbeing/prevention-of-mental-ill-health-and-improved-wellbeing/mental-health-indicators/overview/", "Mental health indicators overview - Public Health Scotland"),
  br(),
  a(href = "https://publichealthscotland.scot/media/12205/children-and-young-people-mental-health-indicator-rationales.pdf", "Children and young people's mental health indicators: construct rationales"),
  br(),
  a(href = "https://publichealthscotland.scot/media/12206/children-and-young-people-mental-health-indicator-set.pdf", "Children and young people's mental health indicators (PDF)"),
  br(),
  a(href = "https://publichealthscotland.scot/media/17228/what-factors-are-associated-with-self-harm-in-childhood_jan23.pdf", "Factors associated with self-harm in childhood - Public Health Scotland")
) #fluidRow
  })