####################### Intro Page #######################

output$intro_page_ui <-  renderUI({

  div(
	     fluidRow(
            h3("What is an evidence and gap map?"),
	           p("An evidence and gap map is a new approach to presenting evidence for Public Health Scotland. It provides a visual representation of the evidence base on a specific policy area, highlighting where there is evidence and where there is not.
EGMs can be used to support decision-making in policy and practice, and to understand where there are gaps in knowledge and where research is needed.
"), 
            linebreaks(2),
            p("[More info to be added about how to use the map]")
	      ) #fluidrow
   ) # div
}) # renderUI
