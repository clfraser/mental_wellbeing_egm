#### Guided tours ####

# add the following to the server script
# 
# <name of>_guide$init()
# 
# observeEvent(input$<name of>_guide, {
#   <name of>_guide$start()
# })

# add the following to ui script
# 
# use_cicerone(),
# actionButton("<name of>_guide", "Click here for a guided tour of the page"),


# define the step-by-step tour (note you can name it whatever you want and can add as many steps as you like, just link them using the $ symbol like below
# the first line in step is the ID of the UI element you want to be highlighted for that particular step (a html div)

egm_guide <- Cicerone$
  new()$ 
  step(
    el = "clear_all_filters_top",
    title = "Evidence and gap map",
    description = "The size of the shapes correspond to the number of reviews. Hover over each shape to see the number of reviews.
    Click the shape to see a table with review details."
  )$
  step(
    "clear_all_filters_top",
    "See EGM as a table",
    "See a table of the number of reviews included in the EGM. This takes account of any filters applied. You can download the table as a CSV."
  )