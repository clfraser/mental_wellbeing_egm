#####################################.
# egm_page_mod.R
# This module creates the layout for the EGM tab
# It is the overall structure of the EGM tab, and other modules can be nested within it
#####################################.

#######################################################
## MODULE UI ----
#######################################################

egm_ui <- function(id, dataset){
  ns <- NS(id)
  
  tagList(
    mainTab <- tabPanel(
      lang = "en",
      div(
        div(class="fa fa-map", 
            role = "navigation"), "Evidence and gap map"), # wrap in div for screenreader / accessibility purposes 
      value = ns("main_page"), # tab ID
      titlePanel(h1("Evidence and gap map")),
      useShinyjs(),
      fluidPage(
        use_cicerone(), # Include Cicerone to give a guide of the page
        actionButton(ns("egm_guide_button"), "Click here for a guided tour of the page"),
        linebreaks(2),
        actionButton(ns("video_link_button"), "Click here to see walkthrough videos"),
        # Set checkbox colour
        tags$head(tags$style("input[type=checkbox] { accent-color: DodgerBlue; }")),
        ## Page titles
        titlePanel(
          if (dataset == "WEMWEBS CYP"){
            "Mental wellbeing in children and young people"
          } else{
            "Mental wellbeing in adults"
          }
          ),
        ## Inputs
        sidebarLayout(
          sidebarPanel(width = 3,
                       # Set to use Shinyjs and set ID, so that checkboxes can be automatically reset
                       shinyjs::useShinyjs(),
                       id = ns("filter_panel"),
                       
                       # Action button to update filters
                       actionButton(
                         ns("filter_update_top"),
                         "Apply filters"
                       ),
                       
                       # Action button to select all filter options
                       actionButton(
                         ns("clear_all_filters_top"),
                         "Clear all"
                       ),
                       
                       linebreaks(2),
                       
                       # Filters
                       
                      #[[FILTERS UI MOD]]
                      
          ), # sidebar panel
          ## Main panel for displaying outputs ----
          mainPanel(
            tabsetPanel(type = "tabs",
                        id = ns("tabset_egm"),
                        tabPanel(ns("EGM"),
                                 linebreaks(1),
                                 actionButton(ns("show_egm_numbers"), "Show EGM as text", `aria-label` = "Show EGM as text button. The visual evidence and gap map (EGM) is not accessible via screenreader. Please click this button to access and download a text version of the EGM. You can use the filters to update this."),
                                 linebreaks(2),
                                 tags$div(withNavySpinner(reactableOutput(ns("egm"))), value = "graph", 'aria-label' = "The visual EGM is not accessible by screen reader. Please use the Show EGM as text button above to access an accessible version."),
                                 # This is meant to focus on the show EGM as text modal, but it's not working
                                 tags$script(HTML(
                                   "Shiny.addCustomMessageHandler('focusModal', function(message) {
      // Set focus to the modal content
      $('#show_egm_numbers_modal').focus();
    });"
                                 ))),
                        tabPanel(ns("Included reviews"),
                                 linebreaks(1),
                                 textOutput(ns("record_count")) %>% 
                                   tagAppendAttributes(class = 'box-info'),
                                 linebreaks(1),
                                 box("Note: shaded rows indicate empty reviews",
                                     actionButton(ns("empty_defs"), "", icon = icon("circle-info", `aria-label` = "Click for a definition of empty reviews")),
                                     width = 12
                                 ),
                                 linebreaks(3),
                                 p("To see more columns, use the scroll bar at the bottom of the table, or click inside the table and use the left and right arrow keys on your keyboard."),
                                 linebreaks(1),
                                 csvDownloadButton(ns("reviews_table"), filename = "egm_reviews.csv"), # To download table as a CSV (defined in core functions script)
                                 withNavySpinner(reactableOutput(ns("reviews_table"))),
                                 value = "included_reviews"))) # For switching tabs on click
        )
      )# Sidebar layout
    ) #Tab panel
    
  )
}