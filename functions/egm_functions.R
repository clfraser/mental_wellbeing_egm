####################### EGM functions #######################

#### function for creating egm plot --------------------------------------------

draw_egm <- function(data){
  ## create EGM plot    
  grouped <- data %>%
    group_by(domain, subdomain, overall_outcome, outcome_definition, intervention_exposure_short, review_type, pos_x, pos_y) %>%
    summarise(count = sum(selected))
  
   gg_egm <- grouped %>%
     ggplot(aes(x = pos_x, y = pos_y,
                col = "black",
                fill = intervention_exposure_short,
                shape = intervention_exposure_short,
                size = count)) +
     geom_point(col = "#655E9D") +
     #  xlim(0,11) +
     #  ylim(0,11) +
     scale_x_continuous(expand = expansion(add = 1)) +
     scale_y_continuous(expand = expansion(add = 1)) +
     #scale_colour_manual(values = "black") +
     scale_fill_manual(values = c("#d7191c", "#c51b7d", "#abd9e9", "#2c7bb6"),
                       name="Intervention or exposure:", drop=FALSE) +
     scale_shape_manual(values = c(21, 22, 24, 19),
                        name="Intervention or exposure:", drop=FALSE) +
     scale_size_continuous(name = "Number of reviews", range = c(3,9)) +
     theme_bw()+
     theme(
       #aspect.ratio=1,
       #text = element_text(size=15),
       axis.title.x=element_blank(),
       axis.text.x=element_blank(),
       axis.ticks.x=element_blank(),
       axis.title.y=element_blank(),
       axis.text.y=element_blank(),
       axis.ticks.y=element_blank(),
       panel.grid = element_blank(),
       legend.position = "left",
       legend.justification = "top",
       legend.title = element_text(size=12, face="bold"),
       legend.text = element_text(size=12),
       strip.placement = "outside",
       strip.text.x = element_text(size = 12),
       strip.text.y = element_text(size = 12),
       strip.background = element_rect(fill="#B3D7F2"),
       strip.text.y.left = element_text(angle = 0)) +
     facet_nested(domain+subdomain~overall_outcome+outcome_definition,
                  scales = "free", switch="y",
                  labeller = labeller(outcome_definition = label_wrap_gen(width = 16)))
   
   return(gg_egm)

}

# Table

tab_egm <- function(chart_data, table_data){
  
  only_selected <-
    chart_data %>%
      filter(selected == 1)
  
  table_data %>%
    select(study_id, title, aim_of_study, "Author conclusions" = summary, outcome_definition, age, intervention_or_exposure, study_setting, overall_domain, all_subdomains, type_of_review, design_of_reviewed_studies, number_of_primary_studies) %>%
    filter(study_id %in% only_selected$study_id) %>%
    arrange(study_id) %>%
    clean_names(., case = "title")
}

# Output
output$egm <- renderPlot({
  draw_egm(filtered())
},height = 1200, width = 1500)

output$data <- renderTable({
  tab_egm(filtered(), reviews_table)
})

# Filtered dataframe
# Create when the app starts (using the ignoreNULL = FALSE argument), and then only update when the Update filter button is pressed

filtered <- eventReactive(input$filter_update, {
  reviews_chart %>%
    mutate(selected = NA,
           selected = if_else(subdomain %in% input$dom_sub &
                                outcome_definition %in% input$outcome &
                                type_of_review %in% input$review_type_input &
                                intervention_exposure_short %in% input$intervention_exposure &
                                (input$RCT == "No" | input$RCT == "Yes" & design_of_reviewed_studies == "Randomised control trials"),
                              1,
                              NA))
}, ignoreNULL = FALSE)

