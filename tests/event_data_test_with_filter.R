# Load packages

library(here)
library(tidyverse)
library(plotly)
library(ggh4x)
library(janitor)
library(purrr)

# Set up data

# Chart data

reviews_chart <- readRDS(here("data/self-harm_egm_chart_data.rds"))

all_selected <- reviews_chart %>%
  mutate(selected = 1)

grouped <- all_selected %>%
  group_by(domain, subdomain, overall_outcome, intervention_exposure_short, review_type, pos_x, pos_y) %>%
  summarise(count = sum(selected))

# Create a list based on the grouped dataframe, so that both subdomain and overall_outcome can be assigned to customdata
# Following instructions here: https://github.com/plotly/plotly.R/issues/1548
grouped_list <- split(grouped, seq_len(nrow(grouped)))

# Table data

reviews_table <- readRDS(here("data/self-harm_egm_table_data.rds"))

# Create list of unique subdomains
#subdomains <- unique(reviews_table$subdomain)

# UI

ui <- fluidPage(
  plotlyOutput("egm"),
  textOutput("subdomain_event"),
  textOutput("subdomain_reac_val"),
  textOutput("outcome_reac_val"),
  tableOutput("table")
)

# Server

server <- function(input, output, session){

# Create chart

gg_egm <- grouped %>%
  ggplot(aes(x = pos_x, y = pos_y,
             col = "black",
             fill = intervention_exposure_short,
             shape = intervention_exposure_short,
             size = count,
             text = paste0(intervention_exposure_short, ": ", count),
             customdata = grouped_list)) +
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
  scale_size_continuous(name = "Number of reviews") +
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
  facet_nested(domain+subdomain~overall_outcome,
               scales = "free",
               switch = "y",
               labeller = labeller(outcome_definition = label_wrap_gen(width = 16)))


# Output - EGM
output$egm <- renderPlotly({
  ggplotly(gg_egm,
           tooltip = "text", source = "egm_click") %>%
    layout(hovermode = "x",
           hoverdistance = 500)
})


# Event data

output$subdomain_event <- renderPrint({
  d <- event_data("plotly_click", source = "egm_click")
    print(d$customdata[1])
})    
output$subdomain_reac_val <- renderPrint({
  print(as.character(current_click()$subdomain))
})
output$outcome_reac_val <- renderPrint({
    print(as.character(current_click()$overall_outcome))
})

# Table

# Table - change what is shown depending on where on the EGM is clicked.
# Start off only filtering on subdomain, and then add outcome area when we know this works.
# Following example in 17.4.1 here: https://plotly-r.com/linking-views-with-shiny.html


# Set a reactive value to maintain the current subdomain
current_click <- reactiveVal()

# update the current category when appropriate
observe({
  cd <- event_data("plotly_click", source = "egm_click")$customdata[[1]]
  current_click(cd)
})


table_data <- reactive({
  
  if (!length(current_click())){
    return(reviews_table %>%
      select(study_id, title, aim_of_study, "Author conclusions" = summary, outcome_definition, age, intervention_or_exposure, study_setting, overall_domain, subdomain, type_of_review, design_of_reviewed_studies, number_of_primary_studies) %>%
      arrange(study_id) %>%
      clean_names(., case = "title"))
  }
  reviews_table %>%
    select(study_id, title, aim_of_study, "Author conclusions" = summary, outcome_definition, age, intervention_or_exposure, study_setting, overall_domain, subdomain, type_of_review, design_of_reviewed_studies, number_of_primary_studies) %>%
    arrange(study_id) %>%
    filter(str_detect(subdomain, current_click()$subdomain),
           overall_outcome == current_click()$overall_outcome) %>%
    clean_names(., case = "title")
  
  
})


output$table <- renderTable({
  table_data()
})

} # end of server script

# Run the application
shinyApp(ui=ui, server=server)
