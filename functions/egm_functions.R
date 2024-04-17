####################### jsTreeR for hierarchical checkboxes #######################

## Functions for creating structures to be used with jsTreeR

create_df_for_nodes <- function(for_tree_tibble, child_level){
  for_tree_tibble %>%
    mutate({{child_level}} := as.character({{child_level}}), # If the child level is a factor, it doesn't convert to nodes properly
           selected = FALSE) %>%
    as.data.frame(.)
}

create_nodes_from_df <- function(df_for_nodes, parent_level, child_level){
  
  # Create separate DFs with different rownames for child or parent
  rownames(df_for_nodes) <- df_for_nodes[[child_level]]
  
  dat <- df_for_nodes %>%
    split(df_for_nodes[[parent_level]])
  
  nodes <- lapply(names(dat), function(parent){
    list(
      text = parent,
      state =
        list(opened = TRUE, selected = if_else(dat[[parent]][[child_level]][1] == "Exclude", df_for_nodes[dat[[parent]][[child_level]][1], "selected"], FALSE)),
      children = lapply(dat[[parent]][[child_level]][dat[[parent]][[child_level]] != "Exclude"], function(child){
        list(
          text = child,
          state =
            list(selected = df_for_nodes[child, "selected"])
        )
      })
    )
  })
  
  return(nodes)
}

# Clear heirarchical checkboxes

clear_tree <- function(df, parent_level, child_level){
  df %>%
    mutate(selected = FALSE) %>%
    create_nodes_from_df(parent_level, child_level)
}

# Putting data in the right format for jsTreeR

# Create initial dataframes for trees

sub_outcomes_for_tree <- data.frame(
  first_level = rep("Any form of self-injurious thoughts and behaviours", 2),
  second_level = c("Exclusively non-suicidal self-harm", "Repetitive, compulsive self-injury")
)

domains_subs_for_tree <- reviews_chart %>%
  select(domain, subdomain) %>%
  distinct(domain, subdomain) %>%
  arrange(domain, subdomain)

age_for_tree <- data.frame(
  first_level = rep("All ages", 2),
  second_level = c("Exclusively 0-18 years", "Up to 25 years")
)

intervention_exposure_for_tree <- reviews_chart %>%
  select(intervention_exposure_short, intervention_classification) %>%
  mutate(intervention_classification = if_else(intervention_exposure_short == "Risk/protective factor", "Exclude", intervention_classification)) %>% # Risk/protective factor should have no children
  distinct(intervention_exposure_short, intervention_classification) %>%
  arrange(intervention_exposure_short) %>%
  filter(!is.na(intervention_exposure_short))

sub_population_for_tree <- reviews_chart %>%
  select(overall_population, sub_population) %>%
  mutate(sub_population = if_else(overall_population == "General population", "Exclude", sub_population)) %>% # General population should have no children
  distinct(overall_population, sub_population) %>%
  arrange(overall_population) %>%
  filter(!is.na(overall_population))

# Turn these into dataframes for creating nodes for the tree
# Note that for the create_df_for_nodes function, the column names shouldn't be in quotes,
# but for the create_nodes_from_df function, they should

outcome_df <- create_df_for_nodes(sub_outcomes_for_tree, second_level)
domains_df <- create_df_for_nodes(domains_subs_for_tree, subdomain)
age_df <- create_df_for_nodes(age_for_tree, second_level)
intervention_risk_df <- create_df_for_nodes(intervention_exposure_for_tree, intervention_classification)
sub_pop_df <- create_df_for_nodes(sub_population_for_tree, sub_population)

# Turn the dataframes into nodes

outcome_nodes <- create_nodes_from_df(outcome_df, "first_level", "second_level")
domain_subs_nodes <- create_nodes_from_df(domains_df, "domain", "subdomain")
age_nodes <- create_nodes_from_df(age_df, "first_level", "second_level")
intervention_risk_nodes <- create_nodes_from_df(intervention_risk_df, "intervention_exposure_short", "intervention_classification")
sub_pop_nodes <- create_nodes_from_df(sub_pop_df, "overall_population", "sub_population")


################### Create definition modals #########################

defs_topic_modal <- function(topic){
  defs_filtered <- glossary_list %>%
    filter(Topic == topic) %>%
    select(-Topic)
  
  output$defs_table <- renderTable(defs_filtered)
  
  showModal(modalDialog(
    title = paste(topic, "definitions"),
    tableOutput("defs_table"),
    easyClose = TRUE
  ))
}
