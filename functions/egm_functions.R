####################### jsTreeR for hierarchical checkboxes #######################

## Functions for creating structures to be used with jsTreeR

create_df_for_nodes <- function(for_tree_tibble, child_level){
  for_tree_tibble %>%
    mutate({{child_level}} := as.character({{child_level}})
           #, # If the child level is a factor, it doesn't convert to nodes properly
           #selected = FALSE
           ) %>%
    as.data.frame(.)
}

create_nodes_from_df <- function(df_for_nodes, parent_level, child_level){
  rownames(df_for_nodes) <- df_for_nodes[[child_level]]
  
  dat <- df_for_nodes %>%
    split(df_for_nodes[[parent_level]])
  
  nodes <- lapply(names(dat), function(parent){
    list(
      text = parent,
      state =
        list(opened = TRUE),
      children = lapply(dat[[parent]][[child_level]], function(child){
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

domains_subs_for_tree <- reviews_chart %>%
  select(domain, subdomain) %>%
  distinct(domain, subdomain) %>%
  arrange(domain, subdomain)

sub_outcomes_for_tree <- data.frame(
  first_level = rep("Any form of self-injurious thoughts and behaviours", 2),
  second_level = c("Exclusively non-suicidal self-harm", "Repetitive, compulsive self-injury")
)

age_for_tree <- data.frame(
  first_level = rep("All ages", 2),
  second_level = c("Exclusively 0-18 years", "Up to 25 years")
)

intervention_exposure_for_tree <- reviews_chart %>%
  select(intervention_exposure_short, intervention_classification) %>%
  mutate(intervention_classification = if_else(intervention_exposure_short == "Risk/protective factor", NA, intervention_classification)) %>%
  distinct(intervention_exposure_short, intervention_classification) %>%
  arrange(intervention_exposure_short)

sub_population_for_tree <- reviews_chart %>%
  select(overall_population, sub_population) %>%
  distinct(overall_population, sub_population) %>%
  arrange(overall_population)

# Turn these into dataframes for creating nodes for the tree
# Note that for the create_df_for_nodes function, the column names shouldn't be in quotes,
# but for the create_nodes_from_df function, they should

domains_df <- create_df_for_nodes(domains_subs_for_tree, subdomain)


domain_subs_nodes <- create_nodes_from_df(domains_df, "domain", "subdomain")