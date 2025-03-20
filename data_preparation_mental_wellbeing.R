### ------------------------------------------------------------------------ ###
###                      EGM WEMWEBS - Shiny preparation                     ###
### ------------------------------------------------------------------------ ###


## remove any existing objects from global environment
rm(list=ls()) 

## disable scientific notation printing
options(scipen=999)

### load packages and data --------

#install.packages("tidyverse")
#install.packages("janitor")
#install.packages("readxl")
#install.packages("snakecase")

## load packages
library(tidyverse)
library(janitor) # for tidying up variable names 
library(readxl) # to read in Excel packages
library(snakecase) # to reformat strings
library(arrow) # Read and write Parquet files
library(here)

## Set working directory - only need to do this if you can't get into the R project
#setwd("/PHI_conf/PHSci/Catriona/EGM/mental_wellbeing_egm/")

## Read in data

# Have saved extracts below in UTF-8 format via Excel so that special characters are displayed properly

# Read in extract 1
extract_1 <- read_csv("data/WEMWBS_search 1 UTF-8.csv")

# Read in extract 2
extract_2 <- read_csv("data/WEMWBS_search 2 UTF-8.csv")

# Fix column names for extract 1
extract_1_renamed <- extract_1 %>%
  select(-Title, # There are two duplicate columns that contain the title. Remove one and then rename the other below.
         -":...10", # Remove extraneous blank columns
         -":...26",
         -":...31",
         -":...36",
         -":...47") %>% 
  rename(Title = "...5",
         Aim = "...6",
         Individual = "...9",
         "Family and friends" = "...25",
         "Learning environment" = "...30",
         Community = "...35",
         Structural = "...46",
         study_type = "Types of studies") %>%
  # Update column names to be consistent with extract 2 so that files can bind together
  # have to use fixed = TRUE if string contains brackets
  rename_with(~ gsub(" Exposure/intervention", "", .x)) %>%
  rename_with(~ gsub("Health behaviours (CYP)", "Health behaviour (CYP)", .x, fixed = TRUE)) %>%
  rename_with(~ gsub("Poverty and material deprivation  (CYP)", "Poverty and material deprivation (CYP)", .x, fixed = TRUE)) %>%
  rename_with(~ gsub("Adults", "Adult", .x)) %>%
  rename_with(~ gsub("Learning and environment", "Learning environment", .x)) %>%
  rename_with(~ gsub("Engagement with local activities", "Engagement in local activities", .x)) %>%
  rename_with(~ gsub("Family and Friends", "Family and friends", .x)) %>%
  rename_with(~ gsub("Poverty and material deprivation (Exposure/intervention)", "Poverty and material deprivation (Adult)", .x, fixed = TRUE))


# Fix column names for extract 2
extract_2_renamed <- extract_2 %>%
  select(-Title...3, # To check with Emma that this is the right duplicate title column to remove
         -":...10",
         -":...26",
         -":...31",
         -":...36",
         -":...47") %>%
  rename(Title = "Title...5",
        Aim = "...6",
        study_type = "Type of study") %>%
  # Update column names to be consistent with extract 1 so that files can bind together
  # have to use fixed = TRUE if string contains brackets
  rename_with(~ gsub(" Exposure(s)", "", .x, fixed = TRUE)) %>%
  rename_with(~ gsub("\\s+$", "", .x)) %>% # Remove whitespace from the end of column names
  rename_with(~ gsub("Discrimination", "Stigma, discrimination and harassment", .x)) %>%
  rename_with(~ gsub("Poverty and material deprivation (exposure/intervention)", "Poverty and material deprivation (Adult)", .x, fixed = TRUE))

# Bind files together
df_together <- rbind(extract_1_renamed, extract_2_renamed)

# Write out together file, for QA
#write_csv(df_together, "data/df_together.csv")

## Process data

# Clean column names
clean_col_names <- df_together %>%
  clean_names() 

# Data cleaning

# A couple of studies are missing and author and date. Add these in, from previous extract given by Emma.
clean_col_names <- clean_col_names %>%
  mutate(study_id = case_when(covidence_number == 1383 ~ "O'Brien 2020",
                              covidence_number == 1395 ~ "Fazia 2020",
                              TRUE ~ study_id))


# Correct Lewis 2017, which should be marked as in individual domain
# Change Kelley 2021, so that it ISN'T marked as in individual domain
# Change population of Paoletti 2018, which was incorrectly marked as Adult
# Mark Hughes 2016 and Lyons 2018 as being in the Structural domain
# Change the punctuation in the subdomain topic description for Bellis 2013, since using a semi-colon caused problems for formatting later
# Add details for Cheng 2014, which was omitted in data extract given
# Create an ID number column
clean_col_names <- clean_col_names %>%
  mutate(individual = case_when(study_id == "Lewis 2017" ~ "Individual",
                                study_id == "Kelley 2021" ~ NA,
                                TRUE ~ individual),
         population = if_else(study_id == "Paoletti 2018", "CYP", population),
         structural = if_else(study_id %in% c("Hughes 2016", "Lyons 2018"), "Structural", structural),
         exposure_to_harm_cyp = if_else(study_id == "Bellis 2013", "Childhood experiences (unhappiness, violence)", exposure_to_harm_cyp)) %>%
  add_row(covidence_number = 604,
          study_id = "Cheng 2014",
          reviewer_name = "Emma",
          title = "The Associations Between Parental Socio-Economic Conditions, Childhood Intelligence, Adult Personality Traits, Social Status and Mental Well-Being",
          aim = "To examine the associations between parental social status indicators (measured at birth), childhood intelligence (measured at age 11), personality traits, educational achievement and occupational prestige in relation to mental well-being (all measured at age 50).",
          population = "Adults",
          study_type = "Association study",
          individual = "Individual",
          other_adult = "personality traits; childhood intelligence",
          structural = "Structural",
          equality_adult = "education; occupation; parental social status") %>%
  # Some covidence numbers are duplicated across different studies, so create an id_number column instead, which has a unique number for each study
  mutate(id_number = row_number())


# Get list of which subdomains relate to each domain
individual_subs <- colnames(clean_col_names)[9:22]
family_friends_subs <- colnames(clean_col_names)[24:26]
learning_environment_subs <- colnames(clean_col_names)[28:30]
community_subs <- colnames(clean_col_names)[32:40]
structural_subs <- colnames(clean_col_names)[42:55]

# There are some duplicate studies between the first and second extract. Find these.
# clean_col_names %>%
#   group_by(study_id) %>% 
#   mutate(dupe = n()>1) %>%
#   ungroup() %>%
#   filter(dupe == TRUE) %>%
#   arrange(study_id, covidence_number) %>%
#   View()
# Not all are true duplicates, because some have different titles

# If there are duplicates, keep one with latest covidence number
no_dups <- clean_col_names %>%
  filter(!covidence_number %in% c(393, 188, 98, 197, 37))

# Format data
pivot_subdomains <- no_dups %>%
  # Change study type column so that it matches with the self-harm data. Change both title of column and coding of study types.
  rename(intervention_exposure_short = study_type) %>%
  mutate(intervention_exposure_short = if_else(intervention_exposure_short == "Intervention study", "Intervention", "Risk/protective factor")) %>%
  pivot_longer(cols = ends_with(c("_cyp", "_adult")),
               names_to = "subdomain",
               values_to = "subdomain_topic") %>%
  # Remove all rows where subdomain topic is NA or #
  filter(!is.na(subdomain_topic) & subdomain_topic != "#") %>%
  # Pivot domains into one column
  pivot_longer(cols = individual:structural,
               names_to = "domain",
               values_to = "domain_desc") %>%
  # Remove all rows without a domain description
  filter(!is.na(domain_desc)) %>%
  # Only keep subdomains that match domains
  filter((domain == "individual" & subdomain %in% individual_subs) |
           (domain == "family_and_friends" & subdomain %in% family_friends_subs) |
           (domain == "learning_environment" & subdomain %in% learning_environment_subs) |
           (domain == "community" & subdomain %in% community_subs) |
           (domain == "structural" & subdomain %in% structural_subs)) %>%
  # Expand rows with multiple subdomain topics
  separate_longer_delim(subdomain_topic, delim = "; ") %>%
  # Change various columns to sentence case. Add in a comma for one of the subdomains and a slash for another.
  mutate(subdomain_topic = str_to_sentence(subdomain_topic),
         domain = str_to_sentence(domain),
         subdomain = str_to_sentence(subdomain),
         subdomain = gsub("_", " ", subdomain),
         subdomain = case_when(subdomain == "Stigma discrimination and harassment adult" ~ "Stigma, discrimination and harassment adult",
                               subdomain == "Financial security debt adult" ~ "Financial security/debt adult",
                               TRUE ~ subdomain),
         domain = gsub("_", " ", domain)) %>%
  # Remove domain_desc column
  select(-domain_desc)

# Add links to papers

# Read in dataframe with links
links <- read_csv("data/wemwebs_links_to_papers.csv")

# Some DOIs don't have the first bit of the URL, so add these in if necessary
links_cleaned <- links %>%
  mutate(DOI = if_else(startsWith(DOI, "10."), paste0("http://dx.doi.org/", DOI), DOI))

# Since some covidence numbers are repeated, use a combination of covidence number and study ID to match to existing data
pivot_subdomains_with_links <- links_cleaned %>%
  select(covidence_number = "Covidence number", study_id = Study, links_title = Title, DOI) %>%
  merge(pivot_subdomains, by = c("covidence_number", "study_id"), all = TRUE)

# Further data cleaning after links added
pivot_subdomains_with_links_cleaned <- pivot_subdomains_with_links %>%
  # Remove Brandling 2011 from data completely, since a link to this paper can't be found
  filter(study_id != "Brandling 2011") %>%
  # Change the date of Scott 2016 to 2013 since this is the date the latest paper can be found for this study
  mutate(study_id = if_else(study_id == "Scott 2016", "Scott 2013", study_id),
  # BarthVedoy 2020 and 2021 and Jonsdottir 2020 haven't matched properly (probably because of special characters in the name). Add the DOIs in manually.
         DOI = case_when(id_number == 41 ~ "http://dx.doi.org/10.1016/j.mhpa.2020.100322",
                         id_number == 142 ~ "http://dx.doi.org/10.1093/eurpub/ckz192",
                         id_number == 208 ~ "http://dx.doi.org/10.1186/s12966-021-01211-x",
                         TRUE ~ DOI)) %>%
  # Some remaining papers without a match in the main extract because the covidence numbers aren't consistent with those from the main extract. These papers still appear in the data, so remove all papers with a missing ID number.
  filter(!is.na(id_number))
  
# Separate out adult and CYP
adult_mwb_separated <- pivot_subdomains_with_links_cleaned %>%
  filter(population == "Adults") %>%
  # Remove 'adult' from end of subdomains
  mutate(subdomain = gsub(" adult$", "", subdomain)) %>%
  # There are no subdomains that don't appear in data, so no need to add any here. Mark all existing subdomains as not dummy.
  mutate(dummy = 0) %>%
  # Turn domains and subdomains into factors so that sorting works as expected
  mutate(domain = factor(domain, levels = c("Individual", "Community", "Structural")),
         subdomain = factor(subdomain, levels = c("Learning and development", "Healthy living", "Family support", "Social media", "General health", "Spirituality", "Participation", "Social support", "Trust", "Safety", "Equality", "Social inclusion", "Poverty and material deprivation", "Stigma, discrimination and harassment", "Financial security/debt", "Physical environment", "Working life", "Violence", "Other"))) %>%
  # Add overall outcome column (so that later multiple outcomes could be added)
  mutate(overall_outcome = "WEMWEBS")

cyp_mwb_separated <- pivot_subdomains_with_links_cleaned %>%
  filter(population == "CYP") %>%
  # Remove 'cyp' from end of subdomains
  mutate(subdomain = gsub(" cyp$", "", subdomain)) %>%
  # Add in subdomains that don't appear in data and mark them as dummy
  mutate(dummy = 0) %>%
  add_row(domain = "Individual", subdomain = "Body image", dummy = 1) %>%
  add_row(domain = "Individual", subdomain = "Perinatal environment", dummy = 1) %>%
  add_row(domain = "Family and friends", subdomain = "Parental health", dummy = 1) %>%
  add_row(domain = "Learning environment", subdomain = "Engagement with learning", dummy = 1) %>%
  add_row(domain = "Learning environment", subdomain = "Pressures and expectations", dummy = 1) %>%
  add_row(domain = "Community", subdomain = "Respect of young people", dummy = 1) %>%
  add_row(domain = "Community", subdomain = "Engagement in local activities", dummy = 1) %>%
  add_row(domain = "Community", subdomain = "Safety", dummy = 1) %>%
  add_row(domain = "Community", subdomain = "Belonging", dummy = 1) %>%
  add_row(domain = "Structural", subdomain = "Social inclusion", dummy = 1) %>%
  add_row(domain = "Structural", subdomain = "Stigma and discrimination", dummy = 1) %>%
  add_row(domain = "Structural", subdomain = "Societal optimism", dummy = 1) %>%
  # Turn domains and subdomains into factors so that sorting works as expected
  mutate(domain = factor(domain, levels = c("Individual", "Family and friends", "Learning environment", "Community", "Structural")),
         subdomain = factor(subdomain, levels = c("Health behaviour", "General health status", "Social media", "Body image", "Perinatal environment", "Learning and development", "Family relations", "Parental health", "Peer and friend relationships", "Engagement with learning", "Educational environment", "Pressures and expectations", "Respect of young people", "Engagement in local activities", "Social support", "Safety", "Belonging", "Equality", "Poverty and material deprivation", "Social inclusion", "Stigma and discrimination", "Physical environment", "Societal optimism", "Exposure to harm", "Other"))) %>%
  # Add overall outcome column (so that later multiple outcomes could be added)
  mutate(overall_outcome = "WEMWEBS")

# To do:
# Consider a less manual way of adding in missing subdomains, e.g. create a complete dataframe with all indicators and merge this in
# Get links/DOIs for papers

# Write out adult as a CSV for QA purposes
#write_csv(adult_mwb_separated, "data/adult_mwb_separated.csv")

## Create format for table

# First for adult
adult_mwb_table <- adult_mwb_separated %>%
  # The domain and subdomain and topic need to be grouped together again
  group_by(across(c(study_id, title, aim, intervention_exposure_short, DOI, dummy, id_number))) %>%
  summarise(domain = paste(unique(domain), collapse = "; "),
            subdomain = paste(unique(subdomain), collapse = "; "),
            subdomain_topic = paste(unique(subdomain_topic), collapse = "; ")) %>%
  filter(dummy == 0) %>%
  ungroup() %>%
  select(study_id, title, aim, intervention_exposure_short, domain, subdomain, subdomain_topic, DOI, id_number)

# Then for CYP
cyp_mwb_table <- cyp_mwb_separated %>%
  # The domain and subdomain and topic need to be grouped together again
  group_by(across(c(study_id, title, aim, intervention_exposure_short, DOI, dummy, id_number))) %>%
  summarise(domain = paste(unique(domain), collapse = "; "),
            subdomain = paste(unique(subdomain), collapse = "; "),
            subdomain_topic = paste(unique(subdomain_topic), collapse = "; ")) %>%
  filter(dummy == 0) %>%
  ungroup() %>%
  select(study_id, title, aim, intervention_exposure_short, domain, subdomain, subdomain_topic, DOI, id_number)

## Save data to be read into EGM
write_parquet(adult_mwb_separated, "data/wemwebs_adult_chart_data.parquet")
write_parquet(cyp_mwb_separated, "data/wemwebs_cyp_chart_data.parquet")
write_parquet(adult_mwb_table, "data/wemwebs_adult_table_data.parquet")
write_parquet(cyp_mwb_table, "data/wemwebs_cyp_table_data.parquet")