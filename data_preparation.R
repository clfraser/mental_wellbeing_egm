### ------------------------------------------------------------------------ ###
###                      EGM self-harm - Shiny preparation                   ###
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
library(here)

## Set working directory - only need to do this if you can't get into the R project
#setwd("/PHI_conf/PHSci/Catriona/EGM/wemwebs_egm/")

## Read in data

# Read in extract 1
extract_1 <- read_csv("data/WEMWBS_search 1.csv")

# Read in extract 2
extract_2 <- read_csv("data/WEMWBS_search 2.csv")

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
write_csv(df_together, "data/df_together.csv")

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

# There are some duplicate studies between the first and second extract. Find these.
clean_col_names %>%
  group_by(study_id) %>% 
  mutate(dupe = n()>1) %>%
  ungroup() %>%
  filter(dupe == TRUE) %>%
  arrange(study_id, covidence_number) %>%
  View()
# Not all are true duplicates, because some have different titles

# If there are duplicates, keep one with latest covidence number
no_dups <- clean_col_names %>%
  filter(!covidence_number %in% c(393, 188, 98, 197, 37))


# Correct Lewis 2017, which should be marked as in individual domain
# And change Kelley 2021, so that it ISN'T marked as in individual domain
clean_col_names <- clean_col_names %>%
  mutate(individual = case_when(study_id == "Lewis 2017" ~ "Individual",
                                study_id == "Kelley 2021" ~ NA,
                                TRUE ~ individual))

# Get list of which subdomains relate to each domain
individual_subs <- colnames(clean_col_names)[9:22]
family_friends_subs <- colnames(clean_col_names)[24:26]
learning_environment_subs <- colnames(clean_col_names)[28:30]
community_subs <- colnames(clean_col_names)[32:40]
structural_subs <- colnames(clean_col_names)[42:55]

# Format data
pivot_subdomains <- clean_col_names %>%
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
  # Change various columns to sentence case
  mutate(subdomain_topic = str_to_sentence(subdomain_topic),
         domain = str_to_sentence(domain),
         subdomain = str_to_sentence(subdomain),
         subdomain = gsub("_", " ", subdomain),
         domain = gsub("_", " ", domain)) %>%
  # Remove domain_desc column
  select(-domain_desc)

# Query with Emma:
# Kelley 2021 doesn't have a subdomain for individual
# Foley 2017 and Marselle 2013 both have NA for types of studies
# Bond 2012 - marked as adult, but under social inclusion CYP
# Other: intervention studies
  
# Separate out adult and CYP
# COME BACK TO - DROP SUBDOMAINS WITH CYP FROM ADULTS AND VICE VERSA?
adult_mwb_separated <- pivot_subdomains %>%
  filter(population == "Adults") %>%
  # Remove 'adult' from end of subdomains
  mutate(subdomain = gsub(" adult$", "", subdomain)) %>%
  # Add in subdomains that don't appear in data
  mutate(dummy = 0) %>%
  add_row(domain = "Structural", subdomain = "Stigma, discrimination and harassment", dummy = 1)

cyp_mwb_separated <- pivot_subdomains %>%
  filter(population == "CYP") %>%
  # Remove 'cyp' from end of subdomains
  mutate(subdomain = gsub(" cyp$", "", subdomain)) %>%
  # Add in subdomains that don't appear in data
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
  add_row(domain = "Structural", subdomain = "Societal optimism", dummy = 1)

# Write out adult as a CSV for QA purposes
write_csv(adult_mwb_separated, "data/adult_mwb_separated.csv")

## Create format for table
adult_mwb_table <- adult_mwb_separated %>%
  # Create a column with both subdomain and subdomain topic
  # CHANGE THIS: HAVE SEPARATE SUB AND TOPIC COLUMNS
  mutate(sub_and_topic = paste0(subdomain, ": ", subdomain_topic)) %>%
  # The domain and subdomain and topic need to be grouped together again
  group_by(across(c(study_id, title, aim, types_of_studies, dummy))) %>%
  summarise(domain = paste(unique(domain), collapse = "; "),
            sub_and_topic = paste(unique(sub_and_topic), collapse="; ")) %>%
  filter(dummy == 0) %>%
  ungroup() %>%
  select(study_id, title, aim, types_of_studies, domain, sub_and_topic)