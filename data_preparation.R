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

## Set working directory - only need to do this if you can't get into the R project
#setwd("/PHI_conf/PHSci/Catriona/EGM/Mental_health_EGM/")

## Load quantitative data and add review type column
df_quant <- read_xlsx("data/230320 SH Phase 2 full data extract.xlsx")  %>%
  mutate(review_type = "Quantitative")

## Load qualitative data, and review type column
df_qual <- read_csv("data/230405 SH data_extract_qual.csv") %>%
  mutate(review_type = "Qualitative")

## Bind quant and qual data together
df_source <- rbind(df_quant, df_qual)

## put names into a more R-friendly format
df_source <- df_source %>% 
  clean_names()

# Come back to this - why is it needed?

## add whitespace to exposure and outcome variables to make all same length
#max_exp_char <- max(nchar(df$exposure_domain))
#max_out_char <- max(nchar(df$outcome_definition))


#df <- df %>% 
#mutate(exposure_definition = str_pad(exposure_definition,max_exp_char,"Exposure and intervention"),
#       outcome_definition = str_pad(outcome_definition,max_exp_char,"Exposure and intervention"))



## EGM specification: ----------------------------------------------------------
# Columns - Outcomes of interest 
# Rows    - Exposures or interventions of interest 
# Colour  - Review or primary study (fixed for now)
# Size    - fixed
# Shape   - Study quality
# Filters - Exposure or intervention
#         - Exposure domain
#         - Age bands (or CYP or adult)
#         - Gender 
#         - Geographical location - e.g. Scotland/UK/EU/High income 
#         - Effect direction 
## -----------------------------------------------------------------------------

# Pivot to create a subdomain column from the current separate subdomain columns
# Drop rows where subdomain is NA and change domain to sentence case
# Create a column to code for exposure, intervention or Exposure and intervention
df_pivot <- df_source %>%
  pivot_longer(cols = individual:structural,
               names_to = "domain",
               values_to = "subdomain") %>%
  drop_na(subdomain) %>%
  mutate(domain = to_sentence_case(domain),
         intervention_exposure_short = case_when(str_detect(intervention_or_exposure, "exposures") ~ "Exposure",
                                                 str_detect(intervention_or_exposure, "Intervention") ~ "Intervention",
                                                 intervention_or_exposure == "Attitudes" ~ "Attitudes"),
         outcome_definition = str_replace(outcome_definition, "SITB", "self-injurous thoughts and behaviours"),
         type_of_review = paste0(type_of_review, " (", review_type, ")"))


# Create columns for each outcome definition,subdomain and intervention classification, since some studies have several
# Also shorten any subdomains that contain 'other' descriptions to just be 'other'
# The qualitative data has a subdomain of "Parental health (including mental health)", but the bit in brackets isn't there for the quantitative data. So, remove the brackets for consistency.
df_separated <- df_pivot %>%
  separate_longer_delim(outcome_definition, delim = "; ") %>%
  separate_longer_delim(subdomain, delim = "; ") %>%
  separate_longer_delim(intervention_or_exposure, delim = "; ") %>%
  separate_longer_delim(intervention_classification, delim = "; ") %>%
  mutate(subdomain = case_when(str_detect(subdomain,"Other:") ~ paste0("Other (", domain, ")"),
                               subdomain == "Parental health (including mental health)" ~ "Parental health",
                               TRUE ~ subdomain))

# Add in some dummy outcomes for display purposes
df_separated <- df_separated %>%
  mutate(overall_outcome = "Self-harm") %>%
  add_row(overall_outcome = "Outcome category 2", outcome_definition = "Outcome 4", domain = "Individual", subdomain = "Mental health", intervention_exposure_short = "Intervention", review_type = "Quantitative") %>%
  add_row(overall_outcome = "Outcome category 2", outcome_definition = "Outcome 5", domain = "Family and friends", subdomain = "Family relations", intervention_exposure_short = "Intervention", review_type = "Qualitative") %>%
  add_row(overall_outcome = "Outcome category 2", outcome_definition = "Outcome 6", domain = "Structural", subdomain = "Exposure to harm", intervention_exposure_short = "Exposure", review_type = "Quantitative") %>%
  add_row(overall_outcome = "Outcome category 3", outcome_definition = "Outcome 7", domain = "Individual", subdomain = "Mental health", intervention_exposure_short = "Intervention", review_type = "Qualitative") %>%
  add_row(overall_outcome = "Outcome category 3", outcome_definition = "Outcome 8", domain = "Family and friends", subdomain = "Family relations", intervention_exposure_short = "Intervention", review_type = "Quantitative") %>%
  add_row(overall_outcome = "Outcome category 3", outcome_definition = "Outcome 9", domain = "Structural", subdomain = "Exposure to harm", intervention_exposure_short = "Exposure", review_type = "Quantitative") %>%
  mutate(overall_outcome = factor(overall_outcome, levels = c("Self-harm", "Outcome category 2", "Outcome category 3")),
         subdomain = factor(subdomain, levels = c("Mental health", "Intrinsic characteristics", "Family relations", "Peer and friend relationships", "Exposure to harm", "Social media use", "Stigma and discrimination", "Social inclusion", "Health behaviours", "Physical health", "Parental health", "Safety", "Belonging", "Poverty and material deprivation", "Educational environment", "Body image", "Engagement in local activities", "Pressure and expectations", "Perinatal environment", "Early development", "Engagement with learning", "Equality", "Social support", "Physical environment", "Other (Individual)", "Other (Family and friends)", "Other (Learning environment)", "Other (Community)", "Other (Structural)")))

# Add x and y co-ordinates to keep intervention and exposure points together (might be able to delete this, with new way of size showing number of studies)
df_separated <- df_separated %>%
  mutate(pos_y = if_else(intervention_exposure_short == "Intervention", 1, 2),
         pos_x = case_when(intervention_exposure_short == "Exposure" ~ 1,
                           intervention_exposure_short == "Intervention" ~ 2,
                           intervention_exposure_short == "Attitudes" ~ 3))

# Add an all subdomains column to the source table for the summary table. Then remove NAs.
df_source <- df_source %>%
  mutate(all_subdomains = paste0(individual, "; ", family_and_friends, "; ", learning_environment, "; ", community, "; ", structural),
         all_subdomains = gsub("NA; ", "", all_subdomains),
         all_subdomains = gsub("; NA", "", all_subdomains))

# Save file for use in Shiny app
saveRDS(df_separated, "data/self-harm_egm_chart_data.rds")

# Save data that hasn't been separated into a separate file for the dashboard table
saveRDS(df_source, "data/self-harm_egm_table_data.rds")