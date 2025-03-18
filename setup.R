####################### Setup #######################

# Shiny packages ----
library(shiny)
library(shinycssloaders)
library(shinymanager)
library(shinyWidgets)
library(reactable)
library(reactablefmtr)
library(tippy)
library(shinyjs)
library(shinyBS)
library(cicerone)
library(jsTreeR)
library(shinydashboard)

# Data wrangling packages ----
library(tidyverse)
library(janitor)
library(jsonlite)
library(stringr)
library(readxl)
library(jsonlite)
library(arrow)

# Plotting packages ----
library(plotly)
library(ggh4x)

# Package for relative file references (needed to deploy app) 
library(here)

# PHS styling packages ----
#library(phsstyles)

# HTML tools
library(htmltools)

### Load in data

# Dataframe for adult plot
wemwebs_adult_chart_data <- read_parquet(here("data/wemwebs_adult_chart_data.parquet"))

# Dataframe for adult table
wemwebs_adult_table_data <- read_parquet(here("data/wemwebs_adult_table_data.parquet"))

# Dataframe for CYP plot
wemwebs_cyp_chart_data <- read_parquet(here("data/wemwebs_cyp_chart_data.parquet"))

# Dataframe for CYP table
wemwebs_cyp_table_data <- read_parquet(here("data/wemwebs_cyp_table_data.parquet"))

# Dataframe for glossary list
#glossary_list <- read_xlsx(here("data/Definitions.xlsx"), sheet = "List")
