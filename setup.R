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
library(shinyTree)

# Data wrangling packages ----
library(tidyverse)
library(janitor)
library(jsonlite)
library(stringr)
library(readxl)

# Plotting packages ----
library(plotly)
library(ggh4x)

# Package for relative file references (needed to deploy app) 
library(here)

# PHS styling packages ----
library(phsstyles)

# HTML tools
library(htmltools)

## Load in data

## Dataframe for plots
reviews_chart <- readRDS(here("data/self-harm_egm_chart_data.rds"))


# Dataframe for table
reviews_table <- readRDS(here("data/self-harm_egm_table_data.rds"))

# Dataframe for glossary list
glossary_list <- read_xlsx(here("data/Definitions.xlsx"), sheet = "List")

# Load core functions functions ----
source("functions/core_functions.R")

## Plotting ----
# Style of x and y axis
xaxis_plots <- list(title = FALSE, tickfont = list(size=14), titlefont = list(size=14),
                    showline = TRUE, fixedrange=TRUE)

yaxis_plots <- list(title = FALSE, rangemode="tozero", fixedrange=TRUE, size = 4,
                    tickfont = list(size=14), titlefont = list(size=14))

# Buttons to remove from plotly plots
bttn_remove <-  list('select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d',
                     'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',
                     'hoverClosestCartesian')

# Home list for introduction tab

home_list<- c("About"= "about",
              "Using the dashboard"= "use",
              "Accessibility"= "accessibility",
              "Last updated" = "last_updated",
              "Contact us"= "contact")

# Set navy colour

navy <- "#010068"

# Get Cicerone functions

source(file.path("functions/guided_tours.R"), local = TRUE)$value