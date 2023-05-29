####################### Setup #######################

# Shiny packages ----
library(shiny)
library(shinycssloaders)
library(shinymanager)
library(shinyWidgets)

# Data wrangling packages ----
library(tidyverse)
library(janitor)

# Plotting packages ----
library(plotly)
library(ggh4x)
library(ggiraph)

# Package for relative file references (needed to deploy app) 
library(here)

# PHS styling packages ----
# This is causing a problem with deploying and it seems to run ok without it, so commenting out for now
library(phsstyles)

## Load in data

## Dataframe for plots
reviews_chart <- readRDS(here("data/self-harm_egm_chart_data.rds"))


# Dataframe for table
reviews_table <- readRDS(here("data/self-harm_egm_table_data.rds"))


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
