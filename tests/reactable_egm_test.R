######## Test to build EGM using Reactable package

#install.packages("reactable")
#install.packages("reactablefmtr") # For easier formatting

### Load packages

library(here)
library(tidyverse)
library(janitor)
library(reactable)
library(reactablefmtr)
library(tippy)

### Set up data

## Chart data

# Read in data processed in data_preparation script
reviews_chart <- readRDS(here("data/self-harm_egm_chart_data.rds"))

# For this test, assume that all reviews have been selected.
# In the full app, deselecting something using filters sets 'selected' to 0.
all_selected <- reviews_chart %>%
  mutate(selected = 1)

# Group and count reviews in each category
grouped <- all_selected %>%
  group_by(domain, subdomain, overall_outcome, intervention_exposure_short) %>% # Don't group on pos_x, pos_y or review_type
  summarise(count = sum(selected)) %>%
  ungroup()

## Create a reactable

# Pivot data into the right format
count_pivot <- grouped %>%
  mutate(overall_outcome = gsub(" |-", "_", overall_outcome)) %>% # Replace spaces and hyphens with underscores for better variable names (hyphens seem to cause problems with grouping columns below)
  pivot_wider(names_from = c(overall_outcome, intervention_exposure_short), values_from = count, names_sep = ".") %>%
  mutate(across(Self_harm.Exposure:Outcome_category_3.Exposure, ~replace_na(., 0))) # Replace NAs with 0


# Create grouped columns for the outcome areas and exposure/intervention types

## Trying a manual way to set font sizes. But we can also do this and add shapes in one go with bubble_grid from reatablefmtr (see below).

# To set font size, use 'em', which changes the font size relative to the font size that's been set for the table. 1em = the set font size.
# Function to normalise font size for all numeric columns
# set_font_size = function(study_count){
#   numeric_cols <- count_pivot[, 3:ncol(count_pivot)]
#   normalised <- (study_count - min(numeric_cols, na.rm = TRUE))/(max(numeric_cols, na.rm = TRUE) - min(numeric_cols, na.rm = TRUE)) * 10
#   return(list(fontSize = paste0(normalised,"em")))
# }
# 
# set_font_size_for_icon = function(study_count){
#   numeric_cols <- count_pivot[, 3:ncol(count_pivot)]
#   normalised <- (study_count - min(numeric_cols, na.rm = TRUE))/(max(numeric_cols, na.rm = TRUE) - min(numeric_cols, na.rm = TRUE)) * 10
#   return(list(fontSize = normalised))
# }
# 
# reactable(
#   count_pivot,
#   groupBy = "domain",
#   columns = list(
#     Self_harm.Exposure = colDef(name = "Exposure", aggregate = "sum",
#                                 style = function(value){
#                                   set_font_size(value)
#                                 }),
#     Self_harm.Intervention = colDef(name = "Intervention", aggregate = "sum",
#                                     style = function(value){
#                                       set_font_size(value)
#                                     }),
#     Self_harm.Attitudes = colDef(name = "Attitudes", aggregate = "sum",
#                                  style = function(value){
#                                    set_font_size(value)
#                                  }),
#     Outcome_category_2.Exposure = colDef(name = "Exposure", aggregate = "sum",
#                                          style = function(value){
#                                            set_font_size(value)
#                                          }),
#     Outcome_category_2.Intervention = colDef(name = "Intervention", aggregate = "sum",
#                                              style = function(value){
#                                                set_font_size(value)
#                                              }),
#     Outcome_category_3.Exposure = colDef(name = "Exposure", aggregate = "sum",
#                                          style = function(value){
#                                            set_font_size(value)
#                                          }),
#     Outcome_category_3.Intervention = colDef(name = "Intervention", aggregate = "sum",
#                                              style = function(value){
#                                                set_font_size(value)
#                                              })
#   ),
#   columnGroups = list(
#     colGroup(name = "Self-harm", columns = c("Self_harm.Exposure", "Self_harm.Intervention", "Self_harm.Attitudes")),
#     colGroup(name = "Outcome category 2", columns = c("Outcome_category_2.Exposure", "Outcome_category_2.Intervention")),
#     colGroup(name = "Outcome category 3", columns = c("Outcome_category_3.Exposure", "Outcome_category_3.Intervention"))
#   )
# )

# Modify bubble_grid function for our purposes
# Change some defaults so that it fits with what we're most likely to use
# Modify in two ways: first, add a traingle as a third shape

bubble_grid_modified <- function(data,
                        shape = "circles",
                        colors = c("#15607A", "#FFFFFF", "#FA8C00"),
                        color_ref = NULL,
                        color_by = NULL,
                        min_value = NULL,
                        max_value = NULL,
                        opacity = 1,
                        bias = 1,
                        number_fmt = NULL,
                        text_size = NULL,
                        text_color = "black",
                        text_color_ref = NULL,
                        show_text = FALSE, # Default changed
                        brighten_text = TRUE,
                        brighten_text_color = "white",
                        bold_text = FALSE,
                        span = TRUE, # Default changed
                        box_shadow = FALSE,
                        tooltip = TRUE, # Default changed
                        animation = "background 1s ease") {
  
  '%notin%' <- Negate('%in%')
  
  if (!is.null(shape) && shape %notin% c("circles", "squares", "triangles") == TRUE) {
    
    stop("`shape` must be either 'circles', 'squares' or 'trinagles")
  } # Include triangles in allowed shapes
  
  if (!is.logical(bold_text)) {
    
    stop("`bold_text` must be TRUE or FALSE")
  }
  
  if (!is.logical(brighten_text)) {
    
    stop("`brighten_text` must be TRUE or FALSE")
  }
  
  if (!is.logical(box_shadow)) {
    
    stop("`box_shadow` must be TRUE or FALSE")
  }
  
  if (!is.logical(tooltip)) {
    
    stop("`tooltip` must be TRUE or FALSE")
  }
  
  if (!is.numeric(bias)) {
    
    stop("`bias` must be numeric")
  }
  
  if (!is.numeric(opacity)) {
    
    stop("`opacity` must be numeric")
  }
  
  if (opacity < 0 | opacity > 1) {
    
    stop("`opacity` must be a value between 0 and 1")
  }
  
  if (length(text_color) > 1) {
    
    stop("multiple colors detected in `text_color`. only one color can be used.")
  }
  
  if (length(brighten_text_color) > 1) {
    
    stop("multiple colors detected in `brighten_text_color` only one color can be used.")
  }
  
  color_pal <- function(x) {
    
    if (!is.na(x))
      rgb(colorRamp(c(colors), bias = bias)(x), maxColorValue = 255)
    else
      NULL
  }
  
  assign_color <- function(x) {
    
    if (!is.na(x)) {
      rgb_sum <- rowSums(colorRamp(c(colors), bias = bias)(x))
      color <- ifelse(rgb_sum >= 395, text_color, brighten_text_color)
      color
    } else
      NULL
  }
  
  if (bold_text == TRUE) {
    
    bold_text <- "bold"
    
  } else bold_text <- "normal"
  
  if (box_shadow == TRUE) {
    
    box_shadow <- "0 6px 6px -4px #888888"
    
  } else box_shadow <- NULL
  
  cell <- function(value, index, name) {
    
    if (is.null(color_ref) & is.null(color_by) & !is.numeric(value)) return(value)
    
    if (is.null(number_fmt)) {
      
      label <- paste0(sub(".*\\.", "", name), ": ", value) # Include the sub-column name in the tooltip (the text after the dot in the column name). Note that the . has to be 'escaped' with "\\".
      
    } else {
      
      label <- paste0(sub(".*\\.", "", name), ": ", number_fmt(value))
      
    }
    
    tooltip_label <- sprintf('<span style="font-size:1.5em">%s</span>', label)
    
    if (is.logical(span)) {
      
      if (span) {
        
        normalized <- (value - min(dplyr::select_if(data, is.numeric), na.rm = TRUE)) / (max(dplyr::select_if(data, is.numeric), na.rm = TRUE) - min(dplyr::select_if(data, is.numeric), na.rm = TRUE))
        
        ### width of data_bars
        size <- if (is.numeric(value) & is.null(max_value) & is.null(min_value)) {
          
          paste0(abs(value) / max(dplyr::select_if(data, is.numeric), na.rm = TRUE) * 100, "px")
          
          ### min_value provided
        } else if (is.numeric(value) & is.null(max_value) & !is.null(min_value)) {
          
          paste0((abs(value) - min_value) / (max(dplyr::select_if(data, is.numeric), na.rm = TRUE) - min_value) * 100, "px")
          
          ### max_value provided
        } else if (is.numeric(value) & !is.null(max_value) & is.null(min_value)) {
          
          paste0((abs(value) / max_value) * 100, "px")
          
          ### min and max provided
        } else if (is.numeric(value) & !is.null(max_value) & !is.null(min_value)) {
          
          paste0((abs(value) - min_value) / (max_value - min_value) * 100, "px")
          
        } else if (!is.numeric(value)) {
          
          return(value)
        }
        
      } else if (!is.null(color_ref)) {
        
        normalized <- dplyr::ntile(data[[name]], n = length(colors))
        
      } else {
        
        ### color_by
        if (is.character(color_by)) {
          
          # color_by column must be numeric
          if (all(color_by %in% names(which(sapply(data, is.numeric))))) {
            
            if (is.character(color_by)) { color_by <- which(names(data) %in% color_by) }
            
            # if there is no variance in the column, assign the same color to each value
            if (is.numeric(data[[color_by]]) & mean((data[[color_by]] - mean(data[[color_by]], na.rm=TRUE)) ^ 2, na.rm=TRUE) == 0) {
              
              normalized <- 1
              
            } else {
              
              normalized <- (data[[color_by]][index] - min(data[[color_by]], na.rm = TRUE)) / (max(data[[color_by]], na.rm = TRUE) - min(data[[color_by]], na.rm = TRUE))
              
            }
            
            cell_color <- color_pal(normalized)
            cell_color <- suppressWarnings(grDevices::adjustcolor(cell_color, alpha.f = opacity))
            font_color <- assign_color(normalized)
            
            ### width of data_bars
            size <- if (is.numeric(data[[color_by]][index]) & is.null(max_value) & is.null(min_value)) {
              
              paste0(abs(data[[color_by]][index]) / max(abs(data[[color_by]]), na.rm = TRUE) * 100, "px")
              
              ### min_value provided
            } else if (is.numeric(data[[color_by]][index]) & is.null(max_value) & !is.null(min_value)) {
              
              paste0((abs(data[[color_by]][index]) - min_value) / (max(abs(data[[color_by]]), na.rm = TRUE) - min_value) * 100, "px")
              
              ### max_value provided
            } else if (is.numeric(data[[color_by]][index]) & !is.null(max_value) & is.null(min_value)) {
              
              paste0((abs(data[[color_by]][index]) / max_value) * 100, "px")
              
              ### min and max provided
            } else if (is.numeric(data[[color_by]][index]) & !is.null(max_value) & !is.null(min_value)) {
              
              paste0((abs(data[[color_by]][index]) - min_value) / (max_value - min_value) * 100, "px")
              
            }
            
          } else {
            
            stop("Attempted to select non-existing column or non-numeric column with color_by")
          }
          
        } else {
          
          # standard normalization (no variance check)
          if (is.numeric(value) & mean((data[[name]] - mean(data[[name]], na.rm=TRUE)) ^ 2, na.rm=TRUE) == 0) {
            
            normalized <- 1
            
          } else {
            
            # standard normalization
            normalized <- (value - min(data[[name]], na.rm = TRUE)) / (max(data[[name]], na.rm = TRUE) - min(data[[name]], na.rm = TRUE))
            
          }
          
          cell_color <- color_pal(normalized)
          cell_color <- suppressWarnings(grDevices::adjustcolor(cell_color, alpha.f = opacity))
          font_color <- assign_color(normalized)
          
          ### width of data_bars
          size <- if (is.numeric(value) & is.null(max_value) & is.null(min_value)) {
            
            paste0(abs(value) / max(abs(data[[name]]), na.rm = TRUE) * 100, "px")
            
            ### min_value provided
          } else if (is.numeric(value) & is.null(max_value) & !is.null(min_value)) {
            
            paste0((abs(value) - min_value) / (max(abs(data[[name]]), na.rm = TRUE) - min_value) * 100, "px")
            
            ### max_value provided
          } else if (is.numeric(value) & !is.null(max_value) & is.null(min_value)) {
            
            paste0((abs(value) / max_value) * 100, "px")
            
            ### min and max provided
          } else if (is.numeric(value) & !is.null(max_value) & !is.null(min_value)) {
            
            paste0((abs(value) - min_value) / (max_value - min_value) * 100, "px")
            
          }
          
        }
        
      }
      
      ### conditional text color
      if (is.character(text_color_ref)) {
        
        if (all(text_color_ref %in% names(which(sapply(data, is.character))))) {
          
          if (is.character(text_color_ref)) { text_color_ref <- which(names(data) %in% text_color_ref) }
          
          font_color <- data[[text_color_ref]][index]
          text_color <- data[[text_color_ref]][index]
          
        } else {
          
          stop("Attempted to select non-existing column or non-character column with text_color_ref")
        }
        
      } else {
        
        font_color <- text_color
      }
      
      ### conditional fill color and font color
      if (is.character(color_ref)) {
        
        if (all(color_ref %in% names(which(sapply(data, is.character))))) {
          
          if (is.character(color_ref)) { color_ref <- which(names(data) %in% color_ref) }
          
          cell_color <- data[[color_ref]][index]
          cell_color <- suppressWarnings(grDevices::adjustcolor(cell_color, alpha.f = opacity))
          
          rgb_sum <- rowSums(grDevices::colorRamp(c(cell_color), bias = bias)(1))
          
          font_color <- ifelse(rgb_sum >= 395, text_color, brighten_text_color)
          
        } else {
          
          stop("Attempted to select non-existing column or non-character column with fill_color_ref")
        }
        
      } else {
        
        cell_color <- color_pal(normalized)
        cell_color <- suppressWarnings(grDevices::adjustcolor(cell_color, alpha.f = opacity))
        font_color <- assign_color(normalized)
        
      }
      
    } else if (is.numeric(span) | is.character(span)) {
      
      if (all(span %in% which(sapply(data, is.numeric))) | all(span %in% names(which(sapply(data, is.numeric))))) {
        
        if (is.character(span)) { span <- which(names(data) %in% span) }
        
        normalized <- (value - min(dplyr::select(data, !!span), na.rm = TRUE)) / (max(dplyr::select(data, !!span), na.rm = TRUE) - min(dplyr::select(data, !!span), na.rm = TRUE))
        cell_color <- if (name %in% colnames(data)[span]) { suppressWarnings(grDevices::adjustcolor(color_pal(normalized), alpha.f = opacity)) }
        font_color <- if (name %in% colnames(data)[span]) { assign_color(normalized) }
        
      } else {
        
        stop("Attempted to select non-existing or non-numeric columns with span")
        
      }
      
    }
    
    # adjust border radius and clipPath based on shape
    if (shape == "circles") {
      radius <- "50%"
    } else radius <- NULL
    
    if (shape == "triangles"){
      clippath <- "polygon(50% 0, 100% 100%, 0 100%)"
    } else clippath <- NULL

    
    if (brighten_text == FALSE & show_text == TRUE) {
      
      if (tooltip == TRUE) {
        
        htmltools::tagAppendChild(
          htmltools::div(
            style = list(background = cell_color,
                         color = text_color,
                         display = "inline-flex",
                         justifyContent = "center",
                         alignItems = "center",
                         textAlign = "center",
                         height = size,
                         width = size,
                         borderRadius = radius,
                         fontWeight = bold_text,
                         boxShadow = box_shadow,
                         fontSize = text_size,
                         transition = animation,
                         clipPath = clippath)),
          tippy::tippy(label,
                       animateFill = FALSE,
                       followCursor = TRUE,
                       tooltip = tooltip_label)
        )
        
      } else {
        
        htmltools::div(label,
                       style = list(background = cell_color,
                                    color = text_color,
                                    display = "inline-flex",
                                    justifyContent = "center",
                                    alignItems = "center",
                                    textAlign = "center",
                                    height = size,
                                    width = size,
                                    borderRadius = radius,
                                    fontWeight = bold_text,
                                    boxShadow = box_shadow,
                                    fontSize = text_size,
                                    transition = animation,
                                    clipPath = clippath))
      }
      
    } else if (brighten_text == TRUE & !is.null(text_color_ref) & show_text == TRUE) {
      
      if (tooltip == TRUE) {
        
        htmltools::tagAppendChild(
          htmltools::div(
            style = list(background = cell_color,
                         color = text_color,
                         display = "inline-flex",
                         justifyContent = "center",
                         alignItems = "center",
                         textAlign = "center",
                         height = size,
                         width = size,
                         borderRadius = radius,
                         boxShadow = box_shadow,
                         fontSize = text_size,
                         transition = animation,
                         clipPath = clippath)),
          tippy::tippy(label,
                       animateFill = FALSE,
                       followCursor = TRUE,
                       tooltip = tooltip_label)
        )
        
      } else {
        
        htmltools::div(label,
                       style = list(background = cell_color,
                                    color = text_color,
                                    display = "inline-flex",
                                    justifyContent = "center",
                                    alignItems = "center",
                                    textAlign = "center",
                                    height = size,
                                    width = size,
                                    borderRadius = radius,
                                    boxShadow = box_shadow,
                                    fontSize = text_size,
                                    transition = animation,
                                    clipPath = clippath))
      }
      
    } else if (brighten_text == FALSE & show_text == FALSE) {
      
      if (tooltip == TRUE) {
        
        htmltools::tagAppendChild(
          htmltools::div(
            style = list(background = cell_color,
                         display = "inline-flex",
                         justifyContent = "center",
                         alignItems = "center",
                         textAlign = "center",
                         height = size,
                         width = size,
                         borderRadius = radius,
                         color = "transparent",
                         boxShadow = box_shadow,
                         fontSize = text_size,
                         transition = animation,
                         clipPath = clippath)),
          tippy::tippy(label,
                       animateFill = FALSE,
                       followCursor = TRUE,
                       tooltip = tooltip_label)
        )
        
      } else {
        
        htmltools::div(label,
                       style = list(background = cell_color,
                                    display = "inline-flex",
                                    justifyContent = "center",
                                    alignItems = "center",
                                    textAlign = "center",
                                    height = size,
                                    width = size,
                                    borderRadius = radius,
                                    fontSize = 0,
                                    boxShadow = box_shadow,
                                    fontSize = text_size,
                                    transition = animation,
                                    clipPath = clippath))
      }
      
    } else if (brighten_text == TRUE & show_text == FALSE) {
      
      if (tooltip == TRUE) {
        
        htmltools::tagAppendChild(
          htmltools::div(
            style = list(background = cell_color,
                         display = "inline-flex",
                         justifyContent = "center",
                         alignItems = "center",
                         textAlign = "center",
                         height = size,
                         width = size,
                         borderRadius = radius,
                         color = "transparent",
                         boxShadow = box_shadow,
                         fontSize = text_size,
                         transition = animation,
                         clipPath = clippath)),
          tippy::tippy(label,
                       animateFill = FALSE,
                       followCursor = TRUE,
                       tooltip = tooltip_label)
        )
        
      } else {
        
        htmltools::div(label,
                       style = list(background = cell_color,
                                    display = "inline-flex",
                                    justifyContent = "center",
                                    alignItems = "center",
                                    textAlign = "center",
                                    height = size,
                                    width = size,
                                    borderRadius = radius,
                                    color = "transparent",
                                    boxShadow = box_shadow,
                                    fontSize = text_size,
                                    transition = animation,
                                    clipPath = clippath))
      }
      
    } else {
      
      if (tooltip == TRUE) {
        
        htmltools::tagAppendChild(
          htmltools::div(
            style = list(background = cell_color,
                         color = font_color,
                         display = "inline-flex",
                         justifyContent = "center",
                         alignItems = "center",
                         textAlign = "center",
                         height = size,
                         width = size,
                         borderRadius = radius,
                         boxShadow = box_shadow,
                         fontWeight = bold_text,
                         fontSize = text_size,
                         transition = animation,
                         clipPath = clippath)),
          tippy::tippy(label,
                       animateFill = FALSE,
                       followCursor = TRUE,
                       tooltip = tooltip_label)
        )
        
      } else {
        
        htmltools::div(label,
                       style = list(background = cell_color,
                                    color = font_color,
                                    display = "inline-flex",
                                    justifyContent = "center",
                                    alignItems = "center",
                                    textAlign = "center",
                                    height = size,
                                    width = size,
                                    borderRadius = radius,
                                    boxShadow = box_shadow,
                                    fontWeight = bold_text,
                                    fontSize = text_size,
                                    transition = animation,
                                    clipPath = clippath))
        
      }
    }
  }
}


## Using bubble_grid from reactable_ftmr to display shapes

count_pivot %>%
  reactable(
    defaultColDef = colDef(
    align = 'center'),
    groupBy = "domain",
    onClick = JS("function(rowInfo, column) {
        // Don't handle click events in the domain or subdomain columns
    if (column.id == 'domain' | column.id == 'subdomain') {
      return
    }
    // Display an alert dialog with details for the row
    window.alert('Details for row ' + rowInfo.index + ':\\n' + JSON.stringify(rowInfo.values, null, 2) +
                 '\\n' + 'Details for column' + JSON.stringify(column.id, null, 2))

    // Send the click event to Shiny, which will be available in input$show_details
    // Note that the row index starts at 0 in JavaScript, so we add 1
    if (window.Shiny) {
      Shiny.setInputValue('show_details', { index: rowInfo.index + 1 }, { priority: 'event' })
    }
  }"),
    columns = list(
      Self_harm.Exposure = colDef(name = "", aggregate = "sum",
                                  vAlign = "top",
                                  cell = bubble_grid_modified(
                                    data = .,
                                    colors = '#1b9e77',
                                    tooltip = TRUE
                                  )),
      Self_harm.Intervention = colDef(name = "", aggregate = "sum",
                                      vAlign = "bottom",
                                      cell = bubble_grid_modified(
                                        data = .,
                                        colors = '#d95f02',
                                        tooltip = TRUE,
                                        shape = "squares"
                                      )),
      Self_harm.Attitudes = colDef(name = "Attitudes", aggregate = "sum",
                                   vAlign = "top",
                                   cell = bubble_grid_modified(
                                     data = .,
                                     colors = '#7570b3',
                                     tooltip = TRUE,
                                     shape = "triangles"
                                   )),
      Outcome_category_2.Exposure = colDef(name = "", aggregate = "sum"),
      Outcome_category_2.Intervention = colDef(name = "", aggregate = "sum"),
      Outcome_category_3.Exposure = colDef(name = "Exposure", aggregate = "sum"),
      Outcome_category_3.Intervention = colDef(name = "Intervention", aggregate = "sum")
    ),
    columnGroups = list(
      colGroup(name = "Self-harm", columns = c("Self_harm.Exposure", "Self_harm.Intervention", "Self_harm.Attitudes")),
      colGroup(name = "Outcome category 2", columns = c("Outcome_category_2.Exposure", "Outcome_category_2.Intervention")),
      colGroup(name = "Outcome category 3", columns = c("Outcome_category_3.Exposure", "Outcome_category_3.Intervention"))
  )
)
