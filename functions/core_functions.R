####################### Core functions #######################

# Add n linebreaks
linebreaks <- function(n){HTML(strrep(br(), n))}

# Remove warnings from icons 
icon_no_warning_fn = function(icon_name) {
  icon(icon_name, verify_fa=FALSE)
}

# Generic data table
make_table <- function(input_data_table,
                      rows_to_display = 20
){

# Take out underscores in column names for display purposes
table_colnames  <-  gsub("_", " ", colnames(input_data_table))

dt <- DT::datatable(input_data_table, style = 'bootstrap',
                      class = 'table-bordered table-condensed',
                      rownames = FALSE,
                      filter="top",
                      colnames = table_colnames,
                      options = list(pageLength = rows_to_display,
                                     scrollX = FALSE,
                                     scrollY = FALSE,
                                     dom = 'pftl',
                                     autoWidth = TRUE,
                                     # style header
                                     initComplete = htmlwidgets::JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#C5C3DA', 'color': '#3F3685'});",
                                       "$(this.api().table().row().index()).css({'background-color': '#C5C3DA', 'color': '#3F3685'});",
                                       "}")))


  return(dt)
}

# Set UI for 'how to use EGM' on intro page
# Name this so it can be used when switching tabs
intro_use_ui <- tagList(h2("How to use the Evidence and Gap Map (EGM)"),
                        p("You can click on the 'Click here for a guided tour of the page' button on the Evidence and gap map tab to get a quick guided tour."),
                        p("If you'd like more details, read the information below or watch our walkthrough and example videos."),
                        h3("Walkthrough video"),
                        p("The video below shows a walkthrough of the EGM"),
                        HTML('<iframe width="560" height="315" src="https://youtu.be/_fhDVVfELsM?si=bp-D29qIFVj0_Kp0" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                        h3("Example video"),
                        p("The video below shows an example of how to use the evidence and gap map to answer the question:"),
                        p("What review-level evidence is available on schools-based interventions that could reduce the risk of self-harming behaviour in children aged up to 18?"),
                        p("[video goes here]"),
                        h3("What the self-harm EGM can and can’t do"),
                        p("The self-harm EGM", tags$b("does"), ":"),
                        tags$ul(
                          tags$li("Provide a visual representation of the evidence landscape"),
                          tags$li("Tell you where there is and isn’t evidence; however an empty cell can mean that there was no evidence that met inclusion criteria, i.e. an empty cell in an EGM including only review-level evidence may mean that evidence is available at a lower tier (e.g. primary studies)"),
                          tags$li("Have an intuitive and interactive format"),
                          tags$li("Enable filtering of the evidence through systematic categorisation of reviews"),
                          tags$li("Allow regular updates to the included evidence"),
                          tags$li("Provide a foundation for more focused synthesis of the included evidence.")
                        ),
                        p("The self-harm EGM", tags$b("does not"), ":"),
                        tags$ul(
                          tags$li("Tell you what the evidence says"),
                          tags$li("Assess the quality of the included reviews but the filters allow users to search based on quality criteria (i.e. availability of a pre-registered protocol, review author critical appraisal of primary studies)"),
                          tags$li("Tell you all of the information you will need to know to make decisions. EGMs work best when considered alongside other data and intelligence."),
                        ),
                        
                        h3("How the EGM is organised"),
                        p("The EGM is organised into a matrix. Reviews are thematically categorised by domains/subdomains into rows and allocated to columns based on whether the review explores association of risk/protective factors with self-harm or interventions for self-harm.
            More information about the domains and subdomains are in ", tags$a(href = "https://publichealthscotland.scot/publications/children-and-young-people-mental-health-indicator-resources/",
                                                                               tags$b("Public Health Scotland's children and young people mental health indicator resources"))
                        ),
                        p("The intersecting cell in the matrix shows either a coloured shape for that combination of domain/subdomain and review type (green square = risk/protective factor reviews and blue circle = intervention reviews) or an empty cell. The size of the shape gives an indication of the quantity of available reviews and hovering over it provides the exact number of reviews. An empty cell indicates that review-level evidence is lacking."),
                        p("Clicking on the shapes in the cell retrieves the underlying reference(s) and study details in an evidence table."),
                        
                        h3("How the table is organised"),
                        p("The table presents key characteristics of each included review, such as:"),
                        tags$ul(
                          tags$li("author and date"),
                          tags$li("study title and aim"),
                          tags$li("outcome definition"),
                          tags$li("population characteristics"),
                          tags$li("study setting"),
                          tags$li("domain/subdomain"),
                          tags$li("review type and design of primary studies"),
                          tags$li("number of primary studies"),
                          tags$li("inclusion of quality indicators i.e., whether the review authors undertook critical appraisal of primary studies, and whether a preregistered protocol is available or not"),
                          tags$li("whether it is an empty review i.e. the authors of the review searched for but did not find evidence relevant to this self-harm EGM.")
                        ),
                        p("The table also provides a hyperlink to the database record of the review."),
                        p("Shaded rows highlight empty reviews."),
                        p("The table has a free-text search function and the search results or the full table can be downloaded as CSV file."),
                        
                        h3("How the filters work"),
                        p("Using the filters helps to refine the EGM and limit the contents of the matrix to specific types of evidence. The EGM can be filtered by:"),
                        tags$ul(
                          tags$li("outcome definition"),
                          tags$li("domains and subdomains"),
                          tags$li("population age"),
                          tags$li("population characteristics"),
                          tags$li("type of synthesis"),
                          tags$li("specific types of interventions"),
                          tags$li("inclusion of author quality appraisal of primary studies"),
                          tags$li("existence of a pre-registered protocol")
                        ),
                        p("More than one filter can be selected at a time. Applying these filters will impact both the EGM and table. The EGM and table can be reset by clearing all the filters.")
                        
) # tagList

# Modify bubble_grid function for our purposes
# Change some defaults so that it fits with what we're most likely to use

bubble_grid_modified <- function(data,
                                 shape = "circles",
                                 colors = c("#15607A", "#FFFFFF", "#FA8C00"),
                                 color_ref = NULL,
                                 color_by = NULL,
                                 min_value = 7, # Was 7
                                 max_value = 70, # Was 50
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
      
      label <- paste0(sub("Risk_protective_factor", "Risk/protective factor", sub(".*\\.", "", name)), ": ", value) # Include the sub-column name in the tooltip (the text after the dot in the column name). Note that the . has to be 'escaped' with "\\".
      
    } else {
      
      label <- paste0(sub("Risk_protective_factor", "Risk/protective factor", sub(".*\\.", "", name)), ": ", number_fmt(value))
      
    }
    
    tooltip_label <- sprintf('<span style="font-size:1.5em">%s</span>', label)
    
    if (is.logical(span)) {
      
      if (span) {
        
        normalized <- (value - min(dplyr::select_if(data, is.numeric), na.rm = TRUE)) / (max(dplyr::select_if(data, is.numeric), na.rm = TRUE) - min(dplyr::select_if(data, is.numeric), na.rm = TRUE))
        
        ### width of data_bars
        size <- if (max(value == 0)){ "0px" } # Show nothing on the chart if everything has been filtered out
          
          else if ((is.numeric(value) & is.null(max_value) & is.null(min_value)) | value == 0) { # Added - use this condition if the value is 0
          
          paste0(abs(value) / max(dplyr::select_if(data, is.numeric), na.rm = TRUE) * 100, "px")
          
          ### min_value provided
        } else if (is.numeric(value) & is.null(max_value) & !is.null(min_value)) {
          
          paste0((abs(value) - min_value) / (max(dplyr::select_if(data, is.numeric), na.rm = TRUE) - min_value) * 100, "px")
          
          ### max_value provided
        } else if (is.numeric(value) & !is.null(max_value) & is.null(min_value)) {
          
          paste0((abs(value) / max_value) * 100, "px")
          
          ### min and max provided
        } else if (is.numeric(value) & !is.null(max_value) & !is.null(min_value)) {
          
          paste0(min_value + (value - min(dplyr::select_if(data, is.numeric), na.rm = TRUE) * (max_value - min_value))
                 / (max(dplyr::select_if(data, is.numeric), na.rm = TRUE) - min(dplyr::select_if(data, is.numeric), na.rm = TRUE)) * 100, "px")
          
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
            size <- if ((is.numeric(data[[color_by]][index]) & is.null(max_value) & is.null(min_value)) | value == 0) { # Added - use this condition if the value is 0
              
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
          size <- if ((is.numeric(value) & is.null(max_value) & is.null(min_value)) | value == 0) { # Added - use this condition if the value is 0
            
            paste0(abs(value) / max(abs(data[[name]]), na.rm = TRUE) * 100, "px")
            
            ### min_value provided
          } else if (is.numeric(value) & is.null(max_value) & !is.null(min_value)) {
            
            paste0((abs(value) - min_value) / (max(dplyr::select_if(data, is.numeric), na.rm = TRUE) - min_value) * 100, "px")
            
            ### max_value provided
          } else if (is.numeric(value) & !is.null(max_value) & is.null(min_value)) {
            
            paste0((abs(value) / max_value) * 100, "px")
            
            ### min and max provided
          } else if (is.numeric(value) & !is.null(max_value) & !is.null(min_value)) {
            
            paste0(min_value + (value - min(dplyr::select_if(data, is.numeric), na.rm = TRUE) * (max_value - min_value))
                   / (max(dplyr::select_if(data, is.numeric), na.rm = TRUE) - min(dplyr::select_if(data, is.numeric), na.rm = TRUE)), "px")
            
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
    
    # Don't display a shape if size = 0
    if(size == "0px"){
      display_var <- "none"
    } else display_var <- "inline-flex"
    
    
    if (brighten_text == FALSE & show_text == TRUE) {
      
      if (tooltip == TRUE) {
        
        htmltools::tagAppendChild(
          htmltools::tags$button(
            type = "button",
            class = "shape-button",
            'aria-label' = "The visual EGM is not accessible by screen reader. Please use the Show EGM as text button above to access an accessible version.",
            style = list(background = cell_color,
                         color = text_color,
                         display = display_var,
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
                         clipPath = clippath,
                         borderStyle = none,
                         padding = "0px")),
          tippy::tippy(label,
                       animateFill = FALSE,
                       followCursor = TRUE,
                       tooltip = tooltip_label)
        )
        
      } else {
        
        htmltools::tags$button(label,
                               type = "button",
                               class = "shape-button",
                               'aria-label' = "The visual EGM is not accessible by screen reader. Please use the Show EGM as text button above to access an accessible version.",
                       style = list(background = cell_color,
                                    color = text_color,
                                    display = display_var,
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
                                    clipPath = clippath,
                                    borderStyle = "none",
                                    padding = "0px"))
      }
      
    } else if (brighten_text == TRUE & !is.null(text_color_ref) & show_text == TRUE) {
      
      if (tooltip == TRUE) {
        
        htmltools::tagAppendChild(
          htmltools::tags$button(
            type = "button",
            class = "shape-button",
            'aria-label' = "The visual EGM is not accessible by screen reader. Please use the Show EGM as text button above to access an accessible version.",
            style = list(background = cell_color,
                         color = text_color,
                         display = display_var,
                         justifyContent = "center",
                         alignItems = "center",
                         textAlign = "center",
                         height = size,
                         width = size,
                         borderRadius = radius,
                         boxShadow = box_shadow,
                         fontSize = text_size,
                         transition = animation,
                         clipPath = clippath,
                         borderStyle = "none",
                         padding = "0px")),
          tippy::tippy(label,
                       animateFill = FALSE,
                       followCursor = TRUE,
                       tooltip = tooltip_label)
        )
        
      } else {
        
        htmltools::tags$button(label,
                               type = "button",
                               class = "shape-button",
                               'aria-label' = "The visual EGM is not accessible by screen reader. Please use the Show EGM as text button above to access an accessible version.",
                       style = list(background = cell_color,
                                    color = text_color,
                                    display = display_var,
                                    justifyContent = "center",
                                    alignItems = "center",
                                    textAlign = "center",
                                    height = size,
                                    width = size,
                                    borderRadius = radius,
                                    boxShadow = box_shadow,
                                    fontSize = text_size,
                                    transition = animation,
                                    clipPath = clippath,
                                    borderStyle = "none",
                                    padding = "0px"))
      }
      
    } else if (brighten_text == FALSE & show_text == FALSE) {
      
      if (tooltip == TRUE) {
        
        htmltools::tagAppendChild(
          htmltools::tags$button(
            type = "button",
            class = "shape-button",
            'aria-label' = "The visual EGM is not accessible by screen reader. Please use the Show EGM as text button above to access an accessible version.",
            style = list(background = cell_color,
                         display = display_var,
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
                         clipPath = clippath,
                         borderStyle = "none",
                         padding = "0px")),
          tippy::tippy(label,
                       animateFill = FALSE,
                       followCursor = TRUE,
                       tooltip = tooltip_label)
        )
        
      } else {
        
        htmltools::tags$button(label,
                               type = "button",
                               class = "shape-button",
                               'aria-label' = "The visual EGM is not accessible by screen reader. Please use the Show EGM as text button above to access an accessible version.",
                       style = list(background = cell_color,
                                    display = display_var,
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
                                    clipPath = clippath,
                                    borderStyle = "none",
                                    padding = "0px"))
      }
      
    } else if (brighten_text == TRUE & show_text == FALSE) {
      
      if (tooltip == TRUE) {
        
        htmltools::tagAppendChild(
          htmltools::tags$button(
            type = "button",
            class = "shape-button",
            'aria-label' = "The visual EGM is not accessible by screen reader. Please use the Show EGM as text button above to access an accessible version.",
            style = list(background = cell_color,
                         display = display_var,
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
                         clipPath = clippath,
                         borderStyle = "none",
                         padding = "0px")),
          tippy::tippy(label,
                       animateFill = FALSE,
                       followCursor = TRUE,
                       tooltip = tooltip_label)
        )
        
      } else {
        
        htmltools::tags$button(label,
                               type = "button",
                               class = "shape-button",
                               'aria-label' = "The visual EGM is not accessible by screen reader. Please use the Show EGM as text button above to access an accessible version.",
                       style = list(background = cell_color,
                                    display = display_var,
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
                                    clipPath = clippath,
                                    borderStyle = "none",
                                    padding = "0px"))
      }
      
    } else {
      
      if (tooltip == TRUE) {
        
        htmltools::tagAppendChild(
          htmltools::tags$button(
            type = "button",
            class = "shape-button",
            'aria-label' = "The visual EGM is not accessible by screen reader. Please use the Show EGM as text button above to access an accessible version.",
            style = list(background = cell_color,
                         color = font_color,
                         display = display_var,
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
                         clipPath = clippath,
                         borderStyle = "none",
                         padding = "0px")),
          tippy::tippy(label,
                       animateFill = FALSE,
                       followCursor = TRUE,
                       tooltip = tooltip_label)
        )
        
      } else {
        
        htmltools::tags$button(label,
                               type = "button",
                               class = "shape-button",
                               'aria-label' = "The visual EGM is not accessible by screen reader. Please use the Show EGM as text button above to access an accessible version.",
                       style = list(background = cell_color,
                                    color = font_color,
                                    display = display_var,
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
                                    clipPath = clippath,
                                    borderStyle = "none",
                                    padding = "0px"))
        
      }
    }
  }
}

# CSV download button for table

csvDownloadButton <- function(id, filename = "data.csv", label = "Download as CSV") {
  tags$button(
    tagList(icon("download"), label),
    onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')", id, filename)
  )
}

# Show a navy navigation spinner

withNavySpinner <- function(out){
  withSpinner(out, color = navy)
}

# Get click data

get_click_data <- JS("function(rowInfo, column) {
        // Don't handle click events in the domain or subdomain columns
    if (column.id === 'domain' || column.id === 'subdomain') {
      return
    }
    // Send the click event to Shiny, which will be available in input$click_details
    if (window.Shiny) {
      Shiny.setInputValue('click_details', { domain: rowInfo.values.domain, subdomain: rowInfo.values.subdomain, outcome_and_type: column.id }, { priority: 'event' })
    }
  }")