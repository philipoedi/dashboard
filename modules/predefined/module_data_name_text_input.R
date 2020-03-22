module_data_name_text_input_ui <- function(
  id
) {
  ns <- shiny::NS(id)
  
  tagList(
    shiny::uiOutput(
      outputId = ns("text_input")
    ),
    shiny::uiOutput(
      outputId = ns("name_error")
    )
  )
}

module_data_name_text_input <- function(
  input, output, session, .values, .label = NULL, .value = "", 
  .allow_reset = TRUE, .reset = NULL, .update_value = NULL
) {
  
  ns <- session$ns
  
  valid_characters <- c(letters, LETTERS, 0:9, "", "-")
  
  rvs <- shiny::reactiveValues(
    name_error = FALSE,
    # The value is retrieved from the ui the first time the input renders
    value = NULL
  )
  
  .group_name_error <- list(
    is_null = "Name muss mindestens aus einem Zeichen bestehen",
    invalid_character = "Name darf nur A-Z, a-z, 0-9 und '-' enthalten."
  )
  
  output$text_input <- shiny::renderUI({
    label <- handle_fun(.label)
    
    if (.allow_reset) {
      label <- div(
        label,
        actionButton(
          inputId = ns("reset"),
          label = "Zurücksetzen"
        )
      )
    }
    
    shiny::textInput(
      inputId = ns("name_text"),
      label = label,
      value = handle_fun(.value)
    )
  })
  
  output$name_error <- shiny::renderUI({
    error <- FALSE
    
    if (purrr::is_null(input$name_text) || 
        input$name_text == "") {
      error <- TRUE
      error_type <- "is_null"
    } else if (!all(stringr::str_split(input$name_text, "")[[1]] %in% 
                    valid_characters)) {
      error <- TRUE
      error_type <- "invalid_character"
    }
    
    if (error) {
      rvs$name_error <- TRUE
      # Specific error message dependent on error type
      return(.group_name_error[[error_type]])
    } else {
      rvs$name_error <- FALSE
      return(NULL)
    }
  })
  
  shiny::observeEvent(input$reset, {
    updateTextInput(
      session = session,
      inputId = "name_text",
      value = handle_fun(.value)
    )
  })
  
  if (!purrr::is_null(.reset)) {
    shiny::observeEvent(.reset(), {
      shiny::updateTextInput(
        session = session,
        inputId = "name_text",
        value = rvs$value
      )
    })
  }
  
  # name returns the name only if it is allowed otherwise it stops with req,
  # whereas null_name returns NULL if an error occured, so that it is useable
  # in for example the fallback function
  name <- shiny::reactive({
    req(!rvs$name_error)
    input$name_text
  })
  
  null_name <- shiny::reactive({
    if (rvs$name_error) {
      return(NULL)
    } else {
      return(input$name_text)
    }
  })
  
  return_list <- list(
    name = name,
    null_name = null_name,
    error = shiny::reactive({rvs$name_error})
  )
  
  return(return_list)
}