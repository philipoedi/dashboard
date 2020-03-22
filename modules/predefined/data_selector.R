data_selector_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(
    outputId = ns("module_ui")
  )
}

data_selector <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  output$module_ui <- renderUI({
    selectInput(
      inputId = ns("select_data_name"),
      label = div(
        "Wähle Datensatz",
        actionButtonQW(
          inputId = ns("open_data"),
          label = NULL,
          icon = icon("table"),
          tooltip = "Datensatz öffnen"
        )
      ),
      choices = .values$data_storage$get_names()
    )
  })
  
  data <- reactive({
    .values$data_storage$get_object(req(input$select_data_name))$get_value()
  })
  
  observeEvent(input$open_data, {
    .values$viewer$append_tab(
      tab = tabPanel(
        title = paste("Datensatz:", input$select_data_name),
        dataTableOutput(
          outputId = ns("data" %_% input$select_data_name)
        )
      )
    )
    
    output[["data" %_% input$select_data_name]] <- renderDataTable({
      # Keine Abhängigkeit von data(), da sich die Tabelle ansonsten ändert, wenn
      # der Benutzer eine anderen Namen auswählt
      isolate(datatable(data()))
    })
  })
  
  return_list <- list(
    data = data,
    name = reactive(input$select_data_name)
  )
  
  return(return_list)
}