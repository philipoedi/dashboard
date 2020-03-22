excel_csv_file_input_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fileInput(
      inputId = ns("file"),
      label = "Lade eine Datei hoch:",
      placeholder = "Klicke hier."
    ),
    uiOutput(
      outputId = ns("names_data")
    ),
    uiOutput(
      outputId = ns("add_preview")
    ),
    uiOutput(
      outputId = ns("add_data")
    )
  )
}

excel_csv_file_input <- function(input, output, session, .values) {
  
  ns <- session$ns
  
  output$names_data <- renderUI({
    if (!file_type_not_supported()) {
      l <- list()
      for (i in 1:sheets()) {
        l[[i]] <- fluidRow(
          column(
            width = 6,
            textInput(
              inputId = ns(paste("name_data", i, sep = "_")),
              label = paste("Name des ", i, ". Tabellenblattes"),
              value = excel_sheets(file()$datapath)[[i]]
            )
          ),
          column(
            width = 6,
            textOutput(
              outputId = ns(paste("warning_name_data", i, sep = "_"))
            )
          )
        )
      }
      ui <- map(seq_len(sheets()), function(i) {
        fluidRow(
          column(
            width = 6,
            textInput(
              inputId = ns("name_data" %_% i),
              label = paste("Name des ", i, ". Tabellenblattes"),
              value = excel_sheets(file()$datapath)[[i]]
            )
          ),
          column(
            width = 6,
            textOutput(
              outputId = ns(paste("warning_name_data", i, sep = "_"))
            )
          )
        )
      }) 
    } else {
      ui <- NULL
    }
    ui
  })
  
  output$add_preview <- renderUI({
    if (!file_type_not_supported()) {
      actionButtonQW(
        inputId = ns("add_preview"),
        label = "Vorschau"
      )
    }
  })
  
  output$add_data <- renderUI({
    if (!error()) {
      ui <- actionButtonQW(
        inputId = ns("add_data"),
        label = "Füge Datensatz hinzu."
      )
    } else {
      ui <- uiOutput(
        outputId = ns("ui_error")
      )
    }
  })
  
  file <- reactive({
    req(input$file)
  })
  
  file_type <- reactive({
    path <- file()$datapath
    split_path <- str_split(path, pattern = "\\.")
    # Extrahiere Dateiendung
    split_path[[1]][length(split_path[[1]])]
  })
  
  sheets <- reactive({
    if (file_type() == "xlsx" || file_type() == "xls") {
      len <- length(excel_sheets(file()$datapath))
    } else {
      len <- 1 
    }
    
    len
  })
  
  file_type_not_supported <- reactive({
    !(file_type() %in% c("xlsx", "xls", "csv"))
  })
  
  data_names <- reactive({
    if (!file_type_not_supported()) {
      map_chr(seq_len(sheets()), function(i) {
        req(input[["name_data" %_% i]])
      })
    } else {
      character()
    }
  })
  
  name_in_use <- reactive({
    any(data_names() %in% .values$data_storage$get_names())
  })
  
  which_names_in_use <- reactive({
    data_names()[which(data_names() %in% .values$data_storage$get_names())]
  })
  
  error <- reactive({
    file_type_not_supported() ||
      name_in_use()
  })
  
  data_preview <- reactive({
    if (!file_type_not_supported()) {
      data <- list()
      type <- file_type()
      
      if (type == "xlsx" || type == "xls") {
        for (i in 1:sheets()) {
          data[[i]] <- read_excel(
            path = file()$datapath,
            sheet = i,
            col_names = TRUE
          )
        }
        data <- map(seq_len(sheets()), function(sheet) {
          read_excel(
            path = file()$datapath,
            sheet = sheet,
            col_names = TRUE
          )
        })
      } else if (type == "csv") {
        data[[input$name_data_1]] <- read_csv2(
          file = file()$datapath
        )
      }
      
      return(data)
    }
  })
  
  data <- reactive({
    if (!error()) {
      data <- list()
      type <- file_type()
      if (type == "xlsx" || type == "xls") {
        for (i in seq_len(sheets())) {
          name <- input[["name_data" %_% i]]
          if (name == "") {
            
          } else {
            data[[name]] <- read_excel(
              path = file()$datapath,
              sheet = i,
              col_names = TRUE
            )
          }
        }
      } else if (type == "csv") {
        data[[input$name_data_1]] <- read_csv2(
          file = file()$datapath
        )
      }
      
      return(data)
    }
  })

  observeEvent(input$add_preview, {
    .values$viewer$append_tab(
      tab = tabPanel(
        title = "Vorschau",
        value = "preview",
        uiOutput(
          outputId = ns("select_preview_sheet")
        ),
        dataTableOutput(
          outputId = ns("preview_data")
        )
      )
    )
  })
  
  output$select_preview_sheet <- renderUI({
    if (sheets() > 1) {
      selectInput(
        inputId = ns("select_preview_sheet"),
        label = "Wähle das anzuzeigende Tabellenblatt aus:",
        choices = seq_len(sheets())
      )
    }
  })
  
  output$preview_data <- renderDataTable({
    if (sheets() > 1) {
      data_preview()[[as.numeric(req(input$select_preview_sheet))]]
    } else {
      data_preview()[[1]]
    }
  })
  
  output$ui_error <- renderUI({
    if (file_type_not_supported()) {
      not_supported <- paste0(
        "Dateiendung .", file_type(), " wird nicht untersützt. "
      )
    } else {
      not_supported <- NULL
    }
    
    if (name_in_use()) {
      name_in_use <- paste0(
        "Es existieren bereits Datensätze mit den Namen ", 
        paste(which_names_in_use(), collapse = ", "), ". "
      )
    } else {
      name_in_use <- NULL
    }
    
    ui <- tagList(
      not_supported,
      name_in_use
    )
  })
  
  observeEvent(input$add_data, {
    data <- data()
    walk(seq_len(sheets()), function(i) {
      object <- DataObject$new(
        name = input[["name_data" %_% i]],
        value = data[[i]]
      )
      .values$data$add_object(
        object
      )
    })
  })
}