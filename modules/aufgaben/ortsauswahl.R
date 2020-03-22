ortsauswahl_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    data_selector_ui(
      id = ns("id_data_selector")
    ),
    sliderInput(
      inputId = ns("number_bins"),
      label = "Waehle die Anzahl der Kasten",
      min = 1,
      max = 30,
      value = 8,
    ),
    actionButtonQW(
      inputId = ns("add_histogram"),
      label = NULL,
      icon = icon("area-chart"),
      tooltip = "Öffne Histogramm"
    ),
    actionButtonQW(
      inputId = ns("show_table"),
      label = NULL,
      icon = icon("table"),
      tooltip = "Öffne Datensatz"
    )
  )
}

ortsauswahl <- function(input, output, session, .values) {
  
  ns <- session$ns
  
  data <- reactive({
    data_selector_return$data()
  }) 
  dqe_color <- "#3E8EBF"
  
  
  histogram <- reactive({
    data <- data()
    hist <- ggplot(data, mapping = aes(x = Flugdauer)) +
      geom_histogram(data, mapping = aes(x = Flugdauer), bins = input$number_bins, fill = dqe_color, color = dqe_color,alpha=0.75) + 
      theme_bw()+labs(title = "Häufigkeit der Flugdauer")
    histogram <- ggplotly(hist)
  })
  
  data_transformed <- reactive({
    data <- data()
    data_transformed <- data %>% group_by(Ort) %>% mutate(sd = sd(Flugdauer)) %>% count(sd) %>% mutate(cp = cp <- 0.5 / (sd * 6)) 
    return(data_transformed)
  })
  
  observeEvent(input$add_histogram, {
    .values$viewer$append_tab(
      tab = tabPanel(
        title = "Häufigkeit Flugdauer",
        value = "histogram",
        plotlyOutput(
          outputId = ns("histogram")
        )
      )
    )
  })
  
  observeEvent(input$show_table, {
    .values$viewer$append_tab(
      tab = tabPanel(
        title = "Cp Werte",
        value = "cpWerte",
        dataTableOutput(
          outputId = ns("cpWerte")
        )
      )
    )
  })
  
  output$histogram <- renderPlotly({
    histogram()
  })
  
  output$cpWerte <- renderDataTable({
    # data_transformed()
    isolate(datatable(data_transformed()))
  })
  
  data_selector_return <- callModule(
    module = data_selector,
    id = "id_data_selector",
    .values = .values
  )
}