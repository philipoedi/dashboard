standardisierung_ui <- function(id) {
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
      value = 5,
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
    ),
    h2('Bilder und Diagramme'),
    h3('Ursache-Wirkungs-Diagramm'),
    img(src ='Ursache-Wirkungs-Diagramm.png', height="100%", width="100%"),
    h3('Abwurfeinrichtung'),
    img(src = 'Abwurfeinrichtung.png', height="50%", width="50%"),
    h3('Abwurfprozess'),
    img(src = 'Abwurfprozess.png', height="50%", width="50%"),
    h3('Zeitmessung'),
    img(src = 'Zeitmessung.png', height="50%", width="50%"),
    h3('Rueckholeinrichtung'),
    img(src = 'Rueckholeinrichtung.png', height="50%", width="50%")
  )
}

standardisierung <- function(input, output, session, .values) {
  
  ns <- session$ns
  
  data <- reactive({
    data_selector_return$data()
  })
  
  histogram <- reactive({
    data <- data()
    hist <-ggplot(data, mapping = aes(x = Flugdauer)) +
      geom_histogram(data, mapping = aes(x = Flugdauer), bins = input$number_bins, fill = "grey", color = "black") + 
      theme_bw()
    histogram <- ggplotly(hist)
  })
  
  data_transformed <- reactive({
    data <- data()
#    data_transformed <- data %>% mutate(sd = sd(Flugdauer)) %>% count(sd) %>% mutate(cp = cp <- 0.5 / (sd * 6)) 
    data_transformed <- data %>% group_by(Ort) %>% summarize(sd=sd(Flugdauer),cp=0.5/(6*sd(Flugdauer)),Anzahl=n()) 
    print(data_transformed)
    return(data_transformed)
  })
  
  observeEvent(input$add_histogram, {
    .values$viewer$append_tab(
      tab = tabPanel(
        title = "Histogram",
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
    datatable(data_transformed())
    #isolate(datatable(data_transformed()))
})

  data_selector_return <- callModule(
    module = data_selector,
    id = "id_data_selector",
    .values = .values
  )

}