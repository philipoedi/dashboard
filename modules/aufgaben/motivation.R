motivation_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # checkboxInput(inputId = ns("budget"),
    #               label = "Zeige Budgetplan"
    #               ),
    # tags$iframe(styl = "height:1400px; width:100%", src = "Budgetplan.pdf"),
    img(src ='535-463-max.png', height="100%", width="100%"),
    tags$ul(
      tags$li("Verbesserung der Flugzeiten des DAP-Copters um Wettbewerbsfähigkeit zu steigern und neue Kunden zu gewinnen und zu halten"),
      tags$li("Entwicklung einer Applikation für die Firma DAP-COpter Gmbh, um die gesamte Produktpalette verbessern zu können")
    )
  
  )
 }

motivation <- function(input, output, session, .values) {
  
  ns <- session$ns
  
  # output$newtabs <- renderUI({
  #   
  # })
  
  data <- reactive({
    data_selector_return$data()
  })
  
}
