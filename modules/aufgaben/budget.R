budget_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    img(src ='Budgetplan.png', height="100%", width="100%")
    
    
  )
  #tabsetPanel(type = "tab",
  #             tabPanel("Budgetplan", tags$iframe(style = "height:400px; width:100%, scrolling=yes", src = "Budgetplan.pdf")))
}

budget <- function(input, output, session, .values) {
  
  ns <- session$ns
  
  # output$newtabs <- renderUI({
  #   
  # })
  
  data <- reactive({
    data_selector_return$data()
  })
  
}
