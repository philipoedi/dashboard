steepest_ascent_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    strong("Steepest Ascent Daten:"),
    data_selector_ui(
      id = ns("id_data_selector")
    ),
    strong("Versuchsplandaten:"),
    data_selector_ui(
      id = ns("id_data_selector_2")
    ),
    textInput(
      inputId = ns("model_parameter"),
      label = "Wähle Modellparameter (z.B. A+B,A*C usw.):",
      "A*B*C*D"
    ),
    #einstellungen für surface und contour
    strong("Wähle Einstellungen des Surface/Contourplots:"),
    checkboxInput(inputId = ns("dimension"),
                  label = "3-Dimensional"),
    selectInput(
      inputId = ns("x_Achse"),
      label = "x-Achse",
      choices = c("A", "B", "C", "D"),
      selected = "A"
    ),
    selectInput(
      inputId = ns("y_Achse"),
      label = "y-Achse",
      choices = c("A", "B", "C", "D"),
      selected = "B"
    ),
    actionButtonQW(
      inputId = ns("plots"),
      label = "Plots"
    )
  )
}

steepest_ascent <- function(input, output, session, .values) {
  
  ns <- session$ns
  
  #Konstanten deklarieren
  factor_names  <- list(A = "Fluegellaeng", B = "Koerperlaenge", C = "Einschnitt",D = "Papierstaerke")
  #farbe des Fensters für steep_ascent_vergleichme und bar charts
  dqe_color <- "#3E8EBF"
  
  data <- reactive({
    data <- data_selector_return$data()
  })
  
  
  data_2 <- reactive({
    data_selector_return_2$data()
  })
  
  vp <- reactive({
    data <- data_2()
    vp <- qualityTools::facDesign(k = 4,
                                  replicates = 2,
                                  centerCube = 16)
    names(vp) <-
      c("Fluegellaenge",
        "Koerperlaenge",
        "Einschnitt",
        "Papierstaerke")
    #min und max values ermitteln
    lows <- data %>% dplyr::select(names(vp)) %>% sapply(min)
    highs <- data %>% dplyr::select(names(vp)) %>% sapply(max)
    lows(vp) <- lows
    highs(vp) <- highs
    units(vp) <- c("mm", "mm", "mm", "g/mm^2")
    
    #runOrder anpassen, nach standardorder sortiere und dann über wurf-spalte neubestimmen
    data <-
      data %>% dplyr::select(
        A = Fluegellaenge,
        B = Koerperlaenge,
        C = Einschnitt,
        D = Papierstaerke,
        Wurf,
        Flugdauer
      ) %>% arrange(A, B, C, D)
    #reihenfolge der versuche
    sorted <-
      as_tibble(vp) %>% arrange(A, B, C, D, StandOrd) %>% dplyr::select(StandOrd)
    data["stand_new"] <- sorted
    new_run_order <-
      data %>% arrange(stand_new) %>% dplyr::select(RunOrder = Wurf)
    vp@runOrder <- as.data.frame(new_run_order)
    data <- data %>% arrange(Wurf)
    qualityTools::response(vp) <- data["Flugdauer"]
    return(vp)
  })
  
  model <- reactive({
    data <- vp()
    #transformiert die eingabe in eine formel
    formula <-
      as.formula(gsub("explain", input$model_parameter, "Flugdauer ~ explain"))
    return(lm(formula, data = data))
  })
  
  
  steep_ascent_vergleich <- reactive({
    data <- data_kodiert()
    
    pred <- data %>% filter(is.na(Flugdauer)) %>% dplyr::select(Delta, Flugdauer = predicted)
    pred$type <- "Hervorsage"
    real <- data %>% filter(!is.na(Flugdauer)) %>% dplyr::select(Delta, Flugdauer)
    real$type <- "Versuche"
    
    data <- bind_rows(pred,real)
    
    p <- ggplot(data, aes(x = Delta, y = Flugdauer,color= as.factor(type),fill =as.factor(type))) +
      geom_line() + geom_point(data = real,aes(x=Delta,y=Flugdauer)) + 
      #geom_line(data = data%>%filter(is.na(Flugdauer)) , aes(x=Delta,y=predicted,color="blue"))+
      theme_bw()+theme(plot.title = element_text(size = 18, hjust = 0.5)) +
      labs(title="Vergleich Steepest Ascent und Versuche",colour="",fill="")
    
    
    # p <- ggplot(data %>% filter(!is.na(Flugdauer)), aes(x = Delta, y = Flugdauer,color= "red")) +
    #          geom_line() + geom_point() + 
    #         geom_line(data = data%>%filter(is.na(Flugdauer)) , aes(x=Delta,y=predicted,color="blue"))+
    #          theme_bw()+theme(plot.title = element_text(size = 18, hjust = 0.5)) + scale_color_manual(values = c("blue","red"),labels=c("Versuche","Hervorsage"))+
    #   labs(title="Vergleich Steepest Ascent und Versuche,Blau:Versuche,Rot:Hervorsage",colour="") + xlab("Delta")
    p <- ggplotly(p)
    return(p)
  })
  
  data_kodiert <- reactive({
    input$check
    vp <- vp()
    low <- as_tibble(lows(vp))
    low$level <- "low"
    high <- as_tibble(highs(vp))
    high$level <- "high"
    levels <- bind_rows(low, high)
    levels <- levels %>% gather(Faktor, Wert, -level)
    levels <- levels %>% 
      group_by(Faktor) %>%
      summarize(ref = mean(Wert), diff = diff(Wert)/2)
    data <-  data()
    code <- levels %>% filter(Faktor == "A") 
    k <- (data$Fluegellaenge - code$ref) / code$diff
    
    data$A <- (data$Fluegellaenge - code$ref) / code$diff 
    #
    code <- levels %>% filter(Faktor == "B") 
    data$B <- (data$Koerperlaenge - code$ref) / code$diff 
    #
    code <- levels %>% filter(Faktor == "C") 
    data$C <- (data$Einschnitt - code$ref) / code$diff 
    #
    code <- levels %>% filter(Faktor == "D") 
    data$D <- (data$Papierstaerke - code$ref) / code$diff 
    
    model <- model()
    data$predicted <- predict(model,newdata = data %>% dplyr::select(A,B,C,D))
    
    data <- data %>% group_by(A,B,C,D) %>% summarize(Flugdauer= mean(Flugdauer), predicted = mean(predicted))
    
    A.seq <- seq(0, max(abs(max(data$A)),abs(min(data$A))),length.out= 100) * ifelse(sum(data$A)<0 ,-1,1)  
    B.seq <- seq(0, max(abs(max(data$B)),abs(min(data$B))),length.out= 100) * ifelse(sum(data$B)<0 ,-1,1)
    C.seq <- seq(0, max(abs(max(data$C)),abs(min(data$C))),length.out= 100) * ifelse(sum(data$C)<0 ,-1,1)
    D.seq <- seq(0, max(abs(max(data$D)),abs(min(data$D))),length.out= 100) * ifelse(sum(data$D)<0 ,-1,1)
    
    to_predict <- tibble(A = A.seq,B = B.seq, C= C.seq,D=D.seq)
    to_predict$Flugdauer = NULL
    to_predict$predicted <- predict(model,newdata=to_predict)
    
    data <- bind_rows(data,to_predict)
    
    center <- as_tibble(vp) %>% filter(A == 0) 
    mean_versuche_center <- mean(center$Flugdauer)
    model_zero <- model[["coefficients"]]["(Intercept)"]
    
    data <- bind_rows(data,list(A=0,B=0,C=0,D=0,Flugdauer=mean_versuche_center,predicted=model_zero))
    data <- data%>% mutate(Delta = max(A,B,C,D))
    return(data)
  })
  
  contour_plot <- reactive({
    new_data <- data_kodiert()
    model <- model()
    coeffs <- names(model$coefficients)
    facs <- coeffs[nchar(coeffs) == 1]
    vp <- vp()
    #Auswahl von x und y
    X <- input$x_Achse
    Y <- input$y_Achse
    
    #achsenbeschriftungen erstellen in form zb A: Fluegellaenge
    x_achse <- paste(X, ": ", factor_names[X])
    y_achse <- paste(Y, ": ", factor_names[Y])
    
    #holt sich alle versuche die nicht im center sind
    cube <- vp@cube
    #hervorsage mit modell des cubes
    cube <-
      cube %>% 
      mutate(Flugdauer_pred = predict(model, newdata = cube))
    
    # nach Auswahl gruppieren und mitteln der Flugdauern
    cube <-
      cube %>% group_by_(X, Y) %>% summarize(Flugdauer = mean(Flugdauer_pred))
    
    
    a_max <- max(new_data$A)
    a_min <- min(new_data$A)
    b_max <- max(new_data$B)
    b_min <- min(new_data$B)
    c_max <- max(new_data$C)
    c_min <- min(new_data$C)
    d_max <- max(new_data$D)
    d_min <- min(new_data$D)
    
    
    a <- seq(a_min-0.5,a_max+0.5,length.out = 25)
    b <- seq(b_min-0.5,b_max+0.5,length.out = 25)
    c <- seq(c_min-0.5,c_max+0.5,length.out = 25)
    d <- seq(d_min-0.5,d_max+0.5,length.out = 25)
    
    axis_ticks <- tibble("A" = a,"B" = b,"C" = c, "D" = d)
    x_seq <- seq(-1, 1, by = 0.2)
    
    df_seq <-
      expand.grid(A = a,
                  B = b,
                  C = c,
                  D = d)
    #modell nutzen um alle kombis aus grid hervorzusagen
    df_seq$predict <- predict(model, df_seq)
    
    #formula aus x und y auswahl erstellen für acast
    formula <- gsub("XX", X, "YY ~ XX")
    formula <- as.formula(gsub("YY", Y, formula))
    
    #transformation und mittelung
    m_means <-
      reshape2::acast(df_seq,
                      fun.aggregate = mean,
                      formula,
                      value.var = "predict")
    new_data <- new_data %>% filter(!is.na(Flugdauer))
    new_data <- new_data %>% arrange(Delta)
    
    #checkbox mit 3d wird gecheckt
    if (input$dimension) {
      #tibble mit den centerpoints erstellen 
      center <- as_tibble(vp) %>% filter(A == 0)
      #flugdauer werte für centerpoint hervorsagen
      center_predict <- vp@centerCube %>% distinct()
      center_predict <-
        center_predict %>% mutate(Flugdauer = predict(model, newdata = center_predict))
      
      #surfaceplot erstellen
      
      
      p <-
        plot_ly(
          type = "surface",
          x = pull(axis_ticks[X]),
          y = pull(axis_ticks[Y]),
          z = m_means,
          showscale = FALSE
        ) %>%
        add_trace(
          type= "scatter3d",
          x = new_data[X]%>% pull(),
          y = new_data[Y]%>% pull(),
          z= new_data$Flugdauer, name ="Versuche", mode="markers",line = list(color = "red", width = 4)
        )%>%
        layout(
          title = "Surfaceplot",
          scene = list(
            xaxis = list(title = x_achse),
            yaxis = list(title = y_achse),
            zaxis = list(title = "Flugdauer")
          ),
          legend = list(
            orientation = "h",
            # show entries horizontally
            xanchor = "center",
            # use center of legend as anchor
            x = 0.5,
            y = 0
          )
        )
      
    }
    else{
      #contour plot 3d == false
      p <-
        plot_ly(
          x = axis_ticks[X]%>% pull(),
          y = axis_ticks[Y]%>% pull(),
          z = m_means,
          showscale = TRUE,
          type = "contour", name ="Flugdauer"
        ) %>%
        add_trace(
          x = new_data[X]%>%pull(),
          y = new_data[Y]%>%pull(),
          showscale = TRUE,
          type = "scatter",mode="markers",color="orange", name = "Versuche"
        ) %>%
        add_trace(
          x = new_data[X]%>%pull(),
          y = new_data[Y]%>%pull(),
          showscale = TRUE,
          type = "scatter",mode="lines",color="orange",name = NULL
        ) %>%
        layout(
          title = "Contourplot",
          xaxis = list(title = x_achse),
          yaxis = list(title = y_achse)
        )
    }
    return(p)
  })
  
  
  
  #fügt direkt 2 plots zu 
  observeEvent(input$plots, {
    #erster plot in viewer
    .values$viewer$append_tab(tab = tabPanel(
      title = "Contour/Surface",
      value = "contour",
      plotlyOutput(outputId = ns("contour"), height = "600px")
    ))
    #zweiter plot in viewer
    .values$viewer$append_tab(
      tab = tabPanel(
        title = "Steepest Ascent vs. Versuche",
        value = "plot1",
        plotlyOutput(outputId = ns("plot1"),height="600px")
      ))
  })
  
  
  data_selector_return <- callModule(
    module = data_selector,
    id = "id_data_selector",
    .values = .values
  )
  
  data_selector_return_2 <- callModule(
    module = data_selector,
    id = "id_data_selector_2",
    .values = .values
  )
  
  
  # erster Plot
  output$plot1 <- renderPlotly({
    steep_ascent_vergleich()
  })
  
  # erster Plot
  output$contour <- renderPlotly({
    contour_plot()
  })
 
  
}