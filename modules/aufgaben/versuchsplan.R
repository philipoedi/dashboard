# to-do
# tabs: neue namen
# contour/surface: achsenbeschriftung, wechselwirkung?, einfügen der versuchswerte in plot
# residuen: plot beschriftungen


versuchsplan_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    data_selector_ui(id = ns("id_data_selector")),
    
    #Auswahl des Faktors, für den wechselwirkungen geplottet werden sollen
    selectInput(
      inputId = ns("interaction_choice"),
      label = "Wechselwirkungen von:",
      choices = c("A", "B", "C", "D"),
      selected = NULL
    ),
    #spezifizieren, welches modell trainiert werden soll
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
    #button der im viewer tabs mit überblick zu versuchsplan levels und histogram flugzeiten
    actionButtonQW(
      inputId = ns("add_vp"),
      label = "Überblick",
      tooltip = "Übersicht Versuchsplan und Häufgkeit der Flugzeiten"
    ),
    #pareto,interaction und effect
    actionButtonQW(
      inputId = ns("add_effects"),
      label = "Effekte/Wechselwirkungen/Pareto",
      tooltip = "Haupteffekte, Wechselwirkungsplots und Paretoplot"
    ),
    #residuen und centerpoints
    actionButtonQW(
      inputId = ns("add_residuals"),
      label = "Centerpoints/Residuen",
      tooltip = "Residuen und Centerpoints"
    ),
    #contour
    actionButtonQW(
      inputId = ns("add_contour"),
      label = "Contour/Surfaceplot",
      tooltip = "Contour/Surfaceplot"
    ),
    
    #hier kommen die Inhalte der Präsentation bzw. Notizen hin
    h4("Vollfaktorieller Versuchsplan"),
    p("Vollfaktorieller Versuchsplan mit 2 Stufen und 4 Faktoren"),
    tags$ul(
      tags$li("A: Fluegellaenge"),
      tags$li("B: Koerperlaenge"),
      tags$li("C: Einschnitt"),
      tags$li("D: Papierstaerke")
    ),
    h4("Haupt- und Wechselwirkungen"),
    p("Visualisierung der Einflüsse auf die Flugdauer"),
    tags$ul(
      tags$li("Effectplot: Steigung als Maß für Stärke des Effektes"),
      tags$li(
        "Interactionplot: Kombinierter Linienverlauf gibt Aufschluss über die Wechselwirkung"
      ),
      tags$li(
        "Paretoplot: Standardisierte Haupteffekte und Wechselwirkungen"
      )
    ),
    h4("Regressionsmodell"),
    p("Nur signifikate Haupt- und Wechselwirkungen"),
    tags$ul(
      tags$li("Betrachtung der Residuen"),
      tags$li("Vergleich mit Centerpoints/Refrenzdesign"),
      tags$li("Contour- und Surfaceplots")
    ),
    h4("Wichtige Aspekte bei der Versuchsplanung"),
    tags$ul(
      tags$li(
        "Randomisierung des Versuchsablaufs: Ausschließen systematischer Fehler"
      ),
      tags$li(
        "Möglichkeiten zur Verringerung der Versuche: Teilfaktorielle Versuchsplanung"
      )
    )
  )
}

versuchsplan <- function(input, output, session, .values) {
  ns <- session$ns
  
  #Konstanten deklarieren
  factor_names  <- list(A = "Fluegellaeng", B = "Koerperlaenge", C = "Einschnitt",D = "Papierstaerke")
  #farbe des Fensters für histogramme und bar charts
  dqe_color <- "#3E8EBF"
  
  data <- reactive({
    data_selector_return$data()
  })
  
  #contour bzw surface plot
  contour_plot <- reactive({
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
    
    #grid erstellen von -1 bis 1, alle kombis A,B,C,D
    x_seq <- seq(-1, 1, by = 0.2)
    df_seq <-
      expand.grid(A = x_seq,
                  B = x_seq,
                  C = x_seq,
                  D = x_seq)
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
          #eckpunkte des cubes
          x = cube[X] %>% pull(),
          y = cube[Y] %>% pull(),
          z = cube$Flugdauer,
          type = "scatter3d",
          mode = "markers",
          marker = list(size = 6, color = "blue"),
          name = "Mittelwert Extrempunkte"
        ) %>%
        #surface erstellen
        add_trace(
          type = "surface",
          x = x_seq,
          y = x_seq,
          z = m_means,
          showscale = FALSE
        ) %>%
        # versuchs werte des Referenzmodells als punkte
        add_trace(
          data = center_predict,
          x = c(0),
          y = c(0),
          z = ~ Flugdauer,
          type = "scatter3d",
          marker = list(size = 10, color = "orange"),
          mode = "markers",
          name = "Mittelwert Versuchsplan"
        ) %>%
        # center des hervorsagemodells
        add_trace(
          data = center,
          x = 1:nrow(center) * 0,
          y = 1:nrow(center) * 0,
          z = ~ Flugdauer,
          type = "scatter3d",
          marker = list(size = 4, color = "green"),
          mode = "markers",
          name = "Centerpoints"
        ) %>%
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
          x = x_seq,
          y = x_seq,
          z = m_means,
          showscale = TRUE,
          type = "contour"
        ) %>%
        layout(
          title = "Contourplot",
          xaxis = list(title = x_achse),
          yaxis = list(title = y_achse)
        )
    }
    return(p)
  })
  
  
  #data_tidy object erstellen fpr effect plot
  data_tidy <- reactive({
    data <- vp()
    data <-
      as_tibble(data) %>% dplyr::select(c(A, B, C, D, Flugdauer))
    #wechselwirkung kodierung ermitteln
    data <-
      data %>% mutate(
        A_B = A * B,
        A_C = A * C,
        A_D = A * D,
        B_C = B * C,
        B_D = B * D,
        C_D = C * D
      )
    data_tidy <-
      data %>% gather(
        key = "Faktor",
        value = "Kodierung",
        c("A", "B", "C", "D", "A_B", "A_C", "A_D", "B_C", "B_D", "C_D")
      )
    return(data_tidy)
  })
  
  #facDesign objekt von eingelesenen daten erstellen
  vp <- reactive({
    data <- data()
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
  
  
  #tabelle mit übersicht zu den levels
  vp_plot <- reactive({
    input$add_vp
    data <- vp()
    data <-
      tibble(
        Faktor = names(data),
        lows = as.character(lows(data)),
        highs = as.character(highs(data)),
        units = units(data))
    return(data)
  })
  
  #regressionsmodell
  model <- reactive({
    data <- vp()
    #transformiert die eingabe in eine formel
    formula <-
      as.formula(gsub("explain", input$model_parameter, "Flugdauer ~ explain"))
    return(lm(formula, data = data))
  })
  
  
  #plot für den vergleich der centerpoints/referenzmodell und modellintercept
  center_points <- reactive({
    vp <- vp()
    data <- as_tibble(vp)
    model <- model()
    #nur daten für referenzmodell
    center <- data %>% filter(A == 0)
    #mean und sd für späteres nutzen bei stat_fun
    mean_center <- mean(center$Flugdauer)
    sd_center  <- sd(center$Flugdauer)
    #intercept des modell
    intercept <- model[["coefficients"]]["(Intercept)"]
    #limits für das plotten auf x achse
    limits <-
      c(min(min(center$Flugdauer), intercept) - 0.1, max(max(center$Flugdauer), intercept) +
          0.1)
    bins2 <- nclass.Sturges(center$Flugdauer)
    t_result <- t.test(center$Flugdauer,mu=intercept)
    t_text <- paste("t-test(centerpoints und intercept)\np-value: ",t_result$p.value)
    p <- ggplot(center, aes(Flugdauer)) +
      geom_histogram(
        bins = 10,
        color = dqe_color,
        fill = dqe_color,
        alpha = 0.75
      ) +
#      annotate("text",
#               x = mean(limits),
#               y = -1,
#               label = t_text)+
      geom_vline(xintercept = model[["coefficients"]]["(Intercept)"], linetype =
                   "dashed") +
      scale_x_continuous(limits = limits) +
      # density plot mit sd und mean der referenzmodelle
      stat_function(
        fun = dnorm,
        args = list(mean = mean_center, sd = sd_center),
        n = 1000
      ) +
      # bereich der auserhalb 2 standardabweichungen fällt ,links
      stat_function(
        fun = dnorm,
        args = list(mean = mean_center, sd = sd_center),
        n = 1000,
        fill = "red",
        alpha = 0.6,
        geom = "area",
        xlim = c(limits[1], mean_center - (2 * sd_center))
      ) +
      # bereich der auserhalb 2 standardabweichungen fällt ,rechts
      stat_function(
        fun = dnorm,
        args = list(mean = mean_center, sd = sd_center),
        n = 1000,
        fill = "red",
        alpha = 0.6,
        geom = "area",
        xlim = c(mean_center + (2 * sd_center), limits[2])
      ) +
      theme_bw() +
      theme(plot.title = element_text(size = 18, hjust = 0.5)) +
      labs(title = "Vergleich Centerpoints und Modellintercept")
    p <- ggplotly(p)
  })
  
  
  #residuen über zeit plotten
  residuals <- reactive({
    vp <- vp()
    data <- as_tibble(vp)
    model <- model()
    data["Residuen"] <- model$residuals
    data["Flugdauer_model"] <- model$fitted.values
    center <- data %>% filter(A == 0)
    #korrelationstest von residuen und zeit
    correlation <- cor.test(center$RunOrder, center$Residuen)
    #labels die später in plot eingefügt werden und pvalue und corr zeigen
    correlation_text <-
      gsub("XX", round(correlation$estimate, 2), "corr: XX\np-value: YY")
    correlation_text <-
      gsub("YY", round(correlation$p.value, 2), correlation_text)
    p <-
      ggplot(center, aes(x = RunOrder, y = Residuen)) + geom_point() +
      geom_smooth(method = "lm", se = FALSE,linetype="dashed",color="red") + theme_bw() +
      theme(plot.title = element_text(size = 18, hjust = 0.5)) +
      annotate("text",
               x = mean(center$RunOrder),
               y = 0,
               label = correlation_text) +
      labs(title = "Residuen der Centerpoints vs. RunOrder")
    p <- ggplotly(p)
    return(p)
  })
  
  
  # histogamm über die flugzeiten
  histogram <- reactive({
    input$add_vp
    p <- isolate({
      data <- vp()
      response <- qualityTools::response(data)
      bins <- nclass.Sturges(response$Flugdauer)
      p <- ggplot(data = response, mapping = aes(x = Flugdauer)) +
        geom_histogram(bins = bins,color= dqe_color,fill= dqe_color,alpha=0.75) +
        theme_bw() +
        theme(plot.title = element_text(size = 18, hjust = 0.5))+
      labs(title = "Häufigkeit Flugdauer")
      p <- ggplotly(p)
    })
    return(p)
  })
  
  
  # hauptwirkungsplots
  effects <- reactive({
    input$add_effects
    p <- isolate({
      data <- data_tidy()
      effect_data <-
        data %>%
        filter(Kodierung != 0, Faktor %in% c("A", "B", "C", "D")) %>%
        group_by(Faktor, Kodierung) %>%
        summarize(Flugdauer = mean(Flugdauer))
      
      #wechselwirkungen ausrechnen
      effect_summary <- effect_data %>% spread(Kodierung,Flugdauer)
      names(effect_summary) <- c("Faktor","minus","plus")
      effect_summary <- effect_summary %>% mutate(Effekt = plus - minus)
      
      #den faktoren die namen zuordnen für späteren Plot      
      effect_data$Faktor <- as_factor(effect_data$Faktor)
      effect_summary$Faktor <- as_factor(effect_summary$Faktor)
      levels(effect_data$Faktor) <- c("A: Fluegellaenge","B: Koerperlaenge","C: Einschnitt","D: Papierstaerke") 
      levels(effect_summary$Faktor) <- c("A: Fluegellaenge","B: Koerperlaenge","C: Einschnitt","D: Papierstaerke") 
      
      p <-
        ggplot(effect_data, aes(Kodierung, Flugdauer)) + geom_line() +
        geom_point(col = "red", size = 1.5) + facet_wrap(~ Faktor) +
        theme_bw() +
        labs(title = "Haupteffekte") +
        theme(plot.title = element_text(size = 18, hjust = 0.5)) +
        geom_text(data=effect_summary, aes(x= 0, y= min(effect_data$Flugdauer), label=round(Effekt,2),hjust=-0.1,vjust =-1), 
                  colour="black", inherit.aes=FALSE, parse=FALSE)+
      scale_y_continuous("Flugdauer")
      
      p <- ggplotly(p)
    })
  })
  
  #tidy
  interactions <- reactive({
    input$add_effects
    p <- isolate({
      interaction <- quo(input$interaction_choice)
      data <- vp()
      data <-
        as_tibble(data) %>% dplyr::select(A, B, C, D, Flugdauer) %>%
        filter(A != 0) %>%
        gather(Faktor, Kodierung, -c(!!interaction, Flugdauer))
      names(data)[1] <- "base"
      data <- data %>% mutate(x = Kodierung * base) 
     

      interaction_summary <-  data %>% dplyr::select(Flugdauer,Faktor,x) %>% mutate(Flugdauer = Flugdauer * x) %>%
        group_by(x,Faktor) %>% summarize(Flugdauer = mean(Flugdauer)) %>%
        group_by(Faktor) %>% summarize(Effekt = sum(Flugdauer))

      data <- data %>%
        group_by(base, Faktor, Kodierung) %>%
        summarize(Flugdauer = mean(Flugdauer))
      p <- data %>%
        ggplot(aes(
          x = base,
          y =Flugdauer,
          color = as.factor(Kodierung),
          group = as.factor(Kodierung)
        )) +
        geom_line() +
        facet_wrap( ~ Faktor) +
        geom_point(size = 1.5) +
        theme_bw() +
        geom_text(data=interaction_summary, aes(x= 0, y= min(data$Flugdauer)-0.125, label=round(Effekt,2),hjust=-0.1,vjust =-1), 
                              colour="black", inherit.aes=FALSE, parse=FALSE)+
        ggtitle(paste("Wechselwirkungen von ",input$interaction_choice,": ",factor_names[input$interaction_choice])) +
        theme(
          legend.position = "bottom",
          legend.title = element_blank(),
          plot.title = element_text(
            size = 18,
            vjust = 2.5 ,
            hjust = 0.5
          )
        ) +
        scale_x_continuous(name = paste("\nKodierte Werte von ",input$interaction_choice,": ",factor_names[input$interaction_choice])) +
        scale_y_continuous(name = "Flugdauer") 
      p <- ggplotly(p)
    })
    return(p)
  })
  
  
  
  #paretoplot
  pareto <- reactive({
    input$add_effects
    #paretoplot wird aktualisiert, wenn modell neu eingegeben wird
    model <- model()
    #t-values holen
    t_vals <- summary(model)[["coefficients"]][, "t value"]
    t_sig <- abs(qt(0.01 / 2, df = df.residual(model)))
    title <-
      gsub("XX",
           round(t_sig, 3),
           "Haupteffekte und Wechselwirkungen \n (t_krit = XX, alpha = YY)")
    title <- gsub("YY", "0.01", title)
    
    pareto_data <-
      tibble(effects_interactions = names(t_vals),
             t_value = abs(t_vals)) %>%
      filter(effects_interactions != "(Intercept)") %>%
      arrange(desc(t_value))
    
    p <-
      ggplot(pareto_data,
             aes(
               x = effects_interactions,
               y = t_value,
               label = round(t_value, 3)
             )) + geom_bar(
               stat =
                 "identity",
               color = dqe_color,
               fill = dqe_color,
               alpha = 0.75
             ) +
      scale_x_discrete(limits = pareto_data$effects_interactions,
                       name = "")  +
      scale_y_continuous(name = "t-Wert") +
      geom_hline(yintercept = t_sig,linetype="dashed") +
      theme_bw() +
      theme(
        legend.position = "none",
        plot.title = element_text(
          size = 12,
          vjust = 0.01,
          hjust = 0.5
        ),
        axis.text.x = element_text(angle = 45)
      ) +
      labs(title = title) +
      geom_text(nudge_y = 0.5)
    p <- ggplotly(p)
    return(p)
  })
  
  
  observeEvent(input$add_contour, {
    .values$viewer$append_tab(tab = tabPanel(
      title = "Contour/Surface",
      value = "contour",
      plotlyOutput(outputId = ns("contour"), height = "600px")
    ))
  })
  
  
  observeEvent(input$add_effects, {
    .values$viewer$append_tab(tab = tabPanel(
      title = "Paretoplot",
      value = "pareto",
      plotlyOutput(outputId = ns("pareto"), height = "600px")
    ))
    .values$viewer$append_tab(tab = tabPanel(
      title = "Wechselwirkungen",
      value = "interactions",
      plotlyOutput(outputId = ns("interactions"), height = "600px")
    ))
    .values$viewer$append_tab(tab = tabPanel(
      title = "Haupteffekte",
      value = "effects",
      plotlyOutput(outputId = ns("effects"), height = "600px")
    ))
  })
  
  observeEvent(input$add_residuals, {
    .values$viewer$append_tab(tab = tabPanel(
      title = "Residuen",
      value = "residuals",
      plotlyOutput(outputId = ns("residuals"))
    ))
    .values$viewer$append_tab(tab = tabPanel(
      title = "Centerpoints",
      value = "center_points",
      plotlyOutput(outputId = ns("center_points"))
    ))
    
  })
  
  observeEvent(input$add_vp, {
    .values$viewer$append_tab(
      tab = tabPanel(
        title = "Vollfaktorieller Versuchsplan",
        value = "vp_plot",
        dataTableOutput(outputId = ns("vp_plot"))
      ))
    .values$viewer$append_tab(
      tab = tabPanel(
        title = "Histogramm der Flugdauer",
        value = "histogram",
        plotlyOutput(outputId = ns("histogram"))
      ))
  })
  
  
  output$vp_plot <- renderDataTable({
    vp_plot()
  })
  
  
  output$center_points <- renderPlotly({
    center_points()
  })
  
  output$contour <- renderPlotly({
    contour_plot()
  })
  
  output$pareto <- renderPlotly({
    pareto()
  })
  
  output$interactions <- renderPlotly({
    interactions()
  })
  
  output$histogram <- renderPlotly({
    histogram()
  })
  output$residuals <- renderPlotly({
    residuals()
  })
  output$effects <- renderPlotly({
    effects()
  })
  
  data_selector_return <- callModule(module = data_selector,
                                     id = "id_data_selector",
                                     .values = .values)
}