

# Falls es an dieser Stelle Fehlermeldungen gibt, müssen die Packages mit
# install.packages installiert werden
library(shiny)
# Dashboard
library(shinydashboard)
# Tidyverse
library(tidyverse)
# DT-Tabellen in shiny
library(DT)
# plotly-Plots
library(plotly)
# wird für source_directory benötigt
library(R.utils)
# Import von .xls- und .xlsx-Dateien
library(readxl)
# Import von .csv-Dateien
library(readr)
# Bearbeiten von Strings
library(stringr)
# Objektorientiertes System; z.B. TabBox ist ein R6-Objekt
library(R6)
# UI
library(shinyWidgets)
# Tooltips
library(shinyBS)
#
library(qualityTools)
library(ggplot2)

# Source source_directory.R
source("./modules/predefined/source_directory.R", encoding = "UTF-8")

# Nutze source_directory, um gesamten Ordner zu sourcen; setze verbose = FALSE,
# um keine Mitteilungen in der Konsole zu sehen
source_directory(
  "./modules",
  encoding = "UTF-8",
  modifiedOnly = FALSE,
  chdir = TRUE,
  verbose = TRUE,
  envir = globalenv()
)

# Erzeuge einen Viewer, in dem Plots und Tabellen in einzelnen Tabs dargestellt
# werden können
viewer <- TabBox$new(id = "viewer",
                     title = "Viewer",
                     width = 12)

# Scrollen in zu breiten DT-Tabellen
options(DT.options = list(scrollX = TRUE))

ui <- div(
  tags$head(
    # Include custom css styles
    tags$link(rel = "stylesheet",
              type = "text/css",
              href = "styles.css")
  ),
  dashboardPage(
    dashboardHeader(title = "DQE-App"),
    dashboardSidebar(sidebarMenu(
      menuItem(text = "Import",
               tabName = "import"),
      menuItem(
        text = "Projekt",
        menuSubItem(text = "Motivation",
                    tabName = "motivation"),
        menuSubItem(text = "Budgetplan",
                    tabName = "budget"),
        menuSubItem(text = "Ortsauswahl",
                    tabName = "ortsauswahl"),
        menuSubItem(text = "Standardisierung",
                    tabName = "standardisierung"),
        menuSubItem(text = "Versuchsplan",
                    tabName = "versuchsplan"),
        menuSubItem(text = "Steepest-Ascent",
                    tabName = "steepest_ascent")
      )
    )),
    dashboardBody(fluidRow(
      column(
        width = 6,
        tabItems(
          tabItem(
            tabName = "import",
            box(
              title = "Import",
              width = 12,
              excel_csv_file_input_ui(id = "id_excel_csv_file_input")
            )
          ),
          tabItem(tabName = "motivation",
                  box(
                    title = "Motivation",
                    width = 18,
                    motivation_ui(id = "id_motivation")
                  )),
          tabItem(tabName = "budget",
                  box(
                    title = "Budgetplan",
                    width = 18,
                    budget_ui(id = "id_budget")
                  )),
          tabItem(tabName = "ortsauswahl",
                  box(
                    title = "Ortsauswahl",
                    width = 12,
                    ortsauswahl_ui(id = "id_ortsauswahl")
                  )),
          tabItem(
            tabName = "standardisierung",
            box(
              title = "Standardisierung",
              width = 12,
              standardisierung_ui(id = "id_standardisierung")
            )
          ),
          tabItem(
            tabName = "versuchsplan",
            box(
              title = "Versuchsplan",
              width = 12,
              versuchsplan_ui(id = "id_versuchsplan")
            )
          ),
          tabItem(
            tabName = "steepest_ascent",
            box(
              title = "Steepest-Ascent",
              width = 12,
              steepest_ascent_ui(id = "id_steepest_ascent")
            )
          )
        )
      ),
      column(width = 6,
             # Container, in dem die Inhalte des Viewers dargestellt werden
             viewer$tabBox())
    ))
  )
)

server <- function(input, output, session) {
  # Verknüpfe Viewer mit der session
  viewer$set_session(session)
  
  # Erzeuge eine Liste, die allen Modulen als Argument übergeben wird
  .values <- list(data_storage = ObjectStorage$new(),
                  viewer = viewer)
  
  # Rufe Module auf
  callModule(module = excel_csv_file_input,
             id = "id_excel_csv_file_input",
             .values = .values)
  
  callModule(module = motivation,
             id = "id_motivation",
             .values = .values)
  
  callModule(module = budget,
             id = "id_budget",
             .values = .values)
  
  callModule(module = ortsauswahl,
             id = "id_ortsauswahl",
             .values = .values)
  
  callModule(module = standardisierung,
             id = "id_standardisierung",
             .values = .values)
  
  callModule(module = steepest_ascent,
             id = "id_steepest_ascent",
             .values = .values)
  
  callModule(module = versuchsplan,
             id = "id_versuchsplan",
             .values = .values)
}

# Erzeuge die App
shinyApp(ui, server)