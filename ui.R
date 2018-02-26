library(shiny)
library(shinydashboard)
library(shinyDND)
library(magick)


dashboardPage(
  skin = "purple",
  
  dashboardHeader(title = "Montage photos"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    sidebarLayout(
      sidebarPanel(
        # Input: Select a file ----
        fileInput("files", "Select image(s)",
                  multiple = TRUE,
                  accept = c(".jpg",
                             ".jpeg",
                             ".png",
                             ".gif")),
        uiOutput('images')
      ),
      mainPanel(
        dropUI("div_drop",row_n = 2, col_n = 1)
        
      )
    )
  )
)
