library(shiny)
library(shinydashboard)
library(shinyDND)
library(magick)
library(purrr)

#fonctions

#' animation de n images
#' 
#' @param src Les images à animer

anim <- function(src, vitesse, taille ){
  n <- length(src)
  img <- src %>%
    image_read() %>%
    image_scale(taille) %>%
    image_animate(fps = vitesse)
  img
}

#UI
ui <- dashboardPage(
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
        tabsetPanel(
          tabPanel("Animation",
                   box(title = "Paramètres : ", width = 3,
                       sliderInput("vitesse", label = "vitesse", min = 1, max = 20, value = 4),
                       sliderInput("taille", label = "taille", min = 10, max = 2000, value = 400),
                       actionButton("rotation", label = "Tourner")
                       ),
                   box(title = "Aperçu", width = 4,
                       imageOutput("apercu_gif"),
                       actionButton("save", label = "Enregistrer")
                       )
                   ),
          tabPanel("Montage",
                   dropUI("div_drop",row_n = 2, col_n = 1)
                   )
        )
      )
    )
  )
)

# Define server 
server <- function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^2) 
  output$files <- renderTable(input$files)
  
  files <- reactive({
    files <- input$files
    #files$datapath <- gsub("\\\\", "/", files$datapath)
    files
  })
  
  smaller_files <- reactive({
    fichiers <- files()$datapath
    dossiers <- dirname(fichiers)
    smaller_files_name <- paste0(dossiers, "/", "small_", basename(fichiers))
    fichiers %>%
      walk2(smaller_files_name, ~ {
        image_read(.x) %>%
          image_scale("x150") %>%
          image_write(path = .y )
      })
    smaller_files_name
  })
  
  
  output$images <- renderUI({
    req(input$files)
    image_output_list <- map(1:nrow(files()), ~ {
      dragUI(paste("div_drag",.x),
             imageOutput(paste0("image", .),
                         height = 160))
    })
    div(image_output_list)
  })
  
  output$apercu_gif <- renderImage({
    req(input$files)
    #gif
    img <- anim(src = files()$datapath, vitesse = input$vitesse, taille = input$taille)
    tmpfile <- img %>%
      image_write(tempfile(fileext='gif'), format = 'gif')
    # Return a list
    list(src = tmpfile, contentType = "image/gif")
  })
  
  observe({
    req(input$files)
    
    #apercus images
    map(1:nrow(files()), ~ {

      imagename = paste0("image", .x)
      print(imagename)
      output[[imagename]] <- renderImage({
        list(src = smaller_files()[.x],
             alt = "Image failed to render")
      }, deleteFile = FALSE)
      
    })
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

