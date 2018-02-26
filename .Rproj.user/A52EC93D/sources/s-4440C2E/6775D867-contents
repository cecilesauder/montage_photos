library(shiny)
library(shinyDND)
library(magick)
library(purrr)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  output$files <- renderTable(input$files)
  
  files <- reactive({
    files <- input$files
    files$datapath <- gsub("\\\\", "/", files$datapath)
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
  
  observe({
    req(input$files)
 
    for (i in 1:nrow(files()))
    {
      #print(i)
      local({
        my_i <- i
        imagename = paste0("image", my_i)
        
        output[[imagename]] <- 
          renderImage({
            list(src = smaller_files()[my_i],
                 alt = "Image failed to render")
          }, deleteFile = FALSE)
      })
    }
  })
  
 
})