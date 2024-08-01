'''
images available in jpg, jpeg, png, webp form, png images will not show marker on the background but coordinate will register
use image converter if needed, see link: https://cloudconvert.com/jpg-converter
coords_list is going to contain all of the landmarks selected, press confirm to save the landmarks from each image.
Undo will remove the most recent marker, reset all clears entire list
both shiny and magick are required packages for this code, download available below
install.packages(c(shiny,magick))
When finalising, the csv will save as coords_list.csv, and should save to the working directory
'''

library(shiny)
library(magick)

ui = fluidPage(
  titlePanel("Image Landmarker"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("filename", "Filename", "coords_list.csv"),
      fileInput("file", "Choose an image file",
                accept = c("image/jpeg", "image/jpg", "image/png", "image/webp")),
      actionButton("reset_all", "Reset All", width = '45%'),
      actionButton("reflect", "Reflect Image",width = '45%'),
      actionButton("undo", "Undo",width = '45%'),
      actionButton("clear_image", "Clear Image",width = '45%'),
      actionButton("confirm", "Confirm", width = '92%'),
      actionButton("finalise", "Finalise",width = '92%'),
      h4('Landmark Co-ordinates'),
      tableOutput('click_coords')
    ),
    
    mainPanel(
      tags$style(type = 'text/css', "#image { cursor: crosshair; }"),
      imageOutput("image", click = "image_click")
    )
  )
)

server = function(input, output, session) {
  
  #initialises reactive values
  values = reactiveValues(original = NULL, modified = NULL, coords_temp = list(), coords_list = list())
  
  # Observes image
  observeEvent(input$file, {
    req(input$file)
    values$original = image_read(input$file$datapath)
    values$modified = values$original
    values$coords_temp = list()  # Reset temp coordinates list when a new file is loaded
  })
  
  # Render the image
  output$image = renderImage({
    req(values$modified)
    outfile = tempfile(fileext = '.png')
    image_write(values$modified, path = outfile, format = "png")
    list(src = outfile, contentType = 'image/png', alt = "Uploaded Image")
  }, deleteFile = TRUE)
  
  # Observe click
  observeEvent(input$image_click, {
    req(values$modified)
    
    # Get the click coordinates
    x = as.integer(input$image_click$x)
    y = as.integer(input$image_click$y)
    
    # Create a pixel matrix, edit here for larger markers
    coord_pixel = image_blank(width = 1, height = 1, color = "#00FF00")
    visual_pixel = image_blank(width = 5, height = 5, color = "#000000")
    
    # Adds pixel
    values$modified = image_composite(values$modified, visual_pixel, offset = paste0("+", x, "+", y))
    values$modified = image_composite(values$modified, coord_pixel, offset = paste0("+", x, "+", y))
    
    # Adds landmark to coords_temp list
    values$coords_temp = append(values$coords_temp, list(c(x, y)))
  })
  
  #Reset Image button, clears temp list
  observeEvent(input$clear_image, {
    req(values$original)
    values$modified <- values$original
    values$coords_temp <- list()
  })
  
  # Confirm button: adds coords_temp to coords_list
  observeEvent(input$confirm, {
    # Add the temporary coordinates to the permanent list
    values$coords_list = append(values$coords_list, values$coords_temp)
    values$coords_temp = list()  # Clear the temporary list
    
    # Updates coords_list
    coords_df <- do.call(rbind, lapply(values$coords_list, function(coord) {
      data.frame(X = coord[1], Y = coord[2])
    }))
    assign("coords_list", coords_df, envir = .GlobalEnv)
  })
  
  #Reset all button, clears coords_list
  observeEvent(input$reset_all, {
    # Clear the current image and reset the entire app state
    values$original = NULL
    values$modified = NULL
    values$coords_temp = list()
    values$coords_list = list()
    assign("coords_list", data.frame(X = integer(0), Y = integer(0)), envir = .GlobalEnv)
  })
  
  # Code for reflection horizontally
  observeEvent(input$reflect, {
    req(values$modified)
    values$modified = image_flop(values$modified)
  })
  
  # Undo last point
  observeEvent(input$undo, {
    req(values$modified)
    req(length(values$coords_temp) > 0)  # Ensure there is at least one coordinate to undo
    values$coords_temp <- values$coords_temp[-length(values$coords_temp)] #removes previous point
    
    #recreates points on the image from coords_temp
    values$modified <- values$original
    for (coord in values$coords_temp) {
      x <- coord[1]
      y <- coord[2]
      coord_pixel <- image_blank(width = 1, height = 1, color = "#00FF00")
      visual_pixel <- image_blank(width = 5, height = 5, color = "#000000")
      values$modified <- image_composite(values$modified, visual_pixel, offset = paste0("+", x, "+", y))
      values$modified <- image_composite(values$modified, coord_pixel, offset = paste0("+", x, "+", y))
    }
  })
  
  # Finalise button, creates csv and ends session
  observeEvent(input$finalise, {
    coords_df <- do.call(rbind, lapply(values$coords_list, function(coord) {
      data.frame(X = coord[1], Y = coord[2])
    }))
    filename = input$filename
    if (filename == "") {
      filename = "coords_list.csv"
    }
    write.csv(coords_df, file = filename, row.names = FALSE) #change name here for different file name
    stopApp()  # End the Shiny app session
  })
  
  # Renders coordinate table
  output$click_coords = renderTable({
    req(values$coords_temp)
    do.call(rbind, lapply(values$coords_temp, function(coord) {
      data.frame(X = coord[1], Y = coord[2])
    }))
  })
}
#Run the app
shinyApp(ui, server)

