library(shiny)

source("my_server.R")
source("my_ui.R")

shinyApp(ui = ui, server = server)
