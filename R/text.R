textUI <- function(id){
  ns <- NS(id)
  textOutput(ns("text"))
}

textServer <- function(id, data){
  moduleServer(id, function(input, output, session) {
    output$text <- renderPrint({
      data()
    })  
  })
}

