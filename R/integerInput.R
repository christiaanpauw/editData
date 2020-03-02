
# Module UI

#' @title   integerInputUI and integerInput
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_numericInputInt
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
integerInputUI <- function(id, ...){
  ns <- NS(id)
  tagList(
       shiny::numericInput(ns("intInput"), ...)
  )
}

# Module Server

#' @rdname integerInput
#' @export
#' @keywords internal

integerInput <- function(input, output, session){

     x <- reactive({as.integer(gsub(pattern = "[^[\\,\\.]]", replacement = "\\1", x = input$intInput))})

     observeEvent(input$intInput,{
          updateNumericInput(session = session, inputId = "intInput", value = x()
          )
     })
     return( x )
}


#' @title toets_integerInput
#' @export

toets_integerInput <- function(){

     uii <- shiny::tagList(
          integerInputUI(id = "toetsID", label = "Heelgetal Toetsarea", value = 10),
          verbatimTextOutput("uit"),
          uiOutput("uit2")
     )
     serverr <- function(input, output, session){
          ns <- session$ns
          output$uit2 <- renderUI({tagList(numericInput3(inputId = "a", label = "b",
                                                        value = input$intInput
                                                        ))})
          output$uit <- renderText(callModule(integerInput, "toetsID")())

     }

     shinyApp(ui = uii, server = serverr, options = list(test.mode = TRUE))

}


## To be copied in the UI
# integerInputUI("integerInput1")

## To be copied in the server
# callModule(integerInput, "integerInput1")
