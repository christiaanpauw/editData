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
<<<<<<< HEAD
=======
     assign("xxx", reactiveValuesToList(input), envir = .GlobalEnv)
>>>>>>> 5a7bb1d2a3248a84aedd50e319e0bb19d3cfabd8

     observeEvent(input$intInput,{
          updateNumericInput(session = session, inputId = "intInput", value = x()
          )
     })
     return( x )
}


#' @title toets_integerInput
#' @export

toets_integerInput <- function(){

<<<<<<< HEAD
     uii <- shiny::tagList(
=======
     uii <- tagList(
>>>>>>> 5a7bb1d2a3248a84aedd50e319e0bb19d3cfabd8
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

