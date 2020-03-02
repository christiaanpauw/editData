#'Create a side-by-side integerInput
#'@param title integerInput3
#'@param inputId The input slot that will be used to access the value.
#'@param label Display label for the control, or NULL for no label.
#'@param value Initial value.
#'@param min Minimum allowed value
#'@param max Maximum allowed value
#'@param step Interval to use when stepping between min and max
#'@param width The width of the input in pixel
#'@param ... arguments to be passed to integerInput
#'@export
#'@examples
#'library(shiny)
#'# Only run examples in interactive R sessions
#'if (interactive()) {
#'   ui <- fluidPage(
#'          textInput3("id", "id", ""),
#'          integerInput3("score","score",value=1)
#'     )
#'     server <- function(input, output) {
#'
#'     }
#'     shinyApp(ui, server)
#'}
integerInput3<-function (inputId, label, value, min=NA,max=NA,step=NA,width=100,...)
{
     div(style="display:inline-block;",
         tags$label(label, `for` = inputId,class="control-label"),
         tags$input(id = inputId, type = "number", class="form-control",
                    value = value,
                    oninput="this.value = this.value.replace(/[^0-9]/g, '');",
                    min=min,max=max,step=step,style=paste("width: ",width,"px;",sep=""),...)
     )
}

#' editableDT_toets
#' @title editableDT_toets
#' @export

editableDT_toets <- function(){

     uii <- fluidPage(editableDTUI(id = "id1"),
                      dataTableOutput("dataout"))
     serverr <- function(input, output, session) {
          df <- reactive(mtcars)
          res <- callModule(module = editableDT, id = "id1", data = df, session =session)
          output$dataout <- renderDataTable({res()})
     }
     shinyApp(uii, serverr)
}
