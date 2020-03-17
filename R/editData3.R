
#' A shiny app for editing a 'data.frame'
#' @param data A tibble or a tbl_df or a data.frame to manipulate
#' @param viewer Specify where the gadget should be displayed. Possible choices are c("dialog","browser","pane")
#' @param mode An integer
#' @importFrom rstudioapi getActiveDocumentContext
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#' @importFrom utils read.csv str write.csv
#' @importFrom shiny stopApp callModule runGadget column fileInput downloadButton renderPrint observeEvent tagList uiOutput browserViewer dialogViewer downloadHandler h4 hr paneViewer checkboxInput
#' @return A manipulated 'data.frame' or NULL
#' @export
#'
editData3 <- function(data = NULL, viewer = c("dialog", "browser", "pane")[1], mode = c(1:4)[4]) {

        if (!isNamespaceLoaded("tidyverse")){
                attachNamespace("tidyverse")
        }

        ui <- miniUI::miniPage(
                miniUI::gadgetTitleBar("editable DataTable"),
                miniUI::miniContentPanel(
                        fluidRow(
                                column(6,
                                       fileInput(inputId = "file1", label = "Upload CSV file", multiple = FALSE),
                                       checkboxInput(inputId = "strAsFactor", label = "strings As Factor", value=FALSE)),
                                column(6)),
                        editableDT3UI("table1"),
                        downloadButton(outputId = "downloadData", label = "Download as CSV")
                ))

        server <- function(input,output,session) {

                if (is.null(data)) { data <- mtcars }
                dfInput <- reactiveVal(data)

                observeEvent(input$file1, {
                        if (!is.null(input$file1)) {
                                dfInput(read.csv(input$file1$datapath,stringsAsFactors = input$strAsFactor))
                        }
                })

                dfOutput <- callModule(editableDT3, "table1", data = dfInput, inputwidth = reactive(170), mode = reactive(mode))

                output$downloadData <- downloadHandler(
                        filename = function() {
                                paste("edited-",Sys.Date(),".csv", sep = "")
                        },
                        content = function(file) {
                                write.csv(dfOutput(), file, row.names = FALSE)
                        }
                )
                observeEvent(input$done, {
                        result <- dfOutput()
                        stopApp(result)
                })

                observeEvent(input$cancel, {
                        stopApp()
                })
        }

        myviewer <- switch(viewer,
                           dialog = dialogViewer("editData", width = 1000, height = 800),
                           browser = browserViewer(),
                           paneViewer())

        runGadget(ui, server, viewer = myviewer)
}
