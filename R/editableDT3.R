#' UI of editData Shiny module
#' @param id A string
#' @importFrom shiny NS icon fluidPage fluidRow actionButton p conditionalPanel numericInput textInput
#' @importFrom DT dataTableOutput
#' @export
#'
editableDT3UI <- function(id, mode = c(1:4)[4]){

    ns <- NS(id)
    fluidPage(
        fluidRow(
            shinyjs::useShinyjs(),
            if(mode %in% c(1,2,3)) actionButton(ns("delRow"),"Delete Row",icon=icon("remove",lib="glyphicon")),
            if(mode %in% c(1,2,3,4)) actionButton(ns("editData"),"Edit Data",icon=icon("wrench",lib="glyphicon")),
            if(mode %in% c(1,3)) actionButton(ns("newCol"),"New Col",icon=icon("plus-sign",lib="glyphicon")),
            if(mode %in% c(1,3)) actionButton(ns("removeCol"),"Remove Col",icon=icon("trash",lib="glyphicon")),
            if(mode %in% c(2,3)) actionButton(ns("reset"),"Reset",icon=icon("remove-sign",lib="glyphicon")),
            if(mode %in% c(2,3)) actionButton(ns("restore"),"Restore",icon=icon("heart",lib="glyphicon")),
            p(""),
            DT::DTOutput(ns("origTable"))
        )
    )
}

#' Server function of editData Shiny module
#'
#' @param input input
#' @param output output
#' @param session session
#' @param data A data object
#' @param inputwidth Numeric indicating default input width in pixel
#' @param mode An integer
#' @importFrom shiny updateTextInput updateNumericInput reactive validate need showModal modalDialog updateDateInput updateCheckboxInput updateSelectInput observe modalButton renderUI textAreaInput updateTextAreaInput removeModal
#' @importFrom DT renderDataTable datatable dataTableProxy replaceData selectPage
#' @importFrom dplyr select
#' @importFrom magrittr "%>%"
#' @export
editableDT3 <- function(input, output, session,
                       data = reactive(NULL),
                       inputwidth = reactive(100),
                       mode = reactive(2),
                       excludeCols = c(),
                       defaultlen = 20) {


    rv <- reactiveValues()
    rv$result <- ""
    rv$no <- 1
    rv$page <- 1
    rv$resetPage <- FALSE
    rv$dataInternal <- NULL

    observe({ rv$dataInternal <- data() })

    output$origTable <- DT::renderDT({
        if (length(rv$dataInternal) == 0) {
            rv$resetPage <- FALSE
            return(NULL)
        }

        rv$resetPage <- TRUE
        DT::datatable(
            rv$dataInternal[setdiff(names(rv$dataInternal), excludeCols)],
            selection = "single",
            editable=TRUE,
            caption = NULL)
    })

    proxy <- DT::dataTableProxy('origTable')

    observeEvent(input$origTable_cell_edit, {

        info = input$origTable_cell_edit
        i = info$row
        j = info$col
        v = info$value
        x <- rv$dataInternal
        x[i, j] <- DT::coerceValue(v, x[i, j])
        replaceData(proxy, x, resetPaging = FALSE)  # important

        rv$dataInternal <- x
        rv$page <- (i-1) %/% 10+1
    })

    observeEvent(input$delRow,{
        ids <- input$origTable_rows_selected

        if (length(ids) == 0) {
            showModal(
                modalDialog(
                    title = "Delete Row",
                    "Please Select Row(s) To Delete. Press 'Esc' or Press 'OK' button",
                    easyClose = TRUE,
                    footer=modalButton("OK")
                )
            )
            return(invisible(NULL))
        }

        x <- as.data.frame(rv$dataInternal)
        x <- x[-ids,]
        rv$dataInternal <- x
        rv$page <- (ids[1]-1) %/% 10+1
    })

    observeEvent(input$editData,{

        ids <- input$origTable_rows_selected

        if (length(ids) == 0) {
            showModal(
                modalDialog(
                    title = "Edit Row",
                    "Please select a row to edit.",
                    easyClose = TRUE,
                    footer=modalButton("OK")
                )
            )
            return(invisible(NULL))
        }

        if (length(ids) == 1) {
            rv$no <- ids
        } else {
            if (rv$no > nrow(rv$dataInternal)) {
                rv$no <- 1
            }
        }

        editDataModal()
        rv$page <- (ids-1) %/% 10+1
    })

    observeEvent(input$newCol,{

        ns <- session$ns

        showModal(modalDialog(
            title = "Calculate New Column",
            "You can add new column. Press 'Esc' or Press 'Mutate' button",
            textInput(ns("newColText"), "Calculate Column", value = "", placeholder = "LDL = TC - HDL - TG/5"),
            easyClose = TRUE,
            footer=tagList(
                modalButton("Cancel"),
                actionButton(ns("mutateCol"),"Mutate")
            )
        ))

    })

    observeEvent(input$mutateCol,{

        tryCatch({
            x <- as.data.frame(rv$dataInternal)
            eval(parse(text = paste0("x1 <- mutate(x,", input$newColText, ")")))
            if (any(class(x1) %in% c("data.frame","tibble","tbl_df"))) {
                rownames(x1)<- rownames(x)
                rv$dataInternal <- x1
            }
        }, error = function(e) {
            message(sprintf("Failed to create new column: %s", e))
        })

        removeModal()
    })

    observeEvent(input$removeCol,{

        ns <- session$ns

        showModal(
            modalDialog(
            title = "Delete Column",
            "Please select column(s) to delete.",
            selectInput(ns("colRemove"), "Column to Remove", choices = colnames(rv$dataInternal)),
            easyClose = TRUE,
            footer=tagList(
                modalButton("Cancel"),
                actionButton(ns("delCol"),"Remove")
            )
        ))

    })

    observeEvent(input$delCol,{

        x <- as.data.frame(rv$dataInternal)
        x <- eval(parse(text=paste0("select(x,-",input$colRemove,")")))
        rv$dataInternal <- x
        removeModal()

    })

    observeEvent(input$reset,{
        rv$dataInternal <- data()
    })

    observeEvent(input$restore,{
        rv$dataInternal <- data()
    })

    observeEvent(input$resetPage, {
        if (input$resetPage){
            proxy %>% selectPage(rv$page)
            rv$resetPage <- FALSE
        }
    })

    # -------------------------------------------------- #
    # editDataModal

    editDataModal <- reactive({

        ns <- session$ns

        showModal(
            modalDialog(
                title = "Edit Data",
                footer = tagList(
                    actionButton(inputId = ns("remove"), label = "Delete", icon = icon("remove", lib = "glyphicon")),
                    actionButton(inputId = ns("update"), label = "Update", icon = icon("ok", lib = "glyphicon")),
                ),
                easyClose = TRUE,
                makeTagList(ns = session$ns, rv = rv, inputwidth = inputwidth)
            )
        )
    })

    observeEvent(rv$no, {

        if (length(rv$dataInternal) == 0) { return(invisible(NULL)) }

        mydf2 <- rv$dataInternal
        myclass <- lapply(mydf2, class)

        updateNumericInput(session = session, inputId = "rowno", value = rv$no)
        updateTextInput(session, "rowname", value = rownames(mydf2)[rv$no])

        mydf <- as.data.frame(mydf2[rv$no,])

        for (i in 1:ncol(mydf)) {
            myname <- colnames(mydf)[i]
            if ("factor" %in% myclass[[i]]) {
                updateSelectInput(session, myname, choices = levels(mydf[[i]]), selected = mydf[1,i])
                next
            }
            if ("Date" %in% myclass[[i]]) {
                updateDateInput(session, myname,value = mydf[1,i])
                next
            }
            if ("logical" %in% myclass[[i]]) {
                myvalue <- mydf[1,i]
                if (is.na(myvalue)) { myvalue <- FALSE }
                updateCheckboxInput(session, myname, value = myvalue)
                next
            }
            # c("numeric","integer","charater")
            mywidth <- (((max(nchar(mydf2[[i]]), defaultlen, na.rm = TRUE)*8) %/% inputwidth())+1) * inputwidth()
            if (mywidth <= 500) {
                updateTextInput(session, myname, value = mydf[1,i])
            } else{
                updateTextAreaInput(session, myname, value = mydf[1,i])
            }
        }
    })

    observeEvent(input$remove,{

        x <- as.data.frame(rv$dataInternal)
        x <- x[-rv$no,]
        rv$dataInternal <- x

        if (rv$no > nrow(x)) { rv$no <- nrow(x) }
    })

    observeEvent(input$update,{

        ids <- rv$no
        if (length(ids) == 0) { return(invisible(NULL)) }
        if (is.na(ids)) { return(invisible(NULL)) }

        x <- as.data.frame(rv$dataInternal)

        # update rowname
        tryCatch({
            rownames(x)[ids] <- input$rowname
        }, error = function(e) {
            message("Failed to update rowname. %s", e)
        })

        # update values
        myname <- colnames(x)
        for (i in 1:ncol(x)) {

            if ("POSIXct" %in% class(x[[i]])) {
                tz <- ""
                if (!is.null(attr(x[ids,i], "tzone"))) { tz <- attr(x[ids,i],"tzone") }
                x[ids,i] <- as.POSIXct(input[[myname[i]]], tz = tz, origin = "1970-01-01")
                next
            }

            tryCatch({
                x[ids,i] <- input[[myname[i]]]
            }, error = function(e) {
                message(sprintf("Failed to update %s: %s", myname[i], e))
            })
        }

        rv$dataInternal <- x
    })

    observeEvent(input$home,{
        rv$no <- 1
    })

    observeEvent(input$end,{
        rv$no <- nrow(rv$dataInternal)
    })

    observeEvent(input$left,{
        rv$no <- max(rv$no -1, 1)
    })

    observeEvent(input$right,{
        rv$no <- min(rv$no +1, nrow(rv$dataInternal))
    })

    observeEvent(input$rowno,{
        maxno <- nrow(rv$dataInternal)
        # print(maxno)
        # print(input$rowno)
        if (is.na(input$rowno)) { updateNumericInput(session, "rowno", value = maxno) }
        if (input$rowno > maxno) {
            updateNumericInput(session, "rowno", value = maxno)
            rv$no <- maxno
        } else {
            rv$no <- input$rowno
        }
    })

    # -------------------------------------------------- #

    df <- reactive({
        df <- rv$dataInternal
        return(df)
    })

    return(df)

}

#'@title makeTagList
#'@export
makeTagList <- function(ns, rv, inputwidth) {

    mydf2 <- rv$dataInternal
    myclass <- lapply(mydf2, FUN = function(x) {
        clss <- class(x)
        if (clss == "numeric") {
            if (all(na.omit(as.integer(x)) == na.omit(x))) {
                clss <- "integer"
            }
        }
        return(clss)
    })

    mylist <- list(
        actionButton(inputId = ns("home"), label = "", icon = icon("backward", lib = "glyphicon")),
        actionButton(inputId = ns("left"), label = "", icon = icon("chevron-left", lib = "glyphicon")),
        numericInput3(inputId = ns("rowno"), label = "rowno", value = rv$no, min = 1,
                      max = nrow(rv$dataInternal), step = 1, width = 50+10*log10(nrow(rv$dataInternal))),
        actionButton(inputId = ns("right"), label = "", icon = icon("chevron-right", lib = "glyphicon")),
        actionButton(inputId = ns("end"), label = "", icon = icon("forward", lib = "glyphicon")),
        actionButton(inputId = ns("new"), label = "", icon = icon("plus", lib = "glyphicon")),
        textInput3(inputId = ns("rowname"), label = "rowname", value = rownames(rv$dataInternal)[rv$no], width = 150)
    )
    mylist[[length(mylist)+1]] <- hr()

    addno <- length(mylist)
    mydf <- as.data.frame(mydf2[rv$no,])

    for (i in 1:ncol(mydf)){
        myname <- colnames(mydf)[i]
        if ("factor" %in% myclass[[i]]) {
            mylist[[i+addno]] <- selectInput3(inputId = ns(myname), label = myname, choices = levels(mydf[[i]]),
                                              selected = mydf[1,i], width=inputwidth())
            next
        }
        if ("integer" %in% myclass[[i]]) {
            mylist[[i+addno]] <- integerInput3(inputId = ns(myname), label = myname, value = mydf[1,i],
                                               width = inputwidth(), step = 1)
            next
        }
        if ("numeric" %in% myclass[[i]]) {
            mylist[[i+addno]] <- numericInput3(inputId = ns(myname), label = myname, value = mydf[1,i],
                                               width = inputwidth(), step = "ANY")
            next
        }
        if ("Date" %in% myclass[[i]]) {
            mylist[[i+addno]] <- dateInput3(inputId = ns(myname), label = myname, value = mydf[1,i],
                                            width = inputwidth())
            next
        }
        if ("logical" %in% myclass[[i]]) {
            myvalue <- mydf[1,i]
            if (is.na(myvalue)) { myvalue <- FALSE }
            mylist[[i+addno]] <- checkboxInput3(inputId = ns(myname), label = myname, value = myvalue,
                                                width = inputwidth())
            next
        }
        # c("numeric","integer","character")
        mywidth <- (((max(nchar(mydf2[[i]]), defaultlen, na.rm = TRUE)*8) %/% inputwidth()) +1) *inputwidth()
        if (mywidth <= 500) {
            mylist[[i+addno]] <- textInput3(inputId = ns(myname), label = myname, value = mydf[1,i], width = mywidth)
        } else{
            mylist[[i+addno]] <- textAreaInput(inputId = ns(myname), label = myname, value = mydf[1,i], width = "500px")
        }
    }

    return(do.call(tagList, mylist))
}


