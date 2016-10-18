library(shiny)
library(shinyjs)
library(scraper)

function(input, output) {

  # -----------------------------------------------------------------
  ## DATA ACQUISITION AND CLEANING ##
  # -----------------------------------------------------------------

  ## Scrape the Mendeley sqlite database
  mdf <- scraper::Mendeley_DF()

  ## UTF-8 hack to deal with incorrect character formatting
  mdf[c(2:3,5:15)] <- lapply(mdf[c(2:3,5:15)],function(x){
    Encoding(x)<-"UTF-8"
    return(x)})

  # -----------------------------------------------------------------
  ## EXTRA FUNCTIONS ##
  # -----------------------------------------------------------------

  # Function to add create shiny inputs that can be included in datatable
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }

  # -----------------------------------------------------------------
  ## OBSERVATION ##
  # -----------------------------------------------------------------

  observeEvent(input$select_button, {
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    system(paste0('open "', mdf$Paths[selectedRow],'"'))
  })

  # -----------------------------------------------------------------
  ## DATATABLE CREATION  ##
  # -----------------------------------------------------------------



  df <- reactiveValues(data = data.frame(
    Open = shinyInput(actionButton, max(mdf$Id), 'button_', label = "Open",
                      onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' ),
    mdf[,!(colnames(mdf) %in% c("Id","Paths","Main.Folder","Folder.DAG","Notes"))]
  ))


  ## display Imperial rows initially

  output$ex1 <- DT::renderDataTable(
    DT::datatable(df$data[stringr::str_detect(mdf$Main.Folder,"Imperial"),],
                  options = list(
                    columnDefs = list(list(className = 'dt-shorten', targets = c(3,6:11)),
                                      list(width = '300px',targets = c(3,6:11))
                    ),
                    lengthMenu = list(c(5, 15, 25, -1), c('5', '15', '25','All')),
                    pageLength = 15,
                    orderClasses = TRUE,
                    drawCallback = DT::JS("function() {",
                                          "$('.dt-shorten').shorten({",
                                          "showChars : 200",
                                          "});",
                                          "}")
                  ),
                  filter = 'top',
                  escape = FALSE)
  )


  ## display Cambridge rows secondly

  output$ex2 <- DT::renderDataTable(
    DT::datatable(df$data[stringr::str_detect(mdf$Main.Folder,"Cambridge"),],
                  options = list(
                    columnDefs = list(list(className = 'dt-shorten', targets = c(3,6:11)),
                                      list(width = '300px',targets = c(3,6:11))
                    ),
                    lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
                    pageLength = 5,
                    orderClasses = TRUE,
                    drawCallback = DT::JS("function() {",
                                          "$('.dt-shorten').shorten({",
                                          "showChars : 200",
                                          "});",
                                          "}")
                  ),
                  filter = 'top',
                  escape = FALSE)
  )

  ## display all rows thirdly

  output$ex3 <- DT::renderDataTable(
    DT::datatable(df$data,
                  options = list(
                    columnDefs = list(list(className = 'dt-shorten', targets = c(3,6:11)),
                                      list(width = '300px',targets = c(3,6:11))
                    ),
                    lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
                    pageLength = 5,
                    orderClasses = TRUE,
                    drawCallback = DT::JS("function() {",
                                          "$('.dt-shorten').shorten({",
                                          "showChars : 200",
                                          "});",
                                          "}")
                  ),
                  filter = 'top',
                  escape = FALSE)
  )

  # -----------------------------------------------------------------
  # hide the loading message
  hide("loading-content", TRUE, "fade")



  }
