library(shiny)
library(shinyjs)
library(scraper)

tagList(
  useShinyjs(),
  tags$head(
    tags$script(src = "shorten.js"),
    tags$link(href = "style.css", rel = "stylesheet")
  ),
  div(id = "loading-content", h1("Loading...")),


navbarPage(


  title = 'Mendeley DataTable',
  tabPanel('Imperial',     DT::dataTableOutput('ex1'),
           tags$head(tags$script(src=c("shorten.js")))),
  tabPanel('Cambridge',        DT::dataTableOutput('ex2')),
  tabPanel('All',      DT::dataTableOutput('ex3'))
)
)

