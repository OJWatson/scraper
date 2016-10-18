
navbarPage(
  title = 'Mendeley DataTable',
  tabPanel('Imperial',     DT::dataTableOutput('ex1'),
           tags$head(tags$script(src=c("shorten.js")))),
  tabPanel('Cambridge',        DT::dataTableOutput('ex2')),
  tabPanel('All',      DT::dataTableOutput('ex3'))
)

