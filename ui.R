library(shiny)
shinyUI(fluidPage(
  titlePanel("Overlapping Segments Viewer"),
  sidebarLayout(
    sidebarPanel(
      helpText("Initial loading may take some time.",br(),
               "Chromium based (e.g. Chrome or Opera) browsers are recommended.",br(), 
               "(Microsoft Edge won't work)", br(), 
               tags$p("Source available",tags$a(href = "https://github.com/StaffanBetner/shiny-server/tree/master/overlappingsegments", "here."))),
      fileInput('file', 'Upload a CSV File with Chromosome Browser Results (Max. Size 30 MB)',
                accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
      fileInput('file2', '(Optional) Upload a CSV File with Matches List (Max. Size 30 MB)',
                accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
      numericInput("cM", "Minimum cM", value=7),
      selectizeInput("name", "Select matches for comparison",choices = NA, multiple=T),
      selectizeInput("exclude", "Select matches to exclude from comparison",choices = NA, multiple=T),
      downloadButton("downloadData_csv", "Download output (.csv)"),
      downloadButton("downloadData_xlsx", "Download output (.xlsx)")
    ),
    mainPanel(
      dataTableOutput("table")
    )
  )))