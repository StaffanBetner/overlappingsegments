library(shiny)
shinyUI(fluidPage(
  titlePanel("Overlapping Segments Viewer"),
  sidebarLayout(
  sidebarPanel(
    helpText("Upload a chromosome browser results file from FamilyTreeDNA.",br(),
             "Initial loading may take some time.",br(),
             "Chromium based (e.g. Chrome or Opera) browsers are recommended.",br(), 
             "(Microsoft Edge won't work)"),
    fileInput('file', 'Choose CSV File (max. size 30 MB)',
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
    numericInput("cM", "Minimum cM", value=7),
    selectizeInput("name", "Select matches for comparison",choices = NA, multiple=T),
    selectizeInput("exclude", "Select matches to exclude from comparison",choices = NA, multiple=T),
    downloadButton("downloadData", "Download output")
  ),
  mainPanel(
    dataTableOutput("table")
  )
)))