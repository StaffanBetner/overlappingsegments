library(shiny)
shinyUI(fluidPage(
  titlePanel("Overlapping Segments Viewer"),
  sidebarLayout(
    sidebarPanel(
      helpText("Initial loading may take some time.",br(),
               "Chromium based (e.g. Chrome or Opera) browsers are recommended.",br(), 
               "(Microsoft Edge won't work)", br(), 
               tags$p("Source available",tags$a(href = "https://github.com/StaffanBetner/overlappingsegments", "here."))),
      fileInput('file', 'Upload one or more CSV File(s) with Chromosome Browser Results (Max. Size 50 MB)',
                accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'), multiple = T),
      fileInput('file2', '(Optional) Upload one or more CSV File(s) with Matches List(s) (Max. Size 50 MB)',
                accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'), multiple = T),
      numericInput("cM", "Minimum cM", value=7),
      selectizeInput("name", "Select matches for comparison",choices = NA, multiple=T),
      selectizeInput("exclude", "Select matches to exclude from comparison",choices = NA, multiple=T),
      downloadButton("downloadData_csv", "Download output (.csv)"),
      downloadButton("downloadData_xlsx", "Download output (.xlsx)"),
      br(),
      helpText("Contact: staffan@betner.nu")
    ),
    mainPanel(
      dataTableOutput("table")
    )
  )))