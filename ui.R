library(shiny)
library(htmlwidgets)
library(DT)
shinyUI(
  navbarPage(
    "Overlapping Segments Viewer",
             tabPanel(
               "Tool",
    sidebarPanel(
      fileInput('file', 'Upload one or more CSV File(s) with Chromosome Browser Results (Max. Size 50 MB)',
                accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'), multiple = T),
      fileInput('file2', '(Optional) Upload one or more CSV File(s) with Matches List(s) (Max. Size 50 MB)',
                accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'), multiple = T),
      helpText("Initial loading may take some time."),
      numericInput("cM", "Minimum cM", value=7),
      selectizeInput("name", "Select matches for comparison",choices = NA, multiple=T),
      selectizeInput("exclude", "Select matches to exclude from comparison",choices = NA, multiple=T)#,
      #downloadButton("downloadData_csv", "Download output (.csv)"),
      #downloadButton("downloadData_xlsx", "Download output (.xlsx)")
    ),
    mainPanel(
      DTOutput("table")
    )),
    tabPanel("About",
             mainPanel(helpText("Chromium based (e.g. Chrome or Opera) browsers are recommended, Microsoft Edge won't work.", br(), 
                                "The uploaded files will only be stored temporary and will be deleted after the session is closed.", br(),
                                tags$p("Source available",tags$a(href = "https://github.com/StaffanBetner/overlappingsegments", "here.")),br(),
                                "Contact: staffan@betner.nu or @StaffanBetner (Twitter)")))
  ))