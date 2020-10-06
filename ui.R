if (!("pacman" %in% rownames(installed.packages()))) {install.packages("pacman")}
pacman::p_load(shiny, htmlwidgets, DT, shinydashboard, shinycssloaders)

dashboardPage(
  dashboardHeader(title = "Overlapping Segments Viewer", titleWidth = 310),
  dashboardSidebar(fileInput('file', 'Upload one or more CSV File(s) with Chromosome Browser Results (Max. Size 50 MB)',
                             accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'), multiple = T),
                   #  fileInput('file2', '(Optional) Upload one or more CSV File(s) with Matches List(s) (Max. Size 50 MB)',
                   #            accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'), multiple = T),
                   numericInput("cM", "Minimum cM", value=7),
                   selectizeInput("name", "Select matches for comparison",choices = NA, multiple=T),
                   selectizeInput("exclude", "Select matches to exclude from comparison",choices = NA, multiple=T),
                   helpText("FamilyFinder (FamilyTreeDNA) and MyHeritage files supported.",br(),"Chromium based (e.g. Chrome or Opera) browsers are recommended, (Old) Microsoft Edge probably won't work, upgrade it to the Chromium based version.", br(), 
                            "The uploaded files will only be stored temporary and will be deleted after the session is closed.", br(),
                            tags$p("Source code available",tags$a(href = "https://github.com/StaffanBetner/overlappingsegments", "here.")),br(),
                            "Contact: staffan@betner.nu or @StaffanBetner (Twitter)", width = 300),
                   width = 310),
  dashboardBody(DTOutput("table") %>% withSpinner()))