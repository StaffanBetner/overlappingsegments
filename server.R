library(shiny)
options(shiny.maxRequestSize=30*1024^2) 
library(tidyverse)
library(data.table)
library(kableExtra)
#library(xlsx)

findoverlapping_segments <- function(dataset, cM = 7, name = NA, exclude=NA){
  library(data.table)
  library(tidyverse)
  '%!in%' <- function(x,y)!('%in%'(x,y))
  if((is_empty(name))==T){dataset <- dataset %>% dplyr::filter(CENTIMORGANS>cM) %>% dplyr::filter(MATCHNAME %!in% exclude)
  setkey(setDT(dataset), CHROMOSOME, `START LOCATION`, `END LOCATION`)
  olaps = foverlaps(dataset, dataset, type="any", which=TRUE)
  dataset[unique(c(olaps[olaps$xid != olaps$yid, ]$xid, olaps[olaps$xid != olaps$yid, ]$yid)), ] %>%
    mutate(
      CHROMOSOME = CHROMOSOME %>%  factor(
        labels = c(1:22, "X") %>% as.character(),
        levels = (c(1:22, "X")) %>% as.character(),
        ordered = T
      )
    ) %>% 
    arrange(CHROMOSOME) %>% unique}else{
      dataset <- dataset %>% dplyr::filter(CENTIMORGANS>cM) %>% dplyr::filter(MATCHNAME %!in% exclude)
      setkey(setDT(dataset), CHROMOSOME, `START LOCATION`, `END LOCATION`)
      olaps = foverlaps(dataset, dataset, type="any", which=TRUE)
      bind_rows(dataset[olaps[(as.data.frame(cbind(dataset[olaps$xid,2], 
                                                   dataset[olaps$yid,2])[,1])$MATCHNAME %in% name),]$xid,],  #get the rows for those in name
                dataset[olaps[(as.data.frame(cbind(dataset[olaps$xid,2], 
                                                   dataset[olaps$yid,2])[,1])$MATCHNAME %in% name),]$yid,]) %>% unique}} #get the rows for those with overlap with name


shinyServer(function(input, output, session) {
  
  inFile <- reactive({
    if (is.null(input$file)) {
      return(NULL)
    } else {
      input$file
    }
  })
  
  myData <- reactive({
    if (is.null(inFile())) {
      return(NULL)
    } else {
      read_csv(inFile()$datapath, 
               col_types = cols(CENTIMORGANS = col_double(), 
                                CHROMOSOME = col_character(), `END LOCATION` = col_integer(), 
                                `MATCHING SNPS` = col_integer(), 
                                MATCHNAME = col_character(), NAME = col_character(), 
                                `START LOCATION` = col_integer()))
    }
  })
  
  observe({
    updateSelectizeInput(
      session,
      "name",
      choices=myData()$MATCHNAME)
updateSelectizeInput(
    session,
    "exclude",
    choices=myData()$MATCHNAME)
})
  segments <- reactive({
    if (is.null(inFile())) {
      return(NULL)
    } else {
      myData() %>% 
        findoverlapping_segments(cM=input$cM, name = input$name %>% as.vector(), exclude = input$exclude %>% as.vector())
    }
  })
 observe({output$table <- renderDataTable({ if (is.null(inFile())) {
   return(NULL)
 } else {segments()}})
 output$downloadData_csv <- downloadHandler(
   filename = "overlapping segments.csv",
   content = function(file) {
     write.csv(segments(), 
               file, 
               row.names = 
                 FALSE, eol = "\r\n")
   }
 )
 output$downloadData_xlsx <- downloadHandler(
   filename="overlapping segments.xlsx", 
   content = function(file){
     xlsx::write.xlsx(segments(), 
                file, 
                sheetName = "Overlapping segments", 
                row.names = FALSE)
     }
   )
 })
})
