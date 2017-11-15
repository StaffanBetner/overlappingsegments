library(shiny)
library(tidyverse)
library(data.table)
library(kableExtra)
library(reshape2)
#library(xlsx)
options(shiny.maxRequestSize=50*1024^2)

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
overlap_in_lists <- function(out){if(length(unique(out$NAME))>1){
  uniques_matches <- split(out$MATCHNAME, out$NAME) %>% 
    lapply(., unique) %>% 
    melt %>% table() %>% 
    as.data.frame %>% 
    group_by(value) %>% 
    summarise(n=sum(Freq)) %>% 
    dplyr::filter(n>1) %>%
    mutate(value = value %>% parse_character()) %>% 
    .$value
  uniques_matches <- cbind(uniques_matches, unique(out$NAME)) %>% as.vector %>% unique
  out <- out %>% dplyr::filter(MATCHNAME %in% uniques_matches)
  out}
  else{out}}

shinyServer(function(input, output, session) {
  
  inFile <- reactive({
    if (is.null(input$file)) {
      return(NULL)
    } else {
      input$file
    }
  })
  
  
  inFile2 <- reactive({
    if (is.null(input$file2)) {
      return(NULL)
    } else {
      input$file2
    }
  })
  
  myData <- reactive({
    if (is.null(inFile())) {
      return(NULL)
    } else {
     out <- rbindlist(lapply(inFile()$datapath, read_csv,
               col_types = cols(CENTIMORGANS = col_double(), 
                                CHROMOSOME = col_character(), `END LOCATION` = col_integer(), 
                                `MATCHING SNPS` = col_integer(), 
                                MATCHNAME = col_character(), NAME = col_character(), 
                                `START LOCATION` = col_integer()), trim_ws = T)) %>% 
        mutate(MATCHNAME = MATCHNAME %>%
                 gsub("  ", " ", x = .) %>% gsub("  ", " ", x = .) %>% gsub("  ", " ", x = .)) %>%
        group_by(NAME, MATCHNAME) %>% 
        mutate(`Shared cM`= sum(CENTIMORGANS[CHROMOSOME!="X"]) %>% signif(digits = 2), 
               `Longest Block` = max(CENTIMORGANS[CHROMOSOME!="X"]) %>% signif(digits = 2))
     out <-overlap_in_lists(out)
     out}
  })
  
  matchesData <- reactive({
    if (is.null(input$file2)) {
      return(NULL)
    } else {rbindlist(lapply(inFile2()$datapath, read_csv,
                             col_types = cols(`Ancestral Surnames` = col_character(), 
                                              Email = col_character(), `First Name` = col_character(), 
                                              `Full Name` = col_character(), `Last Name` = col_character(), 
                                              `Linked Relationship` = col_character(), 
                                              `Longest Block` = col_double(), `Match Date` = col_date(format = "%m/%d/%Y"), 
                                              `Matching Bucket` = col_character(), 
                                              `Middle Name` = col_character(), 
                                              Notes = col_character(), `Relationship Range` = col_character(), 
                                              `Shared cM` = col_double(), `Suggested Relationship` = col_character(), 
                                              `Y-DNA Haplogroup` = col_character(), 
                                              `mtDNA Haplogroup` = col_character()), na = c("N/A",""))) %>% 
        group_by(`Full Name`) %>% dplyr::filter(`Match Date` == min(`Match Date`)) %>% ungroup %>% 
        mutate(MATCHNAME=`Full Name` %>% 
                 gsub("  "," ", x = .) %>% gsub("  "," ", x = .) %>% gsub("  "," ", x = .)) %>% 
        select(-`Full Name`,
               -`First Name`,
               -`Middle Name`,
               -`Last Name`) %>% 
        mutate(`Shared cM`=`Shared cM` %>% signif(digits=2),
               `Longest Block`=`Longest Block` %>% signif(digits=2))}})
  
  names <- reactive({
    uniques <- data.frame(names=unique(myData()$NAME))
    if(1<nrow(uniques))
        {uniques$names}
        else
        {NULL}
      }
    )
  

  segments <- reactive({
    if (is.null(inFile())) {
      return(NULL)
    } else {if(is.null(matchesData())){
      out <- findoverlapping_segments(dataset = myData(),cM=input$cM, name = input$name %>% as.vector(), exclude = input$exclude %>% as.vector()) %>% 
        transmute(NAME,MATCHNAME,CHR=CHROMOSOME, START = `START LOCATION`, END = `END LOCATION`, CENTIMORGANS, `MATCHING SNPS`)
      out <-overlap_in_lists(out)
      out}
      else{
        out <- findoverlapping_segments(dataset = myData(),cM=input$cM, name = input$name %>% as.vector(), exclude = input$exclude %>% as.vector()) %>% 
            transmute(NAME,MATCHNAME,CHR=CHROMOSOME, START = `START LOCATION`, END = `END LOCATION`, CENTIMORGANS, `MATCHING SNPS`) %>% left_join(matchesData()) %>% 
            select(-`Ancestral Surnames`,-`Y-DNA Haplogroup`,-`mtDNA Haplogroup`,-Notes,-`Shared cM`,-`Longest Block`,-`Suggested Relationship`)
        out <-overlap_in_lists(out)
        out}
    }
  })
  
  segments_out <- reactive({
    if (is.null(inFile())) {
      return(NULL)
    } else {if(is.null(matchesData())){
      out <- findoverlapping_segments(dataset = myData(),cM=input$cM, name = input$name %>% as.vector(), exclude = input$exclude %>% as.vector()) %>% 
        transmute(NAME,MATCHNAME,CHROMOSOME,`START LOCATION`,`END LOCATION`,CENTIMORGANS,`MATCHING SNPS`)}else{
         out<- myData() %>% 
            findoverlapping_segments(cM=input$cM, name = input$name %>% as.vector(), exclude = input$exclude %>% as.vector()) %>% left_join(matchesData()) %>% 
            select(-`Ancestral Surnames`,-`Y-DNA Haplogroup`,-`mtDNA Haplogroup`,-Notes,-`Shared cM`,-`Longest Block`,-`Suggested Relationship`)
         out <-overlap_in_lists(out)
         out}
    }
  })
  observe({updateSelectizeInput(
    session,
    "name",
    choices=myData()$MATCHNAME, selected = names())})
  
  observe({updateSelectizeInput(
    session,
    "exclude",
    choices=myData()$MATCHNAME)})
  
  observe({
    output$table <- renderDataTable({ if (is.null(inFile())) {
    return(NULL)
  } else {segments()}})
    
  output$downloadData_csv <- downloadHandler(
    filename = "overlapping segments.csv",
    content = function(file) {
      write.csv(segments_out(), 
                file, 
                row.names = 
                  FALSE, eol = "\r\n")
    }
  )
  output$downloadData_xlsx <- downloadHandler(
    filename="overlapping segments.xlsx", 
    content = function(file){
      xlsx::write.xlsx(segments_out(), 
                       file, 
                       sheetName = "Overlapping segments", 
                       row.names = FALSE)
    }
  )
  })
})
