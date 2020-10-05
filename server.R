if (!("pacman" %in% rownames(installed.packages()))) {install.packages("pacman")}
pacman::p_load(shiny,
               tidyverse,
               data.table,
               htmlwidgets,
               DT,
               kableExtra,
               reshape2,
               dtplyr,
               rio,
               tidytable)
options(shiny.maxRequestSize=50*1024^2)

'%!in%' <- function(x,y)!('%in%'(x,y))

findoverlapping_segments <- function(dataset, cM = 7, name = NULL, exclude=NULL){
  dataset %>% 
    lazy_dt() %>% 
    filter(CENTIMORGANS > cM) %>% 
    filter(MATCHNAME %!in% exclude) %>% 
    as.data.table() ->
    dataset
  
  setkey(dataset, CHROMOSOME, `START LOCATION`, `END LOCATION`)

  if((is_empty(name)) == T){
    olaps = foverlaps(dataset, dataset, type="any", which=FALSE) %>% 
      lazy_dt() %>% 
      filter(MATCHNAME != i.MATCHNAME) %>% 
      select(1:9) %>% 
      distinct() %>% 
      as.data.table()
    }else{
    dataset_name <- dataset %>% 
      lazy_dt() %>% 
      filter(MATCHNAME %in% name) %>% 
      as.data.table()
    
    olaps = foverlaps(dataset_name, dataset, type="any", which=FALSE)
    olaps %>% 
      lazy_dt() %>% 
      filter(MATCHNAME != i.MATCHNAME) %>% 
      as.data.table() -> olaps
    olaps[,c(1:9)] -> olaps1
    olaps[,c(1,10:17)] -> olaps2
    colnames(olaps2) <- colnames(olaps1)
    olaps2 %>% lazy_dt() %>% full_join(olaps1) %>% distinct() %>% 
      mutate(sorter = !(MATCHNAME %in% name))%>% as.data.table -> olaps
    setkey(olaps, CHROMOSOME, `START LOCATION`, sorter, `END LOCATION`)
    olaps %>% select.(-sorter) %>% as.data.table() -> olaps
  }
olaps %>% 
  lazy_dt() %>% 
  mutate(CHROMOSOME = CHROMOSOME %>% factor(labels = c(1:22, "X") %>% as.character(), 
                                              levels = c(1:22, "X") %>% as.character(), 
                                              ordered = T)) %>% 
  arrange(CHROMOSOME, 
          `START LOCATION`) %>% 
  select(NAME, 
         MATCHNAME,
         CHROMOSOME, 
         `START LOCATION`, 
         `END LOCATION`, 
         CENTIMORGANS, 
         `MATCHING SNPS`, 
         `Shared cM`, 
         `Longest Block`) %>% 
  as.data.table() -> output
return(output)
} 

overlap_in_lists <- function(out){if(length(unique(out$NAME)) > 1){
  out %>% 
    lazy_dt() %>% 
    distinct(MATCHNAME, NAME) %>% 
    group_by(MATCHNAME) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    filter(n > 1) %>% 
    as.data.table() %>% 
    pull(MATCHNAME) %>% 
    c(unique(out$NAME)) -> 
    uniques_shared_matches
  
  out <- out %>% lazy_dt() %>% filter(MATCHNAME %in% uniques_shared_matches) %>% as.data.table()
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
  
  importData <- reactive({
    if (is.null(inFile())) {
      return(NULL)
    } else {
     rbindlist(lapply(inFile()$datapath, import,
                             encoding = "UTF-8", quote = "", setclass="data.table")) %>% 
       lazy_dt() %>% 
       mutate_at(1:3, trimws) %>% 
       rename(NAME = Name,
              MATCHNAME = `Match Name`,
              CHROMOSOME = Chromosome,
              `START LOCATION` = `Start Location`,
              `END LOCATION` = `End Location`,
              CENTIMORGANS = Centimorgans,
              `MATCHING SNPS` = `Matching SNPs`) %>% 
       as.data.table() ->
       dat1
     
     data.table(MATCHNAME = unique(dat1$MATCHNAME)) %>% 
       lazy_dt() %>% 
       mutate(MATCHNAME2 = MATCHNAME %>% str_replace_all("  ", " ") %>% str_replace_all("  ", " ") %>% str_replace_all("  ", " ")) %>% 
       full_join(., dat1) %>% 
       select(NAME, everything()) %>% 
       select(-MATCHNAME) %>% 
       rename(MATCHNAME = MATCHNAME2) %>% 
       as.data.table() %>% 
       nest_by.(NAME) %>% 
       lazy_dt() %>% 
       mutate(NAME = NAME %>% str_remove("\"")) %>% 
       as.data.table() %>% 
       unnest.() %>% 
       lazy_dt() %>% 
       group_by(NAME, MATCHNAME) %>% 
       mutate(`Shared cM`= sum(CENTIMORGANS*(CHROMOSOME != "X")), 
              `Longest Block` = max(CENTIMORGANS*(CHROMOSOME != "X"))) %>% 
       ungroup() %>% 
       mutate(`Shared cM` = `Shared cM` %>% round(2),
              `Longest Block` = `Longest Block` %>% round(2)) %>% 
       as.data.table() -> out
     out <- overlap_in_lists(out)
     out}
  })
  
  matchesData <- reactive({
    if (is.null(input$file2)) {
      return(NULL)
    } else {rbindlist(lapply(inFile2()$datapath, import,
                              #`Match Date` = col_date(format = "%m/%d/%Y"), 
                             encoding = "UTF-8")) %>% #na = c("N/A","")
      #  group_by(`Full Name`) %>% dplyr::filter(`Match Date` == min(`Match Date`)) %>% ungroup %>%  #Unnecessary
        mutate(MATCHNAME=`Full Name` %>% 
                 gsub("  "," ", x = .) %>% gsub("  "," ", x = .) %>% gsub("  "," ", x = .)) %>% 
        select(MATCHNAME, `Match Date`,`Relationship Range`,`Suggested Relationship`,`Shared cM`,`Longest Block`,`Email`,`Ancestral Surnames`,`Y-DNA Haplogroup`,`mtDNA Haplogroup`) %>% 
        mutate(`Shared cM`=`Shared cM` %>% signif(digits=2),
               `Longest Block`=`Longest Block` %>% signif(digits=2))}})
  
  names <- reactive({
    uniques <- data.frame(names=unique(importData()$NAME))
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
      findoverlapping_segments(dataset = importData(),
                                      cM=input$cM, 
                                      name = input$name %>% as.vector(), 
                                      exclude = input$exclude %>% as.vector()) %>% 
        lazy_dt() %>% 
        transmute(NAME,MATCHNAME,
                  CHR=CHROMOSOME, 
                  START = `START LOCATION`, 
                  END = `END LOCATION`, 
                  CENTIMORGANS, 
                  `MATCHING SNPS`) %>% 
        as.data.table() -> 
        out
      
      out <- overlap_in_lists(out)
      out}
      else{
       findoverlapping_segments(dataset = importData(),
                                        cM = input$cM, 
                                        name = input$name %>% as.vector(), 
                                        exclude = input$exclude %>% as.vector()) %>% 
          lazy_dt() %>% 
            transmute(NAME,
                      MATCHNAME,
                      CHR=CHROMOSOME, 
                      START = `START LOCATION`, 
                      END = `END LOCATION`,
                      CENTIMORGANS, 
                      `MATCHING SNPS`, 
                      `Shared cM`, 
                      `Longest Block`) %>% 
          left_join(matchesData()) %>% 
            select(-`Ancestral Surnames`,
                   -`Y-DNA Haplogroup`,
                   -`mtDNA Haplogroup`,
                   -`Shared cM`,
                   -`Longest Block`,
                   -`Suggested Relationship`, 
                   -`Shared cM`, 
                   -`Longest Block`) %>% 
          as.data.table() -> 
          out
        out <- overlap_in_lists(out)
        out}
    }
  })
  
  segments_out <- reactive({
    if (is.null(inFile())) {
      return(NULL)
    } else {if (is.null(matchesData())) {
      findoverlapping_segments(dataset = importData(),
                                      cM = input$cM, 
                                      name = input$name %>% as.vector(), 
                                      exclude = input$exclude %>% as.vector()) %>% 
        transmute(NAME,
                  MATCHNAME,
                  CHROMOSOME,
                  `START LOCATION`,
                  `END LOCATION`,
                  CENTIMORGANS,
                  `MATCHING SNPS`)}else{
         out <- importData() %>% 
            findoverlapping_segments(cM = input$cM, 
                                     name = input$name %>% as.vector(), 
                                     exclude = input$exclude %>% as.vector()) %>% 
           lazy_dt() %>% 
           left_join(matchesData()) %>% 
            select(-`Ancestral Surnames`,
                   -`Y-DNA Haplogroup`,
                   -`mtDNA Haplogroup`,
                   -`Shared cM`,
                   -`Longest Block`,
                   -`Suggested Relationship`) %>% 
           as.data.table() -> out
         out <- overlap_in_lists(out)
         out}
    }
  })
  
  observe({updateSelectizeInput(
    session,
    "name",
    choices=importData()$MATCHNAME, selected = names(), server = TRUE)})
  
  observe({updateSelectizeInput(
    session,
    "exclude",
    choices=importData()$MATCHNAME, server = TRUE)})
  
  observe({
    output$table <- DT::renderDataTable({ if (is.null(inFile())) {
    return(NULL)
  } else {DT::datatable(
    segments(),
    filter = 'top', extensions = c('Buttons', 'Scroller'),
    options = list(scrollY = 650,
                   scrollX = 500,
                   deferRender = TRUE,
                   scroller = TRUE,
                   buttons = list('excel', "csv"),
                   dom = 'lBfrtip',
                   fixedColumns = TRUE), 
    rownames = FALSE)}}, server = TRUE)})
    
#  observe({
#  output$downloadData_csv <- downloadHandler(
#    filename = "overlapping segments.csv",
#    content = function(file) {
#      write.csv(segments_out(), 
#                file, 
#                row.names = 
#                  FALSE, eol = "\r\n")
#    }
#  )
#  })
#  observe({output$downloadData_xlsx <- downloadHandler(
#    filename="overlapping segments.xlsx", 
#    content = function(file){
#      xlsx::write.xlsx(segments_out(), 
#                       file, 
#                       sheetName = "Overlapping segments", 
#                       row.names = FALSE)
#    }
#  )
#  })
})
