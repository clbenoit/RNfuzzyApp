# server-normalization.R


AnalysisRun <- reactiveValues(AnalysisRunValue = FALSE)

observeEvent(input$DEA, {
  progressSweetAlert(
    session = session,
    id = "DEAnalysisProgress",
    title = "Work in progress",
    display_pct = TRUE,
    value = 0
  )
  
  # Create a TCC Object 
  tcc <-
    new("TCC", var$CountData, var$selectedgroups)
  var$tccObject <- tcc

  
  updateProgressBar(
    session = session,
    id = "DEAnalysisProgress",
    title = "DE Analysis in progress...",
    value = 50
  )
  tcc <- calcNormFactors( #function to norm
    tcc,
    norm.method = input$normMethod,
    test.method = input$testMethod,
    FDR = input$fdr,
    floorPDEG = input$floorpdeg,
    iteration = 3
  )
  
  updateProgressBar(
    session = session,
    id = "DEAnalysisProgress",
    title = "DE Analysis in progress...",
    value = 75
  )
  tcc <- estimateDE(tcc,
                    test.method = input$testMethod,
                    FDR = input$fdr)
  
  
  var$tccObject <- tcc
  var$result <- getResult(tcc, sort = FALSE) %>% mutate_if(is.factor, as.character)
  
  var$result_a <- var$result[,-2]
  var$result_m <- var$result_a[,-2]
  colnames(var$result_m) <- c("gene_id", "P Value", "FDR", "Rank", "estimatedDEG")
  var$result_e <- var$result_m[which(var$result_m$estimatedDEG >0),]
  var$result_s <- var$result_e[,-5]
  var$norData <- tcc$getNormalizedData()
  
  
  output$normresultTable <- DT::renderDataTable({
    data <- var$norData
    DT::datatable(
      data,        
      extensions = 'Buttons',
      option = list(
        paging = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = TRUE,
        dom = 'Bfrtip',
        buttons = list(list(
          extend = 'collection',
          buttons = list(extend='csv',
                         filename = "results_conversion"),
          text = 'Download')),
        scrollX = TRUE,
        pageLength = 10,
        searchHighlight = TRUE,
        orderClasses = TRUE
        
      ),
      
      class = "display")
  }, server = FALSE)
  
  output$fullresultTable <- DT::renderDataTable({
    data <- var$norData
    gene_id <- row.names(data)
    data <- cbind(data, gene_id = gene_id)
    
    resultTable <- merge(var$result_m, data, by = "gene_id")
    
    DT::datatable(
      resultTable,        
      extensions = 'Buttons',
      option = list(
        paging = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = TRUE,
        dom = 'Bfrtip',
        buttons = list(list(
          extend = 'collection',
          buttons = list(extend='csv',
                         filename = "results_conversion"),
          text = 'Download')),
        scrollX = TRUE,
        pageLength = 10,
        searchHighlight = TRUE,
        orderClasses = TRUE
        
      ),
      
      class = "display"),
      caption = tags$caption(
        tags$li(
          HTML("<font color=\"#B22222\"><b>Gene Name</b></font> is colored when under FDR cut-off")
        )
      ))%>% formatStyle(
        "gene_id",
        "estimatedDEG",
        color = styleEqual(1, "#B22222"),
        fontWeight = styleEqual(c(0, 1), c("normal", "bold"))
      )
  }, server = F)
  
  output$sortedresultTable <- DT::renderDataTable({
    data <- var$norData
    gene_id <- row.names(data)
    data <- cbind(data, gene_id = gene_id)
    resultTable <- merge(var$result_s, data, by = "gene_id")
    
    DT::datatable(
      resultTable,        
      extensions = 'Buttons',
      option = list(
        paging = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = TRUE,
        dom = 'Bfrtip',
        buttons = list(list(
          extend = 'collection',
          buttons = list(extend='csv',
                         filename = "results_conversion"),
          text = 'Download')),
        scrollX = TRUE,
        pageLength = 10,
        searchHighlight = TRUE,
        orderClasses = TRUE
        
      ),
      
      class = "display")
  }, server = FALSE)
  
  closeSweetAlert(session = session)
  sendSweetAlert(session = session,
                 title = "DONE",
                 text = "DE Analysis was successfully performed.",
                 type = "success")
  
  
  AnalysisRun$AnalysisRunValue <- input$DEA
  updateNavbarPage(session, "tabs", "redirectres")
  
})
resultTable <- reactive({
  var$result
})



# results tables render


output$NormResultTable <- renderUI({
  if(AnalysisRun$AnalysisRunValue){
    tagList(
      fluidRow(column(
        12, DT::dataTableOutput('normresultTable') %>% withSpinner()
      )))} else {
        helpText("Run Normalization to obtain Result Table.")
      }
})


output$mainResultTable <- renderUI({
  if(AnalysisRun$AnalysisRunValue){
    tagList(
      fluidRow(column(
        12, DT::dataTableOutput('fullresultTable') %>% withSpinner()
      )))} else {
        helpText("Run Normalization to obtain Result Table.")
      }
})


output$mainsortedResultTable <- renderUI({
  if(AnalysisRun$AnalysisRunValue){
    tagList(
      fluidRow(column(
        12, DT::dataTableOutput('sortedresultTable') %>% withSpinner()
      )))} else {
        helpText("Run Normalization to obtain Result Table.")
      }
})

