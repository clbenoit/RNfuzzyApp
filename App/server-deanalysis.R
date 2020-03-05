# server-normalization.R


tccRun <- reactiveValues(tccRunValue = FALSE)

observeEvent(input$TCC, {
  progressSweetAlert(
    session = session,
    id = "DEAnalysisProgress",
    title = "Work in progress",
    display_pct = TRUE,
    value = 0
  )
  
  data <- var$CountData
  data.list <- var$groupListConvert
  tcc <- var$tccObject
  if (input$filterLowCount != 0) {
    tcc$count <- tcc$count[rowMeans(tcc$count) > as.numeric(input$filterLowCount), ]
  }
  
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
    floorPDEG = input$floorpdeg
  )
  
  updateProgressBar(
    session = session,
    id = "DEAnalysisProgress",
    title = "DE Analysis in progress...",
    value = 75
  )
  tcc <- estimateDE(tcc, #function to estime DEG
                    test.method = input$testMethod,
                    iteration = 3,
                    FDR = input$fdr)

  
  var$tccObject <- tcc
  var$result <- getResult(tcc, sort = FALSE) %>% mutate_if(is.factor, as.character)
  var$result <- var$result[,-2]
  var$norData <- tcc$getNormalizedData()


  output$fullresultTable <- DT::renderDataTable({
    data <- var$norData
    gene_id <- row.names(data)
    data <- cbind(data, gene_id = gene_id)
    
    resultTable <- merge(var$result, data, by = "gene_id")
    
    DT::datatable(
      resultTable,
      filter = "bottom",
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
                         filename = "full_normalization_results"),
          text = 'Download')),
        scrollX = TRUE,
        pageLength = 10,
        searchHighlight = TRUE,
        orderClasses = TRUE
        
      ),
      
      class = "display",
      caption = tags$caption(
        tags$li(
          HTML("<font color=\"#B22222\"><b>Gene Name</b></font> is colored when under FDR cut-off, and m.value for Log2FC")
        )
      ))%>% formatStyle(
        "gene_id",
        "estimatedDEG",
        color = styleEqual(1, "#B22222"),
        fontWeight = styleEqual(c(0, 1), c("normal", "bold"))
      )
  }, server = F)

  closeSweetAlert(session = session)
  sendSweetAlert(session = session,
                 title = "DONE",
                 text = "DE Analysis was successfully performed.",
                 type = "success")

  
  tccRun$tccRunValue <- input$TCC
  updateNavbarPage(session, "tabs", "redirectres")

})
resultTable <- reactive({
  var$result
})



# result table render

output$mainResultTable <- renderUI({
  if(tccRun$tccRunValue){
  tagList(
  fluidRow(column(
    12, DT::dataTableOutput('fullresultTable') %>% withSpinner()
  )))} else {
    helpText("Click [Run Normalization] to obtain Result Table.")
  }
})

