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
  if (input$filterLowCount != "Do not filter") {
    tcc$count <- tcc$count[rowSums(tcc$count) > as.numeric(input$filterLowCount), ]
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
  
  # Filtered number preview ----
  output$lowCountFilterText <- renderText({
    if (length(var$tccObject) > 0) {
      # tcc <- var$tccObject
      data <- var$count.data
      if (input$filterLowCount != "Do not filter") {
        count <- data[rowSums(data) > as.numeric(input$filterLowCount), ]
      } else {
        count <- data
      }
    } else {
      return()
    }
  })  
  
  
#result table
  output$resultTable <- DT::renderDataTable({
    if (nrow(var$result) == 0) {
      DT::datatable(var$result)
    } else {
      DT::datatable(
        var$result,
        filter = "bottom",
        colnames = c("Gene Name",
                     "Log2FC",
                     "P Value",
                     "Q Value (FDR)",
                     "Rank",
                     "estimated DEG"),
        caption = tags$caption(
          tags$li(
            HTML("<font color=\"#B22222\"><b>Gene Name</b></font> is colored when under FDR cut-off.")
          )
        )
      ) %>% formatRound(
        columns = c("m.value",
                    "p.value",
                    "q.value"),
        digits = 3
      ) %>% formatStyle(
        "gene_id",
        "estimatedDEG",
        color = styleEqual(1, "#B22222"),
        fontWeight = styleEqual(c(0, 1), c("normal", "bold"))
      )
    }
  }, server = FALSE)
  
#download all part
  output$downLoadResultTable <- downloadHandler(
    filename = "result_table_analysis.csv",
    content = function(file) {
      write.csv(resultTable(), file, row.names = FALSE)
    }
  )
  
  # download normalized part
  output$downLoadNormalized <- downloadHandler(
    filename =  "normalized_data_analysis.csv",
    content = function(file) {
      write.csv(var$norData, file)
    })
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
  tagList(fluidRow(column(
    12,
    downloadButton("downLoadResultTable", "Download all result (CSV)"),
    downloadButton("downLoadNormalized", "Download normalized data results (CSV)")
  )),
  tags$br(),
  fluidRow(column(
    12, DT::dataTableOutput('resultTable') %>% withSpinner()
  )))} else {
    helpText("Click [Run Normalization] to obtain Result Table.")
  }
})

