# server-normalization.R



### The normalization is made using TCC. 
### TCC is a package for comparing tag count data 
### with robust normalization strategies
#https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-14-219
#https://bioconductor.org/packages/release/bioc/vignettes/TCC/inst/doc/TCC.pdf


# If the run normalization botton has been clicked, execute normalization
tccRun <- reactiveValues(tccRunValue = FALSE)

observeEvent(input$TCC, {
  progressSweetAlert(
    session = session,
    id = "DEAnalysisProgress",
    title = "Work in progress",
    display_pct = TRUE,
    value = 0
  )

  data <- variables$CountData
  data.list <- variables$groupListConvert
  

  tcc <- variables$tccObject
  
  updateProgressBar(
    session = session,
    id = "DEAnalysisProgress",
    title = "DE Analysis in progress...",
    value = 50
  )
  # Run Normalization and calculate normalized factor
  tcc <- calcNormFactors(
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
    value = 70
  )
  # Estimate DEGs
  tcc <- estimateDE(tcc,
                    test.method = input$testMethod,
                    iteration = 3,
                    FDR = input$fdr)
  variables$tccObject <- tcc
  

  # Get final result of analysis
  variables$result <- getResult(tcc, sort = FALSE) %>% mutate_if(is.factor, as.character)
  variables$norData <- tcc$getNormalizedData()

  

  # Render Noramalization result table on the right top 
  output$resultTable <- DT::renderDataTable({
    if (nrow(variables$result) == 0) {
      DT::datatable(variables$result)
    } else {
      DT::datatable(
        variables$result,
        filter = "bottom",
        colnames = c("Gene Name",
                     "A Value",
                     "M Value",
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
        columns = c("a.value",
                    "m.value",
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
  

  
  # Download Noramlization Result Table function 
  output$downLoadResultTable <- downloadHandler(
    filename = function() {
      paste(
        Sys.Date(),
        input$normMethod,
        input$testMethod,
        input$fdr,
        input$floorpdeg,
        "result_table_analysis.csv",
        sep = "_"
      )
    },
    content = function(file) {
      write.csv(resultTable(), file, row.names = FALSE)
    }
  )
  
  # Download Normalized Table function
  output$downLoadNormalized <- downloadHandler(
    filename = function() {
      paste(
        Sys.Date(),
        input$normMethod,
        input$testMethod,
        input$fdr,
        input$floorpdeg,
        "normalized_data_analysis.csv",
        sep = "_"
      )
    },
    content = function(file) {
      write.csv(variables$norData, file)
    }
  )
  closeSweetAlert(session = session)
  sendSweetAlert(session = session,
                 title = "DONE",
                 text = "DE Analysis was successfully performed. Check the results.",
                 type = "success")
  
  tccRun$tccRunValue <- input$TCC
})

resultTable <- reactive({
  variables$result
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

