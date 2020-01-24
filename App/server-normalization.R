# server-normalization.R

# If the run normalization botton has been clicked, execute normalization ---------

tccRun <- reactiveValues(tccRunValue = FALSE)

observeEvent(input$TCC, {
  progressSweetAlert(
    session = session,
    id = "tccCalculationProgress",
    title = "Work in progress",
    display_pct = TRUE,
    value = 0
  )

  data <- variables$CountData
  data.list <- variables$groupListConvert
  

  tcc <- variables$tccObject
  
  updateProgressBar(
    session = session,
    id = "tccCalculationProgress",
    title = "Normalization in progress...",
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
    id = "tccCalculationProgress",
    title = "Normalization in progress...",
    value = 70
  )
  # Estimate DEGs
  tcc <- estimateDE(tcc,
                    test.method = input$testMethod,
                    iteration = 1,
                    FDR = input$fdr)
  variables$tccObject <- tcc
  

  # Get final result of TCC
  variables$result <- getResult(tcc, sort = FALSE) %>% mutate_if(is.factor, as.character)
  

  variables$norData <- tcc$getNormalizedData()

  # Show computation time notification
  runtime <- round(tcc$DEGES$execution.time[3], 2)
  

  # Render Noramalization result table on the right top ----
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
        ),
        extensions = c("Scroller", "Buttons"),
        option = list(
          dom = 'Bfrtip',
          buttons =
            list(
              'copy',
              'print',
              list(
                extend = 'collection',
                buttons = c('csv', 'excel', 'pdf'),
                text = 'Download'
              )
            ),
          deferRender = TRUE,
          scrollY = 400,
          scroller = TRUE,
          searchHighlight = TRUE,
          orderClasses = TRUE,
          columnDefs = list(
            list(visible = FALSE, targets = -1)
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
  
  
  # Render a table of norm.factors and lib.sizes ----
  output$tccSummation <- DT::renderDataTable({
    df <-
      data.frame(
        tcc$group,
        tcc$norm.factors,
        colSums(tcc$count),
        tcc$norm.factors * colSums(tcc$count)
      )
    colnames(df) <-
      c(
        "Group",
        "Normalization Factor",
        "Sum of Raw Count",
        "Effective Library Size<sup>*2</sup>"
      )
    DT::datatable(df,
                  escape = FALSE,
                  extensions = "Buttons",
                  caption = tags$caption(
                    tags$li(
                      "Effective Library Size",
                      tags$sup("*2"),
                      " = Sum of Raw Count × Normalization Factor."
                    )
                  ), 
                  option = list(dom = "Bt",
                                buttons = list(
                                  'copy',
                                  'print',
                                  list(
                                    extend = 'collection',
                                    buttons = c('csv', 'excel', 'pdf'),
                                    text = 'Download'
                                  )
                                ))) %>% formatRound(
                                  columns = c(
                                    "Normalization Factor",
                                    "Sum of Raw Count",
                                    "Effective Library Size<sup>*2</sup>"
                                  ),
                                  digits = c(3, 0, 0)
                                ) %>% formatStyle(
                                  "Normalization Factor",
                                  background = styleColorBar(range(0, df[, "Normalization Factor"]), 'lightblue'),
                                  backgroundSize = '98% 88%',
                                  backgroundRepeat = 'no-repeat',
                                  backgroundPosition = 'center'
                                ) %>% formatStyle(
                                  "Sum of Raw Count",
                                  background = styleColorBar(range(0, df[, "Sum of Raw Count"]), 'lightblue'),
                                  backgroundSize = '98% 88%',
                                  backgroundRepeat = 'no-repeat',
                                  backgroundPosition = 'center'
                                ) %>% formatStyle(
                                  "Effective Library Size<sup>*2</sup>",
                                  background = styleColorBar(range(0, df[, "Effective Library Size<sup>*2</sup>"]), 'lightblue'),
                                  backgroundSize = '98% 88%',
                                  backgroundRepeat = 'no-repeat',
                                  backgroundPosition = 'center'
                                )
  })
  
  
  # Download Noramlization Result Table function ----
  output$downLoadResultTable <- downloadHandler(
    filename = function() {
      paste(
        Sys.Date(),
        input$normMethod,
        input$testMethod,
        input$fdr,
        input$floorpdeg,
        "result_table_normalization.csv",
        sep = "_"
      )
    },
    content = function(file) {
      write.csv(resultTable(), file, row.names = FALSE)
    }
  )
  
  # Download Normalized Table function ----
  output$downLoadNormalized <- downloadHandler(
    filename = function() {
      paste(
        Sys.Date(),
        input$normMethod,
        input$testMethod,
        input$fdr,
        input$floorpdeg,
        "normalized_data.csv",
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
                 text = "Normalization was successfully performed. Check the results.",
                 type = "success")
  
  tccRun$tccRunValue <- input$TCC
})

resultTable <- reactive({
  variables$result
})


# This function render a series UI of Result table. ----

output$mainResultTable <- renderUI({
  if(tccRun$tccRunValue){
  tagList(fluidRow(column(
    12,
    downloadButton("downLoadResultTable", "Download All Result (CSV)"),
    downloadButton("downLoadNormalized", "Download Normalized Data (CSV)")
  )),
  tags$br(),
  fluidRow(column(
    12, DT::dataTableOutput('resultTable') %>% withSpinner()
  )))} else {
    helpText("Click [Run Normalization] to obtain Result Table.")
  }
})

# Render noramalization summary table ----
output$tccSummationUI <- renderUI({
  if(tccRun$tccRunValue){
  tagList(
    DT::dataTableOutput("tccSummation")
  )} else {
    helpText("Summary of normalization will be shown after normalization.")
  }
})
