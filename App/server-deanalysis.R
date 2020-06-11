# server-normalization.R


AnalysisRun <- reactiveValues(AnalysisRunValue = FALSE) # to precise the run button has not been clicked



output$CondDEAParams <- renderUI({  # if a count data table has been uploaded then it shows the parameters
  if (v$importActionValue){
    uiOutput("DEAParams")
  }else{                            # if not, error message to do it 
    sendSweetAlert(
      session = session,
      title = "ERROR",
      text = "You must upload a count data fist.",
      type = "info"
    )
    helpText("Please upload a count data first.")
  }
})


output$DEAParams <- renderUI({     # set of paramters 
  tagList(
    selectInput(
      "normMethod",
      "Normalization Method",
      c("TMM" = "tmm",
        "DESeq2" = "deseq2")
    ),
    selectInput(
      "testMethod",
      "DEG Identification Method",
      c(
        "edgeR" = "edger",
        "DESeq2" = "deseq2",
        "baySeq" = "bayseq"
      )),
    numericInput(
      inputId = "fdr",
      label = "FDR Cut-off",
      min = 0.00001,
      value = 0.01,
      max = 1,
      step = 0.0001
    ),
    sliderInput(
      "floorpdeg",
      "Elimination of Potential DEGs",
      min = 0,
      max = 1,
      value = 0.05,
      step = 0.05
    ),
    do.call(actionBttn, c(           # run button 
      list(
        inputId = "DEA",
        label = "Run Analysis",
        icon = icon("play")
      )))
  )
})

observeEvent(input$DEA, {           # when the run button is clicked 
  progressSweetAlert(               # progress bar 
    session = session,
    id = "DEAnalysisProgress",
    title = "Work in progress",
    display_pct = TRUE,
    value = 0
  )
  
  # Creation of a TCC Object 
  tcc <-                           
    new("TCC", var$CountData, var$selectedgroups)
  var$tccObject <- tcc             # save the object

  
  updateProgressBar(               # updating progress bar
    session = session,
    id = "DEAnalysisProgress",
    title = "DE Analysis in progress...",
    value = 50
  )
  tcc <- calcNormFactors(         # first calculation of the normalization and estimation of DEGs
    tcc,
    norm.method = input$normMethod,
    test.method = input$testMethod,
    FDR = input$fdr,
    floorPDEG = input$floorpdeg,
    iteration = 3                # iteration value set to 3 
  )
  
  updateProgressBar(             # updating progress bar 
    session = session,
    id = "DEAnalysisProgress",
    title = "DE Analysis in progress...",
    value = 75
  )
  tcc <- estimateDE(tcc,        # final estimation of the DEGs 
                    test.method = input$testMethod,
                    FDR = input$fdr)
  
  
  var$tccObject <- tcc         # save the updated object 
  var$result <- getResult(tcc, sort = FALSE) %>% mutate_if(is.factor, as.character) # get the result of the calculation
  
  if (length(var$groupList) == 2){
    var$result_m <- var$result
    colnames(var$result_m) <- c("gene_id","BaseMean", "Log2FC","P Value", "FDR", "Rank", "estimatedDEG")
    var$result_e <- var$result[which(var$result_m$estimatedDEG >0),] # selection of the DEGs
    var$result_s <- var$result_e[,-7]      # deleting the column showing which one is a DEG and which one is not
  }
  else{
    var$result_a <- var$result[,-2]        # deleting the a value (Basemean) of the results
    var$result_m <- var$result_a[,-2]      # deleting the m value (Log2FC) of the results
    colnames(var$result_m) <- c("gene_id", "P Value", "FDR", "Rank", "estimatedDEG")
    var$result_e <- var$result_m[which(var$result_m$estimatedDEG >0),] # selection of the DEGs
    var$result_s <- var$result_e[,-5]      # deleting the column showing which one is a DEG and which one is not
  }
  



  var$norData <- tcc$getNormalizedData() # only the normalized data
  
  
  output$normresultTable <- DT::renderDataTable({  # normaliszed data table
    data <- var$norData
    DT::datatable(
      data,        
      extensions = 'Buttons',                      # download button 
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
        searchHighlight = TRUE,                  # search bar 
        orderClasses = TRUE
        
      ),
      
      class = "display")
  }, server = FALSE)
  
  output$fullresultTable <- DT::renderDataTable({   # full results table where genes under the cut off are colored in red
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
      
      class = "display",
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
  
  output$sortedresultTable <- DT::renderDataTable({            # only DEGs table 
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
  
  closeSweetAlert(session = session)       # close alert precising the calculation od done
  sendSweetAlert(session = session,
                 title = "DONE",
                 text = "DE Analysis was successfully performed.",
                 type = "success")
  
  
  AnalysisRun$AnalysisRunValue <- input$DEA    # precise the run button has been clicked 
  updateNavbarPage(session, "tabs", "redirectres") # redirection to the full result table
  
})
resultTable <- reactive({   # saving the updated results to plot furtherly 
  var$result
})



# results tables render


output$NormResultTable <- renderUI({
  if(AnalysisRun$AnalysisRunValue){     # if the calculation is done then show the tables 
    tagList(
      fluidRow(column(
        12, DT::dataTableOutput('normresultTable') %>% withSpinner()
      )))} else {                       # if not, message to do it 
        helpText("Run Normalization to obtain Result Table.")
      }
})


output$mainResultTable <- renderUI({
  if(AnalysisRun$AnalysisRunValue){    # if the calculation is done then show the tables 
    tagList(
      fluidRow(column(
        12, DT::dataTableOutput('fullresultTable') %>% withSpinner()
      )))} else {                       # if not, message to do it 
        helpText("Run Normalization to obtain Result Table.")
      }
})


output$mainsortedResultTable <- renderUI({
  if(AnalysisRun$AnalysisRunValue){    # if the calculation is done then show the tables 
    tagList(
      fluidRow(column(
        12, DT::dataTableOutput('sortedresultTable') %>% withSpinner()
      )))} else {                      # if not, message to do it 
        helpText("Run Normalization to obtain Result Table.")
      }
})

