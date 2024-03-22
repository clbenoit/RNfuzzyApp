# app/view/dea.R

box::use(
  shiny[h3, moduleServer, NS, tagList, column, icon, uiOutput, 
        navbarPage, tabPanel, reactiveValues, renderUI, 
        observeEvent, reactive, fluidRow, selectInput, conditionalPanel, 
        ],
  shinydashboard[box],
  shinyWidgets[actionBttn, progressSweetAlert, updateProgressBar],
  methods[new], 
  edgeR[calcNormFactors],
  DESeq2[DESeqDataSetFromMatrix]
)

## Import shiny modules
box::use(
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(
      3,
      box(                   # parameter box
        title = tagList(icon("cogs"), "Parameters"),
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        uiOutput(ns("DEAParams"))
      )),
      
      #result table 
      column(
        9,
        navbarPage("Results",             # result panels
                   id = ns("tabs"),           # id to redirect
                   tabPanel(              # Rmd informations panel
                     title = tagList(icon("question"), "TCC info"),
                     width = NULL,
                     solidHeader = T,
                     status = "primary"#,
                     #includeMarkdown("documents/tccinfo.Rmd")
                   ),
                   tabPanel(             # Normalized data table
                     title = tagList(icon("table"), "Normalization Table"),
                     width = NULL,
                     solidHeader = TRUE,
                     status = "primary",
                     uiOutput("NormResultTable")
                   ),
                   tabPanel(            # all results table
                     title = tagList(icon("table"), "Result Table"),
                     value = 'redirectres', # redirection to this table when the calculation is done
                     width = NULL,
                     solidHeader = TRUE,
                     status = "primary",
                     uiOutput("mainResultTable")
                   ),
                   tabPanel(           # only DEG results table
                     title = tagList(icon("table"), "DEG Table"),
                     width = NULL,
                     solidHeader = TRUE,
                     status = "primary",
                     uiOutput(ns("mainsortedResultTable"))
                   )
        )))
  )
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    #AnalysisRun <- reactiveValues(AnalysisRunValue = FALSE)# to precise the run button has not been clicked
    ns <- session$ns

    output$CondDEAParams <- renderUI({  # if a count data table has been uploaded then it shows the parameters
      if (v$importActionValue){
        uiOutput(ns("DEAParams"))
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
    
    
    output$DEAParams <- renderUI({ 
      tagList(
        selectInput(
          ns("DEAmethod"), "Analysis Method",
          c( TCC = "tcc",
             DESeq2 = "DESeq2",
             edgeR = "edgeR")),
        conditionalPanel(
          condition = "input.DEAmethod == 'tcc'",
          uiOutput(ns("TCCParams"))
        ),
        conditionalPanel(
          condition = "input.DEAmethod == 'DESeq2'",
          uiOutput(ns("DESeq2Params"))
        ),
        conditionalPanel(
          condition = "input.DEAmethod == 'edgeR'",
          uiOutput(ns("edgeRParams"))
        ), 
        do.call(actionBttn, c(           # run button 
          list(
            inputId = ns("DEA"),
            label = "Run Analysis",
            icon = icon("play")
          ))))
    })
    
    output$TCCParams <- renderUI({# set of paramters for tcc method
      tagList(
        selectInput(
          ns("normMethod"),
          "Normalization Method",
          c("TMM" = "tmm",
            "DESeq2" = "deseq2")
        ),
        selectInput(
          ns("testMethod"),
          "DEG Identification Method",
          c(
            "edgeR" = "edger",
            "DESeq2" = "deseq2",
            "baySeq" = "bayseq"
          )),
        numericInput(
          inputId = ns("fdr"),
          label = "FDR Cut-off",
          min = 0,
          value = 0.01,
          max = 1,
          step = 0.0001
        ),
        sliderInput(
          ns("floorpdeg"),
          "Elimination of Potential DEGs",
          min = 0,
          max = 1,
          value = 0.05,
          step = 0.01
        )
      )
    })
    
    output$DESeq2Params <- renderUI({ # set of paramters for deseq2 method 
      numericInput(
        ns("deseq2cutoff"),
        "FDR Cut-Off",
        min = 0,
        max = 1,
        value = 0.01
      )
    })
    
    output$edgeRParams <- renderUI({ # set of paramters for edgeR method 
      tagList(
        selectInput(
          ns("edgeRMethod"),
          "Normalization Method",
          c("TMM" = "TMM",
            "RLE" = "RLE",
            "upperquartile" = "upperquartile",
            "none" = "none")
        ),
        numericInput(
          inputId = ns("edgeRfdr"),
          label = "FDR Cut-off",
          min = 0,
          value = 0.001,
          max = 1,
          step = 0.0001
        ))
    })
    observeEvent(input$DEA, {           # when the run button is clicked 
      progressSweetAlert(               # progress bar 
        session = session,
        id = ns("DEAnalysisProgress"),
        title = "Work in progress",
        display_pct = TRUE,
        value = 0
      )
      
      data$var$newData <- data$var$CountData
      
      if(input$DEAmethod == "tcc"){
        
        # Creation of a TCC Object 
        tcc <-                           
          new("TCC", data$var$newData, data$var$selectedgroups)
        data$var$tccObject <- tcc             # save the object
        
        
        updateProgressBar(               # updating progress bar
          session = session,
          id = ns("DEAnalysisProgress"),
          title = "DE Analysis in progress...",
          value = 50
        )
        print("headtcc")
        print(utils::head(tcc))
        tcc <- calcNormFactors(         # first calculation of the normalization and estimation of DEGs
          tcc,
          norm.method = input$normMethod,
          test.method = input$testMethod,
          FDR = input$fdr,
          floorPDEG = input$floorpdeg,
          iteration = 3                # iteration value set to 3 
        )
        print("aaaaaaaa")
        
        updateProgressBar(             # updating progress bar 
          session = session,
          id = ns("DEAnalysisProgress"),
          title = "DE Analysis in progress...",
          value = 75
        )
        tcc <- estimateDE(tcc,        # final estimation of the DEGs 
                          test.method = input$testMethod,
                          FDR = input$fdr)
        
        
        data$var$tccObject <- tcc         # save the updated object 
        data$var$result <- getResult(tcc, sort = FALSE) %>% mutate_if(is.factor, as.character) # get the result of the calculation
        
        if (length(data$var$groupList2) == 2){
          data$var$result_m <- data$var$result
          colnames(data$var$result_m) <- c("gene_id","BaseMean", "Log2FC","P-Value", "FDR", "Rank", "estimatedDEG")
          data$var$result_e <- data$var$result[which(data$var$result_m$estimatedDEG >0),] # selection of the DEGs
          data$var$result_s <- data$var$result_e[,-7]      # deleting the column showing which one is a DEG and which one is not
        }
        else{
          data$var$result_a <- data$var$result[,-2]        # deleting the a value (Basemean) of the results
          data$var$result_m <- data$var$result_a[,-2]      # deleting the m value (Log2FC) of the results
          colnames(data$var$result_m) <- c("gene_id", "P-Value", "FDR", "Rank", "estimatedDEG")
          data$var$result_e <- data$var$result_m[which(data$var$result_m$estimatedDEG >0),] # selection of the DEGs
          data$var$result_s <- data$var$result_e[,-5]      # deleting the column showing which one is a DEG and which one is not
        }
        
        data$var$norData <- tcc$getNormalizedData() # only the normalized data
        data$var$norDT <- data$var$norData
        data$var$genelist <- data$var$result_s[,1]
        data$var$DEAMETHOD <- 'tcc'
      }
      
      
      ######################################### deseq2 method #################################################
      
      if(input$DEAmethod == "DESeq2"){
        
        tcc <-                           
          new("TCC", data$var$newData, data$var$selectedgroups)
        data$var$tccObject <- tcc             # just to get the groups for pca 
        dds <- DESeqDataSetFromMatrix(countData=data$var$newData, colData=data$var$select, design=data$var$design)
        
        updateProgressBar(               # updating progress bar
          session = session,
          id = "DEAnalysisProgress",
          title = "DE Analysis in progress...",
          value = 25
        )
        
        dds <- DESeq(dds)
        
        updateProgressBar(               # updating progress bar
          session = session,
          id = "DEAnalysisProgress",
          title = "DE Analysis in progress...",
          value = 50
        )
        data$var$resultz <- results(dds)
        data$var$norData <- as.matrix(counts(dds, normalized = TRUE)) # normalization
        data$var$norDT <- data$var$norData
        data$var$resultz <- as.matrix(data$var$resultz)
        data$var$result <- data.frame(row.names(data$var$resultz))
        data$var$result['a.value'] <- data$var$resultz[,1]
        data$var$result['m.value'] <- data$var$resultz[,2]
        data$var$result['p.value'] <- data$var$resultz[,6]
        data$var$result['q.value'] <- p.adjust(data$var$resultz[,6], method = 'fdr')
        names(data$var$result)[1] <- "gene_id"
        
        
        
        if (length(data$var$groupList2) != 2){
          data$var$result <- data$var$result[,-2] # suppr basemean
          data$var$result <- data$var$result[,-2] # supp log2fc
        }
        
        data$var$DESeq2DEGs <- data$var$result[which(data$var$result$q.value <= as.numeric(input$deseq2cutoff)),] 
        data$var$result["estimatedDEG"] = "0"
        data$var$result <- data$var$result[complete.cases(data$var$result), ]
        for (row in 1:nrow(data$var$result)){
          if(data$var$result[row,'q.value'] <= as.numeric(input$deseq2cutoff)){
            data$var$result[row, 'estimatedDEG'] = "1"
          }else{ 
            data$var$result[row,'estimatedDEG'] = "0"
          }
          
        }
        
        data$var$genelist <- data$var$DESeq2DEGs[,1]
        data$var$DEAMETHOD <- 'deseq2'
      }
      
      ################################################ edgeR method ###############################################
      
      
      if(input$DEAmethod == "edgeR"){     # formatting for edgeR'''
        tcc <-                           
          new("TCC", data$var$newData, data$var$selectedgroups)
        data$var$tccObject <- tcc             # just to get the groups for pca 
        
        dgList <- DGEList(counts=data$var$newData, group = data$var$selectedgroups)
        
        updateProgressBar(               # updating progress bar
          session = session,
          id = "DEAnalysisProgress",
          title = "DE Analysis in progress...",
          value = 25
        )
        
        dgList <- calcNormFactors(dgList, method=input$edgeRMethod)
        
        updateProgressBar(               # updating progress bar
          session = session,
          id = "DEAnalysisProgress",
          title = "DE Analysis in progress...",
          value = 50
        )
        
        dgList <- estimateGLMCommonDisp(dgList,
                                        method = "deviance", robust = TRUE,
                                        subset = NULL)
        design <- model.matrix(~data$var$selectedgroups)
        fit <- glmFit(dgList, design)
        lrt <- glmLRT(fit) 
        data$var$result <- data.frame(row.names(lrt$table))
        data$var$result['a.value'] <- lrt$table$logCPM
        data$var$result['m.value'] <- lrt$table$logFC
        data$var$result['p.value'] <- lrt$table$PValue
        data$var$result['q.value'] <- p.adjust(data$var$result$p.value, method = 'fdr')
        names(data$var$result)[1] <- "gene_id"
        
        
        if (length(data$var$groupList2) != 2){
          data$var$result <- data$var$result[,-2] # suppr log2fc
          data$var$result <- data$var$result[,-2] # supp basemean
        }
        data$var$norData <- lrt$fitted.values
        data$var$norDT <- data$var$norData
        data$var$result["estimatedDEG"] = "0"
        for (row in 1:nrow(data$var$result)){
          if(data$var$result[row,'q.value'] <= as.numeric(input$edgeRfdr)){
            data$var$result[row, 'estimatedDEG'] = "1"
          }else{ 
            data$var$result[row,'estimatedDEG'] = "0"
          }}
        data$var$edgeRDEGs <- data$var$result[which(data$var$result$q.value <= as.numeric(input$edgeRfdr)),] 
        data$var$edgeRDEGs <- data$var$edgeRDEGs[,-4]
        data$var$genelist <- data$var$edgeRDEGs[,1]
        data$var$DEAMETHOD <- 'edgeR'
        
      }
      ################################
      
      output$normresultTable <- DT::renderDataTable({  # normaliszed data table
        data <- data$var$norData
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
                             filename = "results_norm_data"),
              text = 'Download')),
            scrollX = TRUE,
            pageLength = 10,
            searchHighlight = TRUE,                  # search bar 
            orderClasses = TRUE
            
          ),
          
          class = "display")
      }, server = FALSE)
      
      output$fullresultTable <- DT::renderDataTable({   # full results table where genes under the cut off are colored in red
        data <- data$var$norData
        
        if(input$DEAmethod =="tcc"){
          gene_id <- row.names(data)
          data <- cbind(data, gene_id = gene_id)
          resultTable <- merge(data$var$result_m, data, by = "gene_id")
        }else{
          data <- as.data.frame(data)
          data['gene_id'] <- row.names(data)
          resultTable <- merge(data$var$result, data, by = "gene_id")
        }
        
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
                             filename = "results_dea"),
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
        data <- data$var$norData
        if(input$DEAmethod == 'tcc'){
          gene_id <- row.names(data)
          data <- cbind(data, gene_id = gene_id)
          resultTable <- merge(data$var$result_s, data, by = "gene_id")
        }
        if(input$DEAmethod == "DESeq2"){
          gene_id <- row.names(data)
          data <- cbind(data, gene_id = gene_id)
          resultTable <- merge(data$var$DESeq2DEGs,data, by = "gene_id")
        }
        if(input$DEAmethod == "edgeR"){
          gene_id <- row.names(data)
          data <- cbind(data, gene_id = gene_id)
          resultTable <- merge(data$var$edgeRDEGs, data, by = "gene_id")
        }
        
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
                             filename = "results_DEGs"),
              text = 'Download')),
            scrollX = TRUE,
            pageLength = 10,
            searchHighlight = TRUE,
            orderClasses = TRUE
            
          ),
          
          class = "display")
      }, server = FALSE)
      
      closeSweetAlert(session = session)       # close alert precising the calculation is done
      sendSweetAlert(session = session,
                     title = "DONE",
                     text = "DE Analysis was successfully performed.",
                     type = "success")
      
      
      data$buttonsStates$AnalysisRun<- input$DEA  # precise the run button has been clicked 
      updateNavbarPage(session, "tabs", "redirectres") # redirection to the full result table
      
    })
    resultTable <- reactive({   # saving the updated results to plot furtherly 
      data$var$result
    })
    
    
    # results tables render
    
    output$genelist <- renderUI({
      if(AnalysisRun$AnalysisRunValue){ # if the calculation is done then show the tables 
        tagList(
          fluidRow(column(
            12, DT::dataTableOutput('genelistTable') %>% withSpinner()
          )))} else {                       # if not, message to do it 
            helpText("Run Normalization to obtain Result Table.")
          }
    })
    
    
    output$NormResultTable <- renderUI({
      if(AnalysisRun$AnalysisRunValue){ # if the calculation is done then show the tables 
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
    
 
  })
}
