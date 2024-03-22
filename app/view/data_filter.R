# app/view/data_filter.R

box::use(
  shiny[h3, moduleServer, NS, tagList, column, icon, uiOutput, 
        navbarPage, tabPanel, reactiveValues, renderUI,  helpText, 
        observeEvent, reactive, fluidPage, fluidRow, tags, 
        fileInput, HTML, sliderInput, selectizeInput, textInput, 
        updateSelectizeInput, req],
  shinydashboard[box, tabBox],
  plotly[renderPlotly, ],
  shinyWidgets[sendSweetAlert, useSweetAlert],
  DT[renderDataTable, dataTableOutput, datatable]
)

## Import shiny modules
box::use(
)

#' @export
ui <- function(id) {
  ns <- NS(id)
 tagList(
   fluidPage(useSweetAlert(), fluidPage(fluidRow(
     column(
       3,
       box(                   # parameter box
         title = tagList(icon("cogs"), "Parameters"),
         width = NULL,
         solidHeader = TRUE,
         status = "primary",
         uiOutput(ns('condFilter'))
         
       )),
     column(9,
            box(    # group assignement box
              title = tagList(icon("tags"), "Normalized Table Check"),
              solidHeader = TRUE,
              status = "primary",
              width = NULL,
              dataTableOutput(ns('filterTable'))
              
            ),
            box(    # group assignement box
              title = tagList(icon("tags"), "DEGs Table Check"),
              solidHeader = TRUE,
              status = "primary",
              width = NULL,
              dataTableOutput(ns('filterTableDEG'))
            )))))
 )
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    # FilterRun <- reactiveValues(FilterRunValue = FALSE)# to precise the run button has not been clicked
    output$condFilter<- renderUI({
      
      if (data$buttonsStates$AnalysisRun){  # if a DEA has been performed, then show parameters
        tagList(
          selectizeInput("filter_samples", label="Select Samples",
                         choices=data$var$cond_id2,
                         selected = data$var$cond_id2,
                         multiple=TRUE),
          helpText(HTML("Choose the 2 groups you want to study for a two groups comparison : MA plot and Volcano plot")),
          do.call(actionBttn,c(   # run button 
            list(
              inputId = "confirmed2groups",
              label = "Confirm groups",
              icon = icon("play")
            ))
          ),
          helpText(HTML("Reset groups to come back to the original normalized table and choose other groups")),
          do.call(actionBttn,c(   # run button 
            list(
              inputId = "resetButton",
              label = "Reset Groups",
              icon = icon("play")
            ))
          )
        )
      }else{                              # if not, error message to do it
        sendSweetAlert(
          session = session,
          title = "ERROR",
          text = "You must perform a DEA before.",
          type = "info"
        )
        helpText("Please perform a DEA first.")
      }
      
      
      
    })
    
    observeEvent(input$resetButton,{
      tagList(
        updateSelectInput(
          session = getDefaultReactiveDomain(),
          'filter_samples',
          label="Select Samples",
          choices=data$var$cond_id2,
          selected = data$var$cond_id2),
        helpText(HTML("Choose the 2 groups you want to study for a two groups comparison : MA plot and Volcano plot"))
      )
      data$var$norData <- data$var$norDT
      data$var$newData <- data$var$CountData
      
    })
    
    observeEvent(input$filter_samples,{
      selectedsamples <- input$filter_samples
      tmprem = match(as.character(data$var$sampleid2[which(!(data$var$cond_id2%in%selectedsamples))]),colnames(data$var$norData))
      tmpkeep = setdiff(1:ncol(data$var$norData),tmprem)
      data$var$norData = data$var$norData[,tmpkeep]
      tmprem2 = match(as.character(data$var$sampleid2[which(!(data$var$cond_id2%in%selectedsamples))]),colnames(data$var$newData))
      tmpkeep2 = setdiff(1:ncol(data$var$newData),tmprem2)
      data$var$newData = data$var$newData[,tmpkeep2]
      data$var$sampleid3 <- colnames(data$var$newData)
      data$var$actualgroups2 <- do.call(rbind,strsplit(data$var$sampleid3,"_",fixed=TRUE))
      data$var$actualgroups2 <- data$var$actualgroups2[,c(2,1)]
      data$var$group3 <- data$var$actualgroups2[,1]
      data$var$cond_id3 <- data$var$actualgroups2[,2]
      group3 <- as.data.frame(data$var$actualgroups2) 
      group3$V1 <- data$var$sampleid3
      data$var$select2 <- as.data.frame(group3$V2, row.names =  colnames(data$var$norData))
      colnames(data$var$select2) <- "group"
      data$var$groupList3 <-  # set the groups
        lapply(unique(group3$V2), function(x) {
          group3[group3$V2 == x, ]$V1
        })
      
      names(data$var$groupList3) <- unique(group3$V2)
      data.list3 <- rep(0, ncol(data$var$norData))
      convertion <- function(x, df) {
        grep(x, colnames(df))
      }
      for (i in 1:length(data$var$groupList3)) { # assign replicates to groups
        data.list3[unlist(lapply(data$var$groupList3[[i]], convertion, df = data$var$norData))] = names(data$var$groupList3[i])
      }
      data$var$selectedgroups <- data.list3
      
      data$var$sampleid4 <- colnames(data$var$newData)
      data$var$actualgroups3 <- do.call(rbind,strsplit(data$var$sampleid4,"_",fixed=TRUE))
      data$var$actualgroups3 <- data$var$actualgroups3[,c(2,1)]
      data$var$groupd2 <- as.data.frame(data$var$actualgroups3)
      group <- data$var$cond_id3
      data$var$select3 <- as.data.frame(group3$V2, row.names =  colnames(data$var$newData))
      colnames(data$var$select3) <-"group"
      data$var$design2 <- formula(as.formula(paste("~", paste(colnames(as.data.frame(group)), collapse = "+"))))
    })
    
    
    observeEvent(input$confirmed2groups, {
      progressSweetAlert(               # progress bar 
        session = session,
        id = "newDEAprogress",
        title = "Work in progress",
        display_pct = TRUE,
        value = 0
      )
      
      ######################################################################
      
      
      if(length(data$var$groupList3) == 2){
        
        if(input$DEAmethod == "tcc"){
          # Creation of a TCC Object 
          tcc <-                           
            new("TCC", data$var$newData, data$var$selectedgroups)
          data$var$tccObject <- tcc             # save the object
          
          
          updateProgressBar(               # updating progress bar
            session = session,
            id = "newDEAprogress",
            title = "Work in progress...",
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
            id = "newDEAprogress",
            title = "Work in progress...",
            value = 75
          )
          tcc <- estimateDE(tcc,        # final estimation of the DEGs 
                            test.method = input$testMethod,
                            FDR = input$fdr)
          
          
          data$var$tccObject <- tcc         # save the updated object 
          data$var$result <- getResult(tcc, sort = FALSE) %>% mutate_if(is.factor, as.character) # get the result of the calculation
          
          data$var$result_m <- data$var$result
          colnames(data$var$result_m) <- c("gene_id","BaseMean", "Log2FC","P-Value", "FDR", "Rank", "estimatedDEG")
          data$var$result_e <- data$var$result[which(data$var$result_m$estimatedDEG >0),] # selection of the DEGs
          data$var$result_s <- data$var$result_e[,-7]      # deleting the column showing which one is a DEG and which one is not
          
          data$var$norData <- tcc$getNormalizedData() # only the normalized data
          data$var$filter_genelist <- data$var$result_s[,1]
          data$var$DEAMETHOD <- 'tcc'
        }
        
        
        ######################################### deseq2 method #################################################
        
        
        if(input$DEAmethod == "DESeq2"){
          
          tcc <-                           
            new("TCC", data$var$newData, data$var$selectedgroups)
          data$var$tccObject <- tcc             # just to get the groups for pca 
          
          dds <- DESeqDataSetFromMatrix(countData=data$var$newData, colData=data$var$select3, design=data$var$design2)
          
          updateProgressBar(               # updating progress bar
            session = session,
            id = "newDEAprogress",
            title = "Work in progress...",
            value = 25
          )
          
          dds <- DESeq(dds)
          
          updateProgressBar(               # updating progress bar
            session = session,
            id = "newDEAprogress",
            title = "Work in progress...",
            value = 50
          )
          data$var$resultz <- results(dds)
          data$var$norData <- as.matrix(counts(dds, normalized = TRUE)) # normalization
          data$var$resultz <- as.matrix(data$var$resultz)
          data$var$result <- data.frame(data$var$resultz[,1], row.names = rownames(data$var$resultz))
          data$var$result['m.value'] <- data$var$resultz[,2]
          data$var$result['p.value'] <- data$var$resultz[,6]
          data$var$result['q.value'] <- p.adjust(data$var$resultz[,6], method = 'fdr')
          names(data$var$result)[1] <- "a.value"
          data$var$DESeq2DEGs <- data$var$result[which(data$var$result$q.value <= as.numeric(input$deseq2cutoff)),] 
          data$var$result["estimatedDEG"] = "0"
          data$var$result <- data$var$result[complete.cases(data$var$result), ]
          data$var$filter_genelist <- rownames(data$var$DESeq2DEGs)
          
          for (row in 1:nrow(data$var$result)){
            if(data$var$result[row,'q.value'] <= as.numeric(input$deseq2cutoff)){
              data$var$result[row, 'estimatedDEG'] = "1"
            }else{ 
              data$var$result[row,'estimatedDEG'] = "0"
            }
          }
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
            id = "newDEAprogress",
            title = "Work in progress...",
            value = 25
          )
          
          dgList <- calcNormFactors(dgList, method=input$edgeRMethod)
          
          updateProgressBar(               # updating progress bar
            session = session,
            id = "newDEAprogress",
            title = "Work in progress...",
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
          data$var$norData <- lrt$fitted.values
          data$var$result["estimatedDEG"] = "0"
          for (row in 1:nrow(data$var$result)){
            if(data$var$result[row,'q.value'] <= as.numeric(input$edgeRfdr)){
              data$var$result[row, 'estimatedDEG'] = "1"
            }else{ 
              data$var$result[row,'estimatedDEG'] = "0"
            }}
          data$var$edgeRDEGs <- data$var$result[which(data$var$result$q.value <= as.numeric(input$edgeRfdr)),] 
          data$var$edgeRDEGs <- data$var$edgeRDEGs[,-4]
          data$var$DEAMETHOD <- 'edgeR'
          data$var$filter_genelist <- data$var$edgeRDEGs[,1]
        }
        
        closeSweetAlert(session = session)       # close alert precising the calculation is done
        sendSweetAlert(session = session,
                       title = "DONE",
                       text = "Work was successfully performed.",
                       type = "success")
      }else{
        sendSweetAlert(
          session = session,
          title = "ERROR",
          text = "You must choose 2 groups only.",
          type = "info"
        )
        helpText("You must choose 2 groups only.")
      }
      
      data$buttonsStates$FilterRun <- input$confirmed2groups  # precise the run button has been clicked 
    })
    
    resultTable <- reactive({   # saving the updated results to plot furtherly 
      data$var$result
    })
    
    output$filterTable <-  renderDataTable({ 
      req(data$var$norData)
      data <- data$var$norData
      
      if(input$DEAmethod =="tcc"){
        gene_id <- row.names(data)
        data <- cbind(data, gene_id = gene_id)
        resultTable <- merge(data$var$result_m, data, by = "gene_id")
      }
      if(input$DEAmethod == "edgeR"){
        data <- as.data.frame(data)
        data['gene_id'] <- row.names(data)
        resultTable <- merge(data$var$result, data, by = "gene_id")
      }
      if(input$DEAmethod == "DESeq2"){
        data <- as.data.frame(data)
        resultTable <- merge(data$var$result, data, by="row.names")
        names(resultTable)[1] <-'gene_id'
      }
      
      datatable(
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
    
    
    
    
    output$filterTableDEG <- renderDataTable({ 
      req(data$var$norData)
      data <- data$var$norData
      
      if(input$DEAmethod == 'tcc'){
        gene_id <- row.names(data)
        data <- cbind(data, gene_id = gene_id)
        resultTable <- merge(data$var$result_s, data, by = "gene_id")
      }
      if(input$DEAmethod == "DESeq2"){
        resultTable <- merge(data$var$DESeq2DEGs, data, by = "row.names")
        names(resultTable[1]) <- 'gene_id'
      }
      if(input$DEAmethod == "edgeR"){
        gene_id <- row.names(data)
        data <- cbind(data, gene_id = gene_id)
        resultTable <- merge(data$var$edgeRDEGs, data, by = "gene_id")
      }
      
      datatable(
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
    

  })
}
