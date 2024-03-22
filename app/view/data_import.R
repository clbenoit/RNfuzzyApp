# app/view/data_import.R

box::use(
  shiny[h3, moduleServer, NS, tagList, column, icon, uiOutput, 
        navbarPage, tabPanel, reactiveValues, renderUI, 
        observeEvent, reactive, fluidPage, fluidRow, tags, 
        fileInput, HTML, sliderInput, selectizeInput, 
        updateSelectizeInput],
  shinydashboard[box, tabBox],
  shinythemes[shinytheme],
  plotly[renderPlotly, ],
  shinyBS[tipify],
  shinyWidgets[sendSweetAlert],
  data.table[fread],
  stats[formula, as.formula]
  #DT[DT]
)

## Import shiny modules
box::use(
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  #fluidRow(
    navbarPage(theme=shinytheme("sandstone"),"Data Visualization", # theme for navbars of the app
    tabPanel(
      title = tagList(icon("table"), "Data"),
      fluidPage(
        fluidRow(column(4,
                        box(        # uplaod data box
                          title = tagList(icon("cloud-upload-alt"), "Upload"),
                          solidHeader = T,
                          status = "primary",
                          width = NULL,
                          fileInput(
                            ns("uploadCountData"),
                            "Upload Count Data",
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv"),
                            buttonLabel = "Upload...",
                            placeholder = "No file has been uploaded."
                          ),
                          tags$div( # instructions
                            HTML('<div class="panel panel-primary">
                    <div class="panel-heading"> <span style="padding-left:10px"><b> Input file description</b> </span></div>
                  <div class="panel-body">
                  <style type="text/css">
                  </style>
                  <table class="tg">
                  <tr>
                  <th class="tg-031e"> <span class="label label-primary"> Format</span></th>
                  <th class="tg-031e"> comma-separated values (CSV)
                  </tr>
                  <tr>
                  <th class="tg-031e"> <span class="label label-primary"> Column 1</span></th>
                  <th class="tg-031e"> Sample ID
                  </tr>
                  <tr>
                  <th class="tg-031e"> <span class="label label-primary"> Column 2-n</span></th>
                  <th class="tg-031e"> Other metadata (condition, covariates) </th>
                  </tr>
                  <tr>
                  </table>
                  </div>
                  </div>'))
                        )),

                 column(4,
                        box(     # filtering box
                          title = tagList(icon("filter"), "Filter Low Count Genes"),
                          solidHeader = TRUE,
                          status = "primary",
                          width = NULL,
                          sliderInput(
                            ns("filterCount"),
                            "Filter Low Count Genes",
                            min = 0,
                            max = 100,
                            value = 0,
                            step = 1
                          )
                        ),
                        box(    # group assignement box
                          title = tagList(icon("tags"), "Group Assignment"),
                          solidHeader = TRUE,
                          status = "primary",
                          width = NULL,
                          selectizeInput(ns("data_samples"), label="Select Samples",
                                         choices=NULL,
                                         multiple=TRUE)
                        )),
                 column(4,
                        box(           # summary of upload table box
                          title = tagList(icon("file-alt"), "Summary"),
                          solidHeader = TRUE,
                          status = "primary",
                          width = NULL,
                          uiOutput(ns("DataSummary")))



                 )),
        tabBox(                  # panels of different tables box
          title = "",
          width = NULL,
          tabPanel(              # final table of selected genes
            title = tagList(icon("chart-bar"), "Actual Table"),
            uiOutput(ns("showTable"))
          ),
          tabPanel(             # raw input table
            title = tagList(icon("chart-bar"), "Input Table"),
            uiOutput(ns("showInputTable"))
          ),
          tabPanel(             # table of filtered datz
            title = tagList(icon("chart-bar"), "Filtered Table"),
            uiOutput(ns("showLowTable"))
          )
        ))),
    tabPanel(   # panel of the count distribution bar chart
      title = tagList(icon("chart-bar"), "Count Distribution"),
      uiOutput(ns("CountDistrib"))
    ),
    tabPanel(  # panel of groups heatmap
      title = tagList(icon("sitemap"), "Hierarchical Clustering"),
      uiOutput(ns("clustUI"))
    ),
    tabPanel(   # panel of PCA
      title = tagList(icon("object-group"), "PCA"),
      uiOutput(ns("pcaUI"))
    )
 )#) 
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    
    observeEvent(input$uploadCountData, {   # when a table is being uploaded 
      tryCatch({
        data$var$InputTable <-    # assign the table as a data frame to this variable 
          data.frame(fread(input$uploadCountData$datapath), row.names = 1)
        data$var$control <- FALSE
        data$var$sampleid <- colnames(data$var$InputTable)
        data$var$groupdfs <- do.call(rbind,strsplit(data$var$sampleid,"_",fixed=TRUE))
        data$var$groupdfs <- data$var$groupdfs[,c(2,1)]
        data$var$rep <- data$var$groupdfs[,1]
        data$var$cond_id <- data$var$groupdfs[,2]
        data$var$matrixcount <- as.matrix(data$var$InputTable)
        data$var$CountData <- data$var$InputTable[,which(colnames(data$var$InputTable) == rownames(data$var$groupdfs))]
        
        
        updateSelectizeInput(session,"data_samples", 
                             choices=data$var$cond_id,selected=data$var$cond_id)
      },
      error = function(e) {  # error messages about the input table 
        sendSweetAlert(
          session = session,
          title = "Input data error!",
          text = as.character(message(e)),
          type = "error"
        )
        return()
      },
      warning = function(w) {
        sendSweetAlert(
          session = session,
          title = "Input data warning!",
          text = "Error in dataset",
          type = "warning"
        )
        return()
      })
      
      
      observeEvent(input$filterCount,{   # when a filter of low count genes is set 
        if (input$filterCount != 0) {  # automatically filter the origianl table and update the summary
          data$var$LowCountGenes <- data$var$InputTable[rowSums(data$var$InputTable >= as.numeric(input$filterCount)) == 0,]
          data$var$CountData <- data$var$InputTable[rowSums(data$var$InputTable >= as.numeric(input$filterCount)) > 0 , ]
        }else{
          data$var$CountData <- data$var$InputTable
          data$var$LowCountGenes <- "No filtered data."
        }
        
        
      })
      
      group <- as.data.frame(data$var$groupdfs) 
      group$V1 <- data$var$sampleid
      data$var$groupdf <- as.data.frame(group$V2, row.names = data$var$sample_id)
      colnames(data$var$groupdf) <- "group"
      data$var$groupList <-  # set the groups
        lapply(unique(group$V2), function(x) {
          group[group$V2 == x, ]$V1
        })
      
      names(data$var$groupList) <- unique(group$V2)
      data.list <- rep(0, ncol(data$var$CountData))
      
      
      # Convert the input of group information to a specific format for normalization.
      convertion <- function(x, df) {
        grep(x, colnames(df))
      }
      
      for (i in 1:length(data$var$groupList)) { # assign replicates to groups
        data.list[unlist(lapply(data$var$groupList[[i]], convertion, df = data$var$CountData))] = names(data$var$groupList[i])
      }
      
      observeEvent(input$data_samples,{
        if(!(input$data_samples[1]=="")) {
          data$var$selectedsamples <- input$data_samples
          tmprem = match(as.character(data$var$sampleid[which(!(data$var$cond_id%in%data$var$selectedsamples))]),colnames(data$var$CountData))
          tmpkeep = setdiff(1:ncol(data$var$CountData),tmprem)
          data$var$CountData = data$var$CountData[,tmpkeep]
          
          #################################################################
          data$var$sampleid2 <- colnames(data$var$CountData)
          data$var$actualgroups <- do.call(rbind,strsplit(data$var$sampleid2,"_",fixed=TRUE))
          data$var$actualgroups <- data$var$actualgroups[,c(2,1)]
          data$var$group2 <- data$var$actualgroups[,1]
          data$var$cond_id2 <- data$var$actualgroups[,2]
          group2 <- as.data.frame(data$var$actualgroups) 
          group2$V1 <- data$var$sampleid2
          data$var$select <- as.data.frame(group2$V2, row.names =  colnames(data$var$CountData))
          colnames(data$var$select) <- "group"
          group <- data$var$cond_id2
          data$var$design <- formula(as.formula(paste("~", paste(colnames(as.data.frame(rev(group))), collapse = "+"))))
          data$var$groupList2 <-  # set the groups
            lapply(unique(group2$V2), function(x) {
              group2[group2$V2 == x, ]$V1
            })
          
          names(data$var$groupList2) <- unique(group2$V2)
          data.list2 <- rep(0, ncol(data$var$CountData))
          convertion <- function(x, df) {
            grep(x, colnames(df))
          }
          for (i in 1:length(data$var$groupList2)) { # assign replicates to groups
            data.list2[unlist(lapply(data$var$groupList2[[i]], convertion, df = data$var$CountData))] = names(data$var$groupList2[i])
          }
          
          data$var$selectedgroups <- data.list2
          
          
          ######################################################
          
        }})
      
      data$var$control <- TRUE
      
    })
    # save the updated table and associate a name to facilitate the use 
    datasetInput <- reactive({
      data$var$CountData
    })
    
    output$DataSummary <- renderUI({  # summary render
      odf <- data$var$InputTable
      dt <- datasetInput()
      orowCount <- nrow(odf)
      if(input$filterCount == 0){ # filtered and raw genes count
        rowCount <- orowCount
        filtCount <- 0
      }else{
        rowCount <- nrow(dt)
        filtCount <- (orowCount - rowCount)
      }
      groupCount <- length(data$var$groupList2)       # groups count and setting 
      groupText <- sapply(data$var$groupList2, length)
      if (length(groupText) > 0) {
        gText <- paste0(names(groupText), ": ", groupText, ';', collapse = "\n")
      } else {
        gText <- NULL
      }
      
      tagList(     # set of info
        tipify(    # actual count
          tags$p(tags$b("N", tags$sub("genes")), ":", rowCount),
          title = "Number of Genes",
          placement = "left"
        ),
        tipify(    # raw initial count
          tags$p(tags$b("N", tags$sub(" input genes")), ":", orowCount),
          title = "Number of Input Genes",
          placement = "left"
        ),
        tipify(   # filtered genes count
          tags$p(tags$b("N", tags$sub("filtered genes")), ":", filtCount),
          title = "Number of Filtered Genes",
          placement = "left"
        ),
        tipify(  # number of groups
          tags$p(tags$b("N", tags$sub("group")), ": ", groupCount),
          title = " Number of Groups",
          placement = "left"
        ),
        tipify(   # replciated per groups
          tags$p(tags$b("N", tags$sub("replicates")), ": ", gText),
          title = " Number of Replicates",
          placement = "left"
        )
      )
    })
    
    # Render a table of raw count data
    output$table <- DT::renderDataTable({
      df <- datasetInput()
      DT::datatable(
        df,
        colnames = c("Gene Name" = 1),
        extensions = c("Scroller", "RowReorder"),
        option = list(
          rowReorder = TRUE,
          deferRender = TRUE,
          scrollY = 400,
          scroller = TRUE,
          scrollX = TRUE,
          searchHighlight = TRUE, # search bar
          orderClasses = TRUE
        )
      )
    })
    
    
    # Render DataTable of row data count
    
    output$showTable <- renderUI({
      if (nrow(datasetInput()) == 0) {  # if no uploaded table or empty, message
        tags$p("No data to show. Upload your dataset.")
      } else {    # if not, render the table
        DT::dataTableOutput(ns('table'))
      }
    })
    ###### input 
    output$inputable <- DT::renderDataTable({
      inputdf <- data$var$InputTable
      DT::datatable(
        inputdf,
        colnames = c("Gene Name" = 1),
        extensions = c("Scroller", "RowReorder"),
        option = list(
          rowReorder = TRUE,
          deferRender = TRUE,
          scrollY = 400,
          scroller = TRUE,
          scrollX = TRUE,
          searchHighlight = TRUE,
          orderClasses = TRUE
        )
      )
    })
    
    # Render DataTable of row data count
    output$showInputTable <- renderUI({
      if (nrow(datasetInput()) == 0) {# if no uploaded table or empty, message
        tags$p("No data to show. Upload your dataset.")
      } else { # if not, render the table
        DT::dataTableOutput(ns('inputable'))
      }
    })
    
    output$filtable <- DT::renderDataTable({
      low <- data$var$LowCountGenes
      DT::datatable(
        low,
        colnames = c("Gene Name" = 1),
        extensions = c("Scroller", "RowReorder"),
        option = list(
          rowReorder = TRUE, deferRender = TRUE, scrollY = 400,
          scroller = TRUE, scrollX = TRUE, searchHighlight = TRUE,
          orderClasses = TRUE
        )
      )
    })
    
    # Render DataTable of row data count
    output$showLowTable <- renderUI({ # if no uploaded table or empty, message
      if (is.data.frame(data$var$LowCountGenes) == FALSE) {
        tags$p("No Filtered data.")
      } else { # if not, render the table
        DT::dataTableOutput('filtable')
      }
    })
    
    v <- reactiveValues(importActionValue = FALSE)
    
    ################### BOXPLOT  #####################
    output$CountDistribBox <- renderPlotly({
      if (length(data$var$CountData) > 0) {
        data <- as.matrix(data$var$CountData)    # set the data to use
        
        cpm <- log2(data + 1)   # counts 
        cpm_stack <- data.frame(stack(cpm))
        
        group <- data.frame("col" = rownames(data$var$select),  # with respect to groups
                            "group" = data$var$select$group)
        
        data <- left_join(cpm_stack, group, by = "col")  # to plot with respect to groups 
        data <- arrange(data, group)
        
        p <- plot_ly(  # plot 
          data,
          x = ~ col,
          y = ~ value,
          type = "box",
          split = ~ group,
          color = ~ group  # color with respect to groups
        ) %>% layout(
          title = input$CountDistribTitle,
          xaxis = list(title = input$CountDistribXlab, categoryarray = "array", categoryarray = ~col),
          yaxis = list(title = "log<sub>2</sub>(Count + 1)")
        )
        p
      } else {
        return()
      }
    })
    
    output$CountDistrib <- renderUI({
      if (data$var$control) {  # if data where imported and everything is ok then it can provides to plots
        tagList(fluidRow(
          column(
            3,
            textInput(   # set of parameters 
              inputId = ns("CountDistribTitle"),
              label = "Title",
              value = "Raw Count",
              placeholder = "Raw Count"
            ),
            textInput(
              inputId = ns("CountDistribXlab"),
              label = "X label",
              value = "Sample",
              placeholder = "Sample"
            )
          ),
          column(
            9,
            plotlyOutput(ns("CountDistribBox"), height = 800) %>% withSpinner()  # render 
          )
        ))
      } else {   # if no data then message 
        helpText("No data for ploting.")
      }
    })
    
    ################### HEATMAP #####################
    output$rawheatmap <- renderPlotly({
      if (length(data$var$CountData) > 0) {
        data <- data$var$CountData # data selection 
        data <- data.frame(1 - cor(data, method = input$correlation)) # with the chosen method of correlation 
        heatmaply( #heatmap
          data,
          hclust_method = "complete",
          labRow = rownames(data),
          labCol = colnames(data),
          colors = rev(RdYlGn(500))
        )
        
      }else {
        return()
      }
    })
    
    # Render UI 
    output$clustUI <- renderUI({
      if (data$var$control) { # if data and no errors then run parameters and plot
        tagList(fluidRow(
          column( #parameter
            3,
            selectInput(
              inputId = ns("correlation"),
              label = "Distance Measure",
              choices = c("Spearman" = "spearman",
                          "Pearson" = "pearson")
            ),
            tags$div(  # instruction
              HTML('<div class="panel panel-primary">
                    <div class="panel-heading"> <span style="padding-left:10px"><b> Distance measures </b> </span></div>
                  <div class="panel-body">
                  <style type="text/css">
                  </style>
                  <table class="tg">
                  <tr>
                  <th class="tg-031e"> <span class="label label-primary"> Spearman </span></th>
                  <th class="tg-031e"> Spearman distance is a square of Euclidean distance between two rank vectors.
                  </tr>
                  <tr>
                  <th class="tg-031e"> <span class="label label-primary"> Pearson</span></th>
                  <th class="tg-031e"> Pearson correlation measures the degree of a linear relationship between two profiles.
                  </tr>
                  </table>
                  </div>
                  </div>'))
          ),
          column(9, plotlyOutput(ns("rawheatmap"),height = 600, width = 800) %>% withSpinner() # render heatmap
          )
        ))
      } else { # if no data, then message 
        helpText("No data for ploting.")
      }
    })
    
    ################### PCA #####################
    
    # 2D Plot 
    output$pcaPlotObject2d <- renderPlotly({
      if (length(data$var$CountData) > 0) {
        data <- as.matrix(data$var$CountData)
        data <- log1p(data) # data selection 
        data <- data[apply(data, 1, var) != 0, ] # selection over counts 
        data <- t(data[order(apply(data, 1, var), decreasing = TRUE)[1:100], ])
        data.pca.all <- prcomp(data,center = T, scale. = T) #pca 
        data <- data.frame(data.pca.all$x)
        data$name <- rownames(data)
        group <- data$var$select
        group$name <- rownames(data$var$select)
        data <- left_join(x = data, y = group, by = "name") # to perform over groups 
        p <- plot_ly(  # plot 
          data = data,
          x = ~ PC1,
          y = ~ PC2,
          color = ~ factor(group),
          text = ~ name,
          textposition = "top right",
          type = "scatter",
          mode = "markers+text"
        ) %>%
          layout(title = "PCA 2D Plot")
        p
      } else {
        return(0)
      }
    })
    
    # 3D Plot
    output$pcaPlotObject3d <- renderPlotly({
      if (length(data$var$CountData) > 0) {
        data <- as.matrix(data$var$CountData)
        data <- log1p(data) # data selection 
        data <- t(data[apply(data, 1, var) != 0, ]) # selection over counts
        data.pca.all <- prcomp(data,center = T,scale. = T) # pca
        
        data <- data.frame(data.pca.all$x)
        data$name <- rownames(data)
        group <- data$var$select
        group$name <- rownames(data$var$select)
        data <- left_join(x = data, y = group, by = "name") # to perform the pca over groups
        p <- plot_ly(   #plot
          data = data,
          x = ~ PC1,
          y = ~ PC2,
          z = ~ PC3,
          color = ~ factor(group),
          text = ~ name,
          textposition = "top right",
          type = "scatter3d",
          mode = "markers+text"
        ) %>%
          layout(title = "PCA 3D Plot")
        p
      } else {
        return(0)
      }
    })
    
    
    # render pca
    output$pcaUI <- renderUI({
      if (data$var$control) {
        tabsetPanel(  # render plots 
          tabPanel(title = "2D Plot", plotlyOutput(ns("pcaPlotObject2d"), width = 1200, height = 600) %>% withSpinner()),
          tabPanel(title = "3D Plot", plotlyOutput(ns("pcaPlotObject3d"), width = 1200, height = 600) %>% withSpinner())
        )
      } else { # if no data, message
        helpText("No data for ploting.")
      }
    })
    

  })
}
