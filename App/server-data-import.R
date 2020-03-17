# server-data-import.R



observeEvent(input$uploadCountData, {
  tryCatch({
    var$CountData <-
      data.frame(fread(input$uploadCountData$datapath), row.names = 1)
    var$tccObject <- NULL
    v$importActionValue <- FALSE
  },
  error = function(e) {
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

})


# Render a table of raw count data, adding color

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
      searchHighlight = TRUE,
      orderClasses = TRUE
    )
  )
})


# Render DataTable of row data count

output$showTable <- renderUI({
  if (nrow(datasetInput()) == 0) {
    tags$p("No data to show. Upload your dataset.")
  } else {
    DT::dataTableOutput('table')
  }
})

observeEvent(input$confirmedGroupList, {
  if (nrow(datasetInput()) == 0) {
    sendSweetAlert(
      session = session,
      title = "ERROR",
      text = "Please input count data table!",
      type = "error"
    )
    return()
  }
  if (input$groupSelect == "") {
    sendSweetAlert(
      session = session,
      title = "ERROR",
      text = "Please input group information!",
      type = "error"
    )
    return()
  }
  
  tryCatch({
    
    group <- fread(input$groupSelect, header = FALSE)
    var$groupList <-
      lapply(unique(group$V2), function(x) {
        group[group$V2 == x, ]$V1
      })
    names(var$groupList) <- unique(group$V2)
    
    data.list <- rep(0, ncol(var$CountData))
    
    # Convert the input of group information to
    # a specific format for normalization.
    convertion <- function(x, df) {
      grep(x, colnames(df))
    }
    
    for (i in 1:length(var$groupList)) {
      data.list[unlist(lapply(var$groupList[[i]], convertion, df = var$CountData))] = names(var$groupList[i])
    }

    var$groupListConvert <- data.list
    selectedgroups = input$confirmedGroupList
    tmprem = match(as.character(rownames(var$CountData)[which(!(var$groupList%in%selectedgroups))]),colnames(var$CountData))
    tmpkeep = setdiff(1:ncol(var$CountData),tmprem)
    var$CountData <- var$CountData[,tmpkeep]
    
    # Create a TCC Object 
    tcc <-
      new("TCC", var$CountData[data.list != 0], data.list[data.list != 0])
    var$tccObject <- tcc
    var$count.data <- tcc$count
    
    closeSweetAlert(session = session)
    sendSweetAlert(
      session = session,
      title = "DONE",
      text = "Group labels were successfully assigned.",
      type = "success"
    )
    
    v$importActionValue <- input$confirmedGroupList
  },
  error = function(e) {
    sendSweetAlert(
      session = session,
      title = "ERROR",
      text = "Check your group information format!",
      type = "error"
    )
    return()
  },
  warning = function(w) {
    sendSweetAlert(
      session = session,
      title = "Group error!",
      text = "Check your group information format!",
      type = "error"
    )
    return()
  })
})


datasetInput <- reactive({
  var$CountData
})



output$DataSummary <- renderUI({
  dt <- datasetInput()
  
  rowCount <- nrow(dt)
  groupCount <- length(var$groupList)
  groupText <- sapply(var$groupList, length)
  if (length(groupText) > 0) {
    gText <- paste0(names(groupText), ": ", groupText, ';', collapse = "\n")
  } else {
    gText <- NULL
  }
  
  data <- var$CountData
  data.list <- var$groupListConvert
  
  tagList(
    tipify(
      tags$p(tags$b("N", tags$sub("gene")), ":", rowCount),
      title = "Number of Genes",
      placement = "left"
    ),
    tipify(
      tags$p(tags$b("N", tags$sub("group")), ": ", groupCount),
      title = "Number of Groups",
      placement = "left"
    ),
    tipify(
      tags$p(tags$b("NR"), ": ", gText),
      title = "Number of Replicates",
      placement = "left"
    )
  )
})

v <- reactiveValues(importActionValue = FALSE)

################### BOXPLOT  #####################
output$sampleDistributionBox <- renderPlotly({
  if (length(var$tccObject) > 0) {
    tcc <- var$tccObject
    data <- tcc$count
    
    cpm <- log2(data + 1)
    cpm_stack <- data.frame(stack(cpm))

    group <-
      data.frame("col" = rownames(tcc$group),
                 "group" = tcc$group$group)

    data <- left_join(cpm_stack, group, by = "col")
    data <- arrange(data, group)

    p <- plot_ly(
      data = data,
      x = ~ col,
      y = ~ value,
      type = "box",
      split = ~ group,
      color = ~ group
    ) %>% layout(
      title = input$sampleDistributionTitle,
      xaxis = list(title = input$sampleDistributionXlab, categoryarray = "array", categoryarray = ~col),
      yaxis = list(title = input$sampleDistributionYlab)
    )
    var$sampleDistributionBar <- p
    p
  } else {
    return()
  }
})




# render UI 
output$sampleDistributionBoxPanel <- renderUI({
  if (v$importActionValue) {
    tagList(fluidRow(
      column(
        3,
        textInput(
          inputId = "sampleDistributionTitle",
          label = "Title",
          value = "Raw Count",
          placeholder = "Raw Count"
        ),
        textInput(
          inputId = "sampleDistributionXlab",
          label = "X label",
          value = "Sample",
          placeholder = "Sample"
        ),
        textInput(
          inputId = "sampleDistributionYlab",
          label = "Y label",
          value = "log<sub>2</sub>(Count + 1)",
          placeholder = "log<sub>2</sub>(Count + 1)"
        )
      ),
      column(
        9,
        plotlyOutput("sampleDistributionBox") %>% withSpinner()
      )
    ))
  } else {
    helpText("No data for ploting.")
  }
})

################### HEATMAP #####################
output$rawheatmap <- renderPlotly({
  if (length(var$tccObject) > 0) {
    tcc <- var$tccObject
    data <- tcc$count[rowSums(tcc$count) > 0,]
    data <- data.frame(1 - cor(data, method = input$correlation))
    data.list.count <- length(unique(tcc$group$group))
    heatmaply(
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
  if (v$importActionValue) {
    tagList(fluidRow(
      column(
        3,
        selectInput(
          inputId = "correlation",
          label = "Distance Measure",
          choices = c("Spearman" = "spearman",
                      "Pearson" = "pearson")
        ),
        tags$div(
          HTML('<div class="panel panel-primary">
                    <div class="panel-heading"> <span style="padding-left:10px"><b> Distance measures </b> </span></div>
                  <div class="panel-body">
                  <style type="text/css">
                  .tg {
                  border-collapse: collapse;
                  border-spacing: 0;
                  border: none;
                  }
                  .tg td {
                  font-family: Arial, sans-serif;
                  font-size: 14px;
                  padding: 10px 5px;
                  border-style: solid;
                  border-width: 0px;
                  overflow: hidden;
                  word-break: normal;
                  }
                  .tg .tg-s6z2 {
                  text-align: center
                  }
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
      column(9, plotlyOutput("rawheatmap",height = 600) %>% withSpinner()
             )
    ))
  } else {
    helpText("No data for ploting.")
  }
})

 ################### PCA #####################

# 2D Plot 
output$pcaPlotObject2d <- renderPlotly({
  if (length(var$tccObject) > 0) {
    tcc <- var$tccObject
    data <- log1p(tcc$count)
    data <- data[apply(data, 1, var) != 0, ]
    if(!is.na(input$pcaTopGene) & input$pcaTopGene < nrow(data)){
      data <- t(data[order(apply(data, 1, var), decreasing = TRUE)[1:input$pcaTopGene], ])
    }
    data.pca.all <- prcomp(data,center = T,scale. = T)
    data <- data.frame(data.pca.all$x)
    data$name <- rownames(data)
    group <- tcc$group
    group$name <- rownames(group)
    data <- left_join(x = data, y = group, by = "name")
    p <- plot_ly(
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
  if (length(var$tccObject) > 0) {
    tcc <- var$tccObject
    data <- log1p(tcc$count)
    data <- data[apply(data, 1, var) != 0, ]
    if(!is.na(input$pcaTopGene) & input$pcaTopGene < nrow(data)){
      data <- t(data[order(apply(data, 1, var), decreasing = TRUE)[1:input$pcaTopGene], ])
    }
    data.pca.all <- prcomp(data,center = T,scale. = T)
    
    data <- data.frame(data.pca.all$x)
    data$name <- rownames(data)
    group <- tcc$group
    group$name <- rownames(group)
    data <- left_join(x = data, y = group, by = "name")
    p <- plot_ly(
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
  if (v$importActionValue) {
    tagList(fluidRow(
      column(
        3,
          numericInput(
            inputId = "pcaTopGene",
            label = "Top Gene",
            value = 100,
            min = 2,
            step = 1
          ),
        tags$div(
          HTML("Choose the number of genes for render. "))
        ),
      column(9,
             tabsetPanel(
               tabPanel(title = "2D Plot", plotlyOutput("pcaPlotObject2d") %>% withSpinner()),
               tabPanel(title = "3D Plot", plotlyOutput("pcaPlotObject3d") %>% withSpinner())
               
             ))
    ))
  } else {
    helpText("No data for ploting.")
  }
})
