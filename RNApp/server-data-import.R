# server-data-import.R

# Convert the input of group information to
# a specific format for normalization.

convertion <- function(x, df) {
  grep(x, colnames(df))
}


observeEvent(input$uploadCountData, {
  tryCatch({
    variables$CountData <-
      data.frame(fread(input$uploadCountData$datapath), row.names = 1)
    variables$tccObject <- NULL
    v$importActionValue <- FALSE
    showNotification("Received uploaded file.", type = "message")
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

datasetInput <- reactive({
  variables$CountData
})


# Render a table of raw count data, adding color

output$table <- DT::renderDataTable({
  df <- datasetInput()
  brks <-
    quantile(df %>% select_if(is.numeric),
             probs = seq(.05, .95, .05),
             na.rm = TRUE)
  
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
  ) %>%
    formatStyle(names(df %>% select_if(is.numeric)), backgroundColor = styleInterval(brks, head(Blues(40), n = length(brks) + 1)))
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
    variables$groupList <-
      lapply(unique(group$V2), function(x) {
        group[group$V2 == x, ]$V1
      })
    names(variables$groupList) <- unique(group$V2)
    
    data.list <- rep(0, ncol(variables$CountData))
    
    for (i in 1:length(variables$groupList)) {
      data.list[unlist(lapply(variables$groupList[[i]], convertion, df = variables$CountData))] = names(variables$groupList[i])
    }
    
    # Storage convert group list to local
    variables$groupListConvert <- data.list
    
    # Create TCC Object 
    tcc <-
      new("TCC", variables$CountData[data.list != 0], data.list[data.list != 0])
    variables$tccObject <- tcc
    variables$count.data <- tcc$count
    
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

output$DataSummary <- renderUI({
  dt <- datasetInput()
  
  rowCount <- nrow(dt)
  groupCount <- length(variables$groupList)
  groupText <- sapply(variables$groupList, length)
  if (length(groupText) > 0) {
    gText <- paste0(names(groupText), ": ", groupText, collapse = "\n")
  } else {
    gText <- NULL
  }
  
  data <- variables$CountData
  data.list <- variables$groupListConvert
  cName <- unlist(variables$groupList)
  
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
  if (length(variables$tccObject) > 0) {
    tcc <- variables$tccObject
    data <- tcc$count
    
    if (input$sampleDistributionFilterLow != -1) {
      data <- data[rowSums(data) > input$sampleDistributionFilterLow,]
    }
    
    cpm <- log2(data + 1)
    cpm_stack <- data.frame(stack(cpm))

    group <-
      data.frame("col" = rownames(tcc$group),
                 "group" = tcc$group$group)

    data <- left_join(cpm_stack, group, by = "col")
    data <- arrange(data, group)
    
    print(head(data))
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
    variables$sampleDistributionBar <- p
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
        sliderInput(inputId = "sampleDistributionFilterLow", 
                    label = "Filter low genes",
                    min = -1, 
                    max = 20, 
                    value = 0),
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
    helpText("No data for ploting. Please import dataset and assign group information first.")
  }
})

################### HEATMAP #####################
output$clustPlotObject <- renderPlotly({
  if (length(variables$tccObject) > 0) {
    tcc <- variables$tccObject
    data <- tcc$count[rowSums(tcc$count) > 0,]
    data <- data.frame(1 - cor(data, method = input$clustCor))
    data.list.count <- length(unique(tcc$group$group))
    heatmaply(
      data,
      k_col = data.list.count,
      k_row = data.list.count,
      hclust_method = "complete",
      labRow = rownames(data),
      labCol = colnames(data),
      colors = rev(GnBu(500))
    )
  } else {
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
          inputId = "clustCor",
          label = "Distance Measure",
          choices = c("Spearman" = "spearman",
                      "Pearson" = "pearson")
        ), tags$div(
          HTML("- Spearman distance is a square of Euclidean distance between two rank vectors.</br></br>
                - Pearson correlation measures the degree of a linear relationship between two profiles."))
      ),
      column(9, plotlyOutput("clustPlotObject") %>% withSpinner())
    ))
  } else {
    helpText("No data for ploting. Please import dataset and assign group information first.")
  }
})

 ################### PCA #####################

# 2D Plot 
output$pcaPlotObject2d <- renderPlotly({
  if (length(variables$tccObject) > 0) {
    tcc <- variables$tccObject
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
    variables$pca2d <- p
    p
  } else {
    return(0)
  }
})

# 3D Plot
output$pcaPlotObject3d <- renderPlotly({
  if (length(variables$tccObject) > 0) {
    tcc <- variables$tccObject
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
    variables$pca3d <- p
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
    helpText("No data for ploting. Please import dataset and assign group information first.")
  }
})

