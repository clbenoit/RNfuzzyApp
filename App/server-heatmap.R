# server-heatmap.R

runHeatmap <- reactiveValues(runHeatmapValue = FALSE, height = 300)

# parameters


observeEvent(input$sider, {
  if (input$sider == "heatmapTab") {
    output$heatmapParameter <- renderUI({
      tagList(
        radioGroupButtons(
          inputId = "heatmapGeneSelectType",
          label = "Select Genes",
          choices = c("By List" = "By list",
                      "By FDR" = "By FDR"),
          justified = TRUE,
          status = "primary"
        ),
        uiOutput("heatmapSelectGene"),
        selectInput(
          "heatmapDist",
          "Distance Measure",
          choices = list(
            "Euclidean" = "euclidean",
            "Maximum" = "maximum",
            "Manhattan" = "manhattan",
            "Canberra" = "canberra",
            "Binary" = "binary",
            "Minkowski" = "minkowski"
          ),
          selected = "euclidean"
        ),
        selectInput(
          "heatmapCluster",
          "Agglomeration Method",
          choices = list(
            "ward.D2" = "ward.D2",
            "Single" = "single",
            "Complete" = "complete",
            "UPGMA" = "average"
          ),
          selected = "complete"
        ),
        sliderInput(
          "clusterswanted",
          "Clusters Wanted",
          min = 0,
          max = 10,
          value = 5,
          step = 1
        ),
        tagList(
          selectInput(
            "heatmapColor",
            "Choose Colormap",
            choices = list(
              "PiYG",
              "PRGn",
              "BrBG",
              "PuOr",
              "OrRd",
              "Oranges",
              "RdGy",
              "RdBu",
              "RdYlBu",
              "RdYlGn",
              "Spectral",
              "coolwarm"
            ),
            selected = "RdYlGn"
          )
        ),
        tags$b("Color Preview"),
        plotOutput("colorPreview", height = "20px"),
        do.call(actionBttn, c(
          list(
            inputId = "heatmapRun",
            label = "Run Heatmap",
            icon = icon("play")
          )
        )))
    })}})



# gene list in param
output$heatmapSelectGene <- renderUI({
  switch(
    input$heatmapGeneSelectType,
    "By list" = textAreaInput(
      "heatmapTextList",
      "Paste Gene List",
      rows = 5,
      placeholder = "Input gene's name (first column in the dataset), one gene per line."
    ),
    "By FDR" =
      tagList(
        numericInput(
          inputId = "heatmapFDR",
          label = "FDR Cut-off",
          min = 0.00001,
          value = 0.001,
          max = 0.01,
          step = 0.001
        ),
      )
  )
})


colorPanel <- reactive({  # Color palette
  colorPal <- c("white")
  if (length(input$heatmapColor) > 0) {
    colorPal <- switch(
      input$heatmapColor,
      "PiYG" = PiYG(20),
      "PRGn" = PRGn(20),
      "BrBG" = BrBG(20),
      "PuOr" = PuOr(20),
      "OrRd" = OrRd(20),
      "Oranges" = Oranges(20),
      "RdGy" = RdGy(20),
      "RdBu" = RdBu(20),
      "RdYlBu" = RdYlBu(20),
      "RdYlGn" = RdYlGn(20),
      "Spectral" = Spectral(20),
      "coolwarm" = cool_warm(20)
    )
  }
  colorPal
})

#palette preview
output$colorPreview <- renderPlot({
  colorPal <- colorPanel()
  op <- par(mar = c(0.5, 0, 0, 0))
  plot(
    c(0, length(colorPal)),
    c(0, 1),
    type = "n",
    xlab = "",
    ylab = "",
    ann = F,
    bty = "n",
    xaxt = "n",
    yaxt = "n"
  )
  i <- 0:(length(colorPal) - 1)
  rect(0 + i, 0, 1 + i, 1, col = colorPal, lwd = 0)
  par(op)
})


# heatmaply obj
observeEvent(input$heatmapRun, {
  data.list <- var$groupListConvert
  data <- var$norData
  data.list <- data.list[data.list != 0]
  if (input$heatmapGeneSelectType == "By list") {
    selectedListForHeatmap <-
      row.names(data) %in% unlist(strsplit(x = input$heatmapTextList, split = '[\r\n]'))
    heatmapTitle <- "Heatmap of specific genes"
  }

  if (input$heatmapGeneSelectType == "By FDR") {

      selectedListForHeatmap <-
        row.names(data) %in% resultTable()[resultTable()$q.value <= input$heatmapFDR,]$gene_id
      heatmapTitle <-
        paste0("Heatmap of gene expression (q.value < ",
               input$heatmapFDR,
               ", ",
               sum(selectedListForHeatmap),
               "DEGs)")
    }
  
  data <- data[selectedListForHeatmap, ]
  
  
  if (nrow(data) == 0) {
    sendSweetAlert(
      session = session,
      title = "ERROR",
      text = "Genes list is empty!",
      type = "error"
    )
    return()
  } else {
    showNotification(paste0(dim(data)[1], " DEGs, ", dim(data)[2], " sample will be used."))
    showNotification("Generating, please be patient...", type = "message")
  }
  colorPal <- colorPanel()
  datat <- t(data)
  datal <-  log1p(datat)
  datan <- heatmaply::normalize(datal)
  dend <- hclust(dist(t(datan), method = input$heatmapDist), method = input$heatmapCluster)
  cut <- cutree(dend, k = input$clusterswanted)
  #cute <- as.data.frame(cut, row.names = t(datan)[1])
  cute <- as.data.frame(cut)
  cute$gene_id <- rownames(cute)
  colnames(cute) <- c("cluster","gene_id")
  rownames(cute) <- NULL
  runHeatmap$height <- 600

  output$heatmap <- renderPlotly({
  
      p <- heatmaply(
        datan,
        colors = colorPal,
        k_col = input$clusterswanted,
        dist_method = input$heatmapDist,
        hclust_method = input$heatmapCluster,
        xlab = "Gene",
        ylab = "Sample",
        main = heatmapTitle,
        margins = c(150, 100, 40, 20),
        scale = "none",
        labCol = colnames(datan),
        labRow = row.names(datan)
      )
      p
  })


  
  #result table 
  output$resultTableInHeatmap <- DT::renderDataTable({
    gene_id <- row.names(data)
    data <- cbind(data, gene_id = gene_id)
    heatdata <- var$result
    heatdata <- var$result[,-2]
    heatdata <- heatdata[,-2]
    heatdata <- heatdata[,-5]
    colnames(heatdata) <- c("gene_id", "PValue","FDR","Rank")
    resultTable <- merge(cute, heatdata, by = "gene_id")
    resultTable <- merge(resultTable, data, by = "gene_id")


    
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
                         filename = "result_heatmap"),
          text = 'Download')),
        scrollX = TRUE,
        pageLength = 10,
        searchHighlight = TRUE,
        orderClasses = TRUE
        
      ),
      
      class = "display")
  }, server = FALSE)
  
  runHeatmap$runHeatmapValue <- input$heatmapRun
  closeSweetAlert(session = session)
  sendSweetAlert(session = session,
                 title = "Completed!",
                 type = "success")
  

  
})

# remder final heatmap 

output$heatmapPlot <- renderUI({
  if (runHeatmap$runHeatmapValue) {
    plotlyOutput("heatmap", height = runHeatmap$height) %>% withSpinner()
  }
  else{
    helpText("Enter parameters to plot the heatmap first.")
  }
})


