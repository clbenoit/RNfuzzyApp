# server-heatmap.R

runHeatmap <- reactiveValues(runHeatmapValue = FALSE, height = 300)

# Generate heatmap parameters panel ---------------------------------------


observeEvent(input$sider, {
  if (input$sider == "heatmapTab") {
    output$heatmapParameter <- renderUI({
      "By FDR" = tagList(
        tagList(
          sliderInput(
            "heatmapFDR",
            "Select genes by FDR Cut-off",
            min = 0.01,
            max = 1,
            value = 0.01
          )),
        
        radioGroupButtons(
          inputId = "heatmapData",
          label = "Source",
          choices = c("Original" = "o",
                      "Normalized" = "n"),
          justified = TRUE,
          status = "primary"
        ),
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
            "ward.D" = "ward.D",
            "ward.D2" = "ward.D2",
            "Single" = "single",
            "Complete" = "complete",
            "UPGMA" = "average",
            "WPGMA" = "mcquitty",
            "WOGMC" = "median",
            "UPGMC" = "centroid"
          ),
          selected = "complete"
        ),
        materialSwitch(
          inputId = "heatmapLogTrans",
          label = "log(1+x) transform",
          value = TRUE,
          right = TRUE,
          status = "primary"
        ),
        materialSwitch(
          inputId = "heatmapNor",
          label = "Normalization",
          value = FALSE,
          right = TRUE,
          status = "primary"
        ),
        radioGroupButtons(
          inputId = "heatmapScale",
          label = "Scale",
          choices = list(
            "None" = "none",
            "Row" = "row",
            "Column" = "column"
          ),
          justified = TRUE,
          status = "primary"
        ),
        selectInput(
          inputId = "colorSelectionMethod",
          label = "Color Selection Method",
          choices = c("Color map", "Two colors", "Three colors")
        ),
        uiOutput("heatmapColorSelectionPanel"),
        tags$b("Color Preview"),
        plotOutput("colorPreview", height = "20px"),
        numericInput(
          inputId = "heatmapHeight",
          label = "Height of Heatmap",
          value = 500,
          min = 100
        ),
        do.call(actionBttn, c(
          list(
            inputId = "heatmapRun",
            label = "Run Heatmap",
            icon = icon("play")
          )
        )))
    })}})

# According to color selection method, render color selection part --------


observeEvent(input$colorSelectionMethod, {
  if (input$colorSelectionMethod == "Color map") {
    output$heatmapColorSelectionPanel <- renderUI({
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
      )
    })
  }
  if (input$colorSelectionMethod %in% c("Two colors", "Three colors")) {
    output$heatmapColorSelectionPanel <- renderUI({
      tagList(
        spectrumInput(
          inputId = "heatmapTwoColorLow",
          label = "Low",
          choices = list(
            list(
              "blue",
              'black',
              'white',
              'blanchedalmond',
              'steelblue',
              'forestgreen'
            ),
            as.list(brewer.pal(n = 9, name = "Blues")),
            as.list(brewer.pal(n = 9, name = "Greens")),
            as.list(brewer.pal(n = 11, name = "Spectral")),
            as.list(brewer.pal(n = 8, name = "Dark2"))
          ),
          options = list(`toggle-palette-more-text` = "Show more")
        ),
        if (input$colorSelectionMethod == "Three colors") {
          spectrumInput(
            inputId = "heatmapTwoColorMiddle",
            label = "Middle",
            choices = list(
              list(
                "white",
                'black',
                'blanchedalmond',
                'steelblue',
                'forestgreen'
              ),
              as.list(brewer.pal(n = 9, name = "Blues")),
              as.list(brewer.pal(n = 9, name = "Greens")),
              as.list(brewer.pal(n = 11, name = "Spectral")),
              as.list(brewer.pal(n = 8, name = "Dark2"))
            ),
            options = list(`toggle-palette-more-text` = "Show more")
          )
        },
        spectrumInput(
          inputId = "heatmapTwoColorHigh",
          label = "High",
          choices = list(
            list(
              "red",
              'black',
              'white',
              'blanchedalmond',
              'steelblue',
              'forestgreen'
            ),
            as.list(brewer.pal(n = 9, name = "Blues")),
            as.list(brewer.pal(n = 9, name = "Greens")),
            as.list(brewer.pal(n = 11, name = "Spectral")),
            as.list(brewer.pal(n = 8, name = "Dark2"))
          ),
          options = list(`toggle-palette-more-text` = "Show more")
        )
      )
    })
  }
})

# Color palette reactive value --------------------------------------------


colorPanel <- reactive({
  colorPal <- c("white")
  # Create color palette
  if (input$colorSelectionMethod == "Color map" && length(input$heatmapColor) > 0) {
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
  if (input$colorSelectionMethod == "Two colors" && length(input$heatmapTwoColorLow) > 0) {
    colorPal <-
      colorRampPalette(c(input$heatmapTwoColorLow, input$heatmapTwoColorHigh))(20)
  }
  if (input$colorSelectionMethod == "Three colors" && length(input$heatmapTwoColorLow) > 0) {
    colorPal <-
      colorRampPalette(
        c(
          input$heatmapTwoColorLow,
          input$heatmapTwoColorMiddle,
          input$heatmapTwoColorHigh
        )
      )(20)
  }
  
  colorPal
})

# Render a plot of color preview 


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



# Create heatmaply object and DataTable object 

observeEvent(input$heatmapRun, {
  data.cl <- variables$groupListConvert
  
  # Using Original Dataset or Normalized Dataset.
  if (input$heatmapData == "o") {
    data <- variables$CountData[data.cl != 0]
  } else {
    data <- variables$norData
  }
  data.cl <- data.cl[data.cl != 0]
  
  # Select DEGs (Row)
      selectedListForHeatmap <-
        row.names(data) %in% resultTable()[resultTable()$q.value <= input$heatmapFDR,]$gene_id
      
      heatmapTitle <-
        paste0("Heatmap of gene expression (q.value < ",
               input$heatmapFDR,
               ", ",
               sum(selectedListForHeatmap),
               "DEGs)")

    
    data <- data[selectedListForHeatmap, ]
    
    
    colorPal <- colorPanel()
    
    dataBackup <- t(data)
    
    # Create Plotly object
    output$heatmap <- renderPlotly({
      isolate({
        runHeatmap$height <- input$heatmapHeight
        # Log transform and normalization
        if (input$heatmapLogTrans == TRUE) {
          dataBackup <-  log1p(dataBackup)
        }
        if (input$heatmapNor == TRUE) {
          dataBackup <- heatmaply::normalize(dataBackup)
        }
        
        p <- heatmaply(
          dataBackup,
          k_row = length(variables$groupList),
          colors = colorPal,
          dist_method = input$heatmapDist,
          hclust_method = input$heatmapCluster,
          xlab = "Gene",
          ylab = "Sample",
          main = heatmapTitle,
          margins = c(150, 100, 40, 20),
          scale = input$heatmapScale,
          labCol = colnames(dataBackup),
          labRow = row.names(dataBackup)
        )
        
        variables$heatmapObject <- p
        p
        
      })
    })
    
    
    updateProgressBar(
      session = session,
      id = "heatmapProgress",
      title = "All done",
      value = 100
    )
    
    runHeatmap$runHeatmapValue <- input$heatmapRun
    
    closeSweetAlert(session = session)
    sendSweetAlert(session = session,
                   title = "Completed!",
                   type = "success")
  
})

# Render interactive heatmap plot -----------------------------------------


output$heatmapPlot <- renderUI({
  if (runHeatmap$runHeatmapValue) {
    plotlyOutput("heatmap", height = runHeatmap$height) %>% withSpinner()
  }
  else{
    helpText("Click [Generate Heatmap] to plot the heatmap first.")
  }
})