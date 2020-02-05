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


# color selection method
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
        sliderInput(
          "heatmapFDR",
          "FDR Cut-off",
          min = 0.01,
          max = 1,
          value = 0.01
        ),
        textOutput("heatmapGeneCountPreview")
      )
  )
})


colorPanel <- reactive({  # Color palette 
  colorPal <- c("white")
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
  data.cl <- variables$groupListConvert
  data <- variables$norData
  data.cl <- data.cl[data.cl != 0]
  selectedListForHeatmap <- #deg select
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
  
  # Plotly obj
  output$heatmap <- renderPlotly({
    isolate({
      runHeatmap$height <- input$heatmapHeight
      dataBackup <-  log1p(dataBackup)
      dataBackup <- heatmaply::normalize(dataBackup)
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
        scale = "none",
        labCol = colnames(dataBackup),
        labRow = row.names(dataBackup)
      )
      
      variables$heatmapObject <- p
      p
      
    })
  })
  runHeatmap$runHeatmapValue <- input$heatmapRun
  closeSweetAlert(session = session)
  sendSweetAlert(session = session,
                 title = "Completed! Wait patiently",
                 type = "success")
  
})

# remder final heatmap (little time consuming)

output$heatmapPlot <- renderUI({
  if (runHeatmap$runHeatmapValue) {
    plotlyOutput("heatmap", height = runHeatmap$height) %>% withSpinner()
  }
  else{
    helpText("Click [Generate Heatmap] to plot the heatmap first.")
  }
})