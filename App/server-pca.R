# server-pca.R

runPCA <- reactiveValues(runPCAValue = FALSE)

observeEvent(input$sider, {
  if (input$sider == "pcaTab") {
    output$pcaParameter <- renderUI({
      tagList(
          tipify(sliderInput(
            "pcFDR",
            "FDR Cut-off",
            min = 0.01,
            max = 1,
            value = 0.05
          ),
          title = "Genes under the FDR cut-off will be used for PCA. Set 1 for exploratory analysis with all genes."),

        do.call(actionBttn, c(
          list(
            inputId = "pcRun",
            label = "Run PCA",
            icon = icon("play")
          )
        ))
      )
    })
  }
})



# [Run PCA] button has been clicked, then run the whole PCA Analysis

observeEvent(input$pcRun, {
  runPCA$runPCAValue <- input$pcRun
  variables$pcaParameter <- list("pcData" = input$pcData,
                                 "pcFDR" = input$pcFDR,
                                 "pcTransform" = T,
                                 "pcCenter" = T,
                                 "pcScale" = T)
  tcc <- variables$tccObject
  
  # Using Original Dataset or Normalized Dataset.

  data <- getNormalizedData(tcc)
  
  result <- getResult(tcc)
  
  # Select DEGs (Row)

    data <- data[result$q.value <= input$pcFDR,]
  
  # PCA processing
    data <- t(log1p(data))
  data.pca <- prcomp(data[, apply(data, 2, var) != 0],
                     center = T,
                     scale. = T)
  
  variables$data.pca <- data.pca
  })



# Render PCA 2d Plot
output$pca2d <- renderPlotly({
  if (length(variables$data.pca) > 0) {
    tcc <- variables$tccObject
    data.pca <- variables$data.pca
    data <- data.frame(data.pca$x)
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
      layout(title = "PCA Plot (2D)")
    variables$pca2d <- p
    p
  } else {
    return()
  }
})

# Render 2D Plot UI 
output$pca2dPlotUI <- renderUI({
  if (runPCA$runPCAValue) {
    plotlyOutput("pca2d") %>% withSpinner()
  } else {
    helpText("Click [Run PCA] to compute first.")
  }
})

# Scatter Plot 3D plotly object 
output$pca3d <- renderPlotly({
  if (length(variables$data.pca) > 0) {
    tcc <- variables$tccObject
    data.pca <- variables$data.pca
    data <- data.frame(data.pca$x)
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
      layout(title = "PCA Plot (3D)")
    variables$pca3d <- p
    p
  } else {
    return()
  }
})

# Render 3D Plot UI 
output$pca3dPlotUI <- renderUI({
  if (runPCA$runPCAValue) {
    plotlyOutput("pca3d") %>% withSpinner()
  } else {
    helpText("Click [Run PCA] to compute first.")
  }
})

