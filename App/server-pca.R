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



# launch

observeEvent(input$pcRun, {
  runPCA$runPCAValue <- input$pcRun
  tcc <- var$tccObject
  data <- getNormalizedData(tcc)
  result <- getResult(tcc)
  data <- data[result$q.value <= input$pcFDR,] 
  data <- t(log1p(data)) 
  data.pca <- prcomp(data[, apply(data, 2, var) != 0],
                     center = T,
                     scale. = T)
  
  var$data.pca <- data.pca
  })



# 2D plotly object
output$D2pca <- renderPlotly({
  if (length(var$data.pca) > 0) {
    tcc <- var$tccObject
    data.pca <- var$data.pca
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
    p
  } else {
    return()
  }
})

#  3D plotly object 
output$D3pca <- renderPlotly({
  if (length(var$data.pca) > 0) {
    tcc <- var$tccObject
    data.pca <- var$data.pca
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
    p
  } else {
    return()
  }
})


# Render 2D Plot UI 
output$D2PlotUI <- renderUI({
  if (runPCA$runPCAValue) {
    plotlyOutput("D2pca") %>% withSpinner()
  } else {
    helpText("Click [Run PCA] to compute first.")
  }
})

# Render 3D Plot UI 
output$D3PlotUI <- renderUI({
  if (runPCA$runPCAValue) {
    plotlyOutput("D3pca") %>% withSpinner()
  } else {
    helpText("Click [Run PCA] to compute first.")
  }
})

